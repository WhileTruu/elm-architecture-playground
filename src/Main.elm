module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Main.Page
import Session
import Skeleton
import Url



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }



-- MODEL


type alias Model =
    { session : Session.Data
    , page : Main.Page.Model
    }


init : Session.Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        session : Session.Data
        session =
            Session.init flags url key
    in
    Main.Page.init session |> Tuple.mapBoth (Model session) (Cmd.map PageMsg)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map PageMsg (Main.Page.subscriptions model.page)


view : Model -> Browser.Document Msg
view model =
    Main.Page.view model.page
        |> Skeleton.view PageMsg



-- UPDATE


type Msg
    = NoOp
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | PageMsg Main.Page.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.session.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            let
                session : Session.Data
                session =
                    model.session |> (\a -> { a | url = url })

                ( page, pageCmd ) =
                    Main.Page.stepUrl session model.page
            in
            ( { model | page = page, session = Main.Page.save page session }
            , Cmd.map PageMsg pageCmd
            )

        PageMsg pageMsg ->
            let
                ( page, pageCmd ) =
                    Main.Page.update pageMsg model.page

                session : Session.Data
                session =
                    Main.Page.save page model.session
            in
            ( { model | page = page, session = session }
            , Cmd.map PageMsg pageCmd
            )
