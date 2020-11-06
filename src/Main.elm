module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Main.Page
import Session
import Skeleton
import Url



-- MAIN


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


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        ( session, sharedCmd ) =
            Session.init () url key

        ( page, pageCmd ) =
            Main.Page.init session
    in
    ( Model session page
    , Cmd.batch
        [ Cmd.map PageMsg pageCmd
        ]
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Main.Page.subscriptions model.page
        |> Sub.map PageMsg


view : Model -> Browser.Document Msg
view model =
    Skeleton.view PageMsg (Main.Page.view model.page)



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
                    ( model
                    , Nav.pushUrl model.session.key (Url.toString url)
                    )

                Browser.External href ->
                    ( model
                    , Nav.load href
                    )

        UrlChanged url ->
            let
                original =
                    model.session

                session =
                    { original | url = url }

                ( page, pageCmd ) =
                    Main.Page.init session
            in
            ( { model | page = page, session = Main.Page.save page session }
            , Cmd.map PageMsg pageCmd
            )

        PageMsg pageMsg ->
            let
                ( page, pageCmd ) =
                    Main.Page.update pageMsg model.page

                shared =
                    Main.Page.save page model.session
            in
            ( { model | page = page, session = shared }
            , Cmd.map PageMsg pageCmd
            )
