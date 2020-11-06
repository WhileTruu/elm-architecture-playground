module Main.Page exposing
    ( Model
    , Msg
    , init
    , load
    , save
    , subscriptions
    , update
    , view
    )

import Page exposing (Page)
import Page.Home as Home
import Page.Home2 as Home2
import Page.Poop as Poop
import Session
import Skeleton
import Url.Parser as Parser exposing (Parser, oneOf, s, top)



-- TYPES


type Model
    = Home__Model Home.Model
    | Home2__Model Home2.Model
    | Poop__Model Poop.Model


type Msg
    = Home__Msg Home.Msg
    | Home2__Msg Home2.Msg
    | Poop__Msg Poop.Msg



-- INIT


init : Session.Data -> ( Model, Cmd Msg )
init session =
    let
        parser =
            oneOf
                [ route top
                    (pages.home.init session)
                , route (s "home2")
                    (pages.home2.init session)
                , route (s "poop")
                    (pages.poop.init session)
                ]
    in
    case Parser.parse parser session.url of
        Just answer ->
            answer

        Nothing ->
            pages.poop.init session


route : Parser a b -> a -> Parser (b -> c) c
route parser handler =
    Parser.map handler parser



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update bigMsg bigModel =
    case ( bigMsg, bigModel ) of
        ( Home__Msg msg, Home__Model model ) ->
            pages.home.update msg model

        ( Home2__Msg msg, Home2__Model model ) ->
            pages.home2.update msg model

        ( Poop__Msg msg, Poop__Model model ) ->
            pages.poop.update msg model

        _ ->
            ( bigModel, Cmd.none )



-- BUNDLE - (view + subscriptions)


bundle : Model -> Bundle
bundle bigModel =
    case bigModel of
        Home__Model model ->
            pages.home.bundle model

        Home2__Model model ->
            pages.home2.bundle model

        Poop__Model model ->
            pages.poop.bundle model


view : Model -> Skeleton.Details Msg
view model =
    (bundle model).view ()


subscriptions : Model -> Sub Msg
subscriptions model =
    (bundle model).subscriptions ()


save : Model -> Session.Data -> Session.Data
save model =
    (bundle model).save ()


load : Model -> Session.Data -> ( Model, Cmd Msg )
load model =
    (bundle model).load ()



-- UPGRADING PAGES


type alias Upgraded model msg =
    { init : Session.Data -> ( Model, Cmd Msg )
    , update : msg -> model -> ( Model, Cmd Msg )
    , bundle : model -> Bundle
    }


type alias Bundle =
    { view : () -> Skeleton.Details Msg
    , subscriptions : () -> Sub Msg
    , save : () -> Session.Data -> Session.Data
    , load : () -> Session.Data -> ( Model, Cmd Msg )
    }


upgrade : (model -> Model) -> (msg -> Msg) -> Page model msg -> Upgraded model msg
upgrade toModel toMsg page =
    let
        init_ session =
            page.init session |> Tuple.mapBoth toModel (Cmd.map toMsg)

        update_ msg model =
            page.update msg model |> Tuple.mapBoth toModel (Cmd.map toMsg)

        bundle_ model =
            { view = \_ -> page.view model |> Skeleton.map toMsg
            , subscriptions = \_ -> page.subscriptions model |> Sub.map toMsg
            , save = \_ -> page.save model
            , load = \_ -> load_ model
            }

        load_ model shared =
            page.load shared model |> Tuple.mapBoth toModel (Cmd.map toMsg)
    in
    { init = init_
    , update = update_
    , bundle = bundle_
    }


pages :
    { home : Upgraded Home.Model Home.Msg
    , home2 : Upgraded Home2.Model Home2.Msg
    , poop : Upgraded Poop.Model Poop.Msg
    }
pages =
    { home = Home.page |> upgrade Home__Model Home__Msg
    , home2 = Home2.page |> upgrade Home2__Model Home2__Msg
    , poop = Poop.page |> upgrade Poop__Model Poop__Msg
    }
