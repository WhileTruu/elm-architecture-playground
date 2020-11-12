module Main.Page exposing
    ( Model
    , Msg
    , init
    , load
    , save
    , stepUrl
    , subscriptions
    , update
    , view
    )

import Main.Sheet as Sheet
import Page exposing (Page, Sheet)
import Page.Home as Home
import Page.NotFound as NotFound
import Page.Preconditions as Pre
import Session
import Skeleton
import Url.Parser as Parser exposing (Parser, oneOf)



-- TYPES


type Model
    = Home__Model Home.PageModel
    | NotFound__Model NotFound.PageModel
    | Pre__Model Pre.PageModel


type Msg
    = Home__Msg Home.PageMsg
    | NotFound__Msg NotFound.PageMsg
    | Pre__Msg Pre.PageMsg



-- INIT


init : Session.Data -> ( Model, Cmd Msg )
init session =
    stepUrl session
        (Tuple.first (pages.notFound.init session))


stepUrl : Session.Data -> Model -> ( Model, Cmd Msg )
stepUrl ({ url } as session) model =
    let
        parser : Parser (( Model, Cmd Msg ) -> c) c
        parser =
            oneOf
                [ Parser.map (\_ -> stepHome session model) Home.parser
                , Parser.map (stepNotFound session model) NotFound.parser
                , Parser.map (stepPre session model) Pre.parser
                ]
    in
    case Parser.parse parser url of
        Just answer ->
            answer

        Nothing ->
            pages.notFound.init session


links : List Skeleton.Segment
links =
    [ Skeleton.linkSegment { url = "/", text = "home" }
    , Skeleton.linkSegment { url = "/not-found", text = "not-found" }
    , Skeleton.linkSegment { url = "/pre", text = "pre" }
    ]


stepHome : Session.Data -> Model -> ( Model, Cmd Msg )
stepHome session model =
    case model of
        Home__Model _ ->
            load model session

        _ ->
            pages.home.init session


stepPre : Session.Data -> Model -> ( Model, Cmd Msg )
stepPre session model =
    case model of
        Pre__Model _ ->
            load model session

        _ ->
            pages.pre.init session


stepNotFound : Session.Data -> Model -> ( Model, Cmd Msg )
stepNotFound session model =
    case model of
        NotFound__Model _ ->
            load model session

        _ ->
            pages.notFound.init session



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update bigMsg bigModel =
    case ( bigMsg, bigModel ) of
        ( Home__Msg msg, Home__Model model ) ->
            pages.home.update msg model

        ( NotFound__Msg msg, NotFound__Model model ) ->
            pages.notFound.update msg model

        ( Pre__Msg msg, Pre__Model model ) ->
            pages.pre.update msg model

        ( _, _ ) ->
            ( bigModel, Cmd.none )



-- BUNDLE - (view + subscriptions)


bundle : Model -> Bundle
bundle bigModel =
    case bigModel of
        Home__Model model ->
            pages.home.bundle model

        NotFound__Model model ->
            pages.notFound.bundle model

        Pre__Model model ->
            pages.pre.bundle model


view : Model -> Skeleton.Config Msg
view model =
    (bundle model).view ()
        |> (\config -> { config | header = links ++ config.header })


subscriptions : Model -> Sub Msg
subscriptions model =
    (bundle model).subscriptions ()


save : Model -> Session.Data -> Session.Data
save model =
    (bundle model).save ()


load : Model -> Session.Data -> ( Model, Cmd Msg )
load model session =
    (bundle model).load () session



-- UPGRADING PAGES


type alias Upgraded model msg =
    { init : Session.Data -> ( Model, Cmd Msg )
    , update : msg -> model -> ( Model, Cmd Msg )
    , bundle : model -> Bundle
    }


type alias Bundle =
    { view : () -> Skeleton.Config Msg
    , subscriptions : () -> Sub Msg
    , save : () -> Session.Data -> Session.Data
    , load : () -> Session.Data -> ( Model, Cmd Msg )
    }


upgrade : (model -> Model) -> (msg -> Msg) -> Page model msg -> Upgraded model msg
upgrade toModel toMsg page =
    let
        init_ : Session.Data -> ( Model, Cmd Msg )
        init_ session =
            page.init session |> Tuple.mapBoth toModel (Cmd.map toMsg)

        update_ : msg -> model -> ( Model, Cmd Msg )
        update_ msg model =
            page.update msg model |> Tuple.mapBoth toModel (Cmd.map toMsg)

        bundle_ : model -> Bundle
        bundle_ model =
            { view = \_ -> page.view model |> Skeleton.map toMsg
            , subscriptions = \_ -> page.subscriptions model |> Sub.map toMsg
            , save = \_ -> page.save model
            , load = \_ -> load_ model
            }

        load_ : model -> Session.Data -> ( Model, Cmd Msg )
        load_ model shared =
            page.load shared model |> Tuple.mapBoth toModel (Cmd.map toMsg)
    in
    { init = init_
    , update = update_
    , bundle = bundle_
    }


pages :
    { home : Upgraded Home.PageModel Home.PageMsg
    , notFound : Upgraded NotFound.PageModel NotFound.PageMsg
    , pre : Upgraded Pre.PageModel Pre.PageMsg
    }
pages =
    { home = Home.page |> upgrade Home__Model Home__Msg
    , notFound = NotFound.page |> upgrade NotFound__Model NotFound__Msg
    , pre = Pre.page |> upgrade Pre__Model Pre__Msg
    }



-- UTIL


orElse : Maybe a -> Maybe a -> Maybe a
orElse ma mb =
    case mb of
        Nothing ->
            ma

        Just _ ->
            mb
