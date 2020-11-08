module Page exposing
    ( Page, NonLoadingPage
    , Sheet
    , static, element, applicationWithSheet, loadedApplication
    , Msg(..), Status(..)
    )

{-|

@docs Page, NonLoadingPage
@docs Sheet
@docs static, sandbox, element, application, applicationWithSheet, loadedApplication
@docs Upgraded, Bundle, upgrade

-}

import Main.Sheet as Sheet
import Session
import Skeleton


type alias NonLoadingPage model msg =
    Page () model () msg


type alias Page loadingModel model loadingMsg msg =
    { init :
        Session.Data
        -> ( Status loadingModel model, Cmd (Msg loadingMsg msg) )
    , update :
        Msg loadingMsg msg
        -> Status loadingModel model
        -> ( Status loadingModel model, Cmd (Msg loadingMsg msg) )
    , view : model -> Skeleton.Config (Msg loadingMsg msg)
    , sheet : Maybe (Sheet model msg)
    }


type Status lm m
    = Loading lm
    | Ok m


type Msg lm m
    = LoadingMsg lm
    | OkMsg m


type alias Sheet model msg =
    { show : model -> Bool
    , view : model -> Sheet.Details msg
    , onHide : msg
    }


static :
    { view : Session.Data -> Skeleton.Config msg
    }
    -> Page loadingModel Session.Data loadingMsg msg
static page =
    { init = \session -> ( Ok session, Cmd.none )
    , update = \_ model -> ( model, Cmd.none )
    , view = page.view >> Skeleton.map OkMsg
    , sheet = Nothing
    }


element :
    { init : Session.Data -> ( model, Cmd msg )
    , update : msg -> model -> ( model, Cmd msg )
    , view : model -> Skeleton.Config msg
    }
    -> Page loadingModel model loadingMsg msg
element page =
    { init =
        \session ->
            page.init session
                |> Tuple.mapBoth Ok (Cmd.map OkMsg)
    , update =
        \msg model ->
            case ( msg, model ) of
                ( OkMsg msg_, Ok model_ ) ->
                    page.update msg_ model_
                        |> Tuple.mapBoth Ok (Cmd.map OkMsg)

                ( _, _ ) ->
                    ( model, Cmd.none )
    , view = page.view >> Skeleton.map OkMsg
    , sheet = Nothing
    }


applicationWithSheet :
    { init : Session.Data -> ( model, Cmd msg )
    , update : msg -> model -> ( model, Cmd msg )
    , view : model -> Skeleton.Config msg
    , sheet : Sheet model msg
    }
    -> NonLoadingPage model msg
applicationWithSheet page =
    { init =
        \session ->
            page.init session
                |> Tuple.mapBoth Ok (Cmd.map OkMsg)
    , update =
        \msg model ->
            case ( msg, model ) of
                ( OkMsg msg_, Ok model_ ) ->
                    page.update msg_ model_
                        |> Tuple.mapBoth Ok (Cmd.map OkMsg)

                ( _, _ ) ->
                    ( model, Cmd.none )
    , view = page.view >> Skeleton.map OkMsg
    , sheet = Just page.sheet
    }


loadedApplication :
    { init : data -> ( model, Cmd msg )
    , update : msg -> model -> ( model, Cmd msg )
    , view : model -> Skeleton.Config msg

    -- loading
    , loadingInit : Session.Data -> ( loadingModel, Cmd loadingMsg )
    , loadingUpdate : loadingMsg -> loadingModel -> ( loadingModel, Cmd loadingMsg )
    , loadingModelToData : loadingModel -> Maybe data
    }
    -> Page loadingModel model loadingMsg msg
loadedApplication page =
    let
        initIfLoaded loadingModel =
            page.loadingModelToData loadingModel
                |> Maybe.map (page.init >> Tuple.mapBoth Ok (Cmd.map OkMsg))
    in
    { init =
        \session ->
            page.loadingInit session
                |> Tuple.mapBoth Loading (Cmd.map LoadingMsg)
    , update =
        \msg model ->
            case ( msg, model ) of
                ( LoadingMsg msg_, Loading model_ ) ->
                    let
                        ( loadingModel, loadingCmd ) =
                            page.loadingUpdate msg_ model_
                    in
                    initIfLoaded loadingModel
                        |> Maybe.withDefault
                            (( loadingModel, loadingCmd )
                                |> Tuple.mapBoth Loading (Cmd.map LoadingMsg)
                            )

                ( OkMsg msg_, Ok model_ ) ->
                    page.update msg_ model_
                        |> Tuple.mapBoth Ok (Cmd.map OkMsg)

                ( _, _ ) ->
                    ( model, Cmd.none )
    , view = page.view >> Skeleton.map OkMsg
    , sheet = Nothing
    }
