module Page exposing
    ( Page
    , Sheet
    , static, element, applicationWithSheet, loadedApplication
    , LoadedModel(..), LoadedMsg(..)
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


type alias Page model msg =
    { init : Session.Data -> ( model, Cmd msg )
    , update : msg -> model -> ( model, Cmd msg )
    , view : model -> Maybe (Skeleton.Config msg)
    , subscriptions : model -> Sub msg
    , save : model -> Session.Data -> Session.Data
    , load : Session.Data -> model -> ( model, Cmd msg )
    , sheet : Maybe (Sheet model msg)
    }


type alias Sheet model msg =
    { show : model -> Bool
    , view : model -> Sheet.Details msg
    , onHide : msg
    }


static : { view : Session.Data -> Skeleton.Config msg } -> Page Session.Data msg
static page =
    { init = \session -> ( session, Cmd.none )
    , update = \_ model -> ( model, Cmd.none )
    , view = page.view >> Just
    , subscriptions = \_ -> Sub.none
    , save = always identity
    , load = always (identity >> ignoreEffect)
    , sheet = Nothing
    }


element :
    { init : Session.Data -> ( model, Cmd msg )
    , update : msg -> model -> ( model, Cmd msg )
    , view : model -> Skeleton.Config msg
    }
    -> Page model msg
element page =
    { init = page.init
    , update = page.update
    , view = page.view >> Just
    , subscriptions = \_ -> Sub.none
    , save = always identity
    , load = always (identity >> ignoreEffect)
    , sheet = Nothing
    }


applicationWithSheet :
    { init : Session.Data -> ( model, Cmd msg )
    , update : msg -> model -> ( model, Cmd msg )
    , view : model -> Skeleton.Config msg
    , sheet : Sheet model msg
    }
    -> Page model msg
applicationWithSheet page =
    { init = page.init
    , update = page.update
    , view = page.view >> Just
    , subscriptions = \_ -> Sub.none
    , save = always identity
    , load = always (identity >> ignoreEffect)
    , sheet = Just page.sheet
    }



-- LOADED


type LoadedModel loaded loading
    = LoadingModel loading
    | SuccessModel loaded


type LoadedMsg loaded loading
    = LoadingMsg loading
    | SuccessMsg loaded


loadedApplication :
    { init : data -> ( model, Cmd msg )
    , update : msg -> model -> ( model, Cmd msg )
    , view : model -> Skeleton.Config msg

    -- loading
    , loadingInit : Session.Data -> ( loadingModel, Cmd loadingMsg )
    , loadingUpdate : loadingMsg -> loadingModel -> ( loadingModel, Cmd loadingMsg )
    , loadingView : loadingModel -> Maybe (Skeleton.Config loadingMsg)
    , loadingModelToData : loadingModel -> Maybe data
    }
    -> Page (LoadedModel model loadingModel) (LoadedMsg msg loadingMsg)
loadedApplication page =
    let
        initIfLoaded : loadingModel -> Maybe ( LoadedModel model loading, Cmd (LoadedMsg msg a) )
        initIfLoaded loadingModel =
            page.loadingModelToData loadingModel
                |> Maybe.map (page.init >> Tuple.mapBoth SuccessModel (Cmd.map SuccessMsg))
    in
    { init =
        \session ->
            page.loadingInit session
                |> Tuple.mapBoth LoadingModel (Cmd.map LoadingMsg)
    , update =
        \msg model ->
            case ( msg, model ) of
                ( LoadingMsg msg_, LoadingModel model_ ) ->
                    let
                        ( loadingModel, loadingCmd ) =
                            page.loadingUpdate msg_ model_
                    in
                    initIfLoaded loadingModel
                        |> Maybe.withDefault
                            (( loadingModel, loadingCmd )
                                |> Tuple.mapBoth LoadingModel (Cmd.map LoadingMsg)
                            )

                ( SuccessMsg msg_, SuccessModel model_ ) ->
                    page.update msg_ model_
                        |> Tuple.mapBoth SuccessModel (Cmd.map SuccessMsg)

                ( _, _ ) ->
                    ( model, Cmd.none )
    , view =
        \model ->
            case model of
                LoadingModel model_ ->
                    page.loadingView model_ |> Maybe.map (Skeleton.map LoadingMsg)

                SuccessModel model_ ->
                    page.view model_ |> Skeleton.map SuccessMsg |> Just
    , subscriptions = \_ -> Sub.none
    , save = always identity
    , load = always (identity >> ignoreEffect)
    , sheet = Nothing
    }


ignoreEffect : model -> ( model, Cmd msg )
ignoreEffect model =
    ( model, Cmd.none )
