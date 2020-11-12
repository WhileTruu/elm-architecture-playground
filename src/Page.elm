module Page exposing
    ( Page
    , Sheet
    , static, element, application, loadedApplication
    , LoadedModel, LoadedMsg, SheetedModel, SheetedMsg, withSheet
    )

{-|

@docs Page, NonLoadingPage
@docs Sheet
@docs static, sandbox, element, application, applicationWithSheet, loadedApplication
@docs Upgraded, Bundle, upgrade

-}

import Element
import Main.Sheet as Sheet
import Session
import Skeleton


type alias Page model msg =
    { init : Session.Data -> ( model, Cmd msg )
    , update : msg -> model -> ( model, Cmd msg )
    , view : model -> Skeleton.Config msg
    , subscriptions : model -> Sub msg
    , save : model -> Session.Data -> Session.Data
    , load : Session.Data -> model -> ( model, Cmd msg )
    }


static : { view : Session.Data -> Skeleton.Config msg } -> Page Session.Data msg
static page =
    element
        { init = \session -> ( session, Cmd.none )
        , update = \_ model -> ( model, Cmd.none )
        , view = page.view
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
    , view = page.view
    , subscriptions = \_ -> Sub.none
    , save = always identity
    , load = always (identity >> ignoreEffect)
    }


application :
    { init : Session.Data -> ( model, Cmd msg )
    , update : msg -> model -> ( model, Cmd msg )
    , view : model -> Skeleton.Config msg
    , subscriptions : model -> Sub msg
    , save : model -> Session.Data -> Session.Data
    , load : Session.Data -> model -> ( model, Cmd msg )
    }
    -> Page model msg
application page =
    { init = page.init
    , update = page.update
    , view = page.view
    , subscriptions = page.subscriptions
    , save = page.save
    , load = page.load
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
    , subscriptions : model -> Sub msg
    , save : model -> Session.Data -> Session.Data
    , load : Session.Data -> model -> ( model, Cmd msg )

    -- loading
    , loadingInit : Session.Data -> ( loadingModel, Cmd loadingMsg )
    , loadingUpdate : loadingMsg -> loadingModel -> ( loadingModel, Cmd loadingMsg )
    , loadingModelToData : loadingModel -> Maybe data
    }
    -> Page (LoadedModel model loadingModel) (LoadedMsg msg loadingMsg)
loadedApplication page =
    let
        initIfLoaded :
            loadingModel
            -> Maybe ( LoadedModel model loadingModel, Cmd (LoadedMsg msg loadingMsg) )
        initIfLoaded loadingModel =
            page.loadingModelToData loadingModel
                |> Maybe.map (page.init >> Tuple.mapBoth SuccessModel (Cmd.map SuccessMsg))
    in
    { init = page.loadingInit >> Tuple.mapBoth LoadingModel (Cmd.map LoadingMsg)
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
                LoadingModel _ ->
                    loadingView

                SuccessModel model_ ->
                    page.view model_ |> Skeleton.map SuccessMsg
    , subscriptions =
        \model ->
            case model of
                LoadingModel _ ->
                    Sub.none

                SuccessModel model_ ->
                    page.subscriptions model_ |> Sub.map SuccessMsg
    , save =
        \model ->
            case model of
                LoadingModel _ ->
                    identity

                SuccessModel model_ ->
                    page.save model_
    , load =
        \session model ->
            case model of
                LoadingModel model_ ->
                    ( LoadingModel model_, Cmd.none )

                SuccessModel model_ ->
                    page.load session model_
                        |> Tuple.mapBoth SuccessModel (Cmd.map SuccessMsg)
    }



-- SHEETED


type alias Sheet model msg =
    { show : model -> Bool
    , view : model -> Sheet.Details msg
    , onHide : msg
    }


type alias SheetedModel model =
    { component : model
    , sheet : Sheet.Model
    }


type SheetedMsg msg
    = SheetMsg Sheet.Msg
    | ComponentMsg msg


withSheet : Sheet model msg -> Page model msg -> Page (SheetedModel model) (SheetedMsg msg)
withSheet sheet page =
    let
        showOrHideSheet model =
            if sheet.show model.component then
                { model | sheet = Sheet.show model.sheet }

            else
                { model | sheet = Sheet.hide model.sheet }
    in
    { init =
        \session ->
            page.init session
                |> (\( model, cmd ) ->
                        ( { component = model, sheet = Sheet.init }
                        , Cmd.map ComponentMsg cmd
                        )
                   )
                |> Tuple.mapFirst showOrHideSheet
    , update =
        \msg model ->
            case msg of
                SheetMsg msg_ ->
                    ( { model | sheet = Sheet.update msg_ model.sheet }, Cmd.none )

                ComponentMsg msg_ ->
                    page.update msg_ model.component
                        |> Tuple.mapFirst (\a -> { model | component = a })
                        |> Tuple.mapFirst showOrHideSheet
                        |> Tuple.mapSecond (Cmd.map ComponentMsg)
    , view =
        \model ->
            page.view model.component
                |> addSheetToSkeleton model.sheet sheet model.component
                |> Skeleton.map ComponentMsg
    , subscriptions =
        \model ->
            Sub.batch
                [ page.subscriptions model.component |> Sub.map ComponentMsg
                , Sheet.subscriptions model.sheet |> Sub.map SheetMsg
                ]
    , save = \model session -> page.save model.component session
    , load =
        \session model ->
            page.load session model.component
                |> Tuple.mapBoth
                    (\a -> { model | component = a })
                    (Cmd.map ComponentMsg)
                |> Tuple.mapFirst showOrHideSheet
    }


addSheetToSkeleton : Sheet.Model -> Sheet model msg -> model -> Skeleton.Config msg -> Skeleton.Config msg
addSheetToSkeleton model sheet componentModel config =
    let
        sheetView : Element.Element msg
        sheetView =
            if Sheet.isVisible model then
                Sheet.view model (sheet.view componentModel) { onClose = sheet.onHide }

            else
                Element.none
    in
    { config | attrs = Element.inFront sheetView :: config.attrs }



-- UTIL


ignoreEffect : model -> ( model, Cmd msg )
ignoreEffect model =
    ( model, Cmd.none )


loadingView : Skeleton.Config msg
loadingView =
    { title = "Loading..."
    , header = []
    , attrs = []
    , kids =
        Element.el [ Element.centerY, Element.centerX ]
            (Element.text "LOADING")
    , isLoading = False
    }
