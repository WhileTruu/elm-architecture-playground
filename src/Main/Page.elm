module Main.Page exposing
    ( Model
    , Msg
    , init
    , subscriptions
    , update
    , view
    )

import Element exposing (Element)
import Element.Background
import Main.Sheet as Sheet
import Page exposing (Page)
import Page.Home as Home
import Page.Poop as Poop
import Page.Preconditions as Pre
import Page.Preconditions.Loading as PreLoading
import Page.Red as Red
import Session
import Skeleton
import Url.Parser as Parser exposing (Parser, oneOf, s, top)



-- TYPES


type alias Model =
    { sheet : Sheet.Model
    , previousPage : Maybe PageModel
    , page : PageModel
    }


type PageModel
    = Home__Model (Page.Status () Home.Model)
    | Red__Model (Page.Status () Red.Model)
    | Poop__Model (Page.Status () Poop.Model)
    | Pre__Model (Page.Status PreLoading.Model Pre.Model)


type Msg
    = Home__Msg (Page.Msg () Home.Msg)
    | Red__Msg (Page.Msg () Red.Msg)
    | Poop__Msg (Page.Msg () Poop.Msg)
    | Pre__Msg (Page.Msg PreLoading.Msg Pre.Msg)
    | SheetMsg Sheet.Msg



-- INIT


init : Session.Data -> Maybe Model -> ( Model, Cmd Msg )
init session bigModel =
    let
        pages_ =
            pages (Maybe.andThen .previousPage bigModel)

        parser : Parser (( Model, Cmd Msg ) -> c) c
        parser =
            oneOf
                [ route top
                    (pages_.home.init session)
                , route (s "red")
                    (pages_.red.init session)
                , route (s "poop")
                    (pages_.poop.init session)
                , route (s "pre")
                    (pages_.pre.init session)
                ]
    in
    case Parser.parse parser session.url of
        Just answer ->
            answer

        Nothing ->
            pages_.poop.init session


route : Parser a b -> a -> Parser (b -> c) c
route parser handler =
    Parser.map handler parser


links : List Skeleton.Segment
links =
    [ Skeleton.linkSegment { url = "/", text = "home" }
    , Skeleton.linkSegment { url = "/red", text = "red" }
    , Skeleton.linkSegment { url = "/poop", text = "poop" }
    , Skeleton.linkSegment { url = "/pre", text = "pre" }
    ]



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update bigMsg bigModel =
    let
        pages_ =
            pages bigModel.previousPage
    in
    case bigMsg of
        Home__Msg msg ->
            case bigModel.page of
                Home__Model model ->
                    pages_.home.update msg model bigModel.sheet

                _ ->
                    ( bigModel, Cmd.none )

        Red__Msg msg ->
            case bigModel.page of
                Red__Model model ->
                    pages_.red.update msg model bigModel.sheet

                _ ->
                    ( bigModel, Cmd.none )

        Poop__Msg msg ->
            case bigModel.page of
                Poop__Model model ->
                    pages_.poop.update msg model bigModel.sheet

                _ ->
                    ( bigModel, Cmd.none )

        Pre__Msg msg ->
            case bigModel.page of
                Pre__Model model ->
                    pages_.pre.update msg model bigModel.sheet

                _ ->
                    ( bigModel, Cmd.none )

        SheetMsg msg ->
            ( { bigModel | sheet = Sheet.update msg bigModel.sheet }, Cmd.none )



-- BUNDLE - (view + subscriptions)


bundle : PageModel -> Maybe Bundle
bundle page =
    let
        pages_ =
            pages (Just page)

        bundleFromModel toBundle model_ =
            case model_ of
                Page.Ok model ->
                    Just (toBundle model)

                Page.Loading _ ->
                    Nothing
    in
    case page of
        Home__Model model ->
            bundleFromModel pages_.home.bundle model

        Red__Model model ->
            bundleFromModel pages_.red.bundle model

        Poop__Model model ->
            bundleFromModel pages_.poop.bundle model

        Pre__Model model ->
            bundleFromModel pages_.pre.bundle model


orElse : Maybe a -> Maybe a -> Maybe a
orElse ma mb =
    case mb of
        Nothing ->
            ma

        Just _ ->
            mb


view : Model -> Skeleton.Config Msg
view model =
    Maybe.map (viewWithBundle model) (bundle model.page)
        |> orElse
            (Maybe.andThen bundle model.previousPage
                |> Maybe.map (viewWithBundle model >> addLoadingOverlayToSkeleton)
            )
        |> Maybe.withDefault Skeleton.Loading


viewWithBundle : Model -> Bundle -> Skeleton.Config Msg
viewWithBundle model bundle_ =
    let
        addSheet : Skeleton.Config Msg -> Skeleton.Config Msg
        addSheet =
            bundle_.sheet ()
                |> Maybe.map (addSheetToSkeleton model)
                |> Maybe.withDefault identity
    in
    bundle_.view ()
        |> addSheet
        |> (\a ->
                case a of
                    Skeleton.Details details ->
                        Skeleton.Details { details | header = links ++ details.header }

                    Skeleton.Loading ->
                        Skeleton.Loading
           )


addLoadingOverlayToSkeleton : Skeleton.Config msg -> Skeleton.Config msg
addLoadingOverlayToSkeleton skeletonConfig =
    case skeletonConfig of
        Skeleton.Details details ->
            Skeleton.Details
                { details
                    | attrs =
                        Element.inFront
                            (Element.el
                                [ Element.Background.color (Element.rgba255 255 255 255 0.5)
                                , Element.width Element.fill
                                , Element.height Element.fill
                                ]
                                Element.none
                            )
                            :: details.attrs
                }

        Skeleton.Loading ->
            Skeleton.Loading


addSheetToSkeleton :
    Model
    -> Page.Sheet Model msg
    -> Skeleton.Config msg
    -> Skeleton.Config msg
addSheetToSkeleton model sheet skeletonConfig =
    case skeletonConfig of
        Skeleton.Details details ->
            Skeleton.Details
                { details
                    | attrs =
                        Element.inFront
                            (if Sheet.isVisible model.sheet then
                                Sheet.view model.sheet (sheet.view model) { onClose = sheet.onHide }

                             else
                                Element.none
                            )
                            :: details.attrs
                }

        Skeleton.Loading ->
            Skeleton.Loading


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sheet.subscriptions model.sheet |> Sub.map SheetMsg
        ]



-- UPGRADING PAGES


type alias Upgraded loadingModel model loadingMsg msg =
    { init : Session.Data -> ( Model, Cmd Msg )
    , update : Page.Msg loadingMsg msg -> Page.Status loadingModel model -> Sheet.Model -> ( Model, Cmd Msg )
    , bundle : model -> Bundle
    }


type alias Bundle =
    { view : () -> Skeleton.Config Msg
    , sheet : () -> Maybe (Page.Sheet Model Msg)
    }


upgrade :
    Maybe PageModel
    -> (Page.Status loadingModel model -> PageModel)
    -> (Page.Msg loadingMsg msg -> Msg)
    -> Page loadingModel model loadingMsg msg
    -> Upgraded loadingModel model loadingMsg msg
upgrade prevPage toPageModel toMsg page =
    let
        toModel : Sheet.Model -> Page.Status loadingModel model -> Model
        toModel sheetModel status =
            toPageModel status
                |> (\pageModel ->
                        { sheet = sheetModel
                        , previousPage =
                            case status of
                                Page.Loading _ ->
                                    prevPage

                                Page.Ok _ ->
                                    Just pageModel
                        , page = pageModel
                        }
                   )

        init_ : Session.Data -> ( Model, Cmd Msg )
        init_ session =
            page.init session
                |> Tuple.mapBoth (toModel Sheet.init) (Cmd.map toMsg)

        update_ : Page.Msg loadingMsg msg -> Page.Status loadingModel model -> Sheet.Model -> ( Model, Cmd Msg )
        update_ msg model sheetModel =
            page.update msg model
                |> Tuple.mapFirst
                    (\pageModel ->
                        let
                            isSheetVisible =
                                Sheet.isVisible sheetModel

                            showSheet =
                                case pageModel of
                                    Page.Ok pageModel_ ->
                                        page.sheet |> Maybe.map (\{ show } -> show pageModel_) |> Maybe.withDefault False

                                    _ ->
                                        False
                        in
                        if showSheet then
                            toModel (Sheet.show sheetModel) pageModel

                        else if isSheetVisible then
                            toModel (Sheet.hide sheetModel) pageModel

                        else
                            toModel Sheet.init pageModel
                    )
                |> Tuple.mapSecond (Cmd.map toMsg)

        bundle_ : model -> Bundle
        bundle_ model =
            { view = \_ -> page.view model |> Skeleton.map toMsg
            , sheet = \_ -> sheet_ model
            }

        sheet_ : model -> Maybe (Page.Sheet a Msg)
        sheet_ model =
            Maybe.map
                (\sheet ->
                    { show = \_ -> sheet.show model
                    , view = \_ -> sheet.view model |> Sheet.map (toMsg << Page.OkMsg)
                    , onHide = (toMsg << Page.OkMsg) sheet.onHide
                    }
                )
                page.sheet
    in
    { init = init_
    , update = update_
    , bundle = bundle_
    }


pages :
    Maybe PageModel
    ->
        { home : Upgraded () Home.Model () Home.Msg
        , red : Upgraded () Red.Model () Red.Msg
        , poop : Upgraded () Poop.Model () Poop.Msg
        , pre : Upgraded PreLoading.Model Pre.Model PreLoading.Msg Pre.Msg
        }
pages previousPage =
    { home =
        Home.page
            |> upgrade previousPage Home__Model Home__Msg
    , red =
        Red.page
            |> upgrade previousPage Red__Model Red__Msg
    , poop =
        Poop.page
            |> upgrade previousPage Poop__Model Poop__Msg
    , pre =
        Pre.page
            |> upgrade previousPage Pre__Model Pre__Msg
    }
