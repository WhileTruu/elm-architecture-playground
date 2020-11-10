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

import Element exposing (Element)
import Main.Sheet as Sheet
import Page exposing (Page, Sheet)
import Page.Home as Home
import Page.NotFound as NotFound
import Page.Preconditions as Pre
import Session
import Skeleton
import Url.Parser as Parser exposing (Parser, oneOf)



-- TODO: Previous page view cannot be interacted with while loading another
--
-- TYPES


type alias Model =
    { sheet : Sheet.Model
    , previousPage : Maybe PageModel
    , page : PageModel
    }


type PageModel
    = Home__Model Home.PageModel
    | NotFound__Model NotFound.PageModel
    | Pre__Model Pre.PageModel


type PageMsg
    = Home__Msg Home.PageMsg
    | NotFound__Msg NotFound.PageMsg
    | Pre__Msg Pre.PageMsg


type Msg
    = Page__Msg PageMsg
    | SheetMsg Sheet.Msg



-- INIT


parser : Session.Data -> Maybe Model -> Parser (( Model, Cmd Msg ) -> c) c
parser session model =
    oneOf
        [ Parser.map (pages.home.init session) Home.parser
        , Parser.map (pages.notFound.init session) NotFound.parser
        , Parser.map (pages.pre.init session) Pre.parser
        ]
        |> Parser.map
            (Tuple.mapFirst
                (\a ->
                    { a
                        | sheet = Maybe.map .sheet model |> Maybe.withDefault Sheet.init
                        , previousPage = Maybe.map .page model
                    }
                )
            )


init : Session.Data -> Maybe Model -> ( Model, Cmd Msg )
init session prevModel =
    case Parser.parse (parser session prevModel) session.url of
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



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update bigMsg model =
    case bigMsg of
        Page__Msg msg ->
            updatePage msg model

        SheetMsg msg ->
            ( { model | sheet = Sheet.update msg model.sheet }, Cmd.none )


updatePage : PageMsg -> Model -> ( Model, Cmd Msg )
updatePage bigMsg bigModel =
    case ( bigMsg, bigModel.page ) of
        ( Home__Msg msg, Home__Model model ) ->
            pages.home.update msg model bigModel.sheet
                |> Tuple.mapFirst (\a -> { a | previousPage = bigModel.previousPage })

        ( NotFound__Msg msg, NotFound__Model model ) ->
            pages.notFound.update msg model bigModel.sheet
                |> Tuple.mapFirst (\a -> { a | previousPage = bigModel.previousPage })

        ( Pre__Msg msg, Pre__Model model ) ->
            pages.pre.update msg model bigModel.sheet
                |> Tuple.mapFirst (\a -> { a | previousPage = bigModel.previousPage })

        ( _, _ ) ->
            ( bigModel, Cmd.none )



-- BUNDLE - (view + subscriptions)


bundle : PageModel -> Bundle
bundle bigModel =
    case bigModel of
        Home__Model model ->
            pages.home.bundle model

        NotFound__Model model ->
            pages.notFound.bundle model

        Pre__Model model ->
            pages.pre.bundle model


view : Model -> Maybe (Skeleton.Config Msg)
view model =
    let
        bundledPage : Bundle
        bundledPage =
            bundle model.page

        addSheet : Skeleton.Config Msg -> Skeleton.Config Msg
        addSheet =
            bundledPage.sheet ()
                |> Maybe.map (addSheetToSkeleton model)
                |> Maybe.withDefault identity
    in
    bundledPage.view ()
        |> orElse (Maybe.andThen (previousView model) model.previousPage)
        |> Maybe.map addSheet
        |> Maybe.map (\config -> { config | header = links ++ config.header })


previousView : Model -> PageModel -> Maybe (Skeleton.Config Msg)
previousView model page =
    let
        bundledPage : Bundle
        bundledPage =
            bundle page

        addSheet : Skeleton.Config Msg -> Skeleton.Config Msg
        addSheet =
            bundledPage.sheet ()
                |> Maybe.map (addSheetToSkeleton model)
                |> Maybe.withDefault identity
    in
    bundledPage.view ()
        |> Maybe.map (\config -> { config | isLoading = True })
        |> Maybe.map addSheet


addSheetToSkeleton : Model -> Page.Sheet Model msg -> Skeleton.Config msg -> Skeleton.Config msg
addSheetToSkeleton model sheet config =
    let
        sheetView : Element.Element msg
        sheetView =
            if Sheet.isVisible model.sheet then
                Sheet.view model.sheet (sheet.view model) { onClose = sheet.onHide }

            else
                Element.none
    in
    { config | attrs = Element.inFront sheetView :: config.attrs }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ (bundle model.page).subscriptions ()
        , Sheet.subscriptions model.sheet |> Sub.map SheetMsg
        ]


save : Model -> Session.Data -> Session.Data
save model =
    (bundle model.page).save ()


load : Model -> Session.Data -> ( Model, Cmd Msg )
load model session =
    (bundle model.page).load () session model.sheet



-- UPGRADING PAGES


type alias Upgraded model msg =
    { init : Session.Data -> ( Model, Cmd Msg )
    , update : msg -> model -> Sheet.Model -> ( Model, Cmd Msg )
    , bundle : model -> Bundle
    }


type alias Bundle =
    { view : () -> Maybe (Skeleton.Config Msg)
    , subscriptions : () -> Sub Msg
    , save : () -> Session.Data -> Session.Data
    , load : () -> Session.Data -> Sheet.Model -> ( Model, Cmd Msg )
    , sheet : () -> Maybe (Page.Sheet Model Msg)
    }


upgrade : (model -> PageModel) -> (msg -> PageMsg) -> Page model msg -> Upgraded model msg
upgrade toPageModel toPageMsg page =
    let
        toMsg : msg -> Msg
        toMsg =
            toPageMsg >> Page__Msg

        toModel : Sheet.Model -> model -> Model
        toModel sheetModel =
            toPageModel
                >> (\pageModel ->
                        { sheet = sheetModel
                        , previousPage = Nothing
                        , page = pageModel
                        }
                   )

        init_ : Session.Data -> ( Model, Cmd Msg )
        init_ session =
            page.init session
                |> Tuple.mapBoth (toModel Sheet.init) (Cmd.map toMsg)

        update_ : msg -> model -> Sheet.Model -> ( Model, Cmd Msg )
        update_ msg model sheetModel =
            page.update msg model
                |> Tuple.mapFirst (\a -> toModel (updateSheet sheetModel a) a)
                |> Tuple.mapSecond (Cmd.map toMsg)

        updateSheet : Sheet.Model -> model -> Sheet.Model
        updateSheet sheet model =
            let
                showSheet : model -> Bool
                showSheet =
                    page.sheet |> Maybe.map .show |> Maybe.withDefault (always False)
            in
            if showSheet model then
                Sheet.show sheet

            else
                Sheet.hide sheet

        bundle_ : model -> Bundle
        bundle_ model =
            { view = \_ -> page.view model |> Maybe.map (Skeleton.map toMsg)
            , subscriptions = \_ -> page.subscriptions model |> Sub.map toMsg
            , save = \_ -> page.save model
            , load = \_ -> load_ model
            , sheet = \_ -> sheet_ model
            }

        load_ : model -> Session.Data -> Sheet.Model -> ( Model, Cmd Msg )
        load_ model shared sheetModel =
            page.load shared model
                |> Tuple.mapBoth (toModel sheetModel) (Cmd.map toMsg)

        sheet_ : model -> Maybe (Page.Sheet Model Msg)
        sheet_ model =
            Maybe.map (sheetToModelSheet model) page.sheet

        sheetToModelSheet : model -> Page.Sheet model msg -> Page.Sheet Model Msg
        sheetToModelSheet model sheet =
            { show = \_ -> sheet.show model
            , view = \_ -> sheet.view model |> Sheet.map toMsg
            , onHide = toMsg sheet.onHide
            }
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
