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
import Page exposing (Page)
import Page.Home as Home
import Page.Poop as Poop
import Page.Red as Red
import Session
import Skeleton
import Url.Parser as Parser exposing (Parser, oneOf, s, top)



-- TYPES


type alias Model =
    { sheet : Sheet.Model
    , page : PageModel
    }


type PageModel
    = Home__Model Home.Model
    | Red__Model Red.Model
    | Poop__Model Poop.Model


type Msg
    = Home__Msg Home.Msg
    | Red__Msg Red.Msg
    | Poop__Msg Poop.Msg
    | SheetMsg Sheet.Msg



-- INIT


init : Session.Data -> ( Model, Cmd Msg )
init session =
    let
        parser : Parser (( Model, Cmd Msg ) -> c) c
        parser =
            oneOf
                [ route top
                    (pages.home.init session)
                , route (s "red")
                    (pages.red.init session)
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


links : List Skeleton.Segment
links =
    [ Skeleton.linkSegment { url = "/", text = "home" }
    , Skeleton.linkSegment { url = "/red", text = "red" }
    , Skeleton.linkSegment { url = "/poop", text = "poop" }
    ]



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update bigMsg bigModel =
    case ( bigMsg, bigModel.page ) of
        ( Home__Msg msg, Home__Model model ) ->
            pages.home.update msg model bigModel.sheet

        ( Red__Msg msg, Red__Model model ) ->
            pages.red.update msg model bigModel.sheet

        ( Poop__Msg msg, Poop__Model model ) ->
            pages.poop.update msg model bigModel.sheet

        ( SheetMsg msg, _ ) ->
            ( { bigModel | sheet = Sheet.update msg bigModel.sheet }, Cmd.none )

        ( _, _ ) ->
            ( bigModel, Cmd.none )



-- BUNDLE - (view + subscriptions)


bundle : Model -> Bundle
bundle bigModel =
    case bigModel.page of
        Home__Model model ->
            pages.home.bundle model

        Red__Model model ->
            pages.red.bundle model

        Poop__Model model ->
            pages.poop.bundle model


view : Model -> Skeleton.Details Msg
view model =
    let
        bundle_ : Bundle
        bundle_ =
            bundle model

        addSheet : Skeleton.Details Msg -> Skeleton.Details Msg
        addSheet =
            bundle_.sheet ()
                |> Maybe.map (addSheetToSkeleton model)
                |> Maybe.withDefault identity
    in
    bundle_.view ()
        |> addSheet
        |> (\a -> { a | header = links ++ a.header })


addSheetToSkeleton : Model -> Page.Sheet Model msg -> Skeleton.Details msg -> Skeleton.Details msg
addSheetToSkeleton model sheet skeleton =
    { skeleton
        | attrs =
            Element.inFront
                (if Sheet.isVisible model.sheet then
                    Sheet.view model.sheet (sheet.view model) { onClose = sheet.onHide }

                 else
                    Element.none
                )
                :: skeleton.attrs
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ (bundle model).subscriptions ()
        , Sheet.subscriptions model.sheet |> Sub.map SheetMsg
        ]


save : Model -> Session.Data -> Session.Data
save model =
    (bundle model).save ()


load : Model -> Session.Data -> ( Model, Cmd Msg )
load model session =
    (bundle model).load () session model.sheet



-- UPGRADING PAGES


type alias Upgraded model msg =
    { init : Session.Data -> ( Model, Cmd Msg )
    , update : msg -> model -> Sheet.Model -> ( Model, Cmd Msg )
    , bundle : model -> Bundle
    }


type alias Bundle =
    { view : () -> Skeleton.Details Msg
    , subscriptions : () -> Sub Msg
    , save : () -> Session.Data -> Session.Data
    , load : () -> Session.Data -> Sheet.Model -> ( Model, Cmd Msg )
    , sheet : () -> Maybe (Page.Sheet Model Msg)
    }


upgrade : (model -> PageModel) -> (msg -> Msg) -> Page model msg -> Upgraded model msg
upgrade toPageModel toMsg page =
    let
        toModel : Sheet.Model -> model -> Model
        toModel sheetModel =
            toPageModel
                >> (\pageModel ->
                        { sheet = sheetModel
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
                |> Tuple.mapFirst
                    (\pageModel ->
                        if page.sheet |> Maybe.map (\{ show } -> show pageModel) |> Maybe.withDefault False then
                            toModel (Sheet.show sheetModel) pageModel

                        else
                            toModel (Sheet.hide sheetModel) pageModel
                    )
                |> Tuple.mapSecond (Cmd.map toMsg)

        bundle_ : model -> Bundle
        bundle_ model =
            { view = \_ -> page.view model |> Skeleton.map toMsg
            , subscriptions = \_ -> page.subscriptions model |> Sub.map toMsg
            , save = \_ -> page.save model
            , load = \_ -> load_ model
            , sheet = \_ -> sheet_ model
            }

        load_ : model -> Session.Data -> Sheet.Model -> ( Model, Cmd Msg )
        load_ model shared sheetModel =
            page.load shared model
                |> Tuple.mapBoth (toModel sheetModel) (Cmd.map toMsg)

        sheet_ : model -> Maybe (Page.Sheet a Msg)
        sheet_ model =
            Maybe.map
                (\sheet ->
                    { show = \_ -> sheet.show model
                    , view = \_ -> sheet.view model |> Sheet.map toMsg
                    , onHide = toMsg sheet.onHide
                    }
                )
                page.sheet
    in
    { init = init_
    , update = update_
    , bundle = bundle_
    }


pages :
    { home : Upgraded Home.Model Home.Msg
    , red : Upgraded Red.Model Red.Msg
    , poop : Upgraded Poop.Model Poop.Msg
    }
pages =
    { home = Home.page |> upgrade Home__Model Home__Msg
    , red = Red.page |> upgrade Red__Model Red__Msg
    , poop = Poop.page |> upgrade Poop__Model Poop__Msg
    }
