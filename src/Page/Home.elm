module Page.Home exposing (PageModel, PageMsg, page, parser)

import Browser.Navigation as Navigation
import Element exposing (Element)
import Element.Background
import Element.Input
import Main.Sheet as Sheet
import Page exposing (Page)
import Session
import Skeleton
import Url.Parser as Parser exposing (Parser)



-- MODEL


type alias Model =
    { navKey : Navigation.Key
    , showSheet : Bool
    }


init : Session.Data -> ( Model, Cmd msg )
init session =
    let
        route : Route
        route =
            Parser.parse parser session.url
                |> Maybe.withDefault Top
    in
    ( case route of
        Top ->
            { navKey = session.key, showSheet = False }

        Sheet ->
            { navKey = session.key, showSheet = True }
    , Cmd.none
    )



-- UPDATE


type Msg
    = ClickedShowSheet
    | ClickedHideSheet


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedShowSheet ->
            ( model
            , Navigation.pushUrl model.navKey "/sheet"
            )

        ClickedHideSheet ->
            ( model
            , Navigation.pushUrl model.navKey "/"
            )



-- VIEW


view : Model -> Skeleton.Config Msg
view model =
    { title = "Home"
    , header = []
    , attrs = []
    , kids = viewContent model
    , isLoading = False
    }


viewContent : Model -> Element Msg
viewContent _ =
    Element.column
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.Background.color (Element.rgb255 0 0 0)
        , Element.padding 20
        ]
        [ Element.Input.button
            [ Element.Background.color (Element.rgb 0 0 255)
            , Element.alignBottom
            , Element.centerX
            , Element.padding 20
            ]
            { onPress = Just ClickedShowSheet
            , label = Element.text "open sheet"
            }
        ]



-- SHEET VIEW


sheetView : Model -> Sheet.Details Msg
sheetView _ =
    { attrs =
        [ Element.width (Element.px 500)
        , Element.height (Element.px 500)
        ]
    , kids =
        Element.column
            [ Element.width Element.fill
            , Element.height Element.fill
            , Element.padding 20
            , Element.spacing 20
            ]
            [ Element.text "lol"
            , Element.Input.button
                [ Element.Background.color (Element.rgb255 0 0 255)
                , Element.focused
                    [ Element.Background.color (Element.rgb255 255 0 255) ]
                , Element.padding 20
                ]
                { onPress = Just ClickedHideSheet
                , label = Element.text "My Button"
                }
            , Element.link
                [ Element.Background.color (Element.rgb255 255 255 0)
                , Element.focused [ Element.Background.color (Element.rgb255 255 0 0) ]
                , Element.padding 20
                ]
                { url = "/", label = Element.text "home" }
            , Element.link
                [ Element.Background.color (Element.rgb255 255 255 0)
                , Element.focused [ Element.Background.color (Element.rgb255 255 0 0) ]
                , Element.padding 20
                ]
                { url = "/pre", label = Element.text "show me a kanye quote" }
            ]
    }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- PAGE


type Route
    = Top
    | Sheet


parser : Parser (Route -> a) a
parser =
    Parser.oneOf
        [ Parser.map Top Parser.top
        , Parser.map Sheet (Parser.s "sheet")
        ]


type alias PageModel =
    Page.SheetedModel Model


type alias PageMsg =
    Page.SheetedMsg Msg


page : Page PageModel PageMsg
page =
    Page.application
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        , save = always identity
        , load =
            \session model ->
                let
                    route : Route
                    route =
                        Parser.parse parser session.url
                            |> Maybe.withDefault Top
                in
                ( case route of
                    Top ->
                        { model | showSheet = False }

                    Sheet ->
                        { model | showSheet = True }
                , Cmd.none
                )
        }
        |> Page.withSheet sheet


sheet : Page.Sheet Model Msg
sheet =
    { show = .showSheet
    , view = sheetView
    , onHide = ClickedHideSheet
    }
