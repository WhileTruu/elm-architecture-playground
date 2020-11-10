module Page.Home exposing (PageModel, PageMsg, page, parser)

import Element exposing (Element)
import Element.Background
import Element.Input
import Main.Sheet as Sheet
import Page exposing (Page)
import Skeleton
import Url.Parser as Parser exposing (Parser)



-- MODEL


type alias Model =
    { showSheet : Bool
    }


init : ( Model, Cmd msg )
init =
    ( { showSheet = False }
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
            ( { model | showSheet = True }, Cmd.none )

        ClickedHideSheet ->
            ( { model | showSheet = False }, Cmd.none )



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


parser : Parser a a
parser =
    Parser.top


type alias PageModel =
    Model


type alias PageMsg =
    Msg


page : Page PageModel PageMsg
page =
    Page.applicationWithSheet
        { init = \_ -> init
        , view = view
        , update = update
        , sheet = sheet
        }


sheet : Page.Sheet PageModel PageMsg
sheet =
    { show = .showSheet
    , view = sheetView
    , onHide = ClickedHideSheet
    }
