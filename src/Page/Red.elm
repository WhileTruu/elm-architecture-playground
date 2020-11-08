module Page.Red exposing (Model, Msg, page)

import Element exposing (Element)
import Element.Background
import Element.Input
import Main.Sheet as Sheet
import Page exposing (NonLoadingPage, Page)
import Skeleton



-- MODEL


type alias Model =
    { showSheet : Bool
    }


type Sheet
    = MySheet


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
    Skeleton.Details
        { title = "Home"
        , header = []
        , attrs = []
        , kids = viewContent model
        }


viewContent : Model -> Element Msg
viewContent model =
    Element.column
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.Background.color (Element.rgb255 255 0 0)
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
sheetView model =
    { attrs = []
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
            ]
    }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- PAGE


page : NonLoadingPage Model Msg
page =
    Page.applicationWithSheet
        { init = \_ -> init
        , view = view
        , update = update
        , sheet = sheet
        }


sheet : Page.Sheet Model Msg
sheet =
    { show = .showSheet
    , view = sheetView
    , onHide = ClickedHideSheet
    }
