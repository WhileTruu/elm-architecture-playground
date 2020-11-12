module Page.Preconditions exposing (PageModel, PageMsg, page, parser)

import Element
import Element.Font
import Page exposing (Page)
import Page.Preconditions.Loading as Loading
import Skeleton
import Url.Parser as Parser exposing (Parser)



-- MODEL


type alias Model =
    { kanyeQuote : String
    , taylorSwiftQuote : String
    , donaldTrumpQuote : String
    }


init : Loading.Data -> ( Model, Cmd Msg )
init a =
    ( Model a.kanyeQuote a.taylorSwiftQuote a.donaldTrumpQuote
    , Cmd.none
    )



-- UPDATE


type Msg
    = Never


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Never ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Skeleton.Config msg
view model =
    { title = "Home"
    , header = []
    , attrs = []
    , kids =
        Element.el
            [ Element.Font.size 50
            , Element.centerX
            , Element.centerY
            ]
            (Element.paragraph
                [ Element.padding 20 ]
                [ Element.text model.kanyeQuote ]
            )
    , isLoading = False
    }



-- PAGE


parser : Parser a a
parser =
    Parser.s "pre"


type alias PageModel =
    Page.LoadedModel Model Loading.Model


type alias PageMsg =
    Page.LoadedMsg Msg Loading.Msg


page : Page PageModel PageMsg
page =
    Page.loadedApplication
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        , save = always identity
        , load = \_ model -> ( model, Cmd.none )

        -- loading
        , loadingInit = \_ -> Loading.init
        , loadingUpdate = Loading.update
        , loadingModelToData = Loading.modelToData
        }
