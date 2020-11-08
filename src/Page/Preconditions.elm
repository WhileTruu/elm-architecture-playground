module Page.Preconditions exposing (..)

import Element
import Element.Font
import Page exposing (NonLoadingPage, Page)
import Page.Preconditions.Loading as Loading
import Skeleton



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
    Skeleton.Details
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
        }



-- PAGE


page : Page Loading.Model Model Loading.Msg Msg
page =
    Page.loadedApplication
        { init = init
        , update = update
        , view = view

        -- loading
        , loadingInit = \_ -> Loading.init
        , loadingUpdate = Loading.update
        , loadingModelToData = Loading.modelToData
        }
