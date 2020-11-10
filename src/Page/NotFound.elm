module Page.NotFound exposing (PageModel, PageMsg, page, parser)

import Element
import Element.Font
import Page exposing (Page)
import Session
import Skeleton
import Url.Parser as Parser exposing (Parser)



-- MODEL


type alias Model =
    Session.Data



-- UPDATE


type alias Msg =
    Never



-- VIEW


view : Session.Data -> Skeleton.Config msg
view _ =
    { title = "Home"
    , header = []
    , attrs = []
    , kids =
        Element.el
            [ Element.Font.size 500
            , Element.centerX
            , Element.centerY
            ]
            (Element.text "💩")
    , isLoading = False
    }



-- PAGE


parser : Parser a a
parser =
    Parser.s "not-found"


type alias PageModel =
    Model


type alias PageMsg =
    Msg


page : Page PageModel PageMsg
page =
    Page.static { view = view }
