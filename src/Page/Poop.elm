module Page.Poop exposing (..)

import Element
import Element.Font
import Page exposing (Page)
import Session
import Skeleton


type alias Model =
    Session.Data


type alias Msg =
    Never



-- PAGE


page : Page Model msg
page =
    Page.static { view = view }


view : Session.Data -> Skeleton.Details msg
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
    }
