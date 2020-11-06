module Page.Poop exposing (..)

import Element
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
    Page.static
        { view = view
        }


view : Session.Data -> Skeleton.Details msg
view _ =
    { title = "Home"
    , header =
        [ Skeleton.linkSegment { url = "/", text = "home" }
        , Skeleton.linkSegment { url = "/home2", text = "home2" }
        , Skeleton.linkSegment { url = "/poop", text = "poop" }
        ]
    , warning = Skeleton.NoProblems
    , kids =
        Element.el [ Element.height Element.fill, Element.width Element.fill ]
            (Element.text "💩")
    }
