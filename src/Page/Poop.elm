module Page.Poop exposing (..)

import Element
import Element.Font
import Page exposing (NonLoadingPage, Page)
import Session
import Skeleton


type alias Model =
    Session.Data


type alias Msg =
    Never



-- PAGE


page : NonLoadingPage Model Msg
page =
    Page.static { view = view }


view : Session.Data -> Skeleton.Config msg
view _ =
    Skeleton.Details
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
