module Skeleton exposing
    ( Details
    , Segment
    , Warning(..)
    , linkSegment
    , map
    , view
    )

import Browser
import Element exposing (Element)
import Element.Lazy
import Html exposing (..)



-- NODE


type alias Details msg =
    { title : String
    , header : List Segment
    , warning : Warning
    , kids : Element msg
    }


type Warning
    = NoProblems
    | WarnOld



-- SEGMENT


type Segment
    = Text String
    | Link { url : String, text : String }


linkSegment : { url : String, text : String } -> Segment
linkSegment =
    Link



-- VIEW


view : (a -> msg) -> Details a -> Browser.Document msg
view toMsg details =
    { title = details.title
    , body = [ body details |> Html.map toMsg ]
    }


body : Details msg -> Html msg
body details =
    Element.layout
        [ Element.width Element.fill
        , Element.height Element.fill
        ]
        (Element.column
            [ Element.width Element.fill
            , Element.height Element.fill
            ]
            [ viewHeader details.header
            , Element.Lazy.lazy viewWarning details.warning
            , details.kids
            , viewFooter
            ]
        )


map : (a -> msg) -> Details a -> Details msg
map toMsg details =
    { title = details.title
    , header = details.header
    , warning = details.warning
    , kids = details.kids |> Element.map toMsg
    }



-- VIEW HEADER


viewHeader : List Segment -> Element msg
viewHeader segments =
    Element.row [ Element.padding 10 ]
        (case segments of
            [] ->
                [ Element.none ]

            _ ->
                List.intersperse slash (List.map viewSegment segments)
        )


slash : Element msg
slash =
    Element.text " / "


viewSegment : Segment -> Element msg
viewSegment segment =
    case segment of
        Text string ->
            Element.text string

        Link { url, text } ->
            Element.link [] { url = url, label = Element.text text }



-- VIEW WARNING


viewWarning : Warning -> Element msg
viewWarning warning =
    Element.row [] <|
        case warning of
            NoProblems ->
                []

            WarnOld ->
                [ Element.el []
                    (Element.text "NOTE — this package is not compatible with Elm 0.19.1")
                ]



-- VIEW FOOTER


viewFooter : Element msg
viewFooter =
    Element.row [ Element.padding 10 ]
        [ Element.link []
            { url = "https://elm-lang.org"
            , label = Element.text "Hello I am unicorn."
            }
        , Element.text " - 🦄"
        ]
