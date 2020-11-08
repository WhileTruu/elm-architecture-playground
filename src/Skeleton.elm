module Skeleton exposing
    ( Config(..)
    , Segment
    , linkSegment
    , map
    , view
    )

import Browser
import Element exposing (Element)
import Html exposing (..)



-- NODE


type Config msg
    = Details
        { title : String
        , header : List Segment
        , attrs : List (Element.Attribute msg)
        , kids : Element msg
        }
    | Loading



-- SEGMENT


type Segment
    = Link { url : String, text : String }


linkSegment : { url : String, text : String } -> Segment
linkSegment =
    Link



-- VIEW


view : (a -> msg) -> Config a -> Browser.Document msg
view toMsg config =
    case config of
        Details details ->
            { title = details.title
            , body = [ body config |> Html.map toMsg ]
            }

        Loading ->
            { title = "Loading"
            , body = [ body config |> Html.map toMsg ]
            }


body : Config msg -> Html msg
body config =
    case config of
        Details details ->
            Element.layout
                ([ Element.width Element.fill
                 , Element.height Element.fill
                 ]
                    ++ details.attrs
                )
                (Element.column
                    [ Element.width Element.fill
                    , Element.height Element.fill
                    ]
                    [ viewHeader details.header
                    , details.kids
                    , viewFooter
                    ]
                )

        Loading ->
            Element.layout
                [ Element.width Element.fill
                , Element.height Element.fill
                ]
                (Element.text "LOADING")


map : (a -> msg) -> Config a -> Config msg
map toMsg config =
    case config of
        Details details ->
            Details
                { title = details.title
                , header = details.header
                , attrs = details.attrs |> List.map (Element.mapAttribute toMsg)
                , kids = details.kids |> Element.map toMsg
                }

        Loading ->
            Loading



-- VIEW HEADER


viewHeader : List Segment -> Element msg
viewHeader segments =
    Element.row [ Element.padding 10 ]
        (case segments of
            [] ->
                [ Element.none ]

            _ ->
                List.intersperse pipe (List.map viewSegment segments)
        )


pipe : Element msg
pipe =
    Element.text " | "


viewSegment : Segment -> Element msg
viewSegment segment =
    case segment of
        Link { url, text } ->
            Element.link [ Element.padding 20 ]
                { url = url, label = Element.text text }



-- VIEW FOOTER


viewFooter : Element msg
viewFooter =
    Element.row [ Element.padding 10 ]
        [ Element.link []
            { url = "https://duckduckgo.com/?q=unicorn"
            , label = Element.text "Hello I am unicorn."
            }
        , Element.text " - 🦄"
        ]
