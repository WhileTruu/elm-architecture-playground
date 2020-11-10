module Skeleton exposing
    ( Config
    , Segment
    , linkSegment
    , map
    , view
    )

import Browser
import Element exposing (Element)
import Element.Background
import Html exposing (..)



-- NODE


type alias Config msg =
    { title : String
    , header : List Segment
    , attrs : List (Element.Attribute msg)
    , kids : Element msg
    , isLoading : Bool
    }



-- SEGMENT


type Segment
    = Link { url : String, text : String }


linkSegment : { url : String, text : String } -> Segment
linkSegment =
    Link



-- VIEW


view : (a -> msg) -> Config a -> Browser.Document msg
view toMsg config =
    { title = config.title
    , body = [ body config |> Html.map toMsg ]
    }


body : Config msg -> Html msg
body config =
    Element.layout
        ([ Element.width Element.fill
         , Element.height Element.fill
         ]
            ++ config.attrs
        )
        (Element.column
            [ Element.width Element.fill
            , Element.height Element.fill
            ]
            [ viewHeader config.isLoading config.header
            , config.kids
            , viewFooter
            ]
        )


map : (a -> msg) -> Config a -> Config msg
map toMsg config =
    { title = config.title
    , header = config.header
    , attrs = config.attrs |> List.map (Element.mapAttribute toMsg)
    , kids = config.kids |> Element.map toMsg
    , isLoading = config.isLoading
    }



-- VIEW HEADER


viewHeader : Bool -> List Segment -> Element msg
viewHeader isLoading segments =
    let
        loadingStyle =
            if isLoading then
                [ Element.below
                    (Element.el
                        [ Element.width Element.fill
                        , Element.height (Element.px 10)
                        , Element.Background.color (Element.rgb255 255 0 0)
                        ]
                        Element.none
                    )
                ]

            else
                []
    in
    Element.row ([ Element.padding 10, Element.width Element.fill ] ++ loadingStyle)
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
