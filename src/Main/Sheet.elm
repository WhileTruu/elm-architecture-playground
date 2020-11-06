module Main.Sheet exposing
    ( Details
    , Model
    , Msg
    , hide
    , init
    , isVisible
    , map
    , show
    , subscriptions
    , update
    , view
    )

import Animator
import Animator.Inline
import Element exposing (Element)
import Element.Background
import Element.Border
import Html.Attributes
import Html.Events
import Json.Decode as Decode
import Time


isVisible : Model -> Bool
isVisible (Model model) =
    (Animator.current model /= Nothing)
        || Animator.upcoming Nothing model



-- MODEL


type Model
    = Model (Animator.Timeline (Maybe Sheet))


type Sheet
    = Sheet


init : Model
init =
    Model (Animator.init Nothing)



-- UPDATE


type Msg
    = Tick Time.Posix


update : Msg -> Model -> Model
update (Tick time) (Model model) =
    Model (Animator.update time animator model)


show : Model -> Model
show (Model model) =
    Model (Animator.go Animator.verySlowly (Just Sheet) model)


hide : Model -> Model
hide (Model model) =
    Model (Animator.go Animator.verySlowly Nothing model)


animator : Animator.Animator (Animator.Timeline (Maybe Sheet))
animator =
    Animator.animator
        |> Animator.watchingWith identity (\newSheet _ -> newSheet) (always False)


subscriptions : Model -> Sub Msg
subscriptions (Model model) =
    animator |> Animator.toSubscription Tick model



-- NODE


type alias Details msg =
    { attrs : List (Element.Attribute msg)
    , kids : Element msg
    }


map : (a -> msg) -> Details a -> Details msg
map toMsg details =
    { attrs = details.attrs |> List.map (Element.mapAttribute toMsg)
    , kids = details.kids |> Element.map toMsg
    }



-- VIEW


view : Model -> Details msg -> { onClose : msg } -> Element msg
view (Model model) details { onClose } =
    let
        overlayId : String
        overlayId =
            "sheet-overlay"

        decoder : a -> Decode.Decoder a
        decoder msg =
            Decode.at [ "target", "id" ] Decode.string
                |> Decode.andThen
                    (\id ->
                        if id == overlayId then
                            Decode.succeed msg

                        else
                            Decode.fail "Not a click on overlay."
                    )
    in
    Element.el
        [ Element.Background.color (Element.rgba255 0 0 0 0.5)
        , Element.width Element.fill
        , Element.height Element.fill
        , Element.htmlAttribute (Html.Attributes.id overlayId)
        , Element.htmlAttribute
            (Animator.Inline.opacity model <|
                \currentPage ->
                    if currentPage /= Nothing then
                        Animator.at 1

                    else
                        Animator.at 0
            )
        , Element.htmlAttribute (Html.Events.on "click" (decoder onClose))
        ]
        (Element.el
            ([ Element.Background.color (Element.rgb255 40 40 40)
             , Element.Border.rounded 10
             , Element.centerX
             , Element.centerY
             , Element.htmlAttribute
                (Animator.Inline.xy model <|
                    \currentPage ->
                        if currentPage /= Nothing then
                            { x = Animator.at 0
                            , y = Animator.at 0
                            }

                        else
                            { x = Animator.at 0
                            , y = Animator.at 200
                            }
                )
             ]
                ++ details.attrs
            )
            details.kids
        )
