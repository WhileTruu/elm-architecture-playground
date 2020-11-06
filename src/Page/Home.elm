module Page.Home exposing (Model, Msg, page)

import Animator
import Animator.Inline
import Element exposing (Element)
import Element.Background
import Element.Input
import Html.Attributes
import Html.Events
import Json.Decode as Decode
import Page exposing (Page)
import Skeleton
import Time



-- MODEL


type alias Model =
    { sheet : Animator.Timeline (Maybe Sheet)
    }


type Sheet
    = MySheet


init : ( Model, Cmd msg )
init =
    ( { sheet = Animator.init Nothing }
    , Cmd.none
    )



-- UPDATE


type Msg
    = ClickedShowSheet
    | ClickedHideSheet
    | Tick Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedShowSheet ->
            ( { model | sheet = Animator.go Animator.verySlowly (Just MySheet) model.sheet }
            , Cmd.none
            )

        ClickedHideSheet ->
            ( { model | sheet = Animator.go Animator.verySlowly Nothing model.sheet }
            , Cmd.none
            )

        Tick time ->
            ( Animator.update time animator model
            , Cmd.none
            )



-- VIEW


orElse : Maybe a -> Maybe a -> Maybe a
orElse maybeA maybeB =
    case maybeB of
        Just b ->
            Just b

        Nothing ->
            maybeA


view : Model -> Skeleton.Details Msg
view model =
    { title = "Home"
    , header =
        [ Skeleton.linkSegment { url = "/", text = "home" }
        , Skeleton.linkSegment { url = "/home2", text = "home2" }
        , Skeleton.linkSegment { url = "/poop", text = "poop" }
        ]
    , warning = Skeleton.NoProblems
    , kids = viewContent model
    }


viewContent : Model -> Element Msg
viewContent model =
    Element.column
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.Background.color (Element.rgb255 0 0 0)
        , Element.padding 20
        , Element.inFront
            (if
                (Animator.current model.sheet /= Nothing)
                    || Animator.upcoming Nothing model.sheet
             then
                sheetView model ClickedHideSheet

             else
                Element.none
            )
        ]
        [ Element.Input.button
            [ Element.Background.color (Element.rgb 0 0 255)
            , Element.alignBottom
            , Element.centerX
            , Element.padding 20
            ]
            { onPress = Just ClickedShowSheet
            , label = Element.text "open sheet"
            }
        ]


sheetView : Model -> msg -> Element msg
sheetView model onClose =
    let
        overlayId =
            "sheet-overlay"

        decoder msg =
            Decode.at [ "target", "id" ] Decode.string
                |> Decode.andThen
                    (\id ->
                        if id == overlayId then
                            Decode.succeed msg

                        else
                            Decode.fail "Is not a direct click."
                    )
    in
    Element.el
        [ Element.Background.color (Element.rgba255 0 0 0 0.5)
        , Element.width Element.fill
        , Element.height Element.fill
        , Element.htmlAttribute (Html.Attributes.id overlayId)
        , Element.htmlAttribute (Html.Events.on "click" (decoder onClose))
        , Element.htmlAttribute
            (Animator.Inline.opacity model.sheet <|
                \currentPage ->
                    if currentPage /= Nothing then
                        Animator.at 1

                    else
                        Animator.at 0
            )
        ]
        (Element.column
            [ Element.width (500 |> Element.px)
            , Element.height (500 |> Element.px)
            , Element.Background.color (Element.rgb255 255 0 0)
            , Element.centerX
            , Element.centerY
            , Element.htmlAttribute
                (Animator.Inline.xy model.sheet <|
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
            [ Element.text "lol" ]
        )



-- SUBSCRIPTIONS


animator : Animator.Animator Model
animator =
    Animator.animator
        -- *NOTE*  We're using `the Animator.Css.watching` instead of `Animator.watching`.
        -- Instead of asking for a constant stream of animation frames, it'll only ask for one
        -- and we'll render the entire css animation in that frame.
        |> Animator.watchingWith .sheet
            (\newSheet model ->
                { model | sheet = newSheet }
            )
            (always False)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ animator |> Animator.toSubscription Tick model ]



-- PAGE


page : Page Model Msg
page =
    Page.element
        { init = \_ -> init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
