module Session exposing (..)

import Browser.Navigation exposing (Key)
import Url exposing (Url)


type alias Flags =
    ()


type alias Data =
    { url : Url
    , key : Key
    }


init : Flags -> Url -> Key -> ( Data, Cmd Msg )
init flags url key =
    ( Data url key
    , Cmd.none
    )



-- UPDATE


type Msg
    = NoOp


update : Msg -> Data -> ( Data, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


subscriptions : Data -> Sub Msg
subscriptions model =
    Sub.none
