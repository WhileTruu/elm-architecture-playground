module Session exposing (Data, Flags, init)

import Browser.Navigation exposing (Key)
import Url exposing (Url)


type alias Flags =
    ()


type alias Data =
    { url : Url
    , key : Key
    }


init : Flags -> Url -> Key -> Data
init _ url key =
    Data url key
