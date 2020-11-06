module Page exposing
    ( Page
    , static, sandbox, element, application
    )

{-|

@docs Page
@docs static, sandbox, element, application
@docs Upgraded, Bundle, upgrade

-}

import Session
import Skeleton


type alias Page model msg =
    { init : Session.Data -> ( model, Cmd msg )
    , update : msg -> model -> ( model, Cmd msg )
    , view : model -> Skeleton.Details msg
    , subscriptions : model -> Sub msg
    , save : model -> Session.Data -> Session.Data
    , load : Session.Data -> model -> ( model, Cmd msg )
    }


static :
    { view : Session.Data -> Skeleton.Details msg
    }
    -> Page Session.Data msg
static page =
    { init = \session -> ( session, Cmd.none )
    , update = \_ model -> ( model, Cmd.none )
    , view = page.view
    , subscriptions = \_ -> Sub.none
    , save = always identity
    , load = always (identity >> ignoreEffect)
    }


sandbox :
    { init : Session.Data -> model
    , update : msg -> model -> model
    , view : model -> Skeleton.Details msg
    }
    -> Page model msg
sandbox page =
    { init = \session -> ( page.init session, Cmd.none )
    , update = \msg model -> ( page.update msg model, Cmd.none )
    , view = page.view
    , subscriptions = \_ -> Sub.none
    , save = always identity
    , load = always (identity >> ignoreEffect)
    }


element :
    { init : Session.Data -> ( model, Cmd msg )
    , update : msg -> model -> ( model, Cmd msg )
    , view : model -> Skeleton.Details msg
    , subscriptions : model -> Sub msg
    }
    -> Page model msg
element page =
    { init = \session -> page.init session
    , update = \msg model -> page.update msg model
    , view = page.view
    , subscriptions = page.subscriptions
    , save = always identity
    , load = always (identity >> ignoreEffect)
    }


application :
    { init : Session.Data -> ( model, Cmd msg )
    , update : msg -> model -> ( model, Cmd msg )
    , view : model -> Skeleton.Details msg
    , subscriptions : model -> Sub msg
    , save : model -> Session.Data -> Session.Data
    , load : Session.Data -> model -> ( model, Cmd msg )
    }
    -> Page model msg
application page =
    page


ignoreEffect : model -> ( model, Cmd msg )
ignoreEffect model =
    ( model, Cmd.none )
