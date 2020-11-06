module Page exposing
    ( Page
    , Sheet
    , static, sandbox, element, application, applicationWithSheet
    )

{-|

@docs Page
@docs Sheet
@docs static, sandbox, element, application, applicationWithSheet
@docs Upgraded, Bundle, upgrade

-}

import Main.Sheet as Sheet
import Session
import Skeleton


type alias Page model msg =
    { init : Session.Data -> ( model, Cmd msg )
    , update : msg -> model -> ( model, Cmd msg )
    , view : model -> Skeleton.Details msg
    , subscriptions : model -> Sub msg
    , save : model -> Session.Data -> Session.Data
    , load : Session.Data -> model -> ( model, Cmd msg )
    , sheet : Maybe (Sheet model msg)
    }


type alias Sheet model msg =
    { show : model -> Bool
    , view : model -> Sheet.Details msg
    , onHide : msg
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
    , sheet = Nothing
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
    , sheet = Nothing
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
    , sheet = Nothing
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
    { init = page.init
    , update = page.update
    , view = page.view
    , subscriptions = page.subscriptions
    , save = page.save
    , load = page.load
    , sheet = Nothing
    }


applicationWithSheet :
    { init : Session.Data -> ( model, Cmd msg )
    , update : msg -> model -> ( model, Cmd msg )
    , view : model -> Skeleton.Details msg
    , subscriptions : model -> Sub msg
    , save : model -> Session.Data -> Session.Data
    , load : Session.Data -> model -> ( model, Cmd msg )
    , sheet : Sheet model msg
    }
    -> Page model msg
applicationWithSheet page =
    { init = page.init
    , update = page.update
    , view = page.view
    , subscriptions = page.subscriptions
    , save = page.save
    , load = page.load
    , sheet = Just page.sheet
    }


ignoreEffect : model -> ( model, Cmd msg )
ignoreEffect model =
    ( model, Cmd.none )
