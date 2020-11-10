module Page.Preconditions.Loading exposing (..)

import Http
import Json.Decode as Decode
import Process
import Task exposing (Task)



-- MODEL


type alias Model =
    { kanyeQuote : Maybe String
    , taylorSwiftQuote : Maybe String
    , donaldTrumpQuote : Maybe String
    }


type alias Data =
    { kanyeQuote : String
    , taylorSwiftQuote : String
    , donaldTrumpQuote : String
    }


init : ( Model, Cmd Msg )
init =
    ( Model Nothing Nothing Nothing
    , Cmd.batch
        [ getKanyeQuote
            |> Cmd.map (Result.map (\a b -> { b | kanyeQuote = Just a }))
        , getTaylorSwiftQuote
            |> Cmd.map (Result.map (\a b -> { b | taylorSwiftQuote = Just a }))
        , getDonaldTrumpQuote
            |> Cmd.map (Result.map (\a b -> { b | donaldTrumpQuote = Just a }))
        ]
        |> Cmd.map LoadingMsg
    )


modelToData : Model -> Maybe Data
modelToData loadingModel =
    Maybe.map3 Data
        loadingModel.kanyeQuote
        loadingModel.taylorSwiftQuote
        loadingModel.donaldTrumpQuote



-- UPDATE


type Msg
    = LoadingMsg (Result Http.Error (Model -> Model))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadingMsg (Result.Ok toModel) ->
            ( toModel model, Cmd.none )

        LoadingMsg (Result.Err _) ->
            ( model, Cmd.none )



-- HTTP


getKanyeQuote : Cmd (Result Http.Error String)
getKanyeQuote =
    let
        decoder : Decode.Decoder String
        decoder =
            Decode.field "quote" Decode.string
    in
    Http.get
        { url = "https://api.kanye.rest"
        , expect = Http.expectJson identity decoder
        }


getTaylorSwiftQuote : Cmd (Result Http.Error String)
getTaylorSwiftQuote =
    let
        decoder : Decode.Decoder String
        decoder =
            Decode.field "quote" Decode.string
    in
    Http.get
        { url = "https://api.taylor.rest"
        , expect = Http.expectJson identity decoder
        }


getDonaldTrumpQuote : Cmd (Result Http.Error String)
getDonaldTrumpQuote =
    let
        decoder : Decode.Decoder String
        decoder =
            Decode.field "value" Decode.string
    in
    Process.sleep 5000
        |> Task.andThen
            (always <|
                getTask
                    { url = "https://www.tronalddump.io/random/quote"
                    , resolver = resolveJson decoder
                    }
            )
        |> Task.attempt identity



-- HTTP


getTask : { url : String, resolver : Http.Resolver x a } -> Task x a
getTask r =
    Http.task
        { method = "GET"
        , headers = []
        , url = r.url
        , body = Http.emptyBody
        , resolver = r.resolver
        , timeout = Nothing
        }


resolveJson : Decode.Decoder a -> Http.Resolver Http.Error a
resolveJson decoder =
    Http.stringResolver <|
        \response ->
            case response of
                Http.BadUrl_ url ->
                    Err (Http.BadUrl url)

                Http.Timeout_ ->
                    Err Http.Timeout

                Http.NetworkError_ ->
                    Err Http.NetworkError

                Http.BadStatus_ metadata _ ->
                    Err (Http.BadStatus metadata.statusCode)

                Http.GoodStatus_ _ body ->
                    case Decode.decodeString decoder body of
                        Ok value ->
                            Ok value

                        Err err ->
                            Err (Http.BadBody (Decode.errorToString err))
