module Page.Preconditions.Loading exposing (..)

import Http
import Json.Decode as Decode



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

        _ ->
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
    Http.get
        { url = "https://www.tronalddump.io/random/quote"
        , expect = Http.expectJson identity decoder
        }
