module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Bytes exposing (Bytes)
import Bytes.Encode exposing (encode, sequence)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Parser exposing (DeadEnd, run)
import Parser.Assemble exposing (..)
import Parser.Extra
import Parser.Opcode exposing (opcodeToEncoder)
import Parser.Program exposing (..)
import Result.Extra exposing (..)
import Url


gauss =
    """var exponent: FloatArray
var frontLedCount: Int
fun init(ledCount: Int): Int {
    var halfFrontLedCount: Int
    var i: Int

    exponent = FloatArray[ledCount];
    frontLedCount = ledCount / 2;
    halfFrontLedCount = frontLedCount / 2;
    i = 0;

    while (i < frontLedCount) {
        exponent[i] = -((i - halfFrontLedCount) * (i - halfFrontLedCount)) / 2.0;
        i = i + 1;
    }
    i = 0;
    while (i < frontLedCount) {
        setW(i, 16);
        i = i + 1;
    }

    return 0;
}
fun main(ledCount: Int, counter: Int): Int {
    var c: Float
    var i: Int

    c = 10000.0 * pow(0.2, pow(counter, 0.3));
    c = c * c;
    i = 0;

    while (i < frontLedCount) {
        setR(i + frontLedCount, 255.0 * exp(exponent[i]/c));
        i = i + 1;
    }

    return 0;
}
"""



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



-- MODEL


type alias LampProgram =
    { tree : LampProgramTree
    , assembly : Assembly
    , byteCode : Bytes
    }


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , code : String
    , result : Maybe (Result (List DeadEnd) LampProgram)
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( Model key url gauss Nothing, Cmd.none )



-- UPDATE


type Msg
    = Compile
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | ChangeCode String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeCode newCode ->
            ( { model | code = newCode, result = Just <| compile newCode }, Cmd.none )

        Compile ->
            ( { model | result = Just <| compile model.code }, Cmd.none )

        LinkClicked _ ->
            ( model, Cmd.none )

        UrlChanged _ ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "URL Interceptor"
    , body =
        [ textarea [ rows 40, cols 120, onInput ChangeCode ] [ text model.code ]
        , button [ onClick Compile ] [ text "Compile" ]
        , text (maybeResultToString model.result)
        ]
    }


maybeResultToString : Maybe (Result (List DeadEnd) LampProgram) -> String
maybeResultToString maybe =
    maybe
        |> Maybe.map resultToString
        |> Maybe.withDefault "Nothing done yet"


resultToString : Result (List DeadEnd) LampProgram -> String
resultToString result =
    result
        |> Result.mapError Parser.Extra.deadEndsToString
        |> Result.map (\_ -> "Success")
        |> Result.Extra.merge


compile : String -> Result (List DeadEnd) LampProgram
compile code =
    run programParser code
        |> Result.map programTreeToProgram


programTreeToProgram : LampProgramTree -> LampProgram
programTreeToProgram tree =
    let
        assembly =
            assemble tree internalFns

        byteCode =
            assembly
                |> Result.map (List.map opcodeToEncoder)
                |> Result.map sequence
                |> Result.map encode
                |> Result.withDefault empty
    in
    { tree = tree
    , assembly = assembly
    , byteCode = byteCode
    }


internalFns : List PoolFunction
internalFns =
    [ internalFnDef 0 "getR" [] Integer
    , internalFnDef 1 "setR" [ Variable "index" Integer, Variable "value" Integer ] Integer
    , internalFnDef 2 "getG" [] Integer
    , internalFnDef 3 "setG" [ Variable "index" Integer, Variable "value" Integer ] Integer
    , internalFnDef 4 "getB" [] Integer
    , internalFnDef 5 "setB" [ Variable "index" Integer, Variable "value" Integer ] Integer
    , internalFnDef 6 "getW" [] Integer
    , internalFnDef 7 "setW" [ Variable "index" Integer, Variable "value" Integer ] Integer
    , internalFnDef 8 "pow" [ Variable "b" Float, Variable "e" Float ] Float
    , internalFnDef 9 "exp" [ Variable "p" Float ] Float
    , internalFnDef 10 "sin" [ Variable "a" Float ] Float
    , internalFnDef 11 "cos" [ Variable "a" Float ] Float
    , internalFnDef 12 "sqrt" [ Variable "s" Float ] Float
    , internalFnDef 13 "setHSV" [ Variable "index" Integer, Variable "hue" Float, Variable "sat" Float, Variable "val" Float ] Integer
    ]


internalFnDef : Int -> String -> List Variable -> Type -> PoolFunction
internalFnDef key name params retTyp =
    { key = key
    , name = name
    , parameters = params
    , returnType = retTyp
    , locals = []
    , statements = []
    , isInternal = True
    }


empty : Bytes
empty =
    Bytes.Encode.encode (Bytes.Encode.sequence [])
