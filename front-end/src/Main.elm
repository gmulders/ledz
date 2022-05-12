module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (..)
import Url
import Parser exposing (run, DeadEnd)
import Parser.Extra exposing (deadEndsToString)
import Parser.Program exposing (..)
import Result.Extra exposing (..)


gauss = """var exponent: FloatArray
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


type alias Model =
  { key : Nav.Key
  , url : Url.Url
  , code : String
  , result : Maybe (Result (List DeadEnd) LampProgram)
  }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
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
    ChangeCode newCode -> ( { model | code = newCode }, Cmd.none )
    Compile -> ( { model | result = Just <| compile model.code }, Cmd.none )
    LinkClicked _ -> ( model, Cmd.none )
    UrlChanged _ -> ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none



-- VIEW

view : Model -> Browser.Document Msg
view model =
  { title = "URL Interceptor"
  , body =
      [ textarea [ rows 40 , cols 120, onInput ChangeCode ] [ text model.code ]
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
