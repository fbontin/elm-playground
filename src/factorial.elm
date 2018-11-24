module Main exposing (Model, Msg, init, main, update, view)

import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (value)
import Html.Events exposing (onInput)


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { input : String }


init : Model
init =
    { input = "0" }



-- UPDATE


type Msg
    = Change String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Change newInput ->
            { model | input = newInput }



-- VIEW


factorial : Int -> Int
factorial n =
    if n > 1 then
        n * factorial (n - 1)

    else
        n


view : Model -> Html Msg
view model =
    div []
        [ Html.input [ value model.input, onInput Change ] []
        , div []
            [ model.input
                |> String.toInt
                |> Maybe.withDefault 0
                |> factorial
                |> String.fromInt
                |> String.append "Factorial: "
                |> text
            ]
        ]
