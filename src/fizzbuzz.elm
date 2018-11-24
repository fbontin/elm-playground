module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { first : Int, last : Int }


init : Model
init =
    { first = 1, last = 100 }



-- UPDATE


type Msg
    = Increment
    | Decrement
    | Reset


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            { model | first = model.first + 1 }

        Decrement ->
            { model | first = model.first - 1 }

        Reset ->
            { model | first = 0 }



-- VIEW


toWords : Int -> String
toWords n =
    case ( modBy 3 n, modBy 5 n ) of
        ( 0, 0 ) ->
            "FizzBuzz"

        ( _, 0 ) ->
            "Buzz"

        ( 0, _ ) ->
            "Fizz"

        _ ->
            String.fromInt n


fizzbuzz : Model -> Html Msg
fizzbuzz model =
    List.range model.first model.last
        |> List.map toWords
        |> List.map (\value -> [ text value ])
        |> List.map (Html.li [])
        |> Html.ul []


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Decrement ] [ text "-" ]
        , div [] [ text (String.fromInt model.first ++ " to " ++ String.fromInt model.last) ]
        , button [ onClick Increment ] [ text "+" ]
        , button [ onClick Reset ] [ text "reset" ]
        , div [] [ fizzbuzz model ]
        ]
