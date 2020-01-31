module Main exposing (Msg(..), main, many_buttons, update, view)

import Html exposing (button, div, text)
import Html.App exposing (beginnerProgram)
import Html.Events exposing (onClick)


main =
    beginnerProgram { model = 1, view = view, update = update }


view model =
    div []
        [ if model > 1 then
            button [ onClick Decrement ] [ text "-" ]

          else
            text "no fewer"
        , div [] [ text (toString model) ]
        , div [] (many_buttons model)
        ]


many_buttons : number -> List (Html.Html Msg)
many_buttons count =
    if count > 0 then
        List.append (many_buttons (count - 1)) [ button [ onClick Increment ] [ text "+" ] ]

    else
        []


type Msg
    = Increment
    | Decrement


update msg model =
    case msg of
        Increment ->
            model + 1

        Decrement ->
            model - 1
