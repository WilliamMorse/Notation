-- pr4
{--
here we want to make a simple interface with two
textboxes a list and a enter button
--}


module Main exposing (Model, Msg(..), f, main, model, unpack_list, update, view)

import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)


main =
    Html.beginnerProgram { model = model, view = view, update = update }



-- model


type alias Model =
    { operation : String --List String -- create a string list
    , equation : String --List String
    , comments : String -- List String
    , opList : List String -- list to contain operation history
    }


model : Model
model =
    Model "" "" "" []



-- update


type Msg
    = Operation String
    | Equation String
    | Comments String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Operation op ->
            { model
                | operation = op
                , opList = List.append [ op ] model.opList
            }

        -- append the new operation to the history
        Equation eq ->
            { model | equation = eq }

        Comments comms ->
            { model | comments = comms }



-- view
--unpack_list : List String -> List (String, Html Msg)


unpack_list l =
    List.map f l



--f : String -> List


f a =
    li [] [ text a ]


view : Model -> Html Msg
view model =
    div []
        [ input [ type' "text", placeholder "operation", onInput Operation ] []
        , input [ type' "text", placeholder "equation", onInput Equation ] []
        , input [ type' "text", placeholder "comments", onInput Comments ] []
        , p [] [ text model.operation ]
        , ul [] (unpack_list model.opList)
        ]
