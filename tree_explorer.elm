import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Tree.Tree as Tree exposing (..)


main =
  Html.beginnerProgram { model = model, view = view, update = update }

-- model
type alias Model =
  { operation : String --List String -- create a string list
  , equation : Tree Int -- Tree number
  , comments : String -- List String
  , opList : List String -- list to contain operation history
  }

model : Model
model =
  Model "" (Tree.fromList [5,3, 21, 1, 2, 33]) "" []


-- update
type Msg
    = Operation String
    | Equation (Int, Int)
    | Comments String

update : Msg -> Model -> Model
update msg model =
  case msg of
    Operation op ->
      { model | operation = op
                , opList = List.append [op] model.opList } -- append the new operation to the history

    Equation (x, y) ->
      { model | equation = (Tree.insertUnder x y Tree.Before model.equation) }

    Comments comms ->
      { model | comments = comms }

-- view


--unpack_list : List String -> List (String, Html Msg)
unpack_list l =
  List.map f l

--f : String -> List
f a =
  li [] [text a]



view : Model -> Html Msg
view model =
  div []
    [ input [ type' "text", placeholder "operation", onInput Operation] []
    , button [ onClick (Equation (3, 5)) ] [text "insertUnder 3 5"]
    , input [ type' "text", placeholder "comments", onInput Comments] []
    , p [] [ text model.operation ]
    , ul [] ( unpack_list model.opList )
    , div [] [displayTree "equation tree " model.equation]
    ]


displayTree : String -> a -> Html msg
displayTree name value =
  div [] [ text (name ++ " ==> " ++ toString value) ]
