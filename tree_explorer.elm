import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Tree.Tree as Tree exposing (..)
import String exposing (..)


main =
  Html.beginnerProgram { model = model, view = view, update = update }

-- model
type alias Model =
  { operation : String --List String -- create a string list
  , equation : Tree Int -- Tree number
  , comments : String -- List String
  , opList : List String -- list to contain operation history
  , under : Int
  , new_node : Int
  }

model : Model
model =
  Model "" (Tree.fromList [5,3, 21, 1, 2, 33]) "" [] 0 0


-- update
type Msg
    = Operation String
    | InsertEquation
    | Under String
    | NewNode String
    | Comments String

update : Msg -> Model -> Model
update msg model =
  case msg of
    Operation op ->
      { model | operation = op
                , opList = List.append [op] model.opList } -- append the new operation to the history

    InsertEquation  ->
      { model | equation = (Tree.insertUnder model.under model.new_node Tree.Before model.equation) }

    Under s ->
      { model | under = Result.withDefault 0 (String.toInt s)}

    NewNode s ->
      { model | new_node = Result.withDefault 0 (String.toInt s)}

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
    , input [ type' "text", placeholder "comments", onInput Comments] []
    , p [] [ text model.operation ]
    , ul [] ( unpack_list model.opList )
    , div [] [
        button [ onClick InsertEquation ] [text "insertUnder"]
      , input [ type' "text", placeholder "under", onInput Under] []
      , input [ type' "text", placeholder "new node", onInput NewNode] []
      , displayTree "equation tree " model.equation]
    ]


displayTree : String -> a -> Html msg
displayTree name value =
  div [] [ text (name ++ " ==> " ++ toString value) ]
