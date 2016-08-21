import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Tree.Tree as Tree exposing (..)
import String exposing (..)


main =
  Html.beginnerProgram { model = model, view = view, update = update }

type alias Step =
  { id : Int, eq : String }

-- model
type alias Model =
  { operation : String --List String -- create a string list
  , equation_edits_tree : Tree Step -- Tree Step
  , current_equation : Int
  , opList : List String -- list to contain operation history
  , new_node : Int
  }

model : Model
model =
  Model "" (Tree.fromList [5,3, 21, 1, 2, 33]) 3 [] 0


-- update
type Msg
    = Operation String
    | InsertEquation
    | NewNode String
    | GoUp
    | GoDown Int


update : Msg -> Model -> Model
update msg model =
  case msg of
    Operation op ->
      { model | operation = op
                , opList = List.append [op] model.opList } -- append the new operation to the history

    InsertEquation  ->
      { model | equation_edits_tree = (Tree.insertUnder model.current_equation model.new_node Tree.Before model.equation_edits_tree) }

    NewNode s ->
      { model | new_node = Result.withDefault 0 (String.toInt s)}

    GoUp ->
      { model | current_equation = (Tree.findParentValue 0 model.current_equation model.equation_edits_tree) }

    GoDown y ->
      { model | current_equation = y }



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
    , p [] [ text model.operation ]
    , ul [] ( unpack_list model.opList )
    , div [] [
        if Tree.hasParentIn model.current_equation model.equation_edits_tree then
          button [ onClick GoUp ] [text "go up"]
        else
          div [] [text "your the tops..."]
      , div [] [text ("the current node is: " ++ toString model.current_equation)]
      , input [ type' "text", placeholder "enter a new child Id", onInput NewNode] []
      , button [ onClick InsertEquation ] [text "add child under this (current) node"]
      , div [] [childNav model.current_equation model.equation_edits_tree]
      , displayTree "equation tree " model.equation_edits_tree]
    ]

makeChildButton : Tree Step -> Html Msg
makeChildButton tree =
  case tree of
    Zip -> span [] []
    Node y cl ->
      button [onClick (GoDown y) ] [text ("go to " ++ toString y)]

childNav : Int -> Tree Step -> Html Msg
childNav node_id tree =
  let
    sub_tree_list n t =
      Tree.findNode n t

    find_node : Int -> Tree Step -> Tree Step
    find_node n t =
      st (List.head (sub_tree_list n t))

    st : Maybe (Tree Step) -> Tree Step
    st some_tree =
      case some_tree of
        Just some_tree -> some_tree
        Nothing -> Zip
  in
    case find_node node_id tree of
      Zip ->
        div [] [text "this node has no children at this time"]
      Node y cl ->
        div [] (List.map makeChildButton cl)

displayTree : String -> a -> Html msg
displayTree name value =
  div [] [ text (name ++ " ==> " ++ toString value) ]
