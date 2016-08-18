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
  , equation_edits_tree : Tree Int -- Tree number
  , current_equation : Int
  , opList : List String -- list to contain operation history
  , under : Int
  , new_node : Int
  }

model : Model
model =
  Model "" (Tree.fromList [5,3, 21, 1, 2, 33]) 3 [] 0 0


-- update
type Msg
    = Operation String
    | InsertEquation
    | Under String
    | NewNode String
    | GoUp


update : Msg -> Model -> Model
update msg model =
  case msg of
    Operation op ->
      { model | operation = op
                , opList = List.append [op] model.opList } -- append the new operation to the history

    InsertEquation  ->
      { model | equation_edits_tree = (Tree.insertUnder model.under model.new_node Tree.Before model.equation_edits_tree) }

    Under s ->
      { model | under = Result.withDefault 0 (String.toInt s)}

    NewNode s ->
      { model | new_node = Result.withDefault 0 (String.toInt s)}

    GoUp -> model



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
        if Tree.hasParentIn model.current_equation model.equation_edits_tree then button [ onClick GoUp ] [text "go up"]
        else div [] [text "your the tops..."]
      , div [] [text ("current node = " ++ toString model.current_equation)]
      , div [] [childNav model.current_equation model.equation_edits_tree]
      , button [ onClick InsertEquation ] [text "insertUnder"]
      , input [ type' "text", placeholder "under", onInput Under] []
      , input [ type' "text", placeholder "new node", onInput NewNode] []
      , displayTree "equation tree " model.equation_edits_tree]
    ]

makeChildButton tree =
  case tree of
    Zip -> span [] []
    Node y cl ->
      button [] [text ("go to " ++ toString y)]

childNav : comparable -> Tree comparable -> Html msg
childNav node tree =
  let
    sub_tree_list n t =
      Tree.findNode n t

    sub_tree n t =
      st (List.head (sub_tree_list n t))

    st some =
      case some of
        Just some -> some
        Nothing -> Zip
  in
    case sub_tree node tree of
      Zip -> div [] [text "no children at this time"]
      Node y cl ->
        div [] (List.map makeChildButton cl)

displayTree : String -> a -> Html msg
displayTree name value =
  div [] [ text (name ++ " ==> " ++ toString value) ]
