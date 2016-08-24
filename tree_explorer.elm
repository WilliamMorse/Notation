import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Tree.Tree as Tree exposing (..)
import String exposing (..)
import Keyboard.Extra as Keyboard



--main =
--  Html.beginnerProgram { model = model, view = view, update = update }
main : Program Never
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }

-- model
type alias Model =
  { keyboardModel : Keyboard.Model
  , laTex : String -- edit to the current nodes laTex string
  , equation_edits_tree : Tree Step -- Tree Step
  , current_equation : Step
  , new_node : Int
  }

--model : Model
--model =
--  Model "" (Tree.fromList [(newStep 5),(newStep 3), (newStep 21), (newStep 1), (newStep 2), (newStep 33)]) (newStep 3) 100
init : ( Model, Cmd Msg )
init =
  let
    ( keyboardModel, keyboardCmd ) =
        Keyboard.init
  in
    ( Model keyboardModel "" (Tree.fromList [(newStep 5),(newStep 3), (newStep 21), (newStep 1), (newStep 2), (newStep 33)]) (newStep 3) 100
    , Cmd.map KeyboardMsg keyboardCmd
    )


-- update
type Msg
    = LaTex String
    | InsertEquation
    | GoUp
    | GoDown Int
    | KeyboardMsg Keyboard.Msg



update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    LaTex s -> ({ model | laTex = s }, Cmd.none)
    KeyboardMsg keyMsg ->
      let
          ( keyboardModel, keyboardCmd ) =
              Keyboard.update keyMsg model.keyboardModel
      in
        if   Keyboard.isPressed Keyboard.Enter keyboardModel
--          && Keyboard.isPressed Keyboard.Shift keyboardModel
        then
          ( { model
              | keyboardModel = keyboardModel
              , equation_edits_tree = (Tree.insertUnder model.current_equation (newStep2 model.new_node model.laTex) Tree.Before model.equation_edits_tree)
              , new_node = model.new_node + 1
            }
          , Cmd.map KeyboardMsg keyboardCmd
          )
        else (model, Cmd.none)
    InsertEquation  ->
      ({ model | equation_edits_tree = (Tree.insertUnder model.current_equation (newStep2 model.new_node model.laTex) Tree.Before model.equation_edits_tree),
                new_node = model.new_node + 1 }, Cmd.none)

    GoUp ->
      ({ model | current_equation = (Tree.findParentNode (newStep 0) model.current_equation model.equation_edits_tree) }
      , Cmd.none)

    GoDown y ->
      ({ model | current_equation = newStep y }, Cmd.none)





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
    [ input [ type' "text", placeholder "laTex", onInput LaTex] []
    , p [] [ text model.laTex ]
    , div [] [
        if Tree.hasParentIn model.current_equation model.equation_edits_tree then
          button [ onClick GoUp ] [text "go up"]
        else
          div [] [text "your the tops..."]
      , div [] [text ("the current node is: " ++ toString model.current_equation)]
--      , input [ type' "text", placeholder "enter a new child Id", onInput NewNode] []
      , button [ onClick InsertEquation ] [text "add child under this (current) node"]
      , div [] [childNav model.current_equation model.equation_edits_tree]
      , displayTree "equation tree " model.equation_edits_tree]
    ]

makeChildButton : Tree Step -> Html Msg
makeChildButton tree =
  case tree of
    Zip -> span [] []
    Node y cl ->
      button [onClick (GoDown y.id) ] [text ("go to " ++ toString y)]

childNav : Step -> Tree Step -> Html Msg
childNav node_id tree =
  let
    sub_tree_list n t =
      Tree.findSubTrees n t

    find_node : Step -> Tree Step -> Tree Step
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


-- Subscriptions
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map KeyboardMsg Keyboard.subscriptions
        ]
