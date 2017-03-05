--port module ListExplorer exposing (..)

import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import List as List exposing (..)
import String exposing (..)
--import Set exposing (..)
import Keyboard.Extra as Keyboard
import Port.Port as Port exposing (..)


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
  , laTeX : String -- edit to the current nodes laTeX string
  , equation_edits_step_list : List Step -- List Step
  , current_equation : Step
  , new_node : Int
  }

--model : Model
--model =
--  Model "" (List.fromList [(newStep 5),(newStep 3), (newStep 21), (newStep 1), (newStep 2), (newStep 33)]) (newStep 3) 100
init : ( Model, Cmd Msg )
init =
  let
    ( keyboardModel, keyboardCmd ) =
        Keyboard.init
  in
    ( Model keyboardModel "" [] 100
    , Cmd.map KeyboardMsg keyboardCmd
    )


-- update
type Msg
    = LaTeX String
    | GoUp
    | GoDown Int
    | KeyboardMsg Keyboard.Msg
    | RenderEquation

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    LaTeX s ->
      { model | laTeX = s } ! []

    KeyboardMsg keyMsg ->
      let
          ( keyboardModel, keyboardCmd ) =
              Keyboard.update keyMsg model.keyboardModel

          next_id = model.new_node + 1
--          get_last_id step =
--            last_child_id cl

          shift_enter = Keyboard.isPressed Keyboard.Enter keyboardModel
            && Keyboard.isPressed Keyboard.Shift keyboardModel
          ctl_back = Keyboard.isPressed Keyboard.ArrowLeft keyboardModel
             && Keyboard.isPressed Keyboard.Control keyboardModel
          ctl_forward = Keyboard.isPressed Keyboard.ArrowRight keyboardModel
             && Keyboard.isPressed Keyboard.Control keyboardModel

      in
        if shift_enter then
          (enterAndGo model keyboardModel next_id) ! [Cmd.map KeyboardMsg keyboardCmd]
        else if ctl_back then
          (goBackUp model) ! [Cmd.map KeyboardMsg keyboardCmd]
--        else if ctl_forward then
--          (goNextDn (get_last_id model.current_equation) model) ! [Cmd.map KeyboardMsg keyboardCmd]
        else
          { model | keyboardModel = keyboardModel } ! [Cmd.map KeyboardMsg keyboardCmd]
--    InsertEquation  ->
--      { model | equation_edits_step_list = (List.insertUnder model.current_equation (newStep2 model.new_node model.laTeX) List.Before model.equation_edits_step_list),
--                new_node = model.new_node + 1 } ! []

    GoUp ->
        (goBackUp model) ! []

    GoDown y ->
      (goNextDn y model) ! []

    RenderEquation ->
      (model, Port.renderEquation model.laTeX)


last_child_id : List Step -> Int
last_child_id cl =
  let
    maybe_last cl =
      List.head (List.reverse cl)
  in
    case (maybe_last cl) of
      Just it ->
        it.id
      Nothing -> -1

goNextDn : Int -> Model -> Model
goNextDn to_id model =
  let
    new_eq = List.find to_id model.equation_edits_step_list
  in
    { model | current_equation = new_eq
            , laTeX = new_eq.eq }

goBackUp : Model -> Model
goBackUp model =
  let
    new_eq = (List.findParentNode (newStep 0) model.current_equation model.equation_edits_step_list)
  in
    { model | current_equation = new_eq
          , laTeX = new_eq.eq }

--enterAndGo : Model -> msg -> Int -> Model
enterAndGo model keyboardModel next_id =
  let
    new_step_list = (List.insertUnder model.current_equation (newStep2 next_id model.laTeX) List.After model.equation_edits_step_list)

    new_current_eq = List.find next_id new_step_list
  in
    { model
      | keyboardModel = keyboardModel
      , equation_edits_step_list = new_step_list
      , current_equation = new_current_eq
      , laTeX = new_current_eq.eq
      , new_node = next_id
    }


-- view
view : Model -> Html Msg
view model =
  div []
    [ input [type' "text", onInput LaTeX, value model.laTeX] []
    , p [id "math-jax-out"] []
    , button [ onClick RenderEquation ] [ text "Render Equation Now" ]
    , div [] [
        if List.hasParentIn model.current_equation model.equation_edits_step_list then
          button [ onClick GoUp ] [text "go back"]
        else
          div [] [text "you're at the very beginning"]
--      , div [] [text ("the current node is: " ++ toString model.current_equation)]
--      , input [ type' "text", placeholder "enter a new child Id", onInput NewNode] []
--      , button [ onClick InsertEquation ] [text "add child under this (current) node"]
      , div [] [childNav model.current_equation model.equation_edits_step_list]
      , displayList "equation edits step_list " model.equation_edits_step_list]
    ]

makeChildButton : List Step -> Html Msg
makeChildButton step_list =
  case step_list of
    Zip -> span [] []
    Node y cl ->
      div [] [button [onClick (GoDown y.id) ] [text "go ahead to:"], span [] [text (toString y)]]

childNav : Step -> List Step -> Html Msg
childNav node_id step_list =
  let
    sub_step_list_list n t =
      List.findSubLists n t

    find_node : Step -> List Step -> List Step
    find_node n t =
      st (List.head (sub_step_list_list n t))

    st : Maybe (List Step) -> List Step
    st some_step_list =
      case some_step_list of
        Just some_step_list -> some_step_list
        Nothing -> Zip
  in
    case find_node node_id step_list of
      Zip ->
        div [] [text "this node has no children at this time"]
      Node y cl ->
        div [] (List.map makeChildButton cl)

displayList : String -> a -> Html msg
displayList name value =
  div [style [("margin-top", "22px")]] [ text (name ++ " ==> " ++ toString value) ]


-- Subscriptions
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map KeyboardMsg Keyboard.subscriptions
        ]
