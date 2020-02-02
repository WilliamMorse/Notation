port module Main exposing (..)
{-| TodoMVC implemented in Elm, using plain HTML and CSS for rendering.

~/.../to_do$ elm-make to_do.elm --output elm.js

This application is broken up into three key parts:

  1. Model  - a full definition of the application's state
  2. Update - a way to step the application state forward
  3. View   - a way to visualize our application state with HTML

This clean division of concerns is a core part of Elm. You can read more about
this in <http://guide.elm-lang.org/architecture/index.html>
-}

import Dom
import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy, lazy2)
import Json.Decode as Json
import String
import Task
import Port.MathJax as MathJax exposing (..)


main : Program (Maybe Model)
main =
  App.programWithFlags
    { init = init
    , view = view
    , update = updateWithStorage
    , subscriptions = \_ -> Sub.none
    }


port setStorage : Model -> Cmd msg


{-| We want to `setStorage` on every update. This function adds the setStorage
command for every step of the update function.
-}
updateWithStorage : Msg -> Model -> (Model, Cmd Msg)
updateWithStorage msg model =
  let
    (newModel, cmds) =
      update msg model
  in
    ( newModel
    , Cmd.batch [ setStorage newModel, cmds ]
    )



-- MODEL


-- The full application state of our todo app.
type alias Model =
    { entries : List Entry
    , field : String
    , uid : Int
    , visibility : String
    }


type alias Entry =
    { description : String
    , do_show : Bool
    , editing : Bool
    , id : Int
    }




emptyModel : Model
emptyModel =
  { entries = []
  , visibility = "All"
  , field = ""
  , uid = 0
  }


newEntry : String -> Int -> Entry
newEntry desc id =
  { description = desc
  , do_show = True
  , editing = False
  , id = id
  }


init : Maybe Model -> ( Model, Cmd Msg )
init savedModel =
  Maybe.withDefault emptyModel savedModel ! []



-- UPDATE


{-| Users of our app can trigger messages by clicking and typing. These
messages are fed into the `update` function as they occur, letting us react
to them.
-}
type Msg
    = NoOp
    | UpdateField String
    | EditingEntry Int Bool
    | UpdateEntry Int String
    | Add
    | Delete Int
    | DeleteComplete
    | Check Int Bool
    | CheckAll Bool
    | ChangeVisibility String
    | RenderEquation Int


-- How we update our Model on a given Msg?
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    NoOp ->
      model ! []

    Add ->
      { model
        | uid = model.uid + 1
        , field = ""
        , entries =
            if String.isEmpty model.field then
              model.entries
            else
              model.entries ++ [newEntry model.field model.uid]
      }
        ! []

    UpdateField str ->
      { model | field = str }
        ! []

    EditingEntry id isEditing ->
      let
        updateEntry t =
          if t.id == id then { t | editing = isEditing } else t

        focus =
          Dom.focus ("todo-" ++ toString id)
      in
        { model | entries = List.map updateEntry model.entries }
          ! [ Task.perform (\_ -> NoOp) (\_ -> NoOp) focus ]

    UpdateEntry id task ->
      let
        updateEntry t =
          if t.id == id then { t | description = task } else t
      in
        { model | entries = List.map updateEntry model.entries }
          ! []

    Delete id ->
      { model | entries = List.filter (\t -> t.id /= id) model.entries }
        ! []

    DeleteComplete ->
      { model | entries = List.filter (not << .do_show) model.entries }
        ! []

    Check id isShown ->
      let
        updateEntry t =
          if t.id == id then { t | do_show = isShown } else t
      in
        { model | entries = List.map updateEntry model.entries }
          ! []

    CheckAll isShown ->
      let
        updateEntry t =
          { t | do_show = isShown }
      in
        { model | entries = List.map updateEntry model.entries }
          ! []

    ChangeVisibility visibility ->
      { model | visibility = visibility }
        ! []

    RenderEquation id ->
      (model, MathJax.renderEquation "x = y")


-- (List.filter (isTheOne id)
isTheOne the_id item =
  item.id == the_id

getFirst list =
  "\\frac {y - b} {m} = x"

-- VIEW


view : Model -> Html Msg
view model =
  div
    [ class "todomvc-wrapper"
    , style [ ("visibility", "hidden") ]
    ]
    [ section
        [ class "todoapp" ]
        [ lazy2 viewEntries model.visibility model.entries
        , lazy viewInput model.field
        , lazy2 viewControls model.visibility model.entries
        ]
    , infoFooter
    ]


viewInput : String -> Html Msg
viewInput task =
  header
    [ class "header" ]
    [ h1 [] [ text "explain" ]
    , input
        [ class "new-todo"
        , placeholder "Enter math in laTeX, mix in some words to..."
        , autofocus True
        , value task
        , name "newTodo"
        , onInput UpdateField
        , onEnter Add
        ]
        []
    ]


onEnter : Msg -> Attribute Msg
onEnter msg =
  let
    tagger code =
      if code == 13 then msg else NoOp
  in
    on "keydown" (Json.map tagger keyCode)



-- VIEW ALL ENTRIES


viewEntries : String -> List Entry -> Html Msg
viewEntries visibility entries =
  let
    isVisible todo =
      case visibility of
        "Shown" -> not todo.do_show
        "Primary" -> todo.do_show
        _ -> True

    allShown =
      List.all .do_show entries

    cssVisibility =
      if List.isEmpty entries then "hidden" else "visible"
  in
    section
      [ class "main"
      , style [ ("visibility", cssVisibility) ]
      ]
      [ Keyed.ul [ class "todo-list" ] <|
          List.map viewKeyedEntry (List.filter isVisible entries)
      ]



-- VIEW INDIVIDUAL ENTRIES


viewKeyedEntry : Entry -> (String, Html Msg)
viewKeyedEntry todo =
  ( toString todo.id, lazy viewEntry todo )


viewEntry : Entry -> Html Msg
viewEntry todo =
  li
    [ classList [ ("show", todo.do_show), ("editing", todo.editing) ] ]
    [ div
        [ class "view" ]
        [ input
            [ class "toggle"
            , type' "checkbox"
            , checked todo.do_show
            , onClick (Check todo.id (not todo.do_show))
            ]
            []
        , div [id ("math-jax-out-" ++ toString todo.id)] [text "here"]
        , label
            [onDoubleClick (EditingEntry todo.id True) ]
            [ text todo.description ]
        , button
            [ class "destroy"
            , onClick (Delete todo.id)
            ]
            []
        ]
    , input
        [ class "edit"
        , value todo.description
        , name "title"
        , id ("todo-" ++ toString todo.id)
        , onInput (UpdateEntry todo.id)
        , onBlur (EditingEntry todo.id False)
        , onEnter (EditingEntry todo.id False)
        ]
        []
    ]



-- VIEW CONTROLS AND FOOTER


viewControls : String -> List Entry -> Html Msg
viewControls visibility entries =
  let
    entriesPrimary =
      List.length (List.filter .do_show entries)
  in
    footer
      [ class "footer"
      , hidden (List.isEmpty entries)
      ]
      [ lazy viewControlsCount entriesPrimary
      , lazy viewControlsFilters visibility
      ]


viewControlsCount : Int -> Html Msg
viewControlsCount entriesLeft =
  let
    steps_ =
      if entriesLeft == 1 then " step" else " steps"
  in
    span
      [ class "todo-count" ]
      [ strong [] [ text (toString entriesLeft) ]
      , text (" primary " ++ steps_)
      ]


viewControlsFilters : String -> Html Msg
viewControlsFilters visibility =
  ul
    [ class "filters" ]
    [ visibilitySwap "#/" "All" visibility
    , text " "
    , visibilitySwap "#/active" "Primary" visibility
    , text " "
    -- , visibilitySwap "#/do_show" "Shown" visibility
    ]


visibilitySwap : String -> String -> String -> Html Msg
visibilitySwap uri visibility actualVisibility =
  li
    [ onClick (ChangeVisibility visibility) ]
    [ a [ href uri, classList [("selected", visibility == actualVisibility)] ]
        [ text visibility ]
    ]


viewControlsClear : Int -> Html Msg
viewControlsClear entriesShown =
  button
    [ class "clear-show"
    , hidden (entriesShown == 0)
    , onClick DeleteComplete
    ]
    [ text ("Clear show (" ++ toString entriesShown ++ ")")
    ]


infoFooter : Html msg
infoFooter =
  footer [ class "info" ]
    [ p [] [ text "Double-click to edit an equation" ]
    ]