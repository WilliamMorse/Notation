
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Random
import Html exposing (Html, Attribute, br, div, input, label, span, text)
import Html.App exposing (beginnerProgram)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck)


main =
  beginnerProgram { model = model, view = view, update = update }

-- MODEL

type alias Step =  { name : String, eq : Tree a }
type alias Model =
  { style : Style, steps : List Step
  }


type Style
  = Red
  | Underline
  | Bold

makeStep name eq_tree =
  {name = name, eq = eq_tree}

model : Model
model =
  { style = Bold, steps = [{name="one", eq=Empty}, makeStep "zingo" Empty, {name="zero", eq=Empty}] }


-- UPDATE

type Msg =
  Switch Style


update : Msg -> Model -> Model
update (Switch newStyle) model =
  { model | style = newStyle }



-- VIEW

ltos : List Step -> String
ltos list =
  case list of
    [] ->
        ""

    first :: rest ->
        first.name ++ " " ++ ltos rest



view : Model -> Html Msg
view model =
  div []
    [ span [toStyle model] [text (ltos model.steps) ]
    , radio Red "red" model
    , radio Underline "underline" model
    , radio Bold "bold" model
    ]


toStyle : Model -> Attribute msg
toStyle model =
  style <|
    case model.style of
      Red ->
        [ ("color", "red") ]

      Underline ->
        [ ("text-decoration", "underline") ]

      Bold ->
        [ ("font-weight", "bold") ]


radio : Style -> String -> Model -> Html Msg
radio style name model =
  let
    isSelected =
      model.style == style
  in
    label []
      [ br [] []
      , input [ type' "radio", checked isSelected, onCheck (\_ -> Switch style) ] []
      , text name
      ]

-- TREES

type Tree a
    = Empty
    | Node a (Tree a) (Tree a)

empty : Tree a
empty =
    Empty

leaf : a -> Tree a
leaf v =
    Node v Empty Empty


insert : a -> Bool -> Tree a -> Tree a
insert x side tree =
    case tree of
      Empty ->
          leaf x

      Node y left right ->
          if side then
            Node y left (insert x side right)
          else
            Node y (insert x side left) right
