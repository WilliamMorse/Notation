module Tree.Tree exposing (..)

import Html exposing (Html, div, text)
import Html.Attributes exposing (style)

-- TREES

type Placement =
  Before | After

type Tree a
    = Zip
    | Node a (List (Tree a))

--type alias Step =
--  { id : Int}
type alias Step =
  { id : Int, eq : String }

newStep : Int -> Step
newStep new_id =
  {id = new_id, eq = ""}

newStep2 : Int -> String -> Step
newStep2 new_id equa =
  {id = new_id, eq = equa}

empty : Tree a
empty =
    Zip


leaf : Step -> Tree Step
leaf v =
    Node v []


insert : Step -> Tree Step -> Tree Step
insert x tree =
    case tree of
      Zip ->
        leaf x

      Node y cl ->
        if x.id <= y.id then
          Node y (List.append [leaf x] cl)
        else
          Node y (List.append cl [leaf x])


{--}
findParentNode : Step -> Step -> Tree Step -> Step
findParentNode p x tree =
  let
    -- lookElsewhere : Step -> Step -> List (Tree Step) -> Step
    lookElsewhere p y cl =
      List.head (List.filter (\a -> a.id > 0) (List.map (findParentNode p x) cl))

    findOtherNodes p y cl =
      let some =
        lookElsewhere p y cl
      in
        case some of
          Just some -> some
          Nothing -> newStep 0

  in
    case tree of
      Zip ->
        newStep 0
      Node y cl ->
        if x.id == y.id then
          p
        else
          findOtherNodes y y cl -- why not getIntVal tree?
--}

{--
getParent : Step -> Tree Step -> Step
getParent node tree =
    case tree of
      Zip ->
        0

      Node y cl ->
        if y == node then
          Step 0
        else
          max_in_list 0 (List.map node cl)
--}

{--
getParentAux : Int -> Tree Int -> Int
getParentAux node tree =
    case tree of
      Zip ->
        0

      Node y cl ->
        if y == node then True
        else
          List.member True (List.map (getParentAux node) cl)
--}


hasParentIn : Step -> Tree Step -> Bool
hasParentIn node tree =
    case tree of
      Zip ->
        False

      Node y cl ->
        if y.id == node.id then False
        else
          List.member True (List.map (hasParentAux node) cl)

hasParentAux : Step -> Tree Step -> Bool
hasParentAux node tree =
    case tree of
      Zip ->
        False

      Node y cl ->
        if y.id == node.id then True
        else
          List.member True (List.map (hasParentAux node) cl)


insertUnder : Step -> Step -> Placement -> Tree Step -> Tree Step
insertUnder this x inPlace tree =
    case tree of
      Zip ->
        Zip

      Node y cl ->
        if y.id == this.id then
          case inPlace of
            After ->
              Node y (List.append cl [leaf x])

            Before ->
              Node y (List.append [leaf x] cl)
        else
          Node y (List.map (insertUnder this x inPlace) cl)


fromList : List Step -> Tree Step
fromList xs =
    List.foldl insert empty xs


max_in_list : Int -> List Int -> Int
max_in_list v l =
  case l of
    [] -> v
    (f :: rest) ->
      if f > v then
        max_in_list f rest
      else
        max_in_list v rest



depth : Tree Step -> Int
depth tree =
    case tree of
      Zip -> 0
      Node v cl ->
          1 + max_in_list 0 (List.map depth cl)

{--}
find : Int -> Tree Step -> Step
find id tree =
  let
    found = List.head (findSubTrees (newStep id) tree)
    found_tree =
      case found of
        Nothing -> leaf (newStep 0)
        Just tree -> tree
  in
    case found_tree of
      Zip -> newStep 0
      Node y cl -> y
--}


{--}
findSubTrees : Step -> Tree Step -> List (Tree Step)
findSubTrees x tree =
  let
    lookElsewhere y cl =
      List.head (List.filter notEmptyList (List.map (findSubTrees x) cl))

    findOtherNodes y cl =
      let some =
        lookElsewhere y cl
      in
        case some of
          Just some -> some

          Nothing -> []

  in
    case tree of
      Zip ->
        []
      Node y cl ->
        if x.id == y.id then
          [tree]
        else
          findOtherNodes y cl
--}

notEmptyList: List (Tree Step) -> Bool
notEmptyList l =
  case l of
    [] -> False
    _ -> True

map : (Step -> Step) -> Tree Step -> Tree Step
map f tree =
    case tree of
      Zip ->
        Zip
      Node v cl ->
        Node (f v) (List.map (map f) cl)


-- TestingGROUNDs


niceTree =
  fromList  [(newStep 5), (newStep 2), (newStep 1), (newStep 4), (newStep 3)]

niceTree2 =
  insertUnder (newStep 5) (newStep 20) Before niceTree

niceTree3 =
  insertUnder (newStep 20) (newStep 22) After niceTree2

niceTree4 =
  insertUnder (newStep 8)  (newStep 90) After (insertUnder (newStep 20) (newStep 8) Before niceTree3)

main =
  div [ style [ ("font-family", "monospace") ] ]
    [
      display "niceTree" niceTree
    , display "niceTree2" niceTree2
    , display "niceTree3" niceTree3
    , display "niceTree4" niceTree4
    , display "depth niceTree" (depth niceTree)
    , display "depth niceTree2" (depth niceTree2)
    , display "depth niceTree3" (depth niceTree3)
    , display "depth niceTree4" (depth niceTree4)
    , display "incremented" (map (\n -> {n | id = n.id + 1}) niceTree)
    , display "insertUnder 5 66 Before " (insertUnder (newStep 5) (newStep 66) Before niceTree)
    , display "insertUnder 5 66 After " (insertUnder (newStep 5) (newStep 66) After niceTree)
    , display "findSubTrees 5 niceTree" (findSubTrees (newStep 5) niceTree)
    , display "findSubTrees 20 niceTree4" (findSubTrees (newStep 20) niceTree4)
    , display "findSubTrees 22 niceTree4" (findSubTrees (newStep 22) niceTree4)
    , display "findSubTrees 8 niceTree4" (findSubTrees (newStep 8) niceTree4)
    , display "findSubTrees 88 niceTree4" (findSubTrees (newStep 88) niceTree4)
    , display "findSubTrees 83 niceTree4" (findSubTrees (newStep 83) niceTree4)
    , display "findSubTrees 8 niceTree4" (findSubTrees (newStep 8) niceTree4)
    , display "notEmptyList niceTree  " (List.map notEmptyList [[], [niceTree]])
    , display "hasParentIn 5 niceTree  " (hasParentIn (newStep 5) niceTree)
    , display "hasParentIn 3 niceTree  " (hasParentIn (newStep 3) niceTree)
    , display "hasParentIn 5 niceTree4  " (hasParentIn (newStep 5) niceTree4)
    , display "hasParentIn 20 niceTree4  " (hasParentIn (newStep 20) niceTree4)
    , display "hasParentIn 22 niceTree4  " (hasParentIn (newStep 22) niceTree4)
    , display "hasParentIn 8 niceTree4  " (hasParentIn (newStep 8) niceTree4)
    , display "hasParentIn 83 niceTree4  " (hasParentIn (newStep 3) niceTree4)
    , display "findParentNode 0 8 niceTree4  " (findParentNode (newStep 0) (newStep 8) niceTree4)
    , display "findParentNode 0 90 niceTree4  " (findParentNode (newStep 0) (newStep 90) niceTree4)
    ]

display : String -> a -> Html msg
display name value =
  div [] [ text (name ++ " ==> " ++ toString value) ]
