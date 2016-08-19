module Tree.Tree exposing (..)

import Html exposing (Html, div, text)
import Html.Attributes exposing (style)

-- TREES

type Placement =
  Before | After

type Tree a
    = Zip
    | Node a (List (Tree a))


empty : Tree a
empty =
    Zip


leaf : a -> Tree a
leaf v =
    Node v []


insert : comparable -> Tree comparable -> Tree comparable
insert x tree =
    case tree of
      Zip ->
        leaf x

      Node y cl ->
        if x <= y then
          Node y (List.append [leaf x] cl)
        else
          Node y (List.append cl [leaf x])


{--}
findParentValue : Int -> Int -> Tree Int -> Int
findParentValue p x tree =
  let
    lookElsewhere p y cl =
      List.head (List.filter (\a -> a > 0) (List.map (findParentValue p x) cl))

    findOtherNodes p y cl =
      let some =
        lookElsewhere p y cl
      in
        case some of
          Just some -> some

          Nothing -> 0

  in
    case tree of
      Zip ->
        0
      Node y cl ->
        if x == y then
          p
        else
          findOtherNodes (getIntVal tree) y cl
--}


getParent : comparable -> Tree comparable -> comparable
getParent node tree =
    case tree of
      Zip ->
        0

      Node y cl ->
        if y == node then
          0
        else
          max_in_list 0 (List.map (getParent node) cl)

{--
getParentAux : comparable -> Tree comparable -> Int
getParentAux node tree =
    case tree of
      Zip ->
        0

      Node y cl ->
        if y == node then True
        else
          List.member True (List.map (getParentAux node) cl)
--}


hasParentIn : comparable -> Tree comparable -> Bool
hasParentIn node tree =
    case tree of
      Zip ->
        False

      Node y cl ->
        if y == node then False
        else
          List.member True (List.map (hasParentAux node) cl)

hasParentAux : comparable -> Tree comparable -> Bool
hasParentAux node tree =
    case tree of
      Zip ->
        False

      Node y cl ->
        if y == node then True
        else
          List.member True (List.map (hasParentAux node) cl)


insertUnder : comparable -> comparable -> Placement -> Tree comparable -> Tree comparable
insertUnder this x inPlace tree =
    case tree of
      Zip ->
        Zip

      Node y cl ->
        if y == this then
          case inPlace of
            After ->
              Node y (List.append cl [leaf x])

            Before ->
              Node y (List.append [leaf x] cl)
        else
          Node y (List.map (insertUnder this x inPlace) cl)


fromList : List comparable -> Tree comparable
fromList xs =
    List.foldl insert empty xs


getIntVal: Tree Int -> Int
getIntVal t =
  case t of
    Node v cl ->
      v
    Zip -> 0

max_in_list : comparable -> List comparable -> comparable
max_in_list v l =
  case l of
    [] -> v
    (f :: rest) ->
      if f > v then
        max_in_list f rest
      else
        max_in_list v rest



depth : Tree comparable -> Int
depth tree =
    case tree of
      Zip -> 0
      Node v cl ->
          1 + max_in_list 0 (List.map depth cl)


{--}
findNode : comparable -> Tree comparable -> List (Tree comparable)
findNode x tree =
  let
    lookElsewhere y cl =
      List.head (List.filter notEmptyList (List.map (findNode x) cl))

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
        if x == y then
          [tree]
        else
          findOtherNodes y cl
--}

notEmptyList: List (Tree number) -> Bool
notEmptyList l =
  case l of
    [] -> False
    _ -> True

map : (a -> a) -> Tree a -> Tree a
map f tree =
    case tree of
      Zip ->
        Zip
      Node v cl ->
        Node (f v) (List.map (map f) cl)


-- TestingGROUNDs


niceTree =
  fromList [5,2,1,4,3]

niceTree2 =
  insertUnder 5 20 Before niceTree

niceTree3 =
  insertUnder 20 22 After niceTree2

niceTree4 =
  insertUnder 8 90 After (insertUnder 20 8 Before niceTree3)

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
    , display "incremented" (map (\n -> n + 1) niceTree)
    , display "insertUnder 5 66 Before " (insertUnder 5 66 Before niceTree)
    , display "insertUnder 5 66 After " (insertUnder 5 66 After niceTree)
    , display "findNode 5 niceTree" (findNode 5 niceTree)
    , display "findNode 20 niceTree4" (findNode 20 niceTree4)
    , display "findNode 22 niceTree4" (findNode 22 niceTree4)
    , display "findNode 8 niceTree4" (findNode 8 niceTree4)
    , display "findNode 88 niceTree4" (findNode 88 niceTree4)
    , display "findNode 83 niceTree4" (findNode 83 niceTree4)
    , display "findNode 8 niceTree4" (findNode 8 niceTree4)
    , display "notEmptyList niceTree  " (List.map notEmptyList [[], [niceTree]])
    , display "hasParentIn 5 niceTree  " (hasParentIn 5 niceTree)
    , display "hasParentIn 3 niceTree  " (hasParentIn 3 niceTree)
    , display "hasParentIn 5 niceTree4  " (hasParentIn 5 niceTree4)
    , display "hasParentIn 20 niceTree4  " (hasParentIn 20 niceTree4)
    , display "hasParentIn 22 niceTree4  " (hasParentIn 22 niceTree4)
    , display "hasParentIn 8 niceTree4  " (hasParentIn 8 niceTree4)
    , display "findParentValue 0 8 niceTree4  " (findParentValue 0 8 niceTree4)
    ]

display : String -> a -> Html msg
display name value =
  div [] [ text (name ++ " ==> " ++ toString value) ]
