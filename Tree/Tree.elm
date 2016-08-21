module Tree.Tree exposing (..)

import Html exposing (Html, div, text)
import Html.Attributes exposing (style)

-- TREES

type Placement =
  Before | After

type Tree a
    = Zip
    | Node a (List (Tree a))

type alias TestEle =
  { id : Int}

empty : Tree a
empty =
    Zip


leaf : TestEle -> Tree TestEle
leaf v =
    Node v []


insert : TestEle -> Tree TestEle -> Tree TestEle
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
findParentNode : TestEle -> TestEle -> Tree TestEle -> TestEle
findParentNode p x tree =
  let
    -- lookElsewhere : TestEle -> TestEle -> List (Tree TestEle) -> TestEle
    lookElsewhere p y cl =
      List.head (List.filter (\a -> a.id > 0) (List.map (findParentNode p x) cl))

    findOtherNodes p y cl =
      let some =
        lookElsewhere p y cl
      in
        case some of
          Just some -> some
          Nothing -> TestEle 0

  in
    case tree of
      Zip ->
        TestEle 0
      Node y cl ->
        if x.id == y.id then
          p
        else
          findOtherNodes y y cl -- why not getIntVal tree?
--}

{--
getParent : TestEle -> Tree TestEle -> TestEle
getParent node tree =
    case tree of
      Zip ->
        0

      Node y cl ->
        if y == node then
          TestEle 0
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


hasParentIn : TestEle -> Tree TestEle -> Bool
hasParentIn node tree =
    case tree of
      Zip ->
        False

      Node y cl ->
        if y == node then False
        else
          List.member True (List.map (hasParentAux node) cl)

hasParentAux : a -> Tree a -> Bool
hasParentAux node tree =
    case tree of
      Zip ->
        False

      Node y cl ->
        if y == node then True
        else
          List.member True (List.map (hasParentAux node) cl)


insertUnder : TestEle -> TestEle -> Placement -> Tree TestEle -> Tree TestEle
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


fromList : List TestEle -> Tree TestEle
fromList xs =
    List.foldl insert empty xs


getIntVal: Tree TestEle -> Int
getIntVal t =
  case t of
    Node v cl ->
      v.id
    Zip -> 0

max_in_list : number -> List number -> number
max_in_list v l =
  case l of
    [] -> v
    (f :: rest) ->
      if f > v then
        max_in_list f rest
      else
        max_in_list v rest



depth : Tree TestEle -> Int
depth tree =
    case tree of
      Zip -> 0
      Node v cl ->
          1 + max_in_list 0 (List.map depth cl)


{--}
findNode : TestEle -> Tree TestEle -> List (Tree TestEle)
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

notEmptyList: List (Tree TestEle) -> Bool
notEmptyList l =
  case l of
    [] -> False
    _ -> True

map : (TestEle -> TestEle) -> Tree TestEle -> Tree TestEle
map f tree =
    case tree of
      Zip ->
        Zip
      Node v cl ->
        Node (f v) (List.map (map f) cl)


-- TestingGROUNDs


niceTree =
  fromList [{id = 5},{id = 2},{id = 1},{id = 4},{id = 3}]

niceTree2 =
  insertUnder {id = 5} {id = 20} Before niceTree

niceTree3 =
  insertUnder {id = 20} {id = 22} After niceTree2

niceTree4 =
  insertUnder {id = 8}  {id = 90} After (insertUnder {id = 20} {id = 8} Before niceTree3)

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
    , display "insertUnder 5 66 Before " (insertUnder {id = 5} {id = 66} Before niceTree)
    , display "insertUnder 5 66 After " (insertUnder {id = 5} {id = 66} After niceTree)
    , display "findNode 5 niceTree" (findNode {id = 5} niceTree)
    , display "findNode 20 niceTree4" (findNode {id = 20} niceTree4)
    , display "findNode 22 niceTree4" (findNode {id = 22} niceTree4)
    , display "findNode 8 niceTree4" (findNode {id = 8} niceTree4)
    , display "findNode 88 niceTree4" (findNode {id = 88} niceTree4)
    , display "findNode 83 niceTree4" (findNode {id = 83} niceTree4)
    , display "findNode 8 niceTree4" (findNode {id = 8} niceTree4)
    , display "notEmptyList niceTree  " (List.map notEmptyList [[], [niceTree]])
    , display "hasParentIn 5 niceTree  " (hasParentIn {id = 5} niceTree)
    , display "hasParentIn 3 niceTree  " (hasParentIn {id = 3} niceTree)
    , display "hasParentIn 5 niceTree4  " (hasParentIn {id = 5} niceTree4)
    , display "hasParentIn 20 niceTree4  " (hasParentIn {id = 20} niceTree4)
    , display "hasParentIn 22 niceTree4  " (hasParentIn {id = 22} niceTree4)
    , display "hasParentIn 8 niceTree4  " (hasParentIn {id = 8} niceTree4)
    , display "hasParentIn 83 niceTree4  " (hasParentIn {id = 3} niceTree4)
    , display "findParentNode 0 8 niceTree4  " (findParentNode {id = 0} {id = 8} niceTree4)
    , display "findParentNode 0 90 niceTree4  " (findParentNode {id = 0} {id = 90} niceTree4)
    ]

display : String -> a -> Html msg
display name value =
  div [] [ text (name ++ " ==> " ++ toString value) ]
