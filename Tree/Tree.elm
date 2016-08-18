module Tree.Tree exposing (..)

--import List exposing(..)

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

hasParentIn : comparable -> Tree comparable -> Bool
hasParentIn node tree =
    case tree of
      Zip ->
        False

      Node y cl ->
        if List.member (leaf node) cl then True
        else List.member True (List.map (hasParentIn node) cl)

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


depth : Tree a -> Int
depth tree =
    case tree of
      Zip -> 0
      Node v [left, right] ->
          1 + max (depth left) (depth right)
      _ -> 0


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
