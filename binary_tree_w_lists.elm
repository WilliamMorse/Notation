
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)


-- TREES

type Placement =
  Before | After

type Tree a
    = Zzz
    | Node a (List (Tree a))


empty : Tree a
empty =
    Zzz


singleton : a -> Tree a
singleton v =
    Node v []


insert : comparable -> Tree comparable -> Tree comparable
insert x tree =
    case tree of
      Zzz ->
        singleton x

      Node y [left, right] ->
          if x > y then
              Node y [left, (insert x right)]

          else if x < y then
              Node y [(insert x left), right]

          else
              tree

      Node y cl ->
        if x <= y then
          Node y (List.append [singleton x] cl)
        else
          Node y (List.append cl [singleton x])
{--

      _ -> tree
--}

insertUnder : comparable -> comparable -> Placement -> Tree comparable -> Tree comparable
insertUnder this x inPlace tree =
    case tree of
      Zzz ->
        Zzz

      Node y cl ->
        if y == this then
          case inPlace of
            After ->
              Node y (List.append cl [singleton x])

            Before ->
              Node y (List.append [singleton x] cl)
        else
          Node y (List.map (insertUnder this x inPlace) cl)


fromList : List comparable -> Tree comparable
fromList xs =
    List.foldl insert empty xs


depth : Tree a -> Int
depth tree =
    case tree of
      Zzz -> 0
      Node v [left, right] ->
          1 + max (depth left) (depth right)
      _ -> 0


map : (a -> a) -> Tree a -> Tree a
map f tree =
    case tree of
      Zzz ->
        Zzz
      Node v cl ->
        Node (f v) (List.map (map f) cl)




-- PLAYGROUND


deepTree =
  fromList [1,2,3]


niceTree =
  fromList [5,2,1,4,3]


main =
  div [ style [ ("font-family", "monospace") ] ]
    [ display "depth deepTree" (depth deepTree)
    , display "depth niceTree" (depth niceTree)
    , display "incremented" (map (\n -> n + 1) niceTree)
    , display "insertUnder 5 20 Before " (insertUnder 5 20 Before niceTree)
    , display "insertUnder 5 20 After " (insertUnder 5 20 After niceTree)
    ]


display : String -> a -> Html msg
display name value =
  div [] [ text (name ++ " ==> " ++ toString value) ]



{-----------------------------------------------------------------

Exercises:

(1) Sum all of the elements of a tree.

       sum : Tree number -> number

(2) Flatten a tree into a list.

       flatten : Tree a -> List a

(3) Check to see if an element is in a given tree.

       isElement : a -> Tree a -> Bool

(4) Write a general fold function that acts on trees. The fold
    function does not need to guarantee a particular order of
    traversal.

       fold : (a -> b -> b) -> b -> Tree a -> b

(5) Use "fold" to do exercises 1-3 in one line each. The best
    readable versions I have come up have the following length
    in characters including spaces and function name:
      sum: 16
      flatten: 21
      isElement: 46
    See if you can match or beat me! Don't forget about currying
    and partial application!

(6) Can "fold" be used to implement "map" or "depth"?

(7) Try experimenting with different ways to traverse a
    tree: pre-order, in-order, post-order, depth-first, etc.
    More info at: http://en.wikipedia.org/wiki/Tree_traversal

-----------------------------------------------------------------}