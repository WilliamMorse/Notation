module Tree exposing (..)

-- TREES


type Placement
    = Before
    | After


type Tree a
    = Zzz
    | Node a (List (Tree a))


empty : Tree a
empty =
    Zzz


leaf : a -> Tree a
leaf v =
    Node v []


insert : comparable -> Tree comparable -> Tree comparable
insert x tree =
    case tree of
        Zzz ->
            leaf x

        Node y cl ->
            if x <= y then
                Node y (List.append [ leaf x ] cl)

            else
                Node y (List.append cl [ leaf x ])


insertUnder : comparable -> comparable -> Placement -> Tree comparable -> Tree comparable
insertUnder this x inPlace tree =
    case tree of
        Zzz ->
            Zzz

        Node y cl ->
            if y == this then
                case inPlace of
                    After ->
                        Node y (List.append cl [ leaf x ])

                    Before ->
                        Node y (List.append [ leaf x ] cl)

            else
                Node y (List.map (insertUnder this x inPlace) cl)


fromList : List comparable -> Tree comparable
fromList xs =
    List.foldl insert empty xs


depth : Tree a -> Int
depth tree =
    case tree of
        Zzz ->
            0

        Node v [ left, right ] ->
            1 + max (depth left) (depth right)

        _ ->
            0


{--}
find : comparable -> Tree comparable -> List (Tree comparable)
find x tree =
    let
        gotsome y cl =
            List.head (List.filter notEmptyList (List.map (find x) cl))

        onlysome y cl =
            let
                some =
                    gotsome y cl
            in
            case some of
                Just some ->
                    some

                Nothing ->
                    []
    in
    case tree of
        Zzz ->
            []

        Node y cl ->
            if x == y then
                [ tree ]

            else
                onlysome y cl
--}


notEmptyList : List (Tree number) -> Bool
notEmptyList l =
    case l of
        [] ->
            False

        _ ->
            True


map : (a -> a) -> Tree a -> Tree a
map f tree =
    case tree of
        Zzz ->
            Zzz

        Node v cl ->
            Node (f v) (List.map (map f) cl)
