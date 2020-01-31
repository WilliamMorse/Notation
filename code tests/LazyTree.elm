module LazyTree exposing (Path, Step, findThis, genPaths, getPath, lazyIndexedMap, lazyLength, pr, q, sa, st, t, treeFromList)

import Lazy.LList as L exposing (LList)
import Lazy.Tree as Tree exposing (Forest, Tree(..))
import Lazy.Tree.Zipper as Zipper exposing (Zipper(..))


type alias Path =
    List Int


type alias Step =
    { operation : String
    , equation : String 
    , note : String
    , ref : Path
    }


lazyLength : LList a -> Int
lazyLength list =
    L.foldl (\_ i -> i + 1) 0 list


lazyIndexedMap : (Int -> a -> b) -> LList a -> LList b
lazyIndexedMap f input =
    let
        index =
            L.fromList (List.range 0 (lazyLength input))
    in
    L.map2 f index input


getPath : Path -> Int -> Tree Step -> Tree Step
getPath parentRef index op =
    let
        r =
            List.append parentRef [ index + 1 ]

        step =
            Tree.item op
    in
    Tree
        { step | ref = r }
        (L.fromList
            (List.indexedMap
                (getPath r)
                (L.toList (Tree.descendants op))
            )
        )


genPaths : Forest Step -> Forest Step
genPaths tree =
    L.fromList (List.indexedMap (getPath []) (L.toList tree))


st =
    Step "" "" "" []


sa =
    Tree.singleton st


findThis =
    Tree.singleton (Step "" "" "" [ 0, 0, 0, 1, 2, 3 ])


treeFromList : Step -> List (Tree Step) -> Tree Step
treeFromList step list =
    Tree step (L.fromList list)


pr =
    treeFromList st


q =
    pr [ sa, sa, sa, pr [ sa, sa, sa, sa ] ]


t =
    pr
        [ sa
        , sa
        , sa
        , pr
            [ sa
            , sa
            , pr
                [ sa
                , sa
                , pr
                    [ pr
                        [ pr
                            [ pr
                                [ pr
                                    [ findThis
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
            , sa
            , sa
            ]
        , sa
        , sa
        , pr
            [ sa
            , sa
            , sa
            , pr
                [ pr
                    [ pr
                        [ sa
                        ]
                    ]
                ]
            ]
        ]
