module Tree exposing (Operation(..), Path, Step, TreeBuilder, branch, extractStepFrom, fileStep, flatpack, flatpackDepths, flatpackSteps, genPath, generatePaths, listRefs, makeTrail, mergeDict, o, pr, q, sa, searchOperation, searchStep, set, setOperaton, sprout, st, t)

import Array as A exposing (Array, fromList)
import Dict exposing (Dict)



-- MODEL


type alias Path =
    List Int


type alias Step =
    { operation : String
    , equation : String
    , note : String
    , ref : Path
    }


type Operation
    = Standalone Step
    | Procedure Step (List Operation)


{-| Flattens a Rose Tree of Operations into one Pre-Order Array. Flat records are easier to
update because we can simply pass the index to the update function.
Use this with the depth function to make an flat records from the operation trees
-}
flatpackSteps : List Operation -> List Step
flatpackSteps operations =
    case operations of
        firstOp :: restOfOps ->
            case firstOp of
                Standalone step ->
                    List.append
                        [ step ]
                        (flatpackSteps restOfOps)

                Procedure step process ->
                    List.append
                        (List.append
                            [ step ]
                            (flatpackSteps process)
                        )
                        (flatpackSteps restOfOps)

        [] ->
            []


{-| Mesures the depth of each node in a Rose Tree of Operations.
The depths are packed into a Pre-Order Array of Ints. Flat records are easier to
update because we can simply pass the index to the update function.
Use this with the flatpack function to make an flat records from the operation trees
-}
flatpackDepths : List Operation -> List Int
flatpackDepths operations =
    case operations of
        firstOp :: restOfOps ->
            case firstOp of
                Standalone step ->
                    List.append
                        [ 0 ]
                        (flatpackDepths restOfOps)

                Procedure step process ->
                    List.append
                        (List.append
                            [ 0 ]
                            (List.map ((+) 1) (flatpackDepths process))
                        )
                        (flatpackDepths restOfOps)

        [] ->
            []


flatpack : List Operation -> ( Array Step, Array Int )
flatpack ops =
    ( A.fromList (flatpackSteps ops)
    , A.fromList (flatpackDepths ops)
    )


{-| unpacks two flat arrays into a Rose tree of operations. The first input Array
represents the steps, the other Array must contain Ints to represent the
tree depth of each step. We assume Pre-order in the input arrays.
-}
type alias TreeBuilder =
    Dict Int (List Operation)


mergeDict : TreeBuilder -> TreeBuilder -> TreeBuilder
mergeDict pushStep laterSteps =
    Dict.merge
        Dict.insert
        (\x a b -> Dict.insert x (List.append a b))
        Dict.insert
        pushStep
        laterSteps
        Dict.empty


fileStep : ( Step, Int ) -> TreeBuilder -> TreeBuilder
fileStep ( step, depth ) d =
    --check if there are any children to add to the tree branch
    case Dict.get (depth + 1) d of
        Just process ->
            d
                |> Dict.remove (depth + 1)
                -- if so, add them to the parent step
                |> mergeDict (Dict.singleton depth [ Procedure step process ])

        Nothing ->
            d
                --If there are no children we are just going to push
                |> mergeDict (Dict.singleton depth [ Standalone step ])


branch : List ( Step, Int ) -> TreeBuilder -> TreeBuilder
branch stepsDepths branchesToAssemble =
    case stepsDepths of
        nextStep :: restOfSteps ->
            branchesToAssemble
                |> fileStep nextStep
                |> branch restOfSteps

        [] ->
            branchesToAssemble


sprout : ( Array Step, Array Int ) -> List Operation
sprout ( steps, depths ) =
    let
        reversedSteps =
            A.foldl (::) [] steps

        reversedDepths =
            A.foldl (::) [] depths

        zipped =
            List.map2 Tuple.pair reversedSteps reversedDepths
    in
    case Dict.get 0 (branch zipped Dict.empty) of
        Just root ->
            root

        Nothing ->
            []



{- Defines an index system to reference nodes on the tree.
   The convention is taken from textbook equation numbering and software versioning
    1.1.1.2 is at detph 4 and has indicies for depth.
-}


genPath : Path -> Int -> Operation -> Operation
genPath parentRef index op =
    let
        r =
            List.append parentRef [ index ]
    in
    case op of
        Standalone step ->
            Standalone
                { step | ref = r }

        Procedure step furtherOps ->
            Procedure
                { step | ref = r }
                (List.indexedMap (genPath r) furtherOps)


generatePaths : List Operation -> List Operation
generatePaths tree =
    List.indexedMap (genPath []) tree


searchOperation : Path -> List Operation -> Maybe Operation
searchOperation path ops =
    case path of
        index :: restOfPath ->
            case List.drop index ops of
                op :: _ ->
                    case op of
                        Standalone _ ->
                            if restOfPath == [] then
                                Just op

                            else
                                Nothing

                        Procedure _ children ->
                            if restOfPath == [] then
                                Just op

                            else
                                searchOperation restOfPath children

                [] ->
                    Nothing

        [] ->
            Nothing


searchStep : Path -> List Operation -> Maybe Step
searchStep path ops =
    case searchOperation path ops of
        Just op ->
            Just (extractStepFrom op)

        Nothing ->
            Nothing


listRefs : List Operation -> List Path
listRefs ops =
    let
        ( flatSteps, _ ) =
            flatpack ops
    in
    List.map .ref (A.toList flatSteps)


extractStepFrom : Operation -> Step
extractStepFrom op =
    case op of
        Standalone step ->
            step

        Procedure step _ ->
            step



{--
splitOperations : Int -> List Operation -> ( List Operation, List Operation, List Operation )
splitOperations index ops =
    let
        operations =
            ops
                |> A.fromList

        opsBefore =
            operations
                |> A.slice 0 index
                |> A.toList

        operation =
            operations
                |> A.slice index (index + 1)
                |> A.toList

        opsAfter =
            operations
                |> A.slice (index + 1) (A.length operations)
                |> A.toList
    in
    ( opsBefore, operation, opsAfter )


updateOperation : Path -> Operation -> List Operation -> List Operation
updateOperation path newOp tree =
    case path of
        head :: tail ->
            let
                ( before, operation, after ) =
                    splitOperations head tree
            in
            case List.head operation of
                Just (Standalone step) ->
                    List.concat [ before, [ newOp ], after ]

                Just (Procedure step process) ->
                    List.concat
                        [ before
                        , [ Procedure
                                step
                                (updateOperation
                                    tail
                                    newOp
                                    process
                                )
                          ]
                        , after
                        ]

                Nothing ->
                    []

        [] ->
            tree

{--
set : Operation -> List Operation -> List Operation
set newOperation tree =
    let
        step =
            extractStepFrom newOperation

        path =
            step.ref
    in
    updateOperation path newOperation tree
--}
--}


makeTrail : Path -> List Path
makeTrail path =
    path
        |> List.repeat (List.length path)
        |> List.indexedMap (\i -> List.take (i + 1))
        |> List.foldl (::) []


setOperaton : Int -> Operation -> List Operation -> List Operation
setOperaton index op forest =
    forest
        |> A.fromList
        |> A.set index op
        |> A.toList


set : Operation -> List Operation -> List (Maybe Operation)
set op tree =
    let
        trail =
            op
                |> extractStepFrom
                |> .ref
                |> makeTrail
    in
    trail
        |> List.map (\p -> searchOperation p tree)


o =
    Standalone <| Step "hi" "low" "middle" [ 3, 1 ]



{--case inputList of
        head :: tail ->
            List.foldl (::)
                []
                (triangeify
                    (chunks ++ [ inputList ])
                    tail
                )

        [] ->
            chunks
--}
-- Test tools:


st =
    Step "" "" "" []


sa =
    Standalone st


pr =
    Procedure st


q =
    [ sa
    , sa
    , sa
    , pr
        [ sa
        , sa
        ]
    ]


t =
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
                                [ sa
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
