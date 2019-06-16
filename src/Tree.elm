module Tree exposing (Operation(..), Step, flatpack, sprout, t)

import Array as A exposing (Array, fromList)
import Dict exposing (Dict)



-- MODEL


type alias Step =
    { operation : String
    , equation : String
    , note : String
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

                Procedure step array ->
                    List.append
                        (List.append
                            [ 0 ]
                            (List.map ((+) 1) (flatpackDepths array))
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



-- Test tools:


st =
    Step "" "" ""


sa =
    Standalone st


pr =
    Procedure st


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
