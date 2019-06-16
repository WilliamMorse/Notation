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
    | Procedure Step (Array Operation)


{-| Flattens a Rose Tree of Operations into one Pre-Order Array. Flat records are easier to
update because we can simply pass the index to the update function.
Use this with the depth function to make an flat records from the operation trees
-}
flatpackSteps : Array Operation -> Array Step
flatpackSteps operations =
    let
        firstOp =
            A.get 0 operations

        restOfOps =
            case List.tail (A.toList operations) of
                Just a ->
                    A.fromList a

                Nothing ->
                    A.empty
    in
    case firstOp of
        Just op ->
            case op of
                Standalone step ->
                    A.append (A.fromList [ step ]) (flatpackSteps restOfOps)

                Procedure step array ->
                    A.append (A.append (A.fromList [ step ]) (flatpackSteps array)) (flatpackSteps restOfOps)

        Nothing ->
            A.empty


{-| Mesures the depth of each node in a Rose Tree of Operations.
The depths are packed into a Pre-Order Array of Ints. Flat records are easier to
update because we can simply pass the index to the update function.
Use this with the flatpack function to make an flat records from the operation trees
-}
flatpackDepths : Array Operation -> Array Int
flatpackDepths operations =
    let
        firstOp =
            A.get 0 operations

        restOfOps =
            case List.tail (A.toList operations) of
                Just a ->
                    A.fromList a

                Nothing ->
                    A.empty
    in
    case firstOp of
        Just op ->
            case op of
                Standalone step ->
                    A.append
                        (A.fromList [ 0 ])
                        (flatpackDepths restOfOps)

                Procedure step array ->
                    A.append
                        (A.append
                            (A.fromList [ 0 ])
                            (A.map ((+) 1) (flatpackDepths array))
                        )
                        (flatpackDepths restOfOps)

        Nothing ->
            A.empty


flatpack : Array Operation -> ( Array Step, Array Int )
flatpack ops =
    ( flatpackSteps ops, flatpackDepths ops )


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
        Just list ->
            d
                |> Dict.remove (depth + 1)
                -- if so, add them to the parent step
                |> mergeDict (Dict.singleton depth [ Procedure step (A.fromList list) ])

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


sprout : ( Array Step, Array Int ) -> Array Operation
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
        Just list ->
            A.fromList list

        Nothing ->
            A.empty



-- Test tools:


st =
    Step "" "" ""


sa =
    Standalone st


pr =
    Procedure st


t =
    A.fromList
        [ sa
        , sa
        , sa
        , pr
            (A.fromList
                [ sa
                , sa
                , pr (A.fromList [ sa, sa ])
                , sa
                , sa
                ]
            )
        , sa
        , sa
        ]
