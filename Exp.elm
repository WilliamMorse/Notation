module Exp exposing (Model, Msg(..), Path, Step, findThis, followPath, fromBool, init, insertAndUpdateIndex, main, makeTrail, pr, q, sa, splitList, st, subscriptions, t, treeFromList, upOrRoot, update, view, viewProcess, viewStandaloneStep)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Event
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Lazy.LList as LList exposing (LList)
import Lazy.Tree as Tree exposing (Forest, Tree(..))
import Lazy.Tree.Zipper as Zipper exposing (Zipper)


main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


type alias Path =
    List Int


type alias Step =
    { operation : String
    , equation : String
    , note : String
    , id : Int
    , editStep : Bool
    , process : Bool
    }


type alias Model =
    Zipper Step


init : () -> ( Model, Cmd Msg )
init _ =
    let
        root =
            Tree.singleton
                (Step
                    "haiiiillloo"
                    "kashdlkjfaslkdjfalskjdf"
                    "qwertyuiopasdfghjklzxcvbnmqwertyuiopasdfghjklzxcvbnm"
                    1000
                    False
                    True
                )

        startingStep =
            Tree.singleton
                (Step "starting Operation" "y=mx+b" "notesnotesnotes" 0 True False)

        z =
            root
                |> Tree.insert startingStep
                |> Zipper.fromTree
    in
    ( z, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


type Msg
    = EditStep (Zipper Step)
    | RenderStep (Zipper Step)
    | OperationText (Zipper Step) String
    | EquationText (Zipper Step) String
    | NotesText (Zipper Step) String
    | ConsecutiveStep (Zipper Step)
    | NewProcessStep (Zipper Step)
    | ToggleProcess (Zipper Step)


blankStep =
    Step "Operation" "Equation" "Notes" 0 True False


upOrRoot : Zipper a -> Zipper a
upOrRoot z =
    case Zipper.up z of
        Just zip ->
            zip

        Nothing ->
            Zipper.root z


splitList : Int -> Forest Step -> ( Forest Step, Forest Step )
splitList index list =
    let
        normalList =
            LList.toList list

        firstPart =
            List.take index normalList

        secondPart =
            List.drop index normalList
    in
    ( LList.fromList firstPart, LList.fromList secondPart )


incrementId : Forest Step -> Forest Step
incrementId list =
    Tree.forestMap (\s -> { s | id = s.id + 1 }) list


nest : Step -> Zipper Step -> Zipper Step
nest newStep parent =
    let
        newId =
            0

        step =
            { newStep | id = newId }
                |> Tree.singleton
                |> (\a -> LList.fromList [ a ])

        inc =
            incrementId (Tree.descendants (Zipper.getTree parent))

        output =
            LList.concat (LList.fromList [ step, inc ])
                |> Tree (Zipper.current parent)
    in
    Zipper.setTree output parent


insertAndUpdateIndex : Step -> Zipper Step -> Zipper Step
insertAndUpdateIndex newStep previousStepZip =
    let
        parent =
            upOrRoot previousStepZip

        newId =
            previousStepZip
                |> Zipper.current
                |> .id
                |> (+) 1

        step =
            { newStep | id = newId }
                |> Tree.singleton
                |> (\a -> LList.fromList [ a ])

        ( first, last ) =
            splitList newId (Tree.descendants (Zipper.getTree parent))

        inc =
            incrementId last

        output =
            LList.concat (LList.fromList [ first, step, inc ])
                |> Tree (Zipper.current parent)
    in
    Zipper.setTree output parent


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EditStep zip ->
            let
                newModel =
                    zip
                        |> Zipper.map (\a -> { a | editStep = False })
                        |> Zipper.updateItem (\a -> { a | editStep = True })
                        |> Zipper.root
            in
            ( newModel, Cmd.none )

        RenderStep zip ->
            ( zip
                |> Zipper.updateItem (\s -> { s | editStep = False })
                |> Zipper.root
            , Cmd.none
            )

        OperationText zip newOp ->
            ( zip
                |> Zipper.updateItem (\s -> { s | operation = newOp })
                |> Zipper.root
            , Cmd.none
            )

        EquationText zip newEq ->
            ( zip
                |> Zipper.updateItem (\s -> { s | equation = newEq })
                |> Zipper.root
            , Cmd.none
            )

        NotesText zip newNote ->
            ( zip
                |> Zipper.updateItem (\s -> { s | note = newNote })
                |> Zipper.root
            , Cmd.none
            )

        ConsecutiveStep zipling ->
            let
                newModel =
                    zipling
                        |> Zipper.updateItem (\s -> { s | editStep = False })
                        |> insertAndUpdateIndex blankStep
                        |> Zipper.root
            in
            ( newModel, Cmd.none )

        NewProcessStep parent ->
            let
                newModel =
                    parent
                        |> Zipper.updateItem (\s -> { s | editStep = False })
                        |> Zipper.updateItem (\s -> { s | process = True })
                        |> nest blankStep
                        |> Zipper.root
            in
            ( newModel, Cmd.none )

        ToggleProcess zip ->
            if Zipper.isEmpty zip then
                ( zip
                    |> Zipper.updateItem (\a -> { a | process = False })
                    |> Zipper.root
                , Cmd.none
                )

            else
                ( zip
                    |> Zipper.updateItem (\a -> { a | process = not a.process })
                    |> Zipper.root
                , Cmd.none
                )


editOrDisplay : Zipper Step -> Element Msg
editOrDisplay zip =
    let
        step =
            Zipper.current zip
    in
    zip
        |> (if step.editStep then
                editStandaloneStep

            else if step.process then
                viewProcessStep

            else
                viewStandaloneStep
           )


viewProcess : Zipper Step -> List (Element Msg)
viewProcess zip =
    List.map editOrDisplay (Zipper.openAll zip)


viewStandaloneStep : Zipper Step -> Element Msg
viewStandaloneStep zip =
    let
        step =
            Zipper.current zip
    in
    column
        [ width fill
        , spacing 15
        ]
        [ row
            [ width fill
            , spacing 15
            ]
            [ paragraph
                [ alignLeft
                , width (fillPortion 1)
                , Event.onDoubleClick (ToggleProcess zip)
                ]
                [ text step.operation ]
            , row
                [ width (fillPortion 5)
                , Event.onDoubleClick (EditStep zip)
                ]
                [ el [ height fill ] (el [ alignBottom ] (text step.equation))
                , el [ alignRight ] (text (labelEquation zip))
                ]
            ]
        , row
            [ width fill
            , spacing 15
            , Event.onDoubleClick (EditStep zip)
            ]
            [ el [ alignLeft, width (fillPortion 1) ] none -- filler to format the equations and operations
            , paragraph [ width (fillPortion 5) ] [ text step.note ]
            ]
        ]


editStandaloneStep : Zipper Step -> Element Msg
editStandaloneStep zip =
    let
        step =
            Zipper.current zip
    in
    column
        [ width fill
        , spacing 15
        ]
        [ row [ width fill, spacing 15 ]
            [ Input.text [ width (fillPortion 1) ]
                { onChange = OperationText zip
                , text = step.operation
                , placeholder = Nothing
                , label = Input.labelHidden "operation Input"
                }
            , Input.text
                [ width (fillPortion 5)
                ]
                { onChange = EquationText zip
                , text = step.equation
                , placeholder = Nothing
                , label = Input.labelRight [ Font.size 14, centerY, padding 5 ] (text (labelEquation zip))
                }
            ]
        , row [ width fill, spacing 15 ]
            [ column
                [ alignLeft
                , width (fillPortion 1)
                ]
                [ Input.button
                    [ padding 5 ]
                    { onPress = Just (ConsecutiveStep zip)
                    , label = text "new step below"
                    }
                , Input.button
                    [ padding 5 ]
                    { onPress = Just (NewProcessStep zip)
                    , label = text "new child step"
                    }
                , Input.button
                    [ padding 5 ]
                    { onPress = Just (RenderStep zip)
                    , label = text "close editor"
                    }
                ]
            , Input.multiline [ Font.size 20, width (fillPortion 5), height (px 200) ]
                { onChange = NotesText zip
                , text = step.note
                , placeholder = Nothing
                , label = Input.labelAbove [ Font.size 5 ] (text "")
                , spellcheck = True
                }
            ]
        ]


viewProcessStep : Zipper Step -> Element Msg
viewProcessStep zip =
    let
        step =
            Zipper.current zip
    in
    column
        [ width fill
        , spacing 15
        ]
        ([]
            ++ [ row
                    [ width fill
                    , spacing 15
                    ]
                    [ paragraph
                        [ alignLeft
                        , width (fillPortion 1)
                        , Event.onDoubleClick (ToggleProcess zip)
                        ]
                        [ text ("Begin " ++ step.operation) ]
                    , el [ width (fillPortion 5) ] (el [ alignRight ] none)
                    ]
               ]
            ++ viewProcess zip
            ++ [ row
                    [ width fill
                    , spacing 15
                    ]
                    [ paragraph [ alignLeft, width (fillPortion 1) ] [ text ("Finish " ++ step.operation) ]
                    , row [ width (fillPortion 5) ]
                        [ el [ height fill ] (el [ alignBottom ] (text step.equation))
                        , el [ alignRight ] (text (labelEquation zip))
                        ]
                    ]
               , row
                    [ width fill
                    , spacing 15
                    ]
                    [ el [ alignLeft, width (fillPortion 1) ] none -- filler to format the equations and operations
                    , paragraph [ width (fillPortion 5) ] [ text step.note ]
                    ]
               ]
        )


editProcessStep : Zipper Step -> Element Msg
editProcessStep zip =
    let
        step =
            Zipper.current zip
    in
    column
        [ width fill
        , spacing 15
        ]
        [ row [ width fill, spacing 15 ]
            [ Input.text [ width (fillPortion 1) ]
                { onChange = OperationText zip
                , text = step.operation
                , placeholder = Nothing
                , label = Input.labelHidden "operation Input"
                }
            , Input.text
                [ width (fillPortion 5)
                ]
                { onChange = EquationText zip
                , text = step.equation
                , placeholder = Nothing
                , label = Input.labelRight [ Font.size 14, centerY, padding 5 ] (text (labelEquation zip))
                }
            ]
        , row [ width fill, spacing 15 ]
            [ column
                [ alignLeft
                , width (fillPortion 1)
                , Event.onDoubleClick (RenderStep zip)
                ]
                [ Input.button
                    [ padding 5 ]
                    { onPress = Just (ConsecutiveStep zip)
                    , label = text "new step below"
                    }
                , Input.button
                    [ padding 5 ]
                    { onPress = Just (NewProcessStep zip)
                    , label = text "new child step"
                    }
                ]
            , Input.multiline [ Font.size 20, width (fillPortion 5), height (px 200) ]
                { onChange = NotesText zip
                , text = step.note
                , placeholder = Nothing
                , label = Input.labelAbove [ Font.size 5 ] (text "")
                , spellcheck = True
                }
            ]
        ]


fromBool : Bool -> String
fromBool bool =
    case bool of
        True ->
            "True"

        False ->
            "False"


labelEquation : Zipper Step -> String
labelEquation zip =
    zip
        |> Zipper.getPath identity
        |> List.map .id
        --gets rid of the root node id
        |> List.drop 1
        -- convert from computer indexing to one that starts at 1
        |> List.map ((+) 1)
        |> List.map String.fromInt
        |> String.join "."


view : Model -> Html Msg
view model =
    layout []
        (column
            [ width fill
            , padding 30
            , spacing 20
            ]
            (model
                |> viewProcess
            )
        )



{--column
        []
        (model
            |> Zipper.getTree
            |> 
            |> Tree.map Tree.item
            |> List.map viewStandaloneStep
        )--}


st =
    Step "" "" "" 1 False False


sa =
    Tree.singleton st


findThis =
    Tree.singleton (Step "" "" "" 3 False False)


treeFromList : Step -> List (Tree Step) -> Tree Step
treeFromList step list =
    Tree step (LList.fromList list)


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



-- Unused function scratchspace


makeTrail : Path -> List Path
makeTrail path =
    path
        |> List.repeat (List.length path)
        |> List.indexedMap (\i -> List.take (i + 1))
        |> List.foldl (::) []


followPath : Path -> Zipper Step -> Zipper Step
followPath path tree =
    Zipper.attemptOpenPath (\a -> \b -> a == b.id) path tree
