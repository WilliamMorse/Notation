--import Element.Background as Background
--import Element.Border as Border
--import Katex as KaTex


port module Exp exposing (Model, Msg(..), Process(..), Step, blankStep, edges, editStep, init, insertBelow, labelEquation, main, nest, render, subscriptions, upOrRoot, update, view, viewKatexEquation, viewNote, viewOperation, viewProcess, viewProcessIcon, viewStep)

import Browser
import Element exposing (..)
import Element.Border as Border
import Element.Events as Event
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes
import Json.Encode as E
import Lazy.Tree as Tree exposing (Forest, Tree(..))
import Lazy.Tree.Zipper as Zipper exposing (Zipper)


port render : E.Value -> Cmd msg


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


type Process
    = Standalone
    | Expanded
    | Collapsed


type alias Step =
    { operation : String
    , equation : String
    , note : String
    , id : Int
    , edit : Bool
    , process : Process
    }


type alias Model =
    Zipper Step


init : () -> ( Model, Cmd Msg )
init _ =
    let
        root =
            Tree.singleton
                (Step
                    "On"
                    "Turtles"
                    "Turtles All the way"
                    -1
                    False
                    Expanded
                )

        startingStep =
            Tree.singleton
                (Step "Starting Operation" "y=mx+b" "notesnotesnotes" 0 False Standalone)

        zip =
            root
                |> Tree.insert startingStep
                |> Zipper.fromTree
    in
    ( zip, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
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
    | RenderAll


blankStep : Step
blankStep =
    Step "Operation" "Equation" "Notes" 0 False Standalone


upOrRoot : Zipper a -> Zipper a
upOrRoot z =
    case Zipper.up z of
        Just zip ->
            zip

        Nothing ->
            Zipper.root z


insertBelow : Step -> Zipper Step -> Zipper Step
insertBelow step zip =
    nest step (upOrRoot zip)


nest : Step -> Zipper Step -> Zipper Step
nest step zip =
    Zipper.insert (Tree.singleton step) zip
        |> Zipper.update (Tree.sortBy .id)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EditStep zip ->
            ( zip
                |> Zipper.map (\a -> { a | edit = False })
                |> Zipper.updateItem (\a -> { a | edit = True })
                |> Zipper.root
            , Cmd.none
            )

        RenderStep zip ->
            ( zip
                |> Zipper.updateItem (\s -> { s | edit = False })
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

        ConsecutiveStep zip ->
            ( zip
                |> Zipper.map (\s -> { s | edit = False })
                |> insertBelow { blankStep | id = 1 + (Zipper.current zip).id }
                |> Zipper.root
            , Cmd.none
            )

        NewProcessStep parent ->
            ( parent
                |> Zipper.map (\s -> { s | edit = False })
                |> Zipper.updateItem (\s -> { s | process = Expanded })
                |> nest { blankStep | id = List.length <| Zipper.children parent }
                |> Zipper.root
            , Cmd.none
            )

        ToggleProcess zip ->
            case (Zipper.current zip).process of
                Standalone ->
                    ( zip |> Zipper.root, Cmd.none )

                Expanded ->
                    ( zip
                        |> Zipper.updateItem (\a -> { a | process = Collapsed })
                        |> Zipper.root
                    , Cmd.none
                    )

                Collapsed ->
                    ( zip
                        |> Zipper.updateItem (\a -> { a | process = Expanded })
                        |> Zipper.root
                    , Cmd.none
                    )

        RenderAll ->
            ( model, Cmd.batch <| List.map renderAllOpen (Zipper.openAll model) )


viewProcess : Zipper Step -> List (Element Msg)
viewProcess zip =
    if (Zipper.current zip).process == Expanded then
        List.map viewStep (Zipper.openAll zip)

    else
        []


viewProcessIcon : Zipper Step -> Element Msg
viewProcessIcon zip =
    let
        step =
            Zipper.current zip

        a =
            Font.family [ Font.monospace ]
    in
    case step.process of
        Standalone ->
            -- plain step
            Input.button [ a ]
                { onPress = Just <| NewProcessStep zip
                , label = text "[^]"
                }

        Expanded ->
            -- step has a process that we are showing
            Input.button [ a ]
                { onPress = Just <| NewProcessStep zip
                , label = text "[^]"
                }

        Collapsed ->
            -- there is a process that we could show
            Input.button [ a ]
                { onPress = Just <| ToggleProcess zip
                , label = text "[+]"
                }


edges =
    { top = 0
    , right = 0
    , bottom = 0
    , left = 0
    }


viewOperation : Zipper Step -> List (Element Msg)
viewOperation zip =
    List.concat
        [ if (Zipper.current zip).process == Expanded then
            row [ width fill, spacing 10 ]
                [ el
                    [ Font.bold
                    , Event.onDoubleClick <| EditStep zip
                    ]
                  <|
                    text (Zipper.current zip).operation
                , Input.button [ Font.family [ Font.monospace ] ]
                    { onPress = Just <| ToggleProcess zip
                    , label = text "[-]"
                    }
                ]
                :: viewProcess zip

          else
            []
        , [ row [ width fill, spacing 10 ]
                [ el
                    [ Font.bold
                    , Event.onDoubleClick <| EditStep zip
                    ]
                    (text (Zipper.current zip).operation)
                , viewProcessIcon zip
                ]
          ]
        ]


viewKatexEquation : Zipper Step -> Element Msg
viewKatexEquation zip =
    row [ width fill ]
        -- equation
        [ -- leave empty so that KaTex can fill in (perhaps it would be better to use the autorender extension?)
          el
            [ centerX ]
            (html <|
                Html.span [ Html.Attributes.id <| labelEquation zip ] []
            )

        -- equation label
        , el [ alignRight ] (el [ width <| px 100, alignLeft ] <| text <| labelEquation zip)
        ]


viewNote : Zipper Step -> Element Msg
viewNote zip =
    text (Zipper.current zip).note


viewStep : Zipper Step -> Element Msg
viewStep zip =
    let
        step =
            Zipper.current zip
    in
    column
        [ width fill
        , spacing 20
        , paddingEach { edges | left = 10 }
        ]
    <|
        List.concat
            -- operation
            [ viewOperation zip

            -- equation
            , [ viewKatexEquation zip ]

            -- Notes
            , [ viewNote zip ]
            ]


editStep : Zipper Step -> Element Msg
editStep zip =
    let
        step =
            Zipper.current zip
    in
    column [ width fill ]
        [ if step.process == Expanded then
            none

          else
            none
        , Input.multiline []
            { onChange = OperationText zip
            , text = step.operation
            , placeholder = Nothing
            , label = Input.labelAbove [] <| text "Operation"
            , spellcheck = True
            }
        , Input.multiline []
            { onChange = EquationText zip
            , text = step.equation
            , placeholder = Just <| Input.placeholder [] <| text "Equation"
            , label = Input.labelAbove [] <| text <| "Equation " ++ labelEquation zip
            , spellcheck = True
            }
        , Input.multiline []
            { onChange = NotesText zip
            , text = step.note
            , placeholder = Nothing
            , label = Input.labelAbove [] <| text "notes"
            , spellcheck = True
            }
        ]


labelEquation : Zipper Step -> String
labelEquation zip =
    if Zipper.isRoot zip then
        --gets rid of the root node id
        "Turtle Shell"

    else
        zip
            -- get id path to current node
            |> Zipper.getPath .id
            -- drop off the root node (it is never renderd)
            |> List.drop 1
            -- start counting at 1 instead of 0
            |> List.map ((+) 1)
            -- convert to string
            |> List.map String.fromInt
            -- join with dot notation
            |> String.join "."


view : Model -> Html Msg
view model =
    layout []
        (column
            [ width fill
            , padding 30
            , spacing 20
            ]
            (List.concat
                [ model
                    |> viewProcess
                , [ Input.button [ Border.width 4 ]
                        { label = text "Render"
                        , onPress = Just <| RenderAll
                        }
                  ]
                ]
            )
        )


sendToKatex : Zipper Step -> Cmd msg
sendToKatex zip =
    render <|
        E.object
            [ ( "id", E.string (zip |> labelEquation) )
            , ( "eq", E.string (zip |> Zipper.current |> .equation) )
            ]


renderAllOpen : Zipper Step -> Cmd msg
renderAllOpen zip =
    case (Zipper.current zip).process of
        Standalone ->
            sendToKatex zip

        Collapsed ->
            sendToKatex zip

        Expanded ->
            case Zipper.openAll zip of
                [] ->
                    sendToKatex zip

                z ->
                    Cmd.batch (sendToKatex zip :: List.map renderAllOpen z)
