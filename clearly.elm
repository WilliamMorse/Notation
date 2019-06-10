module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Array as A exposing (Array)
import Browser exposing (..)
import Browser.Dom as Bd
import Browser.Events as Be
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Event
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Task exposing (..)


main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }



-- MODEL


type alias Step =
    { operation : String
    , equation : String
    , note : String
    }


type alias Model =
    { windowHeight : Int
    , windowWidth : Int
    , ids : Array Int
    , steps : Array Step
    , editStep : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        vp =
            Bd.getViewport

        firstStep =
            { operation = "Operation: Begin With"
            , equation = "y=mx+b"
            , note = "This is a starting example"
            }

        model =
            { windowHeight = 200
            , windowWidth = 200
            , ids = A.initialize 1 identity
            , steps = A.fromList [ firstStep ]
            , editStep = 0
            }
    in
    ( model, Task.perform HeresTheViewport Bd.getViewport )



-- UPDATE


subscriptions : Model -> Sub Msg
subscriptions model =
    Be.onResize WindowSize


type Msg
    = NoOp
    | WindowSize Int Int
    | HeresTheViewport Bd.Viewport
    | GenerateNewEntry
    | OperationText Int Step String
    | EquationText Int Step String
    | NotesText Int Step String
    | EditStep Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        WindowSize w h ->
            ( { model | windowHeight = h, windowWidth = w }, Cmd.none )

        HeresTheViewport vp ->
            ( { model
                | windowHeight = round vp.scene.height
                , windowWidth = round vp.scene.width
              }
            , Cmd.none
            )

        GenerateNewEntry ->
            let
                i =
                    A.initialize (A.length model.steps + 1) identity

                newStep =
                    Step
                        "tell us how you got this one!"
                        "y=mx+b"
                        "On the whole, it would take much less energy to aim at the temperatures than at the densities and would be much more feasible. For this reason, physicists have been attempting, all through the nuclear age, to heat thin wisps of hydrogen to enormous temperature. Since the gas is thin, the nuclei are farther apart and collide with each other far fewer times per second. To achieve fusion ignition, therefore, temperatures must be considerably higher than those at the center of the sun. In 1944 Fermi calculated that it might take a temperature of 50,000,000° to ignite a hydrogen-3 fusion with hydrogen-2 under earthly conditions, and 400,000,000° to ignite hydrogen-2 fusion alone. To ignite hydrogen-1 fusion, which is what goes on in the sun (at a mere 15,000,000°), physicists would have to raise their sights to beyond the billion-degree mark."

                s =
                    A.push newStep model.steps
            in
            ( { model | ids = i, steps = s }, Cmd.none )

        OperationText index step newOperation ->
            ( { model | steps = A.set index { step | operation = newOperation } model.steps }
            , Cmd.none
            )

        EquationText index step newEquation ->
            ( { model | steps = A.set index { step | operation = newEquation } model.steps }
            , Cmd.none
            )

        NotesText index step newNote ->
            ( { model | steps = A.set index { step | operation = newNote } model.steps }
            , Cmd.none
            )

        EditStep index ->
            ( { model | editStep = index }
            , Cmd.none
            )



-- VIEW


grey =
    Element.rgb 0.8 0.8 0.8


getString : Int -> Array String -> String
getString index array =
    case A.get index array of
        Just a ->
            a

        Nothing ->
            ""


getStep : Int -> Array Step -> Step
getStep index array =
    case A.get index array of
        Just a ->
            a

        Nothing ->
            Step "" "" ""


viewSolvingStepParagraphStyle : Step -> Int -> Element Msg
viewSolvingStepParagraphStyle step index =
    column
        [ width fill
        , spacing 15
        , Event.onDoubleClick (EditStep index)
        ]
        [ row
            [ width fill
            , spacing 15
            ]
            [ paragraph [ alignLeft, width (fillPortion 1) ] [ text step.operation ]
            , el [ width (fillPortion 5), height fill ] (el [ alignBottom ] (text step.equation))
            ]
        , row
            [ width fill
            , spacing 15
            ]
            [ el [ alignLeft, width (fillPortion 1) ] none -- filler to format the equations and operations
            , paragraph [ width (fillPortion 5) ] [ text step.note ]
            ]
        ]


viewSolvingStep : Model -> Int -> Element Msg
viewSolvingStep model index =
    let
        step =
            getStep index model.steps
    in
    if model.editStep == index then
        column
            [ width fill, spacing 15 ]
            [ row [ width fill, spacing 15 ]
                [ Input.text [ width (fillPortion 1) ]
                    { onChange = OperationText index step
                    , text = step.operation
                    , placeholder = Nothing
                    , label = Input.labelHidden "operation Input"
                    }
                , Input.text
                    [ width (fillPortion 5) ]
                    { onChange = EquationText index step
                    , text = step.equation
                    , placeholder = Nothing
                    , label = Input.labelRight [ Font.size 14, centerY, padding 5 ] (text (String.fromInt index))
                    }
                ]
            , row [ width fill, spacing 15 ]
                [ el [ alignLeft, width (fillPortion 1) ] none
                , Input.multiline [ Font.size 20, width (fillPortion 5), height (px 200) ]
                    { onChange = NotesText index step
                    , text = step.note
                    , placeholder = Nothing
                    , label = Input.labelAbove [ Font.size 5 ] (text "")
                    , spellcheck = True
                    }
                ]
            ]

    else
        viewSolvingStepParagraphStyle step index


view : Model -> Html Msg
view model =
    layout
        []
        (column
            [ padding 0, width fill, centerX, spacing 15, Border.color (rgba 0 0 0 0), Border.widthXY 20 20 ]
            (List.concat
                [ List.map (viewSolvingStep model) (A.toList model.ids)
                , [ text
                        (model.steps
                            |> A.length
                            |> String.fromInt
                        )
                  , Input.button
                        [ Border.width 2
                        , Border.rounded 5
                        , Border.color grey
                        , padding 10
                        ]
                        { onPress = Just GenerateNewEntry
                        , label = text " New solving step "
                        }
                  ]
                ]
            )
        )
