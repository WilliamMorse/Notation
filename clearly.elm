module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Array as A exposing (Array)
import Browser exposing (..)
import Browser.Dom as Bd
import Browser.Events as Be exposing (onClick)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
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


type alias SolvingStep =
    { operation : String
    , equation : String
    , notes : String
    }


type alias Model =
    { windowHeight : Int
    , windowWidth : Int
    , ids : Array Int
    , equations : Array String
    , operations : Array String
    , notes : Array String
    , editSteps : Bool
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        vp =
            Bd.getViewport

        model =
            { windowHeight = 200
            , windowWidth = 200
            , ids = A.initialize 1 identity
            , equations = A.fromList [ "y=mx+b" ]
            , operations = A.fromList [ "Operation: Begin With" ]
            , notes = A.fromList [ "This is a starting example" ]
            , editSteps = False
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
    | EquationText Int String
    | NotesText Int String
    | ToggleEditing


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
                    A.initialize (A.length model.equations + 1) identity

                e =
                    A.push "y=mx+b" model.equations

                n =
                    A.push "Albert Einstein (/ˈaɪnstaɪn/ EYEN-styne;[4] German: [ˈalbɛɐ̯t ˈʔaɪnʃtaɪn] (About this soundlisten); 14 March 1879 – 18 April 1955) was a German-born theoretical physicist[5] who developed the theory of relativity, one of the two pillars of modern physics (alongside quantum mechanics).[3][6]:274 His work is also known for its influence on the philosophy of science.[7][8] He is best known to the general public for his mass–energy equivalence formula E = mc2, which has been dubbed \"the world's most famous equation\".[9] He received the 1921 Nobel Prize in Physics \"for his services to theoretical physics, and especially for his discovery of the law of the photoelectric effect\",[10] a pivotal step in the development of quantum theory." model.notes

                o =
                    A.push "How did you get that one??" model.operations
            in
            ( { model | ids = i, equations = e, notes = n, operations = o }, Cmd.none )

        EquationText index newEquation ->
            ( { model | equations = A.set index newEquation model.equations }
            , Cmd.none
            )

        NotesText index newNote ->
            ( { model | notes = A.set index newNote model.notes }
            , Cmd.none
            )

        ToggleEditing ->
            if model.editSteps then
                ( { model | editSteps = False }
                , Cmd.none
                )

            else
                ( { model | editSteps = True }
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


viewSolvingStepParagraphStyle : Model -> Int -> Element Msg
viewSolvingStepParagraphStyle model index =
    column
        [ width fill
        , spacing 15
        ]
        [ row
            [ width fill
            , spacing 15
            ]
            [ el [ alignLeft, width (fillPortion 1) ] (paragraph [] [ text (getString index model.operations) ])
            , el [ width (fillPortion 5) ] (text (getString index model.equations))
            ]
        , row
            [ width fill
            , spacing 15
            ]
            [ el [ alignLeft, width (fillPortion 1) ] none
            , paragraph [ width (fillPortion 5) ] [ text (getString index model.notes) ]
            ]
        ]


viewSolvingStep : Model -> Int -> Element Msg
viewSolvingStep model index =
    if model.editSteps then
        column
            []
            [ Input.text
                [ Font.center ]
                { onChange = EquationText index
                , text =
                    case A.get index model.equations of
                        Just a ->
                            a

                        Nothing ->
                            ""
                , placeholder = Nothing
                , label = Input.labelRight [ Font.size 14, centerY, padding 5 ] (text (String.fromInt index))
                }
            , Input.multiline
                [ Font.size 14 ]
                { onChange = NotesText index
                , text =
                    case A.get index model.notes of
                        Just a ->
                            a

                        Nothing ->
                            ""
                , placeholder = Nothing
                , label = Input.labelAbove [ Font.size 5 ] (text "")
                , spellcheck = True
                }
            ]

    else
        viewSolvingStepParagraphStyle model index


view : Model -> Html Msg
view model =
    layout
        []
        (column
            [ padding 0, width fill, centerX, spacing 15, Border.color (rgba 0 0 0 0), Border.widthXY 20 20 ]
            (List.concat
                [ List.map (viewSolvingStep model) (A.toList model.ids)
                , [ text
                        (model.equations
                            |> A.length
                            |> String.fromInt
                        )
                  , Input.button
                        [ Border.width 2
                        , Border.rounded 5
                        , Border.color grey
                        , padding 10
                        ]
                        { onPress = Just GenerateNewEntry, label = text " New solving step " }
                  , Input.button
                        [ Border.width 2
                        , Border.rounded 5
                        , Border.color grey
                        , padding 10
                        ]
                        { onPress = Just ToggleEditing, label = text " toggle editing " }
                  ]
                ]
            )
        )
