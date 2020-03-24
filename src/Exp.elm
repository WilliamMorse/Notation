--import Element.Background as Background
--import Element.Border as Border
--import Katex as KaTex


port module Exp exposing (Model, Msg(..), Process(..), Step, blankStep, edges, editStep, init, insertBelow, labelEquation, main, nest, render, subscriptions, upOrRoot, update, view, viewKatexEquation, viewNote, viewOperation, viewProcess, viewProcessIcon, viewStep)

import Browser
import Browser.Events
import Element exposing (..)
import Element.Background as Backround
import Element.Border as Border
import Element.Events as Event
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes
import Json.Decode as Decode
import Json.Encode as Encode
import Lazy.Tree as Tree exposing (Tree(..))
import Lazy.Tree.Zipper as Zipper exposing (Zipper)


port render : Encode.Value -> Cmd msg


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


type alias IdPath =
    List Int


type alias Step =
    { operation : String
    , equation : String
    , note : String
    , process : Process
    , edit : Bool
    , equationLabel : Int
    , id : Int
    }


type alias Model =
    { steps : Zipper Step
    , seed : Int
    , e : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        root =
            Tree.singleton
                (Step
                    "On"
                    "Turtles"
                    "Turtles All the way"
                    Expanded
                    False
                    -1
                    -1
                )

        startingStep =
            Tree.singleton
                (Step "Starting Operation" "y=mx+b" "notesnotesnotes" Standalone False 0 0)

        zip =
            root
                |> Tree.insert startingStep
                |> Zipper.fromTree
    in
    ( { steps = zip, seed = 1, e = "None yet!" }
    , Cmd.batch <| List.map katexStep (Zipper.openAll zip)
    )



----------------- UPDATE -----------------


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


blankStep : Step
blankStep =
    Step "operation" "equation" "notes notes notes" Standalone False -1 0


upOrRoot : Zipper a -> Zipper a
upOrRoot z =
    Maybe.withDefault (Zipper.root z) (Zipper.up z)


deleteChildren : Zipper a -> Zipper a
deleteChildren zip =
    Zipper.setTree
        (zip
            |> Zipper.current
            |> Tree.singleton
        )
        zip


mapChildren : (a -> a) -> Zipper a -> Zipper a
mapChildren f zip =
    Zipper.openAll zip
        |> List.map (Zipper.updateItem f)
        |> List.map Zipper.getTree
        |> List.foldl Zipper.insert (deleteChildren zip)


incrementGreater : Int -> { a | equationLabel : Int } -> { a | equationLabel : Int }
incrementGreater start a =
    if start <= a.equationLabel then
        { a | equationLabel = a.equationLabel + 1 }

    else
        a


nest : Step -> Zipper Step -> Zipper Step
nest step zip =
    mapChildren (incrementGreater step.equationLabel) zip
        |> Zipper.insert (Tree.singleton step)
        |> Zipper.update (Tree.sortBy .equationLabel)


insertBelow : Step -> Zipper Step -> Zipper Step
insertBelow step zip =
    nest step (upOrRoot zip)



-- ID Management


trace : Zipper Step -> IdPath
trace zip =
    List.drop 1 <| Zipper.getPath .id zip


dive : IdPath -> Zipper { a | id : Int } -> Result String (Zipper { a | id : Int })
dive path zip =
    Zipper.openPath (\a b -> a == b.id) path zip


incrementId : Model -> ( Zipper Step, Cmd Msg ) -> ( Model, Cmd Msg )
incrementId model ( zip, cmd ) =
    ( { model | steps = zip, seed = model.seed + 1 }, cmd )


wrapModel : Model -> ( Zipper Step, Cmd Msg ) -> ( Model, Cmd Msg )
wrapModel model ( zip, cmd ) =
    ( { model | steps = zip }, cmd )



-- update


type Msg
    = NewProcessStep IdPath
    | NewConsecutiveStep IdPath
    | ToggleProcess IdPath
    | EditStep IdPath
    | OperationText IdPath String
    | EquationText IdPath String
    | NotesText IdPath String
    | RenderStep IdPath
    | RenderAll


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewProcessStep parentPath ->
            case dive parentPath model.steps of
                Ok parent ->
                    parent
                        |> Zipper.map (\s -> { s | edit = False })
                        |> Zipper.updateItem (\s -> { s | process = Expanded })
                        |> nest
                            { blankStep
                                | id = model.seed
                                , equationLabel = List.length <| Zipper.children parent
                            }
                        |> upOrRoot
                        |> (\z -> ( Zipper.root z, katexStep z ))
                        |> incrementId model

                Err error ->
                    ( { model | e = error }, Cmd.none )

        NewConsecutiveStep siblingPath ->
            case dive siblingPath model.steps of
                Ok sibling ->
                    sibling
                        |> Zipper.map (\s -> { s | edit = False })
                        |> insertBelow
                            { blankStep
                                | id = model.seed
                                , equationLabel = 1 + (Zipper.current sibling).equationLabel
                                , edit = True
                                , equation = (Zipper.current sibling).equation
                            }
                        --check command
                        |> upOrRoot
                        |> (\z -> ( Zipper.root z, katexStep z ))
                        |> incrementId model

                Err _ ->
                    ( model, Cmd.none )

        ToggleProcess path ->
            case dive path model.steps of
                Ok zip ->
                    case (Zipper.current zip).process of
                        Standalone ->
                            ( model, Cmd.none )

                        Expanded ->
                            zip
                                |> Zipper.updateItem
                                    (\a -> { a | process = Collapsed })
                                |> (\z ->
                                        ( { model | steps = Zipper.root z }
                                        , katexStep z
                                        )
                                   )

                        Collapsed ->
                            zip
                                |> Zipper.updateItem (\a -> { a | process = Expanded })
                                |> (\z -> ( { model | steps = Zipper.root z }, katexStep z ))

                Err _ ->
                    ( model, Cmd.none )

        EditStep path ->
            case dive path model.steps of
                Ok zip ->
                    zip
                        |> Zipper.map (\a -> { a | edit = False })
                        |> Zipper.updateItem (\a -> { a | edit = True })
                        |> (\z -> ( Zipper.root z, katexStep z ))
                        |> wrapModel model

                Err _ ->
                    ( model, Cmd.none )

        OperationText path newText ->
            case dive path model.steps of
                Ok zip ->
                    zip
                        |> Zipper.updateItem (\s -> { s | operation = newText })
                        |> (\z -> ( Zipper.root z, Cmd.none ))
                        |> wrapModel model

                Err _ ->
                    ( model, Cmd.none )

        EquationText path newEq ->
            case dive path model.steps of
                Ok zip ->
                    zip
                        |> Zipper.updateItem (\s -> { s | equation = newEq })
                        |> (\z -> ( Zipper.root z, katexStep z ))
                        |> wrapModel model

                Err _ ->
                    ( model, Cmd.none )

        NotesText path newNote ->
            case dive path model.steps of
                Ok zip ->
                    zip
                        |> Zipper.updateItem (\s -> { s | note = newNote })
                        |> (\z -> ( Zipper.root z, Cmd.none ))
                        |> wrapModel model

                Err _ ->
                    ( model, Cmd.none )

        RenderStep path ->
            case dive path model.steps of
                Ok zip ->
                    zip
                        |> Zipper.updateItem (\s -> { s | edit = False })
                        |> (\z -> ( Zipper.root z, katexStep z ))
                        |> wrapModel model

                Err _ ->
                    ( model, Cmd.none )

        RenderAll ->
            ( model, katexStep model.steps )



-------------------------- View --------------------------


backroundColor : Color
backroundColor =
    rgb 0.9 0.9 0.9


paperColor : Color
paperColor =
    rgb 1 1 1


shadowColor : Color
shadowColor =
    rgb 0.5 0.5 0.5


viewProcess : Zipper Step -> List (Element Msg)
viewProcess zip =
    if (Zipper.current zip).process == Expanded then
        List.map card (Zipper.openAll zip)

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
                { onPress = Just <| NewProcessStep (trace zip)
                , label = text "[^]"
                }

        Expanded ->
            -- step has a process that we are showing
            Input.button [ a ]
                { onPress = Just <| NewProcessStep (trace zip)
                , label = text "[^]"
                }

        Collapsed ->
            -- there is a process that we could show
            Input.button [ a ]
                { onPress = Just <| ToggleProcess (trace zip)
                , label = text "[+]"
                }


viewOperation : Zipper Step -> List (Element Msg)
viewOperation zip =
    List.concat
        [ if (Zipper.current zip).process == Expanded then
            row [ width fill, padding 20 ]
                [ el
                    [ Font.bold ]
                  <|
                    text (Zipper.current zip).operation
                , Input.button [ Font.family [ Font.monospace ] ]
                    { onPress = Just <| ToggleProcess (trace zip)
                    , label = text "[-]"
                    }
                ]
                :: viewProcess zip

          else
            [ none ]
        , [ row [ width fill, padding 20 ]
                [ el
                    [ Font.bold
                    , Font.family [ Font.typeface "Computer Modern", Font.serif ]
                    , Event.onDoubleClick <| EditStep (trace zip)
                    ]
                    (text (Zipper.current zip).operation)
                , viewProcessIcon zip
                ]
          ]
        ]


katexPlaceholder : Int -> Element Msg
katexPlaceholder id =
    html <|
        Html.span [ Html.Attributes.id <| String.fromInt id ] []


viewKatexEquation : Zipper Step -> Element Msg
viewKatexEquation zip =
    row [ width fill ]
        -- equation
        [ -- leave empty so that KaTex can fill in (perhaps it would be better to use the autorender extension?)
          el
            [ centerX ]
            (katexPlaceholder <| .id <| Zipper.current zip)

        -- equation label
        , el [ alignRight ] (el [ width <| px 100, alignLeft ] <| text <| labelEquation zip)
        ]


viewNote : Zipper Step -> Element Msg
viewNote zip =
    el
        [ width fill
        , padding 20
        , Font.family [ Font.typeface "Computer Modern", Font.serif ]
        ]
    <|
        text (Zipper.current zip).note


viewStep : Zipper Step -> Element Msg
viewStep zip =
    let
        step =
            Zipper.current zip
    in
    if step.edit then
        editStep zip

    else
        column [ width fill, spacing 6 ] <|
            List.concat
                -- operation
                [ viewOperation zip

                -- equation
                , [ viewKatexEquation zip ]

                -- Notes
                , [ viewNote zip ]
                ]


edges : { top : Int, right : Int, bottom : Int, left : Int }
edges =
    { top = 0
    , right = -3
    , bottom = 0
    , left = 3
    }


card : Zipper Step -> Element Msg
card zip =
    el
        [ width fill
        , spacing 20
        , Backround.color paperColor
        , Border.rounded 20
        , Border.shadow { blur = 0, color = shadowColor, offset = ( 0, 0 ), size = 1 }
        ]
        (viewStep zip)


editStep : Zipper Step -> Element Msg
editStep zip =
    let
        step =
            Zipper.current zip
    in
    column [ width fill, padding 20, spacing 10, Font.family [ Font.typeface "Computer Modern", Font.serif ] ]
        [ if step.process == Expanded then
            none

          else
            none
        , Input.button [ Font.family [ Font.monospace ] ]
            { onPress = Just <| RenderStep (trace zip)
            , label = text "[x]"
            }
        , Input.button [ Font.family [ Font.monospace ] ]
            { onPress = Just <| NewConsecutiveStep (trace zip)
            , label = text "[=]"
            }
        , Input.multiline []
            { onChange = OperationText (trace zip)
            , text = step.operation
            , placeholder = Nothing
            , label = Input.labelAbove [] <| text "Operation"
            , spellcheck = True
            }
        , el [ centerX ] (katexPlaceholder step.id)
        , Input.multiline []
            { onChange = EquationText (trace zip)
            , text = step.equation
            , placeholder = Just <| Input.placeholder [] <| text "Equation"
            , label = Input.labelRight [ centerY ] <| text <| "Equation " ++ labelEquation zip
            , spellcheck = False
            }
        , Input.multiline []
            { onChange = NotesText (trace zip)
            , text = step.note
            , placeholder = Nothing
            , label = Input.labelAbove [] <| text "notes"
            , spellcheck = True
            }
        ]


labelEquation : Zipper Step -> String
labelEquation zip =
    if Zipper.isRoot zip then
        --gets rid of the root node equationLabel
        "Turtle Shell"

    else
        zip
            -- get equationLabel path to current node
            |> Zipper.getPath .equationLabel
            -- drop off the root node (it is never rendered)
            |> List.drop 1
            -- start counting at 1 instead of 0
            |> List.map ((+) 1)
            -- convert to string
            |> List.map String.fromInt
            -- join with dot notation
            |> String.join "."


view : Model -> Html Msg
view model =
    layout
        [ Backround.color backroundColor
        ]
        (column
            [ width fill
            , padding 30
            , spacing 20
            ]
            (List.concat
                [ model.steps
                    |> viewProcess
                , [ Input.button [ Border.width 4 ]
                        { label = text "Render"
                        , onPress = Just <| RenderAll
                        }
                  ]
                , [ text model.e ]
                ]
            )
        )


katexStep : Zipper Step -> Cmd msg
katexStep zip =
    let
        step =
            Zipper.current zip

        children =
            Zipper.openAll zip
    in
    Cmd.batch
        (katexRenderEquation step
            :: (if step.process == Expanded then
                    List.map katexStep children

                else
                    [ Cmd.none ]
               )
        )


katexRenderEquation : Step -> Cmd msg
katexRenderEquation step =
    render <|
        Encode.object
            [ ( "id", Encode.string <| String.fromInt step.id )
            , ( "eq", Encode.string step.equation )
            ]



{--
onShiftEnter : msg -> Sub msg
onShiftEnter message =
    let
        a =
            Browser.Events.onKeyPress
                (Decode.map2
                    (Decode.field "shiftKey" Decode.bool)
                    (Decode.field "enterKey" Decode.bool)
                )
    in
    Sub.none
--}
