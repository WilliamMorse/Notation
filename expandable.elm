module Main exposing (Model, Step, main)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Event
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Lazy.LList as LList
import Lazy.Tree as Tree exposing (Tree(..))
import Lazy.Tree.Zipper as Zipper exposing (Zipper)


main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


type alias Step =
    { operation : String
    , equation : String
    , note : String
    , ref : List Int
    , editStep : Bool
    }


type alias Model =
    Zipper Step


init : () -> ( Model, Cmd Msg )
init _ =
    let
        z =
            Step "starting Operation" "y=mx+b" "notesnotesnotes" [ 0 ] False
                |> Tree.singleton
                |> Zipper.fromTree
    in
    ( z, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


type Msg
    = EditStep (List Int)


makeTrail : List Int -> List (List Int)
makeTrail path =
    path
        |> List.repeat (List.length path)
        |> List.indexedMap (\i -> List.take (i + 1))
        |> List.foldl (::) []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EditStep path ->
            let
                trail =
                    makeTrail path

                stepToUpdate =
                    Zipper.openPath (\a -> \b -> a == b.ref) trail model

                newModel =
                    Zipper.updateItem {(Zipper.current) | editStep = False )} stepToUpdate
            in
            ( model, Cmd.none )


viewStandaloneStep : Step -> Element Msg
viewStandaloneStep step =
    column
        [ width fill
        , spacing 15
        , Event.onDoubleClick (EditStep step.ref)
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


view : Model -> Html Msg
view model =
    layout [] (text "hi there")


st =
    Step "" "" "" [] False


sa =
    Tree.singleton st


findThis =
    Tree.singleton (Step "" "" "" [ 0, 0, 0, 1, 2, 3 ] False)


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
