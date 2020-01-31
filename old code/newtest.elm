module Main exposing (Point)

import Browser exposing (..)
import Html exposing (..)
import Parser exposing ((|.), (|=), Parser)
import Tuple exposing (..)


type alias Point =
    { x : Float
    , y : Float
    }



{-
   point : Parser Point
   point =
-}


type Node
    = Variable String
    | Substitution String Bool Node
    | Add (List Node)
    | Multiply (List Node)
    | Power ( Node, Node )
    | SquareRoot Node



--| Custom String (List Node)


type alias Equation =
    ( Node, Node )


outputLaTex : Node -> String
outputLaTex side =
    case side of
        Variable str ->
            str

        Substitution subVar showContent content ->
            if showContent then
                outputLaTex content

            else
                subVar

        Add nodeList ->
            String.join " + " (List.map outputLaTex nodeList)

        Multiply nodeList ->
            String.join " * " (List.map outputLaTex nodeList)

        Power ( base, exponent ) ->
            outputLaTex base ++ "^" ++ outputLaTex exponent

        SquareRoot node ->
            "\\sqrt" ++ outputLaTex node


mapSame : (a -> x) -> ( a, a ) -> ( x, x )
mapSame f tuple =
    Tuple.mapBoth f f tuple


toBothSidesOfListlike : Equation -> (List Node -> Node) -> Node -> Equation
toBothSidesOfListlike equation f modify =
    mapSame (\side -> f [ side, modify ]) equation


add : Equation -> Node -> Equation
add equation stuffToAdd =
    toBothSidesOfListlike equation Add stuffToAdd


multiply : Equation -> Node -> Equation
multiply equation stuffToMultiply =
    toBothSidesOfListlike equation Multiply stuffToMultiply


sqrt : Equation -> Equation
sqrt equation =
    mapSame SquareRoot equation



--toThePowerOf : Equation : Node
