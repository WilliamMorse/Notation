module TexParser exposing (Notation(..))

import Dict exposing (Dict)
import Parser exposing (..)
import Set


type alias KaTex =
    String


type Notation
    = Symbol KaTex
    | Substitution
        { shorthand : KaTex
        , longhand : KaTex
        }
    | Delimiter
        { open : KaTex
        , close : KaTex
        }
    | Infix
        { name : KaTex
        , argsLeft : Int
        , argsRight : Int
        }
    | Postfix
        { name : KaTex
        , argsLeft : Int
        , args : List Notation
        }
    | Prefix
        { name : KaTex
        , argsRight : Int
        }


symbol : Parser Notation
symbol =
    succeed Symbol
        |. spaces
        |= variable
            { start = Char.isAlpha
            , inner = Char.isAlphaNum
            , reserved = Set.fromList []
            }


substitution : KaTex -> Parser Notation
substitution subStr =
    succeed <| Substitution { shorthand = "hi", longhand = "love" }


type alias ParserLibrary =
    Dict KaTex Notation


parserLib : ParserLibrary
parserLib =
    Dict.fromList
        [ ( "+", Infix { name = "Plus", argsLeft = 1, argsRight = 1 } )
        , ( "*", Infix { name = "Multiply", argsLeft = 1, argsRight = 1 } )
        , ( "=", Infix { name = "Equals", argsLeft = 1, argsRight = 1 } )
        , ( "y", Symbol "Dependant variable" )
        , ( "x", Symbol "Independant variable" )
        , ( "m", Symbol "constant" )
        , ( "b", Symbol "constant" )
        ]


infixParser : KaTex -> Notation -> Parser Notation
infixParser katexStr op =
    succeed op


genParser : ParserLibrary -> List (Parser Notation)
genParser lib =
    let
        a =
            0
    in
    []


testString =
    "y = mx + b "


addToParserLibrary : KaTex -> Notation -> ParserLibrary -> ParserLibrary
addToParserLibrary description notation lib =
    Dict.insert description notation lib



{--
Notation libraries:

The funtonality of the Notation libraries can be devided into three levels of understandg that the program has:

1, Can we display with KaTex
2, Can we parese into a tree of Type Notation
3, Can we understand Tree transformations as we move from step to step

For each of these levels of computer understanding we have a seperate library of functions that can be built up by the user

1, The enire KaTex syntax library
2, A Notation Type Library that links Notation types with Latex syntax
3, A tree transformation Library that takes Notation Trees and diffs/manipulates them

The second two are partaly user defined and can be shared or modified depending on context. 
we need to make sure that the safety of this large functionality is maintained (organized by context, large enough standard library for specific math level)
Operator Library to difine base level syntax and build up string input 

Transformation library to define tree manipulations in the steps stack 

--}
