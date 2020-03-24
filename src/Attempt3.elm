module Attempt3 exposing (..)

import Char
import Dict exposing (Dict)
import Parser exposing (..)
import Set



-- Operation, Notation, LaTex, Tex, Node, Leaf, Delimiter.


{-| This Type handels the basic chunking of a LaTex string into arguments that
either:
-stand on ther own
-or are nested inside {} and need to be further parsed later
-}



-- AST LEVEL 1 --


type Chunk
    = CurleyBrace String
    | LeafText String



-- AST LEVEL 2 --


type Operation
    = Prefix Int
    | Infix Int Int
    | Postfix Int


type alias Braces =
    { open : String
    , close : String
    }



-- AST LEVEL 3 --


type Notation
    = DefinedOp String Operation
    | Delimiter Braces String
    | Unknown String



-- can we put all the types into one?
-- Delimiter leaf prefix infix postfix


curleyBraces : Parser Chunk
curleyBraces =
    succeed CurleyBrace
        |= (succeed ()
                |. multiComment "{" "}" Nestable
                |> getChompedString
           )


delimiter : Braces -> Parser Notation
delimiter braces =
    succeed (Delimiter braces)
        |= (succeed ()
                |. multiComment braces.open braces.close Nestable
                |> getChompedString
           )


leafTextParser : Parser Chunk
leafTextParser =
    succeed LeafText
        |= variable
            { start = (/=) ' '
            , inner = (/=) ' '
            , reserved = Set.fromList []
            }


chunk : Parser Chunk
chunk =
    oneOf
        [ curleyBraces
        , leafTextParser
        ]


firstPass : Parser (List Chunk)
firstPass =
    loop [] firstPassHelp


firstPassHelp : List LatexNode -> Parser (Step (List LatexNode) (List LatexNode))
firstPassHelp revChunks =
    oneOf
        [ succeed (\s -> Loop (s :: revChunks))
            |. spaces
            |= chunk
        , succeed ()
            |> map (\_ -> Done (List.reverse revChunks))
        ]


{-| Three types of functions. The Ints describe how manny arguments on either side to chomp
-}
opLibrary : Dict String Operation
opLibrary =
    Dict.fromList
        [ ( "=", Infix 1 1 )
        , ( "+", Infix 1 1 )
        , ( "\\int", Prefix 2 )
        , ( "\\frac", Prefix 2 )
        , ( "^", Prefix 1 )
        , ( "_", Prefix 1 )
        ]


delimiterLibrary : Dict String String
delimiterLibrary =
    Dict.fromList
        [ ( "{", "}" )
        , ( "(", ")" )
        ]


checkDefined : LatexNode -> Notation
checkDefined node =
    case node of
        LeafText str ->
            Maybe.map (DefinedOp str) (Dict.get str opLibrary)
                |> Maybe.withDefault (Unknown str)

        CurleyBrace str ->
            Unknown str


label : List LatexNode -> List Notation
label nodes =
    List.map checkDefined nodes


secondPass : Parser (List Notation)
secondPass =
    map (List.map checkDefined) firstPass
