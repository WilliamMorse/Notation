module DictParser exposing (..)

import Dict exposing (Dict)
import Parser exposing (..)
import Set



-- which dictonaries do we need?
-- Keyword (not space seperated like +)
-- Variable (spaces seperated: functions, long variable names )
--


testDict : Dict String String
testDict =
    Dict.fromList <|
        [ ( "hi", "low" )
        , ( "there", "everywhere" )
        , ( "how are you?", "good!" )
        ]



-- from "+" -> keyword ->   Infix "+" 1 left 1 right


type Entry key content
    = Known key content
    | Unknown key


lookInStringDict : Dict String a -> String -> Entry String a
lookInStringDict dict key =
    case Dict.get key dict of
        Just content ->
            Known key content

        Nothing ->
            Unknown key


snippingTool : Parser String
snippingTool =
    oneOf
        [ variable
            { start = Char.isAlpha
            , inner = Char.isAlpha
            , reserved = Set.fromList []
            }
        , variable
            { start = Char.isDigit
            , inner = Char.isDigit
            , reserved = Set.fromList []
            }
        , succeed ()
            |. multiComment "{" "}" Nestable
            |> getChompedString
        , getChompedString <| keyword "\\ "
        , variable
            { start = (==) '\\'
            , inner = (/=) ' '
            , reserved = Set.fromList []
            }
        , getChompedString <| symbol "+"
        , variable
            { start = (/=) ' '
            , inner = (/=) ' '
            , reserved = Set.fromList []
            }
        ]


chunkParser : Parser (Entry String String)
chunkParser =
    map (lookInStringDict testDict) snippingTool


loopOn : Parser a -> Parser (List a)
loopOn p =
    loop [] (testParseHelp p)


testParseHelp : Parser a -> List a -> Parser (Step (List a) (List a))
testParseHelp p reversedChunks =
    oneOf
        [ succeed (\s -> Loop (s :: reversedChunks))
            |. spaces
            |= p
        , succeed ()
            |> map (\_ -> Done (List.reverse reversedChunks))
        ]
