module LaTexParser exposing (Point, point)

import Parser exposing ((|.), (|=), Parser, float, spaces, symbol)


type alias Point =
    { x : Float
    , y : Float
    }


type alias LatexCommand a =
    { cmd : String
    , parser : Parser a
    }



{- parser GUI to make new latex commands. we can chian parsers
   together through steps

   but then we are just string safe and constrained to the
   KaTex library

   how can we keep the user control to make your own top level
   functions and have simple and expandable program structure

   are we writing our own programming language now?

   make a bunch of stuff for every katex symbol
   but let people make their own functions that use those

   is it helpful to ask people to make their own algebraic
   engine??

   Here's how I'd like to use it:
       start with an equation
           y=mx+b
       just say the command
       subtract b
           y-b=mx+b-b
       the first time we run a function like this we don't
       want to fully apply the subtraction
       evaluate b-b=0
           y-b=mx
       now define subtract to simplify if possible
       (thats a whole tree math thing like in sympy)

    do you just use a CAS like sympy as a backend or do you
    make your own? Sympy is verry clearly an object structured
    program. what would the functional equivelent look like?



-}
-- "(3, 4)"


point : Parser Point
point =
    Parser.succeed Point
        |. symbol "("
        |. spaces
        |= float
        |. spaces
        |. symbol ","
        |. spaces
        |= float
        |. spaces
        |. symbol ")"



-- Type Latex
--     =
-- latexArgs : Parser Latex
-- latexArgs = Parser.succeed Latex Parser.multiComment "{" "}" Nestable


type alias Equation =
    { lhs : String
    , rhs : String
    }


type Tree a
    = Leaf a
    | Node a (List (Tree a))
