{--
import Html exposing (text)

some_variable =
  "Hello world! "

{--}
a_number = 5

sq x =
  x^2

main =
  text (
      some_variable ++ toString sq(a_number)
  )

--}
--}

import Html exposing (li, text, ol)
import Html.Attributes exposing (class)


{-| This snippet uses the <ul> and <li> tags to create an unordered
list of French grocery items. Notice that all occurrences of 'ul'
and 'li' are followed by two lists. The first list is for any HTML
attributes, and the second list is all the HTML nodes inside the
tag.

Et maintenant le voyage au supermarché!
-}
main =
  ol [class "grocery-list"]
    [ li [] [text "Pamplemousse"]
    , li [] [text "Ananas"]
    , li [] [text "Jus d'orange"]
    , li [] [text "Boeuf"]
    , li [] [text "Soupe du jour"]
    , li [] [text "Camembert"]
    , li [] [text "Jacques Cousteau"]
    , li [] [text "Baguette"]
    ]
