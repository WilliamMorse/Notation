import Html exposing (text)

some_variable =
  "Hello world! "

{--}
a_number = 5
--}

sq x =
  x^2

main =
  text (
      some_variable ++ toString sq(a_number)
  )

--}
