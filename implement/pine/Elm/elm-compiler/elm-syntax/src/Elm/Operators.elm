module Elm.Operators exposing
    ( SimpleInfix
    , bySymbol
    )

import Dict exposing (Dict)
import Elm.Syntax.Infix exposing (InfixDirection(..))


type alias SimpleInfix =
    { direction : InfixDirection
    , precedence : Int
    , operator : String
    }


bySymbol : Dict String SimpleInfix
bySymbol =
    List.foldl
        (\infix_ acc ->
            Dict.insert infix_.operator infix_ acc
        )
        Dict.empty
        [ -- elm/core
          -- Basics module
          -- infix right 0 (<|) = apL
          { direction = Right
          , precedence = 0
          , operator = "<|"
          }
        , -- infix left  0 (|>) = apR
          { direction = Left
          , precedence = 0
          , operator = "|>"
          }
        , -- infix right 2 (||) = or
          { direction = Right
          , precedence = 2
          , operator = "||"
          }
        , -- infix right 3 (&&) = and
          { direction = Right
          , precedence = 3
          , operator = "&&"
          }
        , -- infix non   4 (==) = eq
          { direction = Non
          , precedence = 4
          , operator = "=="
          }
        , -- infix non   4 (/=) = neq
          { direction = Non
          , precedence = 4
          , operator = "/="
          }
        , -- infix non   4 (<)  = lt
          { direction = Non
          , precedence = 4
          , operator = "<"
          }
        , -- infix non   4 (>)  = gt
          { direction = Non
          , precedence = 4
          , operator = ">"
          }
        , -- infix non   4 (<=) = le
          { direction = Non
          , precedence = 4
          , operator = "<="
          }
        , -- infix non   4 (>=) = ge
          { direction = Non
          , precedence = 4
          , operator = ">="
          }
        , -- infix right 5 (++) = append
          { direction = Right
          , precedence = 5
          , operator = "++"
          }
        , -- infix left  6 (+)  = add
          { direction = Left
          , precedence = 6
          , operator = "+"
          }
        , -- infix left  6 (-)  = sub
          { direction = Left
          , precedence = 6
          , operator = "-"
          }
        , -- infix left  7 (*)  = mul
          { direction = Left
          , precedence = 7
          , operator = "*"
          }
        , -- infix left  7 (/)  = fdiv
          { direction = Left
          , precedence = 7
          , operator = "/"
          }
        , -- infix left  7 (//) = idiv
          { direction = Left
          , precedence = 7
          , operator = "//"
          }
        , -- infix right 8 (^)  = pow
          { direction = Right
          , precedence = 8
          , operator = "^"
          }
        , -- infix left  9 (<<) = composeL
          { direction = Left
          , precedence = 9
          , operator = "<<"
          }
        , -- infix right 9 (>>) = composeR
          { direction = Right
          , precedence = 9
          , operator = ">>"
          }
        , -- List module
          -- infix right 5 (::) = cons
          { direction = Right
          , precedence = 5
          , operator = "::"
          }
        , -- elm/url
          -- Url.Parser module
          -- infix right 7 (</>) = slash
          { direction = Right
          , precedence = 7
          , operator = "</>"
          }
        , -- infix left  8 (<?>) = questionMark
          { direction = Left
          , precedence = 8
          , operator = "<?>"
          }
        , -- elm/parser
          -- Parser and Parser.Advanced modules
          -- infix left 5 (|=) = keeper
          { direction = Left
          , precedence = 5
          , operator = "|="
          }
        , -- infix left 6 (|.) = ignorer
          { direction = Left
          , precedence = 6
          , operator = "|."
          }
        ]
