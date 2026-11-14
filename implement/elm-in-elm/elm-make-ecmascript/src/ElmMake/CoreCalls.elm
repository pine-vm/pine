module ElmMake.CoreCalls exposing (compile)

import ElmMake.JavaScriptSyntax as JS
import ElmMake.Model exposing (globalName)


compile : String -> String -> List JS.Expr -> JS.Expr
compile home name arguments =
    let
        fallback =
            JS.call (JS.Ref (globalName ( "elm/core", home, name ))) arguments
    in
    case ( home, arguments ) of
        ( "Basics", [ argument ] ) ->
            case name of
                "not" ->
                    JS.Unary "!" argument

                "negate" ->
                    JS.Unary "-" argument

                "toFloat" ->
                    argument

                "truncate" ->
                    JS.Binary "|" argument (JS.Literal "0")

                _ ->
                    fallback

        ( "Basics", [ left, right ] ) ->
            case name of
                "add" ->
                    JS.Binary "+" left right

                "sub" ->
                    JS.Binary "-" left right

                "mul" ->
                    JS.Binary "*" left right

                "fdiv" ->
                    JS.Binary "/" left right

                "idiv" ->
                    JS.Binary "|" (JS.Binary "/" left right) (JS.Literal "0")

                "and" ->
                    JS.Binary "&&" left right

                "or" ->
                    JS.Binary "||" left right

                "xor" ->
                    JS.Binary "!==" left right

                "eq" ->
                    equality True left right

                "neq" ->
                    equality False left right

                "lt" ->
                    comparison "<" "<" "0" left right

                "gt" ->
                    comparison ">" ">" "0" left right

                "le" ->
                    comparison "<=" "<" "1" left right

                "ge" ->
                    comparison ">=" ">" "-1" left right

                "remainderBy" ->
                    JS.Binary "%" right left

                "append" ->
                    append left right

                "apL" ->
                    JS.call left [ right ]

                "apR" ->
                    JS.call right [ left ]

                _ ->
                    fallback

        ( "Bitwise", [ value ] ) ->
            if name == "complement" then
                JS.Unary "~" value

            else
                fallback

        ( "Bitwise", [ left, right ] ) ->
            case name of
                "and" ->
                    JS.Binary "&" left right

                "or" ->
                    JS.Binary "|" left right

                "xor" ->
                    JS.Binary "^" left right

                "shiftLeftBy" ->
                    JS.Binary "<<" right left

                "shiftRightBy" ->
                    JS.Binary ">>" right left

                "shiftRightZfBy" ->
                    JS.Binary ">>>" right left

                _ ->
                    fallback

        ( "Tuple", [ value ] ) ->
            case name of
                "first" ->
                    JS.Access value "a"

                "second" ->
                    JS.Access value "b"

                _ ->
                    fallback

        _ ->
            fallback


isLiteral : JS.Expr -> Bool
isLiteral expr =
    case expr of
        JS.Literal _ ->
            True

        _ ->
            False


comparison : String -> String -> String -> JS.Expr -> JS.Expr -> JS.Expr
comparison ideal backup number left right =
    if isLiteral left || isLiteral right then
        JS.Binary ideal left right

    else
        JS.Binary backup (JS.Call (JS.Ref "_Utils_cmp") [ left, right ]) (JS.Literal number)


equality : Bool -> JS.Expr -> JS.Expr -> JS.Expr
equality equal left right =
    let
        boolean expected value =
            if equal == expected then
                value

            else
                JS.Unary "!" value

        zero value =
            if equal then
                JS.Unary "!" value

            else
                JS.Unary "!" (JS.Unary "!" value)
    in
    case ( left, right ) of
        ( JS.Literal "true", value ) ->
            boolean True value

        ( value, JS.Literal "true" ) ->
            boolean True value

        ( JS.Literal "false", value ) ->
            boolean False value

        ( value, JS.Literal "false" ) ->
            boolean False value

        ( JS.Literal "0", value ) ->
            zero value

        ( value, JS.Literal "0" ) ->
            zero value

        _ ->
            if isLiteral left || isLiteral right then
                JS.Binary
                    (if equal then
                        "==="

                     else
                        "!=="
                    )
                    left
                    right

            else
                let
                    result =
                        JS.Call (JS.Ref "_Utils_eq") [ left, right ]
                in
                if equal then
                    result

                else
                    JS.Unary "!" result


append : JS.Expr -> JS.Expr -> JS.Expr
append left right =
    let
        entries value =
            case value of
                JS.Call (JS.Ref "$elm$core$Basics$append") [ a, b ] ->
                    a :: entries b

                JS.Call (JS.Ref "_Utils_ap") [ a, b ] ->
                    a :: entries b

                JS.Binary "+" a b ->
                    a :: entries b

                _ ->
                    [ value ]

        values =
            left :: entries right

        isString expr =
            case expr of
                JS.Literal text ->
                    String.startsWith "'" text

                _ ->
                    False

        join a b =
            if List.any isString values then
                JS.Binary "+" a b

            else
                JS.Call (JS.Ref "_Utils_ap") [ a, b ]
    in
    case List.reverse values of
        last :: rest ->
            List.foldl join last rest

        [] ->
            JS.Literal "''"
