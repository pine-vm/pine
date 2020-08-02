module EvaluateElmTests exposing (..)

import Expect
import Main
import Test


suite : Test.Test
suite =
    Test.describe "Elm evaluation"
        [ Test.test "Just a literal" <|
            \_ ->
                Expect.equal (Ok "\"just a literal ✔️\"")
                    (Main.getValueFromJustExpressionSyntaxAsJsonString """  "just a literal ✔️"  """)
        , Test.test "Concat string literal" <|
            \_ ->
                Expect.equal (Ok "\"first literal  second literal ✔️\"")
                    (Main.getValueFromJustExpressionSyntaxAsJsonString """  "first literal "  ++  " second literal ✔️"  """)
        , Test.test "Concat string via let" <|
            \_ ->
                Expect.equal (Ok "\"literal from let  second literal ✔️\"")
                    (Main.getValueFromJustExpressionSyntaxAsJsonString """
let
    binding_from_let =
        "literal from let "
in
binding_from_let ++ " second literal ✔️"
""")
        , Test.test "Dependency within let" <|
            \_ ->
                Expect.equal (Ok "\"literal\"")
                    (Main.getValueFromJustExpressionSyntaxAsJsonString """
let
    a = "literal"

    b = a
in
b
""")
        , Test.test "Support any order in let" <|
            \_ ->
                Expect.equal (Ok "\"literal\"")
                    (Main.getValueFromJustExpressionSyntaxAsJsonString """
let
    d = c

    a = "literal"

    c = b

    b = a
in
d
""")
        , Test.test "Value from module-level binding" <|
            \_ ->
                Expect.equal (Ok "\"literal\"")
                    (Main.getValueFromExpressionSyntaxAsJsonString
                        [ """
module ModuleName exposing (module_level_binding)


module_level_binding : String
module_level_binding =
    "literal"

""" ]
                        "ModuleName.module_level_binding"
                    )
        , Test.test "Concat string via module level function" <|
            \_ ->
                Expect.equal (Ok "\"literal from module  second literal ✔️\"")
                    (Main.getValueFromExpressionSyntaxAsJsonString
                        [ """
module ModuleName exposing (module_level_binding)


module_level_binding : String -> String
module_level_binding param0 =
    "literal from module " ++ param0


""" ]
                        "ModuleName.module_level_binding \" second literal ✔️\""
                    )
        , Test.test "Depend on binding in current module" <|
            \_ ->
                Expect.equal (Ok "\"literal from module\"")
                    (Main.getValueFromExpressionSyntaxAsJsonString
                        [ """
module ModuleName exposing (module_level_binding)


module_level_binding =
    other_module_level_binding


other_module_level_binding =
    "literal from module"

""" ]
                        "ModuleName.module_level_binding"
                    )
        , Test.test "Function with two named parameters" <|
            \_ ->
                Expect.equal (Ok "\"literal from module ab\"")
                    (Main.getValueFromExpressionSyntaxAsJsonString
                        [ """
module ModuleName exposing (module_level_binding)


module_level_binding param0 param1 =
    "literal from module " ++ param0 ++ param1

""" ]
                        "ModuleName.module_level_binding  \"a\"  \"b\""
                    )
        , Test.test "Partial application" <|
            \_ ->
                Expect.equal (Ok "\"literal from module ab\"")
                    (Main.getValueFromExpressionSyntaxAsJsonString
                        [ """
module ModuleName exposing (partially_applied_a)


partially_applied_a =
    function_with_two_parameters "a"


function_with_two_parameters param0 param1 =
    "literal from module " ++ param0 ++ param1

""" ]
                        "ModuleName.partially_applied_a \"b\""
                    )
        , Test.test "Partial application via multiple modules" <|
            \_ ->
                Expect.equal (Ok "\"a b c\"")
                    (Main.getValueFromExpressionSyntaxAsJsonString
                        [ """
module ModuleA exposing (partially_applied_a)


partially_applied_a =
    function_with_three_parameters "a"


function_with_three_parameters param0 param1 param2 =
    param0 ++ " " ++ param1 ++ " " ++ param2

""", """
module ModuleB exposing (partially_applied_b)


partially_applied_b =
    ModuleA.partially_applied_a "b"


function_with_three_parameters param0 param1 param2 =
    param0 ++ " " ++ param1 ++ " " ++ param2

""" ]
                        "ModuleB.partially_applied_b \"c\""
                    )
        ]
