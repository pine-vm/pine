module ElmEvaluationUsingPineTests exposing (..)

import ElmEvaluationUsingPine exposing (InteractiveContext(..))
import Expect
import Json.Encode
import Test


suite : Test.Test
suite =
    Test.describe "Elm evaluation using common engine"
        [ Test.test "Just a literal String" <|
            \_ ->
                expectEvalResultJsonTextEqual
                    { context = DefaultContext
                    , elmExpressionText = """  "just a literal ✔️"  """
                    , expectedValueAsJson = "\"just a literal ✔️\""
                    }
        , Test.test "Just a literal List String" <|
            \_ ->
                expectEvalResultJsonTextEqual
                    { context = DefaultContext
                    , elmExpressionText = """  [ "just a literal ✔️", "another string" ]  """
                    , expectedValueAsJson = """["just a literal ✔️","another string"]"""
                    }
        , Test.test "Concat string literal" <|
            \_ ->
                expectEvalResultJsonTextEqual
                    { context = DefaultContext
                    , elmExpressionText = """ "first literal " ++ " second literal ✔️" """
                    , expectedValueAsJson = "\"first literal  second literal ✔️\""
                    }
        , Test.test "Apply String.fromInt" <|
            \_ ->
                expectEvalResultJsonTextEqual
                    { context = DefaultContext
                    , elmExpressionText = " String.fromInt 123 "
                    , expectedValueAsJson = "\"123\""
                    }
        , Test.test "Add and apply String.fromInt" <|
            \_ ->
                expectEvalResultJsonTextEqual
                    { context = DefaultContext
                    , elmExpressionText = " String.fromInt (1 + 3) "
                    , expectedValueAsJson = "\"4\""
                    }
        , Test.test "Multiply and apply String.fromInt" <|
            \_ ->
                expectEvalResultJsonTextEqual
                    { context = DefaultContext
                    , elmExpressionText = " String.fromInt (17 * 41) "
                    , expectedValueAsJson = "\"697\""
                    }
        , Test.test "Divide and apply String.fromInt" <|
            \_ ->
                expectEvalResultJsonTextEqual
                    { context = DefaultContext
                    , elmExpressionText = " String.fromInt (31 // 5) "
                    , expectedValueAsJson = "\"6\""
                    }
        , Test.test "Concat string via let" <|
            \_ ->
                expectEvalResultJsonTextEqual
                    { context = DefaultContext
                    , elmExpressionText = """
let
    binding_from_let =
        "literal from let "
in
binding_from_let ++ " second literal ✔️"
"""
                    , expectedValueAsJson = "\"literal from let  second literal ✔️\""
                    }
        , Test.test "Dependency within let" <|
            \_ ->
                expectEvalResultJsonTextEqual
                    { context = DefaultContext
                    , elmExpressionText = """
let
    a = "just a literal"

    b = a
in
b
"""
                    , expectedValueAsJson = "\"just a literal\""
                    }
        , Test.test "Support any order in let" <|
            \_ ->
                expectEvalResultJsonTextEqual
                    { context = DefaultContext
                    , elmExpressionText = """
let
    d = c

    a = "just a literal"

    c = b

    b = a
in
d
"""
                    , expectedValueAsJson = "\"just a literal\""
                    }
        , Test.test "Branch using if and literal True" <|
            \_ ->
                expectEvalResultJsonTextEqual
                    { context = DefaultContext
                    , elmExpressionText = """ if True then "condition is true" else "condition is false" """
                    , expectedValueAsJson = "\"condition is true\""
                    }
        , Test.test "Branch using if and literal False" <|
            \_ ->
                expectEvalResultJsonTextEqual
                    { context = DefaultContext
                    , elmExpressionText = """ if False then "condition is true" else "condition is false" """
                    , expectedValueAsJson = "\"condition is false\""
                    }
        , Test.test "Branch using if and (not False)" <|
            \_ ->
                expectEvalResultJsonTextEqual
                    { context = DefaultContext
                    , elmExpressionText = """ if not False then "condition is true" else "condition is false" """
                    , expectedValueAsJson = "\"condition is true\""
                    }
        , Test.test "Function application one argument" <|
            \_ ->
                expectEvalResultJsonTextEqual
                    { context = DefaultContext
                    , elmExpressionText = """
let
    function_with_one_parameter param0 =
        "literal from function " ++ param0
in
function_with_one_parameter "argument"
"""
                    , expectedValueAsJson = "\"literal from function argument\""
                    }
        , Test.test "Function application two arguments" <|
            \_ ->
                expectEvalResultJsonTextEqual
                    { context = DefaultContext
                    , elmExpressionText = """
let
    function_with_two_parameters param0 param1 =
        "literal from function, " ++ param0 ++ ", " ++ param1
in
function_with_two_parameters "argument 0" "argument 1"
"""
                    , expectedValueAsJson = "\"literal from function, argument 0, argument 1\""
                    }
        , Test.test "Partial application two arguments" <|
            \_ ->
                expectEvalResultJsonTextEqual
                    { context = DefaultContext
                    , elmExpressionText = """
let
    partially_applied_a =
        function_with_two_parameters "argument 0"


    function_with_two_parameters param0 param1 =
        "literal from function, " ++ param0 ++ ", " ++ param1
in
partially_applied_a "argument 1"
           """
                    , expectedValueAsJson = "\"literal from function, argument 0, argument 1\""
                    }
        , Test.test "Partial application three arguments in two groups" <|
            \_ ->
                expectEvalResultJsonTextEqual
                    { context = DefaultContext
                    , elmExpressionText = """
let
    partially_applied_a =
        function_with_three_parameters "argument 0"  "argument 1"


    function_with_three_parameters param0 param1 param2 =
        "literal from function, " ++ param0 ++ ", " ++ param1 ++ ", " ++ param2
in
partially_applied_a "argument 2"
           """
                    , expectedValueAsJson = "\"literal from function, argument 0, argument 1, argument 2\""
                    }
        , Test.test "Lambda with 'var' pattern" <|
            \_ ->
                expectEvalResultJsonTextEqual
                    { context = DefaultContext
                    , elmExpressionText = """ (\\x -> x) "test" """
                    , expectedValueAsJson = "\"test\""
                    }
        , Test.test "Lambda with 'all' pattern" <|
            \_ ->
                expectEvalResultJsonTextEqual
                    { context = DefaultContext
                    , elmExpressionText = """ (\\_ -> "constant") "test" """
                    , expectedValueAsJson = "\"constant\""
                    }
        , Test.test "List.drop 0" <|
            \_ ->
                expectEvalResultJsonTextEqual
                    { context = DefaultContext
                    , elmExpressionText = """ List.drop 0 ["a", "b", "c", "d"]  """
                    , expectedValueAsJson = """["a","b","c","d"]"""
                    }
        , Test.test "List.drop 2" <|
            \_ ->
                expectEvalResultJsonTextEqual
                    { context = DefaultContext
                    , elmExpressionText = """ List.drop 2 ["a", "b", "c", "d"]  """
                    , expectedValueAsJson = """["c","d"]"""
                    }
        , Test.test "Case of expression deconstructing List into empty and non-empty" <|
            \_ ->
                expectEvalResultJsonTextEqual
                    { context = DefaultContext
                    , elmExpressionText = """
let
    describe_list list =
        case list of
        [] -> "This list is empty."
        firstElement :: otherElements ->
            "First element is '" ++ firstElement
                ++ "', " ++ (String.fromInt (List.length otherElements))
                ++ " other elements remaining."
in
[ describe_list [], describe_list [ "single" ], describe_list [ "first_of_two", "second_of_two" ] ]
           """
                    , expectedValueAsJson = """["This list is empty.","First element is 'single', 0 other elements remaining.","First element is 'first_of_two', 1 other elements remaining."]"""
                    }
        , Test.test "Simple List.foldl" <|
            \_ ->
                expectEvalResultJsonTextEqual
                    { context = DefaultContext
                    , elmExpressionText = """
let
    concat a b =
        a ++ b
in
List.foldl concat "_init_" [ "a", "b", "c" ]
           """
                    , expectedValueAsJson = "\"cba_init_\""
                    }
        , Test.test "Literal from module" <|
            \_ ->
                expectEvalResultJsonTextEqual
                    { context = InitContextFromApp { modulesTexts = [ """
module ModuleName exposing (module_level_binding)


module_level_binding : String
module_level_binding =
    "literal"

""" ] }
                    , elmExpressionText = """ ModuleName.module_level_binding """
                    , expectedValueAsJson = "\"literal\""
                    }
        , Test.test "Partial application via multiple modules" <|
            \_ ->
                expectEvalResultJsonTextEqual
                    { context = InitContextFromApp { modulesTexts = [ """
module ModuleA exposing (partially_applied_a)


partially_applied_a =
    function_with_three_parameters "a"


function_with_three_parameters param0 param1 param2 =
    param0 ++ " " ++ param1 ++ " " ++ param2

""", """
module ModuleB exposing (partially_applied_b)


partially_applied_b =
    ModuleA.partially_applied_a named_literal


named_literal =
    "b"

function_with_three_parameters param0 param1 param2 =
    param0 ++ " " ++ param1 ++ " " ++ param2

""" ] }
                    , elmExpressionText = """ ModuleB.partially_applied_b "c" """
                    , expectedValueAsJson = "\"a b c\""
                    }
        ]


expectEvalResultJsonTextEqual :
    { context : ElmEvaluationUsingPine.InteractiveContext
    , elmExpressionText : String
    , expectedValueAsJson : String
    }
    -> Expect.Expectation
expectEvalResultJsonTextEqual { context, elmExpressionText, expectedValueAsJson } =
    Expect.equal (Ok expectedValueAsJson)
        (ElmEvaluationUsingPine.evaluateExpressionText context elmExpressionText
            |> Result.map (Json.Encode.encode 0)
        )
