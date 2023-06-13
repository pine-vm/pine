module ElmInteractiveTests exposing (..)

import BigInt
import ElmInteractive exposing (InteractiveContext(..))
import Expect
import Json.Encode
import Pine
import Test


interactiveScenarios : Test.Test
interactiveScenarios =
    Test.describe "Elm interactive scenarios"
        [ Test.test "Just a literal String" <|
            \_ ->
                expectationForElmInteractiveScenario
                    { context = DefaultContext
                    , previousSubmissions = []
                    , submission = """  "just a literal ✔️"  """
                    , expectedValueElmExpression = "\"just a literal ✔️\""
                    }
        , Test.test "Just a literal List String" <|
            \_ ->
                expectationForElmInteractiveScenario
                    { context = DefaultContext
                    , previousSubmissions = []
                    , submission = """  [ "just a literal ✔️", "another string" ]  """
                    , expectedValueElmExpression = """["just a literal ✔️","another string"]"""
                    }
        , Test.test "Concat string literal" <|
            \_ ->
                expectationForElmInteractiveScenario
                    { context = DefaultContext
                    , previousSubmissions = []
                    , submission = """ "first literal " ++ " second literal ✔️" """
                    , expectedValueElmExpression = "\"first literal  second literal ✔️\""
                    }
        , Test.test "Apply String.fromInt" <|
            \_ ->
                expectationForElmInteractiveScenario
                    { context = DefaultContext
                    , previousSubmissions = []
                    , submission = " String.fromInt 123 "
                    , expectedValueElmExpression = "\"123\""
                    }
        , Test.test "Add and apply String.fromInt" <|
            \_ ->
                expectationForElmInteractiveScenario
                    { context = DefaultContext
                    , previousSubmissions = []
                    , submission = " String.fromInt (1 + 3) "
                    , expectedValueElmExpression = "\"4\""
                    }
        , Test.test "Multiply and apply String.fromInt" <|
            \_ ->
                expectationForElmInteractiveScenario
                    { context = DefaultContext
                    , previousSubmissions = []
                    , submission = " String.fromInt (17 * 41) "
                    , expectedValueElmExpression = "\"697\""
                    }
        , Test.test "Divide and apply String.fromInt" <|
            \_ ->
                expectationForElmInteractiveScenario
                    { context = DefaultContext
                    , previousSubmissions = []
                    , submission = " String.fromInt (31 // 5) "
                    , expectedValueElmExpression = "\"6\""
                    }
        , Test.test "Concat string via let" <|
            \_ ->
                expectationForElmInteractiveScenario
                    { context = DefaultContext
                    , previousSubmissions = []
                    , submission = """
let
    binding_from_let =
        "literal from let "
in
binding_from_let ++ " second literal ✔️"
"""
                    , expectedValueElmExpression = "\"literal from let  second literal ✔️\""
                    }
        , Test.test "Dependency within let" <|
            \_ ->
                expectationForElmInteractiveScenario
                    { context = DefaultContext
                    , previousSubmissions = []
                    , submission = """
let
    a = "just a literal"

    b = a
in
b
"""
                    , expectedValueElmExpression = "\"just a literal\""
                    }
        , Test.test "Support any order in let" <|
            \_ ->
                expectationForElmInteractiveScenario
                    { context =
                        CustomModulesContext
                            { includeCoreModules = False
                            , modulesTexts = []
                            }
                    , previousSubmissions = []
                    , submission = """
let
    d = c

    a = "just a literal"

    c = b

    b = a
in
d
"""
                    , expectedValueElmExpression = "\"just a literal\""
                    }
        , Test.test "Branch using if and literal True" <|
            \_ ->
                expectationForElmInteractiveScenario
                    { context = DefaultContext
                    , previousSubmissions = []
                    , submission = """ if True then "condition is true" else "condition is false" """
                    , expectedValueElmExpression = "\"condition is true\""
                    }
        , Test.test "Branch using if and literal False" <|
            \_ ->
                expectationForElmInteractiveScenario
                    { context = DefaultContext
                    , previousSubmissions = []
                    , submission = """ if False then "condition is true" else "condition is false" """
                    , expectedValueElmExpression = "\"condition is false\""
                    }
        , Test.test "Branch using if and (not False)" <|
            \_ ->
                expectationForElmInteractiveScenario
                    { context = DefaultContext
                    , previousSubmissions = []
                    , submission = """ if not False then "condition is true" else "condition is false" """
                    , expectedValueElmExpression = "\"condition is true\""
                    }
        , Test.test "Function application one argument" <|
            \_ ->
                expectationForElmInteractiveScenario
                    { context = DefaultContext
                    , previousSubmissions = []
                    , submission = """
let
    function_with_one_parameter param0 =
        "literal from function " ++ param0
in
function_with_one_parameter "argument"
"""
                    , expectedValueElmExpression = "\"literal from function argument\""
                    }
        , Test.test "Function application two arguments" <|
            \_ ->
                expectationForElmInteractiveScenario
                    { context = DefaultContext
                    , previousSubmissions = []
                    , submission = """
let
    function_with_two_parameters param0 param1 =
        "literal from function, " ++ param0 ++ ", " ++ param1
in
function_with_two_parameters "argument 0" "argument 1"
"""
                    , expectedValueElmExpression = "\"literal from function, argument 0, argument 1\""
                    }
        , Test.test "Partial application two arguments" <|
            \_ ->
                expectationForElmInteractiveScenario
                    { context = DefaultContext
                    , previousSubmissions = []
                    , submission = """
let
    partially_applied_a =
        function_with_two_parameters "argument 0"


    function_with_two_parameters param0 param1 =
        "literal from function, " ++ param0 ++ ", " ++ param1
in
partially_applied_a "argument 1"
           """
                    , expectedValueElmExpression = "\"literal from function, argument 0, argument 1\""
                    }

        {-
           , Test.test "Partial application three arguments in two groups" <|
                          \_ ->
                              expectationForElmInteractiveScenario
                                  { context = DefaultContext
                                  , previousSubmissions = []
                                  , submission = """
              let
                  partially_applied_a =
                      function_with_three_parameters "argument 0"  "argument 1"


                  function_with_three_parameters param0 param1 param2 =
                      "literal from function, " ++ param0 ++ ", " ++ param1 ++ ", " ++ param2
              in
              partially_applied_a "argument 2"
                         """
                                  , expectedValueElmExpression = "\"literal from function, argument 0, argument 1, argument 2\""
                                  }
        -}
        , Test.test "Lambda with 'var' pattern" <|
            \_ ->
                expectationForElmInteractiveScenario
                    { context = DefaultContext
                    , previousSubmissions = []
                    , submission = """ (\\x -> x) "test" """
                    , expectedValueElmExpression = "\"test\""
                    }
        , Test.test "Lambda with 'all' pattern" <|
            \_ ->
                expectationForElmInteractiveScenario
                    { context = DefaultContext
                    , previousSubmissions = []
                    , submission = """ (\\_ -> "constant") "test" """
                    , expectedValueElmExpression = "\"constant\""
                    }
        , Test.test "List.drop 0" <|
            \_ ->
                expectationForElmInteractiveScenario
                    { context = DefaultContext
                    , previousSubmissions = []
                    , submission = """ List.drop 0 ["a", "b", "c", "d"]  """
                    , expectedValueElmExpression = """["a","b","c","d"]"""
                    }
        , Test.test "List.drop 2" <|
            \_ ->
                expectationForElmInteractiveScenario
                    { context = DefaultContext
                    , previousSubmissions = []
                    , submission = """ List.drop 2 ["a", "b", "c", "d"]  """
                    , expectedValueElmExpression = """["c","d"]"""
                    }
        , Test.test "Case of expression deconstructing List into empty and non-empty" <|
            \_ ->
                expectationForElmInteractiveScenario
                    { context = DefaultContext
                    , previousSubmissions = []
                    , submission = """
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
                    , expectedValueElmExpression = """["This list is empty.","First element is 'single', 0 other elements remaining.","First element is 'first_of_two', 1 other elements remaining."]"""
                    }
        , Test.test "Simple List.foldl" <|
            \_ ->
                expectationForElmInteractiveScenario
                    { context = DefaultContext
                    , previousSubmissions = []
                    , submission = """
           let
               concat a b =
                   a ++ b
           in
           List.foldl concat "_init_" [ "a", "b", "c" ]
                      """
                    , expectedValueElmExpression = "\"cba_init_\""
                    }
        , Test.test "Literal from module" <|
            \_ ->
                expectationForElmInteractiveScenario
                    { context =
                        CustomModulesContext
                            { includeCoreModules = False
                            , modulesTexts = [ """
module ModuleName exposing (module_level_binding)


module_level_binding : String
module_level_binding =
    "literal"

""" ]
                            }
                    , previousSubmissions = []
                    , submission = """ ModuleName.module_level_binding """
                    , expectedValueElmExpression = "\"literal\""
                    }

        {-
            , Test.test "Partial application via multiple modules" <|
                       \_ ->
                           expectationForElmInteractiveScenario
                               { context =
                                   CustomModulesContext
                                       { includeCoreModules = True
                                       , modulesTexts = [ """
           module ModuleA exposing (partially_applied_a)


           partially_applied_a =
               function_with_three_parameters "a"


           function_with_three_parameters param0 param1 param2 =
               param0 ++ " " ++ param1 ++ " " ++ param2

           """, """
           module ModuleB exposing (partially_applied_b)

           import ModuleA exposing (..)


           partially_applied_b =
               ModuleA.partially_applied_a named_literal


           named_literal =
               "b"

           """ ]
                                       }
                               , previousSubmissions = []
                               , submission = """ ModuleB.partially_applied_b "c" """
                               , expectedValueElmExpression = "\"a b c\""
                               }
        -}
        , Test.describe "Operator precedence"
            [ Test.test "Operator asterisk precedes operator plus left and right" <|
                \_ ->
                    expectationForElmInteractiveScenario
                        { context = DefaultContext
                        , previousSubmissions = []
                        , submission = """ 4 + 4 * 3 + 1 """
                        , expectedValueElmExpression = 17 |> Json.Encode.int |> Json.Encode.encode 0
                        }
            , Test.test "Operator asterisk precedes operator plus left" <|
                \_ ->
                    expectationForElmInteractiveScenario
                        { context = DefaultContext
                        , previousSubmissions = []
                        , submission = """ 5 + 3 * 4 """
                        , expectedValueElmExpression = 17 |> Json.Encode.int |> Json.Encode.encode 0
                        }
            , Test.test "Parentheses override operator precedence" <|
                \_ ->
                    expectationForElmInteractiveScenario
                        { context = DefaultContext
                        , previousSubmissions = []
                        , submission = """ (1 + 2) * (3 + 1) """
                        , expectedValueElmExpression = 12 |> Json.Encode.int |> Json.Encode.encode 0
                        }
            , Test.test "Multiplication and division operators have same priority and are applied left to right" <|
                \_ ->
                    expectationForElmInteractiveScenario
                        { context = DefaultContext
                        , previousSubmissions = []
                        , submission = """ 20 * 20 // 30  """
                        , expectedValueElmExpression = 13 |> Json.Encode.int |> Json.Encode.encode 0
                        }
            , Test.test "Use value from previous submission" <|
                \_ ->
                    expectationForElmInteractiveScenario
                        { context = DefaultContext
                        , previousSubmissions = [ """custom_name = "hello" """ ]
                        , submission = """ custom_name ++ " world!" """
                        , expectedValueElmExpression = "hello world!" |> Json.Encode.string |> Json.Encode.encode 0
                        }
            ]
        , Test.test "Char.toCode" <|
            \_ ->
                expectationForElmInteractiveScenario
                    { context = DefaultContext
                    , previousSubmissions = []
                    , submission = """ String.fromInt (Char.toCode '😃') """
                    , expectedValueElmExpression = "128515" |> Json.Encode.string |> Json.Encode.encode 0
                    }
        , Test.test "Literal False" <|
            \_ ->
                expectationForElmInteractiveScenario
                    { context = DefaultContext
                    , previousSubmissions = []
                    , submission = """ False """
                    , expectedValueElmExpression = "False"
                    }
        , Test.test "1 < 3 evaluates to True" <|
            \_ ->
                expectationForElmInteractiveScenario
                    { context = DefaultContext
                    , previousSubmissions = []
                    , submission = """ 1 < 3 """
                    , expectedValueElmExpression = "True"
                    }
        , Test.test "Record syntax with two fields" <|
            \_ ->
                expectationForElmInteractiveScenario
                    { context = DefaultContext
                    , previousSubmissions = []
                    , submission = """ { beta =  "1",  alpha = "0" } """
                    , expectedValueElmExpression = """{ alpha = "0", beta = "1" }"""
                    }
        , Test.test "Empty record syntax" <|
            \_ ->
                expectationForElmInteractiveScenario
                    { context = DefaultContext
                    , previousSubmissions = []
                    , submission = """ {   } """
                    , expectedValueElmExpression = """{}"""
                    }
        , Test.test "Multiply integers with result not fitting into 64 bits" <|
            \_ ->
                expectationForElmInteractiveScenario
                    { context = DefaultContext
                    , previousSubmissions = []
                    , submission = "1234567890 * 3456789012 * 5678901234 * 7890123456"
                    , expectedValueElmExpression = "191221166964169353263730685698459294720"
                    }

        {-
            The current representation in the Elm syntax parser causes a precision loss for integer literals: https://github.com/stil4m/elm-syntax/blob/3d4ee78c007ee8987b8cfe6c3ea07e5500632b6b/src/Elm/Syntax/Expression.elm#L115-L116
            For discussion of this issue, see also https://github.com/stil4m/elm-syntax/issues/108

           , Test.test "Literal integer not fitting into 64 bits" <|
               \_ ->
                   expectationForElmInteractiveScenario
                       { context = DefaultContext
                       , previousSubmissions = []
                       , submission = "289589985200426854031398766651426"
                       , expectedValueElmExpression = "289589985200426854031398766651426"
                       }
        -}
        , Test.test "Reference declaration via module alias - zero params" <|
            \_ ->
                expectationForElmInteractiveScenario
                    { context =
                        CustomModulesContext
                            { includeCoreModules = False
                            , modulesTexts = [ """
module Beta exposing (..)

import Alfa as OtherModule


test = OtherModule.alfa_decl

""", """
module Alfa exposing (..)


alfa_decl = 567

""" ]
                            }
                    , previousSubmissions = []
                    , submission = " Beta.test "
                    , expectedValueElmExpression = "567"
                    }
        , Test.test "Reference declaration via module alias - one param" <|
            \_ ->
                expectationForElmInteractiveScenario
                    { context =
                        CustomModulesContext
                            { includeCoreModules = False
                            , modulesTexts = [ """
module Beta exposing (..)

import Alfa as OtherModule


test = OtherModule.alfa_decl

""", """
module Alfa exposing (..)


alfa_decl param = [17, param]

""" ]
                            }
                    , previousSubmissions = []
                    , submission = " Beta.test 21"
                    , expectedValueElmExpression = "[17,21]"
                    }
        ]


expectationForElmInteractiveScenario :
    { context : ElmInteractive.InteractiveContext
    , previousSubmissions : List String
    , submission : String
    , expectedValueElmExpression : String
    }
    -> Expect.Expectation
expectationForElmInteractiveScenario scenario =
    Expect.equal (Ok scenario.expectedValueElmExpression)
        (ElmInteractive.submissionInInteractive scenario.context scenario.previousSubmissions scenario.submission
            |> Result.map .displayText
        )


evolutionStagesToMakeElmFunction : Test.Test
evolutionStagesToMakeElmFunction =
    [ Test.test "Literal" <|
        \_ ->
            Pine.ListExpression
                [ Pine.LiteralExpression (Pine.valueFromString "Literal")
                , Pine.LiteralExpression (Pine.valueFromString "just a literal")
                ]
                |> Pine.evaluateExpression Pine.emptyEvalContext
                |> Result.andThen (Pine.decodeExpressionFromValue >> Result.mapError Pine.DescribePathEnd)
                |> Expect.equal
                    (Ok (Pine.LiteralExpression (Pine.valueFromString "just a literal")))
    , Test.test "List with one element" <|
        \_ ->
            Pine.ListExpression
                [ Pine.LiteralExpression (Pine.valueFromString "List")
                , Pine.ListExpression
                    [ Pine.ListExpression
                        [ Pine.LiteralExpression (Pine.valueFromString "Literal")
                        , Pine.LiteralExpression (Pine.valueFromString "test")
                        ]
                    ]
                ]
                |> Pine.evaluateExpression Pine.emptyEvalContext
                |> Result.andThen (Pine.decodeExpressionFromValue >> Result.mapError Pine.DescribePathEnd)
                |> Expect.equal
                    (Ok
                        (Pine.ListExpression
                            [ Pine.LiteralExpression (Pine.valueFromString "test") ]
                        )
                    )
    , Test.test "Kernel application with empty list" <|
        \_ ->
            Pine.ListExpression
                [ Pine.LiteralExpression (Pine.valueFromString "KernelApplication")
                , Pine.ListExpression
                    [ Pine.ListExpression
                        [ Pine.LiteralExpression (Pine.valueFromString "functionName")
                        , Pine.LiteralExpression (Pine.valueFromString "equal")
                        ]
                    , Pine.ListExpression
                        [ Pine.LiteralExpression (Pine.valueFromString "argument")
                        , Pine.ListExpression
                            [ Pine.LiteralExpression (Pine.valueFromString "List")
                            , Pine.LiteralExpression (Pine.ListValue [])
                            ]
                        ]
                    ]
                ]
                |> Pine.evaluateExpression Pine.emptyEvalContext
                |> Result.andThen (Pine.decodeExpressionFromValue >> Result.mapError Pine.DescribePathEnd)
                |> Expect.equal
                    (Ok
                        (Pine.KernelApplicationExpression
                            { functionName = "equal"
                            , argument = Pine.ListExpression []
                            }
                        )
                    )
    , Test.test "Kernel application concat" <|
        \_ ->
            Pine.ListExpression
                [ Pine.LiteralExpression (Pine.valueFromString "KernelApplication")
                , Pine.ListExpression
                    [ Pine.ListExpression
                        [ Pine.LiteralExpression (Pine.valueFromString "functionName")
                        , Pine.LiteralExpression (Pine.valueFromString "concat")
                        ]
                    , Pine.ListExpression
                        [ Pine.LiteralExpression (Pine.valueFromString "argument")
                        , Pine.ListExpression
                            [ Pine.LiteralExpression (Pine.valueFromString "Literal")
                            , Pine.EnvironmentExpression
                            ]
                        ]
                    ]
                ]
                |> Pine.evaluateExpression
                    (Pine.emptyEvalContext
                        |> Pine.addToEnvironment
                            [ Pine.valueFromContextExpansionWithName
                                ( "alfa", Pine.valueFromBigInt (BigInt.fromInt 123) )
                            ]
                    )
                |> Result.andThen (Pine.decodeExpressionFromValue >> Result.mapError Pine.DescribePathEnd)
                |> Expect.equal
                    (Ok
                        (Pine.KernelApplicationExpression
                            { functionName = "concat"
                            , argument =
                                Pine.LiteralExpression
                                    (Pine.ListValue
                                        [ Pine.valueFromContextExpansionWithName
                                            ( "alfa", Pine.valueFromBigInt (BigInt.fromInt 123) )
                                        ]
                                    )
                            }
                        )
                    )
    , Test.test "Kernel application concat with context" <|
        \_ ->
            Pine.ListExpression
                [ Pine.LiteralExpression (Pine.valueFromString "KernelApplication")
                , Pine.ListExpression
                    [ Pine.ListExpression
                        [ Pine.LiteralExpression (Pine.valueFromString "functionName")
                        , Pine.LiteralExpression (Pine.valueFromString "concat")
                        ]
                    , Pine.ListExpression
                        [ Pine.LiteralExpression (Pine.valueFromString "argument")
                        , Pine.ListExpression
                            [ Pine.LiteralExpression (Pine.valueFromString "List")
                            , Pine.ListExpression
                                [ Pine.ListExpression
                                    [ Pine.LiteralExpression (Pine.valueFromString "Literal")
                                    , Pine.EnvironmentExpression
                                    ]
                                , Pine.ListExpression
                                    [ Pine.LiteralExpression (Pine.valueFromString "Environment")
                                    , Pine.ListExpression []
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
                |> Pine.evaluateExpression
                    (Pine.emptyEvalContext
                        |> Pine.addToEnvironment
                            [ Pine.valueFromContextExpansionWithName
                                ( "beta", Pine.valueFromBigInt (BigInt.fromInt 345) )
                            ]
                    )
                |> Result.andThen (Pine.decodeExpressionFromValue >> Result.mapError Pine.DescribePathEnd)
                |> Expect.equal
                    (Ok
                        (Pine.KernelApplicationExpression
                            { functionName = "concat"
                            , argument =
                                Pine.ListExpression
                                    [ Pine.LiteralExpression
                                        (Pine.ListValue
                                            [ Pine.valueFromContextExpansionWithName
                                                ( "beta", Pine.valueFromBigInt (BigInt.fromInt 345) )
                                            ]
                                        )
                                    , Pine.EnvironmentExpression
                                    ]
                            }
                        )
                    )
    ]
        |> Test.describe "Make Elm Function"
