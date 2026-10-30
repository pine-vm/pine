module ElmInteractive2024Tests exposing (..)

import BigInt
import ElmCompiler2024
import ElmInteractive2024 exposing (ElmCoreModulesExtent(..), InteractiveContext(..))
import ElmInteractiveParser2024
import Expect
import Json.Encode
import Pine2024
import Test


interactiveScenarios : Test.Test
interactiveScenarios =
    Test.describe "Elm interactive scenarios"
        [ Test.test "Just a literal String" <|
            \_ ->
                expectationForElmInteractiveScenario
                    { context =
                        CustomModulesContext { includeCoreModules = Nothing, modulesTexts = [] }
                    , previousSubmissions = []
                    , submission = """  "just a literal ✔️"  """
                    , expectedValueElmExpression = "\"just a literal ✔️\""
                    }
        , Test.test "Literal float whole" <|
            \_ ->
                expectationForElmInteractiveScenario
                    { context =
                        CustomModulesContext { includeCoreModules = Nothing, modulesTexts = [] }
                    , previousSubmissions = []
                    , submission = """  1.0  """
                    , expectedValueElmExpression = "1"
                    }
        , Test.test "Literal float 0.5" <|
            \_ ->
                expectationForElmInteractiveScenario
                    { context =
                        CustomModulesContext { includeCoreModules = Nothing, modulesTexts = [] }
                    , previousSubmissions = []
                    , submission = """  0.5  """
                    , expectedValueElmExpression = "0.5"
                    }
        , Test.test "Literal float 1.5" <|
            \_ ->
                expectationForElmInteractiveScenario
                    { context =
                        CustomModulesContext { includeCoreModules = Nothing, modulesTexts = [] }
                    , previousSubmissions = []
                    , submission = """  1.5  """
                    , expectedValueElmExpression = "1.5"
                    }
        , Test.test "Literal float -0.7" <|
            \_ ->
                expectationForElmInteractiveScenario
                    { context =
                        CustomModulesContext { includeCoreModules = Nothing, modulesTexts = [] }
                    , previousSubmissions = []
                    , submission = """  -0.7  """
                    , expectedValueElmExpression = "-0.7"
                    }
        , Test.test "floor 0" <|
            \_ ->
                expectationForElmInteractiveScenario
                    { context =
                        CustomModulesContext { includeCoreModules = Just OnlyCoreModules, modulesTexts = [] }
                    , previousSubmissions = []
                    , submission = """ floor 0  """
                    , expectedValueElmExpression = "0"
                    }
        , Test.test "floor 0.1" <|
            \_ ->
                expectationForElmInteractiveScenario
                    { context =
                        CustomModulesContext { includeCoreModules = Just OnlyCoreModules, modulesTexts = [] }
                    , previousSubmissions = []
                    , submission = """ floor 0.1  """
                    , expectedValueElmExpression = "0"
                    }
        , Test.test "floor 0.5" <|
            \_ ->
                expectationForElmInteractiveScenario
                    { context =
                        CustomModulesContext { includeCoreModules = Just OnlyCoreModules, modulesTexts = [] }
                    , previousSubmissions = []
                    , submission = """ floor 0.5  """
                    , expectedValueElmExpression = "0"
                    }
        , Test.test "floor 1.7" <|
            \_ ->
                expectationForElmInteractiveScenario
                    { context =
                        CustomModulesContext { includeCoreModules = Just OnlyCoreModules, modulesTexts = [] }
                    , previousSubmissions = []
                    , submission = """ floor 1.7  """
                    , expectedValueElmExpression = "1"
                    }
        , Test.test "floor negative decimal" <|
            \_ ->
                expectationForElmInteractiveScenario
                    { context =
                        CustomModulesContext { includeCoreModules = Just OnlyCoreModules, modulesTexts = [] }
                    , previousSubmissions = []
                    , submission = """ floor -0.7 """
                    , expectedValueElmExpression = "-1"
                    }
        , Test.test "String.toFloat empty" <|
            \_ ->
                expectationForElmInteractiveScenario
                    { context =
                        CustomModulesContext { includeCoreModules = Just OnlyCoreModules, modulesTexts = [] }
                    , previousSubmissions = []
                    , submission = """ String.toFloat ""  """
                    , expectedValueElmExpression = "Nothing"
                    }
        , Test.test "toFloat 0" <|
            \_ ->
                expectationForElmInteractiveScenario
                    { context =
                        CustomModulesContext { includeCoreModules = Just OnlyCoreModules, modulesTexts = [] }
                    , previousSubmissions = []
                    , submission = """ String.toFloat "0"  """
                    , expectedValueElmExpression = "Just 0"
                    }
        , Test.test "toFloat 1" <|
            \_ ->
                expectationForElmInteractiveScenario
                    { context =
                        CustomModulesContext { includeCoreModules = Just OnlyCoreModules, modulesTexts = [] }
                    , previousSubmissions = []
                    , submission = """ String.toFloat "1"  """
                    , expectedValueElmExpression = "Just 1"
                    }
        , Test.test "toFloat 0.7" <|
            \_ ->
                expectationForElmInteractiveScenario
                    { context =
                        CustomModulesContext { includeCoreModules = Just OnlyCoreModules, modulesTexts = [] }
                    , previousSubmissions = []
                    , submission = """ String.toFloat "0.7"  """
                    , expectedValueElmExpression = "Just 0.7"
                    }
        , Test.test "fromFloat 0.7" <|
            \_ ->
                expectationForElmInteractiveScenario
                    { context =
                        CustomModulesContext { includeCoreModules = Just OnlyCoreModules, modulesTexts = [] }
                    , previousSubmissions = []
                    , submission = """ String.fromFloat 0.7 """
                    , expectedValueElmExpression = "\"0.7\""
                    }
        , Test.test "fromFloat negative fraction" <|
            \_ ->
                expectationForElmInteractiveScenario
                    { context =
                        CustomModulesContext { includeCoreModules = Just OnlyCoreModules, modulesTexts = [] }
                    , previousSubmissions = []
                    , submission = """ String.fromFloat -0.7 """
                    , expectedValueElmExpression = "\"-0.7\""
                    }
        , Test.test "fromFloat leading fractional zero" <|
            \_ ->
                expectationForElmInteractiveScenario
                    { context =
                        CustomModulesContext { includeCoreModules = Just OnlyCoreModules, modulesTexts = [] }
                    , previousSubmissions = []
                    , submission = """ String.fromFloat 0.05 """
                    , expectedValueElmExpression = "\"0.05\""
                    }
        , Test.test "Literal float 0.7" <|
            \_ ->
                expectationForElmInteractiveScenario
                    { context =
                        CustomModulesContext { includeCoreModules = Just OnlyCoreModules, modulesTexts = [] }
                    , previousSubmissions = []
                    , submission = """ 0.7 """
                    , expectedValueElmExpression = "0.7"
                    }
        , Test.test "Just a literal List String" <|
            \_ ->
                expectationForElmInteractiveScenario
                    { context =
                        CustomModulesContext { includeCoreModules = Nothing, modulesTexts = [] }
                    , previousSubmissions = []
                    , submission = """  [ "just a literal ✔️", "another string" ]  """
                    , expectedValueElmExpression = """["just a literal ✔️","another string"]"""
                    }
        , Test.test "Concat string literal" <|
            \_ ->
                expectationForElmInteractiveScenario
                    { context = CustomModulesContext { includeCoreModules = Just OnlyCoreModules, modulesTexts = [] }
                    , previousSubmissions = []
                    , submission = """ "first literal " ++ " second literal ✔️" """
                    , expectedValueElmExpression = "\"first literal  second literal ✔️\""
                    }
        , Test.test "Apply String.fromInt" <|
            \_ ->
                expectationForElmInteractiveScenario
                    { context = CustomModulesContext { includeCoreModules = Just OnlyCoreModules, modulesTexts = [] }
                    , previousSubmissions = []
                    , submission = " String.fromInt 123 "
                    , expectedValueElmExpression = "\"123\""
                    }
        , Test.test "Add integers" <|
            \_ ->
                expectationForElmInteractiveScenario
                    { context = CustomModulesContext { includeCoreModules = Just OnlyCoreModules, modulesTexts = [] }
                    , previousSubmissions = []
                    , submission = " 1 + 3 "
                    , expectedValueElmExpression = "4"
                    }
        , Test.test "Multiply integers" <|
            \_ ->
                expectationForElmInteractiveScenario
                    { context = CustomModulesContext { includeCoreModules = Just OnlyCoreModules, modulesTexts = [] }
                    , previousSubmissions = []
                    , submission = " 17 * 41 "
                    , expectedValueElmExpression = "697"
                    }
        , Test.test "Dependency within let" <|
            \_ ->
                expectationForElmInteractiveScenario
                    { context = CustomModulesContext { includeCoreModules = Nothing, modulesTexts = [] }
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
                        CustomModulesContext { includeCoreModules = Nothing, modulesTexts = [] }
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
        , Test.test "Function application one argument" <|
            \_ ->
                expectationForElmInteractiveScenario
                    { context =
                        CustomModulesContext { includeCoreModules = Nothing, modulesTexts = [] }
                    , previousSubmissions = []
                    , submission = """
let
    function_with_one_parameter param0 =
        [ "literal from function", param0 ]
in
function_with_one_parameter "argument"
"""
                    , expectedValueElmExpression = """["literal from function","argument"]"""
                    }
        , Test.test "Function application two arguments" <|
            \_ ->
                expectationForElmInteractiveScenario
                    { context =
                        CustomModulesContext { includeCoreModules = Nothing, modulesTexts = [] }
                    , previousSubmissions = []
                    , submission = """
let
    function_with_two_parameters param0 param1 =
        [ "literal from function", param0, param1 ]
in
function_with_two_parameters "argument 0" "argument 1"
"""
                    , expectedValueElmExpression = """["literal from function","argument 0","argument 1"]"""
                    }
        , Test.test "Partial application two arguments" <|
            \_ ->
                expectationForElmInteractiveScenario
                    { context = CustomModulesContext { includeCoreModules = Just OnlyCoreModules, modulesTexts = [] }
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
        , Test.test "Partial application three arguments in two groups" <|
            \_ ->
                expectationForElmInteractiveScenario
                    { context = CustomModulesContext { includeCoreModules = Just OnlyCoreModules, modulesTexts = [] }
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
        , Test.test "Lambda with 'var' pattern" <|
            \_ ->
                expectationForElmInteractiveScenario
                    { context = CustomModulesContext { includeCoreModules = Nothing, modulesTexts = [] }
                    , previousSubmissions = []
                    , submission = """ (\\x -> x) "test" """
                    , expectedValueElmExpression = "\"test\""
                    }
        , Test.test "Lambda with 'all' pattern" <|
            \_ ->
                expectationForElmInteractiveScenario
                    { context = CustomModulesContext { includeCoreModules = Nothing, modulesTexts = [] }
                    , previousSubmissions = []
                    , submission = """ (\\_ -> "constant") "test" """
                    , expectedValueElmExpression = "\"constant\""
                    }
        , Test.test "Case of expression deconstructing List into empty and non-empty" <|
            \_ ->
                expectationForElmInteractiveScenario
                    { context = CustomModulesContext { includeCoreModules = Just OnlyCoreModules, modulesTexts = [] }
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
                    { context = CustomModulesContext { includeCoreModules = Just OnlyCoreModules, modulesTexts = [] }
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
                            { includeCoreModules = Nothing
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
        , Test.test "Partial application via multiple modules" <|
            \_ ->
                expectationForElmInteractiveScenario
                    { context =
                        CustomModulesContext
                            { includeCoreModules = Just OnlyCoreModules
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
        , Test.test "Use value from previous submission" <|
            \_ ->
                expectationForElmInteractiveScenario
                    { context = CustomModulesContext { includeCoreModules = Just OnlyCoreModules, modulesTexts = [] }
                    , previousSubmissions = [ """custom_name = "hello" """ ]
                    , submission = """ custom_name ++ " world!" """
                    , expectedValueElmExpression = "hello world!" |> Json.Encode.string |> Json.Encode.encode 0
                    }
        , Test.test "1 < 3 evaluates to True" <|
            \_ ->
                expectationForElmInteractiveScenario
                    { context = CustomModulesContext { includeCoreModules = Just OnlyCoreModules, modulesTexts = [] }
                    , previousSubmissions = []
                    , submission = """ 1 < 3 """
                    , expectedValueElmExpression = "True"
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
                            { includeCoreModules = Nothing
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
                            { includeCoreModules = Nothing
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
    { context : ElmInteractive2024.InteractiveContext
    , previousSubmissions : List String
    , submission : String
    , expectedValueElmExpression : String
    }
    -> Expect.Expectation
expectationForElmInteractiveScenario scenario =
    Expect.equal (Ok scenario.expectedValueElmExpression)
        (ElmInteractiveParser2024.submissionInInteractive scenario.context scenario.previousSubmissions scenario.submission
            |> Result.map .displayText
        )


floatValueRegressions : Test.Test
floatValueRegressions =
    Test.describe "Float values and strings"
        [ Test.test "whole float literal encodes as an integer" <|
            \_ ->
                ElmCompiler2024.valueFromFloat 1.0
                    |> Expect.equal (Pine2024.valueFromInt 1)
        , Test.test "fractional float literal encodes as a rational" <|
            \_ ->
                ElmCompiler2024.valueFromFloat 0.5
                    |> Expect.equal
                        (Pine2024.ListValue
                            [ Pine2024.valueFromString "Elm_Float"
                            , Pine2024.ListValue [ Pine2024.valueFromInt 1, Pine2024.valueFromInt 2 ]
                            ]
                        )
        , Test.test "decimal float literal encodes as a rational" <|
            \_ ->
                ElmCompiler2024.valueFromFloat 0.7
                    |> Expect.equal
                        (Pine2024.ListValue
                            [ Pine2024.valueFromString "Elm_Float"
                            , Pine2024.ListValue [ Pine2024.valueFromInt 7, Pine2024.valueFromInt 10 ]
                            ]
                        )
        , Test.test "negative float literal encodes as a rational" <|
            \_ ->
                ElmCompiler2024.valueFromFloat -0.7
                    |> Expect.equal
                        (Pine2024.ListValue
                            [ Pine2024.valueFromString "Elm_Float"
                            , Pine2024.ListValue [ Pine2024.valueFromInt -7, Pine2024.valueFromInt 10 ]
                            ]
                        )
        , Test.test "fromFloat returns a flat string" <|
            \_ ->
                String.fromFloat 0.7
                    |> String.toList
                    |> Expect.equal [ '0', '.', '7' ]
        , Test.test "fromFloat renders negative fractions" <|
            \_ ->
                String.fromFloat -0.7
                    |> Expect.equal "-0.7"
        , Test.test "fromFloat preserves leading fractional zeros" <|
            \_ ->
                String.fromFloat 0.05
                    |> Expect.equal "0.05"
        ]


evolutionStagesToMakeElmFunction : Test.Test
evolutionStagesToMakeElmFunction =
    [ Test.test "Literal" <|
        \_ ->
            Pine2024.ListExpression
                [ Pine2024.LiteralExpression (Pine2024.valueFromString "Literal")
                , Pine2024.ListExpression [ Pine2024.LiteralExpression (Pine2024.valueFromString "just a literal") ]
                ]
                |> Pine2024.evaluateExpression Pine2024.emptyEvalEnvironment
                |> Result.andThen (Pine2024.parseExpressionFromValue >> Result.mapError Pine2024.DescribePathEnd)
                |> Expect.equal
                    (Ok (Pine2024.LiteralExpression (Pine2024.valueFromString "just a literal")))
    , Test.test "List with one element" <|
        \_ ->
            Pine2024.ListExpression
                [ Pine2024.LiteralExpression (Pine2024.valueFromString "List")
                , Pine2024.ListExpression
                    [ Pine2024.ListExpression
                        [ Pine2024.ListExpression
                            [ Pine2024.LiteralExpression (Pine2024.valueFromString "Literal")
                            , Pine2024.ListExpression [ Pine2024.LiteralExpression (Pine2024.valueFromString "test") ]
                            ]
                        ]
                    ]
                ]
                |> Pine2024.evaluateExpression Pine2024.emptyEvalEnvironment
                |> Result.andThen (Pine2024.parseExpressionFromValue >> Result.mapError Pine2024.DescribePathEnd)
                |> Expect.equal
                    (Ok
                        (Pine2024.ListExpression
                            [ Pine2024.LiteralExpression (Pine2024.valueFromString "test") ]
                        )
                    )
    , Test.test "Kernel application with empty list" <|
        \_ ->
            Pine2024.ListExpression
                [ Pine2024.LiteralExpression (Pine2024.valueFromString "KernelApplication")
                , Pine2024.ListExpression
                    [ Pine2024.LiteralExpression (Pine2024.valueFromString "equal")
                    , Pine2024.ListExpression
                        [ Pine2024.LiteralExpression (Pine2024.valueFromString "List")
                        , Pine2024.ListExpression [ Pine2024.LiteralExpression (Pine2024.ListValue []) ]
                        ]
                    ]
                ]
                |> Pine2024.evaluateExpression Pine2024.emptyEvalEnvironment
                |> Result.andThen (Pine2024.parseExpressionFromValue >> Result.mapError Pine2024.DescribePathEnd)
                |> Expect.equal
                    (Ok
                        (Pine2024.KernelApplicationExpression
                            "equal"
                            (Pine2024.ListExpression [])
                        )
                    )
    , Test.test "Kernel application concat" <|
        \_ ->
            Pine2024.ListExpression
                [ Pine2024.LiteralExpression (Pine2024.valueFromString "KernelApplication")
                , Pine2024.ListExpression
                    [ Pine2024.LiteralExpression (Pine2024.valueFromString "concat")
                    , Pine2024.ListExpression
                        [ Pine2024.LiteralExpression (Pine2024.valueFromString "Literal")
                        , Pine2024.ListExpression [ Pine2024.EnvironmentExpression ]
                        ]
                    ]
                ]
                |> Pine2024.evaluateExpression
                    (Pine2024.evalEnvironmentFromList
                        [ Pine2024.valueFromContextExpansionWithName
                            ( "alfa", Pine2024.valueFromInt 123 )
                        ]
                    )
                |> Result.andThen (Pine2024.parseExpressionFromValue >> Result.mapError Pine2024.DescribePathEnd)
                |> Expect.equal
                    (Ok
                        (Pine2024.KernelApplicationExpression
                            "concat"
                            (Pine2024.LiteralExpression
                                (Pine2024.ListValue
                                    [ Pine2024.valueFromContextExpansionWithName
                                        ( "alfa", Pine2024.valueFromInt 123 )
                                    ]
                                )
                            )
                        )
                    )
    , Test.test "Kernel application concat with context" <|
        \_ ->
            Pine2024.ListExpression
                [ Pine2024.LiteralExpression (Pine2024.valueFromString "KernelApplication")
                , Pine2024.ListExpression
                    [ Pine2024.LiteralExpression (Pine2024.valueFromString "concat")
                    , Pine2024.ListExpression
                        [ Pine2024.LiteralExpression (Pine2024.valueFromString "List")
                        , Pine2024.ListExpression
                            [ Pine2024.ListExpression
                                [ Pine2024.ListExpression
                                    [ Pine2024.LiteralExpression (Pine2024.valueFromString "Literal")
                                    , Pine2024.ListExpression [ Pine2024.EnvironmentExpression ]
                                    ]
                                , Pine2024.ListExpression
                                    [ Pine2024.LiteralExpression (Pine2024.valueFromString "Environment")
                                    , Pine2024.ListExpression []
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
                |> Pine2024.evaluateExpression
                    (Pine2024.evalEnvironmentFromList
                        [ Pine2024.valueFromContextExpansionWithName
                            ( "beta", Pine2024.valueFromInt 345 )
                        ]
                    )
                |> Result.andThen (Pine2024.parseExpressionFromValue >> Result.mapError Pine2024.DescribePathEnd)
                |> Expect.equal
                    (Ok
                        (Pine2024.KernelApplicationExpression
                            "concat"
                            (Pine2024.ListExpression
                                [ Pine2024.LiteralExpression
                                    (Pine2024.ListValue
                                        [ Pine2024.valueFromContextExpansionWithName
                                            ( "beta", Pine2024.valueFromInt 345 )
                                        ]
                                    )
                                , Pine2024.EnvironmentExpression
                                ]
                            )
                        )
                    )
    ]
        |> Test.describe "Make Elm Function"


encodeDecodeChoiceTypeDeclarationTest : Test.Test
encodeDecodeChoiceTypeDeclarationTest =
    Test.test "Encode and decode Elm module choice type" <|
        \_ ->
            ElmCompiler2024.ElmModuleChoiceType
                [ ( "Variant_Alfa", ElmCompiler2024.ElmModuleChoiceTypeTag 0 )
                , ( "Variant_Beta", ElmCompiler2024.ElmModuleChoiceTypeTag 1 )
                , ( "Variant_Gamma", ElmCompiler2024.ElmModuleChoiceTypeTag 2 )
                ]
                |> ElmCompiler2024.ElmModuleChoiceTypeDeclaration
                |> ElmCompiler2024.emitTypeDeclarationValue
                |> ElmCompiler2024.parseTypeDeclarationFromValueTagged
                |> Expect.equal
                    (Ok
                        (ElmCompiler2024.ElmModuleChoiceTypeDeclaration
                            (ElmCompiler2024.ElmModuleChoiceType
                                [ ( "Variant_Alfa", ElmCompiler2024.ElmModuleChoiceTypeTag 0 )
                                , ( "Variant_Beta", ElmCompiler2024.ElmModuleChoiceTypeTag 1 )
                                , ( "Variant_Gamma", ElmCompiler2024.ElmModuleChoiceTypeTag 2 )
                                ]
                            )
                        )
                    )


testCompileRecordAccessPineExpression : Test.Test
testCompileRecordAccessPineExpression =
    Test.test "Compile record access pine expression" <|
        \_ ->
            let
                recordValue =
                    Pine2024.ListValue
                        [ ElmCompiler2024.elmRecordTypeTagNameAsValue
                        , Pine2024.ListValue
                            [ Pine2024.ListValue
                                [ Pine2024.ListValue
                                    [ Pine2024.valueFromString "alfa"
                                    , Pine2024.valueFromInt 123
                                    ]
                                ]
                            ]
                        ]
            in
            Pine2024.ParseAndEvalExpression
                (Pine2024.LiteralExpression ElmCompiler2024.pineFunctionForRecordAccessAsValue)
                (Pine2024.ListExpression
                    [ Pine2024.LiteralExpression recordValue
                    , Pine2024.LiteralExpression (Pine2024.valueFromString "alfa")
                    ]
                )
                |> Pine2024.evaluateExpression Pine2024.emptyEvalEnvironment
                |> Result.mapError Pine2024.displayStringFromPineError
                |> Result.andThen ElmInteractive2024.pineValueAsElmValue
                |> Expect.equal (Ok (ElmInteractive2024.ElmInteger (BigInt.fromInt 123)))


testCompileRecordUpdatePineExpression : Test.Test
testCompileRecordUpdatePineExpression =
    Test.describe "Compile record update pine expression"
        [ Test.test "Single field - update first" <|
            \_ ->
                let
                    recordValue =
                        Pine2024.ListValue
                            [ ElmCompiler2024.elmRecordTypeTagNameAsValue
                            , Pine2024.ListValue
                                [ Pine2024.ListValue
                                    [ Pine2024.ListValue
                                        [ Pine2024.valueFromString "alfa"
                                        , Pine2024.valueFromInt 123
                                        ]
                                    ]
                                ]
                            ]
                in
                Pine2024.ParseAndEvalExpression
                    (Pine2024.LiteralExpression ElmCompiler2024.pineFunctionForRecordUpdateAsValue)
                    (Pine2024.ListExpression
                        [ Pine2024.LiteralExpression recordValue
                        , Pine2024.ListExpression
                            [ Pine2024.ListExpression
                                [ Pine2024.LiteralExpression (Pine2024.valueFromString "alfa")
                                , Pine2024.LiteralExpression (Pine2024.valueFromInt 456)
                                ]
                            ]
                        ]
                    )
                    |> Pine2024.evaluateExpression Pine2024.emptyEvalEnvironment
                    |> Result.mapError Pine2024.displayStringFromPineError
                    |> Result.andThen ElmInteractive2024.pineValueAsElmValue
                    |> Expect.equal
                        (Ok
                            (ElmInteractive2024.ElmRecord
                                [ ( "alfa", ElmInteractive2024.ElmInteger (BigInt.fromInt 456) )
                                ]
                            )
                        )
        , Test.test "Two fields - update first" <|
            \_ ->
                let
                    recordValue =
                        Pine2024.ListValue
                            [ ElmCompiler2024.elmRecordTypeTagNameAsValue
                            , Pine2024.ListValue
                                [ Pine2024.ListValue
                                    [ Pine2024.ListValue
                                        [ Pine2024.valueFromString "alfa"
                                        , Pine2024.valueFromInt 11
                                        ]
                                    , Pine2024.ListValue
                                        [ Pine2024.valueFromString "beta"
                                        , Pine2024.valueFromInt 13
                                        ]
                                    ]
                                ]
                            ]
                in
                Pine2024.ParseAndEvalExpression
                    (Pine2024.LiteralExpression ElmCompiler2024.pineFunctionForRecordUpdateAsValue)
                    (Pine2024.ListExpression
                        [ Pine2024.LiteralExpression recordValue
                        , Pine2024.ListExpression
                            [ Pine2024.ListExpression
                                [ Pine2024.LiteralExpression (Pine2024.valueFromString "alfa")
                                , Pine2024.LiteralExpression (Pine2024.valueFromInt 21)
                                ]
                            ]
                        ]
                    )
                    |> Pine2024.evaluateExpression Pine2024.emptyEvalEnvironment
                    |> Result.mapError Pine2024.displayStringFromPineError
                    |> Result.andThen ElmInteractive2024.pineValueAsElmValue
                    |> Expect.equal
                        (Ok
                            (ElmInteractive2024.ElmRecord
                                [ ( "alfa", ElmInteractive2024.ElmInteger (BigInt.fromInt 21) )
                                , ( "beta", ElmInteractive2024.ElmInteger (BigInt.fromInt 13) )
                                ]
                            )
                        )
        , Test.test "Two fields - update second" <|
            \_ ->
                let
                    recordValue =
                        Pine2024.ListValue
                            [ ElmCompiler2024.elmRecordTypeTagNameAsValue
                            , Pine2024.ListValue
                                [ Pine2024.ListValue
                                    [ Pine2024.ListValue
                                        [ Pine2024.valueFromString "alfa"
                                        , Pine2024.valueFromInt 11
                                        ]
                                    , Pine2024.ListValue
                                        [ Pine2024.valueFromString "beta"
                                        , Pine2024.valueFromInt 13
                                        ]
                                    ]
                                ]
                            ]
                in
                Pine2024.ParseAndEvalExpression
                    (Pine2024.LiteralExpression ElmCompiler2024.pineFunctionForRecordUpdateAsValue)
                    (Pine2024.ListExpression
                        [ Pine2024.LiteralExpression recordValue
                        , Pine2024.ListExpression
                            [ Pine2024.ListExpression
                                [ Pine2024.LiteralExpression (Pine2024.valueFromString "beta")
                                , Pine2024.LiteralExpression (Pine2024.valueFromInt 31)
                                ]
                            ]
                        ]
                    )
                    |> Pine2024.evaluateExpression Pine2024.emptyEvalEnvironment
                    |> Result.mapError Pine2024.displayStringFromPineError
                    |> Result.andThen ElmInteractive2024.pineValueAsElmValue
                    |> Expect.equal
                        (Ok
                            (ElmInteractive2024.ElmRecord
                                [ ( "alfa", ElmInteractive2024.ElmInteger (BigInt.fromInt 11) )
                                , ( "beta", ElmInteractive2024.ElmInteger (BigInt.fromInt 31) )
                                ]
                            )
                        )
        ]
