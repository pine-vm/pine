using AwesomeAssertions;
using Pine.Core.CommonEncodings;
using Xunit;

namespace Pine.Core.Tests.Interpreter.IntermediateVM;

public class PineVMTests
{
    [Fact]
    public void Evaluate_expression()
    {
        var testCases =
            new[]
            {
                new
                {
                    expression = (Expression)
                    Expression.LitralInst(PineValue.Blob([1, 4, 7])),
                    environment = PineValue.EmptyList,

                    expected =
                    Result<string, PineValue>.ok(
                        PineValue.Blob([1, 4, 7]))
                },
                new
                {
                    expression = (Expression)Expression.ListInst([]),
                    environment = PineValue.EmptyList,

                    expected = Result<string, PineValue>.ok(PineValue.EmptyList)
                },
                new
                {
                    expression =
                    (Expression)
                    Expression.BuiltinInst
                    (
                        input: Expression.ListInst([]),
                        function: "concat"),
                    environment = PineValue.EmptyList,

                    expected = Result<string, PineValue>.ok(PineValue.EmptyList)
                },
                new
                {
                    expression =
                    (Expression)
                    Expression.BuiltinInst
                    (
                        input:
                        Expression.ListInst(
                            [
                            Expression.ListInst(
                                [
                                Expression.LitralInst(
                                    IntegerEncoding.EncodeSignedInteger(1)),
                                ]),

                            Expression.BuiltinInst
                            (
                                function: nameof(BuiltinFunction.head),
                                input: Expression.EnvironmentInstance),

                            new Expression.Eval(
                                encoded:
                                Expression.LitralInst(
                                    ExpressionEncoding.EncodeExpressionAsValue(
                                        Expression.BuiltinInst
                                        (
                                            function: nameof(BuiltinFunction.head),
                                            input: Expression.EnvironmentInstance))),
                                environment:
                                Expression.BuiltinInst
                                (
                                    function: "skip",
                                    input: Expression.ListInst(
                                        [
                                        Expression.LitralInst(
                                            IntegerEncoding.EncodeSignedInteger(1)),

                                        Expression.EnvironmentInstance,
                                        ])))
                            ]),
                        function: "concat"),
                    environment =
                    PineValue.List(
                        [
                        PineValue.List([IntegerEncoding.EncodeSignedInteger(3)]),
                        PineValue.List([IntegerEncoding.EncodeSignedInteger(11)]),
                        ]),

                    expected =
                    Result<string, PineValue>.ok(
                        PineValue.List(
                            [
                            IntegerEncoding.EncodeSignedInteger(1),
                            IntegerEncoding.EncodeSignedInteger(3),
                            IntegerEncoding.EncodeSignedInteger(11),
                            ]))
                },

                new
                {
                    expression =
                    (Expression)
                    Expression.ConditionalInst(
                        condition:
                        Expression.LitralInst(PineValue.Blob([4])),
                        falseBranch:
                        Expression.LitralInst(IntegerEncoding.EncodeSignedInteger(17)),
                        trueBranch:
                        Expression.LitralInst(IntegerEncoding.EncodeSignedInteger(13))),
                    environment = PineValue.EmptyList,

                    expected =
                    Result<string, PineValue>.ok(
                        IntegerEncoding.EncodeSignedInteger(13))
                },

                new
                {
                    expression=
                    (Expression)
                    Expression.ConditionalInst(
                        condition:
                        Expression.LitralInst(PineValue.Blob([2])),
                        falseBranch:
                        new Expression.Eval(
                            encoded:
                            Expression.BuiltinInst
                            (
                                function: nameof(BuiltinFunction.head),
                                input:
                                Expression.BuiltinInst
                                (
                                    function: "skip",
                                    input: Expression.ListInst(
                                        [
                                        Expression.LitralInst(
                                            IntegerEncoding.EncodeSignedInteger(1)),

                                        Expression.EnvironmentInstance,
                                        ]))),
                            Expression.EnvironmentInstance),
                        trueBranch:
                        Expression.LitralInst(
                            IntegerEncoding.EncodeSignedInteger(17))),

                    environment =
                    PineValue.List(
                        [
                        IntegerEncoding.EncodeSignedInteger(43),
                        ExpressionEncoding.EncodeExpressionAsValue(
                            Expression.BuiltinInst
                            (
                                function: nameof(BuiltinFunction.head),
                                input: Expression.EnvironmentInstance))
                        ]),

                    expected =
                    Result<string, PineValue>.ok(
                        IntegerEncoding.EncodeSignedInteger(43))
                },

                new
                {
                    expression=
                    (Expression)
                    Expression.ConditionalInst(
                        condition:
                        Expression.LitralInst(PineValue.Blob([4])),
                        falseBranch:
                        new Expression.Eval(
                            encoded:
                            Expression.BuiltinInst
                            (
                                function: nameof(BuiltinFunction.head),
                                input: Expression.EnvironmentInstance),
                            Expression.EnvironmentInstance),
                        trueBranch:
                        Expression.LitralInst(IntegerEncoding.EncodeSignedInteger(17))),

                    environment = PineValue.EmptyList,

                    expected =
                    Result<string, PineValue>.ok(
                        IntegerEncoding.EncodeSignedInteger(17))
                },

                new
                {
                    expression=
                    (Expression)
                    Expression.ConditionalInst(
                        condition:
                        Expression.LitralInst(PineValue.Blob([4])),
                        falseBranch:
                        Expression.LitralInst(IntegerEncoding.EncodeSignedInteger(17)),
                        trueBranch:
                        new Expression.Eval(
                            encoded:
                            Expression.BuiltinInst
                            (
                                function: nameof(BuiltinFunction.head),
                                input:
                                Expression.BuiltinInst
                                (
                                    function: "skip",
                                    input: Expression.ListInst(
                                        [
                                        Expression.LitralInst(
                                            IntegerEncoding.EncodeSignedInteger(1)),

                                        Expression.EnvironmentInstance,
                                        ]))),
                            Expression.EnvironmentInstance)),

                    environment =
                    PineValue.List(
                        [
                        IntegerEncoding.EncodeSignedInteger(41),

                        ExpressionEncoding.EncodeExpressionAsValue(
                            Expression.BuiltinInst
                            (
                                function: nameof(BuiltinFunction.head),
                                input: Expression.EnvironmentInstance))
                        ]),

                    expected =
                    Result<string, PineValue>.ok(IntegerEncoding.EncodeSignedInteger(41))
                },

                // Test Prepend_List_Items: prepend single item to a list
                new
                {
                    expression =
                    (Expression)
                    Expression.BuiltinInst
                    (
                        input:
                        Expression.ListInst(
                            [
                            Expression.ListInst(
                                [
                                Expression.LitralInst(
                                    IntegerEncoding.EncodeSignedInteger(99)),
                                ]),

                            Expression.EnvironmentInstance,
                            ]),
                        function: "concat"),
                    environment =
                    PineValue.List(
                        [
                        IntegerEncoding.EncodeSignedInteger(1),
                        IntegerEncoding.EncodeSignedInteger(2),
                        IntegerEncoding.EncodeSignedInteger(3),
                        ]),

                    expected =
                    Result<string, PineValue>.ok(
                        PineValue.List(
                            [
                            IntegerEncoding.EncodeSignedInteger(99),
                            IntegerEncoding.EncodeSignedInteger(1),
                            IntegerEncoding.EncodeSignedInteger(2),
                            IntegerEncoding.EncodeSignedInteger(3),
                            ]))
                },

                // Test Prepend_List_Items: prepend multiple items to a list
                new
                {
                    expression =
                    (Expression)
                    Expression.BuiltinInst
                    (
                        input:
                        Expression.ListInst(
                            [
                            Expression.ListInst(
                                [
                                Expression.LitralInst(
                                    IntegerEncoding.EncodeSignedInteger(10)),

                                Expression.LitralInst(
                                    IntegerEncoding.EncodeSignedInteger(20)),

                                Expression.LitralInst(
                                    IntegerEncoding.EncodeSignedInteger(30)),
                                ]),

                            Expression.EnvironmentInstance,
                            ]),
                        function: "concat"),
                    environment =
                    PineValue.List(
                        [
                        IntegerEncoding.EncodeSignedInteger(1),
                        IntegerEncoding.EncodeSignedInteger(2),
                        ]),

                    expected =
                    Result<string, PineValue>.ok(
                        PineValue.List(
                            [
                            IntegerEncoding.EncodeSignedInteger(10),
                            IntegerEncoding.EncodeSignedInteger(20),
                            IntegerEncoding.EncodeSignedInteger(30),
                            IntegerEncoding.EncodeSignedInteger(1),
                            IntegerEncoding.EncodeSignedInteger(2),
                            ]))
                },

                // Test Append_List_Items: append single item to a list
                new
                {
                    expression =
                    (Expression)
                    Expression.BuiltinInst
                    (
                        input:
                        Expression.ListInst(
                            [
                            Expression.EnvironmentInstance,

                            Expression.ListInst(
                                [
                                Expression.LitralInst(
                                    IntegerEncoding.EncodeSignedInteger(99)),
                                ]),
                            ]),
                        function: "concat"),
                    environment =
                    PineValue.List(
                        [
                        IntegerEncoding.EncodeSignedInteger(1),
                        IntegerEncoding.EncodeSignedInteger(2),
                        IntegerEncoding.EncodeSignedInteger(3),
                        ]),

                    expected =
                    Result<string, PineValue>.ok(
                        PineValue.List(
                            [
                            IntegerEncoding.EncodeSignedInteger(1),
                            IntegerEncoding.EncodeSignedInteger(2),
                            IntegerEncoding.EncodeSignedInteger(3),
                            IntegerEncoding.EncodeSignedInteger(99),
                            ]))
                },

                // Test Append_List_Items: append multiple items to a list
                new
                {
                    expression =
                    (Expression)
                    Expression.BuiltinInst
                    (
                        input:
                        Expression.ListInst(
                            [
                            Expression.EnvironmentInstance,

                            Expression.ListInst(
                                [
                                Expression.LitralInst(
                                    IntegerEncoding.EncodeSignedInteger(10)),

                                Expression.LitralInst(
                                    IntegerEncoding.EncodeSignedInteger(20)),

                                Expression.LitralInst(
                                    IntegerEncoding.EncodeSignedInteger(30)),
                                ]),
                            ]),
                        function: "concat"),
                    environment =
                    PineValue.List(
                        [
                        IntegerEncoding.EncodeSignedInteger(1),
                        IntegerEncoding.EncodeSignedInteger(2),
                        ]),

                    expected =
                    Result<string, PineValue>.ok(
                        PineValue.List(
                            [
                            IntegerEncoding.EncodeSignedInteger(1),
                            IntegerEncoding.EncodeSignedInteger(2),
                            IntegerEncoding.EncodeSignedInteger(10),
                            IntegerEncoding.EncodeSignedInteger(20),
                            IntegerEncoding.EncodeSignedInteger(30),
                            ]))
                },
            };

        foreach (var testCase in testCases)
        {
            var pineVM =
                Core.Interpreter.IntermediateVM.PineVM.CreateCustom(
                    evalCache: null,
                    evaluationConfigDefault: null,
                    reportFunctionApplication: null,
                    compilationEnvClasses: null,
                    disableReductionInCompilation: false,
                    selectPrecompiled: null,
                    skipInlineForExpression: _ => false,
                    enableTailRecursionOptimization: false,
                    parseCache: null,
                    precompiledLeaves: null,
                    reportEnterPrecompiledLeaf: null,
                    reportExitPrecompiledLeaf: null,
                    optimizationParametersSerial: null,
                    cacheFileStore: null);

            var evaluated =
                pineVM.EvaluateExpression(
                    testCase.expression,
                    testCase.environment);

            evaluated.Should().Be(
                testCase.expected,
                "Evaluated expression should match the expected value.");
        }
    }
}
