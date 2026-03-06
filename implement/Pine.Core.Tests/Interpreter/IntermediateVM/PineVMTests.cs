using AwesomeAssertions;
using Pine.Core.CommonEncodings;
using Xunit;

namespace Pine.Core.Tests.Interpreter.IntermediateVM;

public class PineVMTests
{
    [Fact]
    public void Evaluate_expression()
    {
        var testCases = new[]
        {
            new
            {
                expression = (Expression)
                Expression.LiteralInstance(PineValue.Blob([1, 4, 7])),
                environment = PineValue.EmptyList,

                expected = Result<string, PineValue>.ok(
                    PineValue.Blob([1, 4, 7]))
            },
            new
            {
                expression = (Expression)Expression.ListInstance([]),
                environment = PineValue.EmptyList,

                expected = Result<string, PineValue>.ok(PineValue.EmptyList)
            },
            new
            {
                expression =
                (Expression)
                Expression.KernelApplicationInstance
                (
                    input: Expression.ListInstance([]),
                    function: "concat"
                ),
                environment = PineValue.EmptyList,

                expected = Result<string, PineValue>.ok(PineValue.EmptyList)
            },
            new
            {
                expression =
                (Expression)
                Expression.KernelApplicationInstance
                (
                    input:
                    Expression.ListInstance(
                        [
                            Expression.ListInstance(
                                [
                                    Expression.LiteralInstance(
                                        IntegerEncoding.EncodeSignedInteger(1)),
                                ]),

                            Expression.KernelApplicationInstance
                            (
                                function: nameof(KernelFunction.head),
                                input: Expression.EnvironmentInstance
                            ),

                            new Expression.ParseAndEval(
                                encoded:
                                Expression.LiteralInstance(
                                    ExpressionEncoding.EncodeExpressionAsValue(
                                        Expression.KernelApplicationInstance
                                        (
                                            function: nameof(KernelFunction.head),
                                            input: Expression.EnvironmentInstance
                                        ))),
                                environment:
                                Expression.KernelApplicationInstance
                                (
                                    function: "skip",
                                    input: Expression.ListInstance(
                                        [
                                        Expression.LiteralInstance(
                                            IntegerEncoding.EncodeSignedInteger(1)),

                                        Expression.EnvironmentInstance,
                                        ])
                                ))
                        ]),
                    function: "concat"
                ),
                environment =
                PineValue.List(
                    [
                    PineValue.List([IntegerEncoding.EncodeSignedInteger(3)]),
                    PineValue.List([IntegerEncoding.EncodeSignedInteger(11)]),
                    ]),

                expected = Result<string, PineValue>.ok(PineValue.List(
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
                Expression.ConditionalInstance(
                    condition:
                    Expression.LiteralInstance(PineValue.Blob([4])),
                    falseBranch:
                    Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(17)),
                    trueBranch:
                    Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(13))),
                environment = PineValue.EmptyList,

                expected = Result<string, PineValue>.ok(
                    IntegerEncoding.EncodeSignedInteger(13))
            },

            new
            {
                expression=
                (Expression)
                Expression.ConditionalInstance(
                    condition:
                    Expression.LiteralInstance(PineValue.Blob([2])),
                    falseBranch:
                    new Expression.ParseAndEval(
                        encoded:
                        Expression.KernelApplicationInstance
                            (
                                function: nameof(KernelFunction.head),
                                input:
                                Expression.KernelApplicationInstance
                                (
                                    function: "skip",
                                    input: Expression.ListInstance(
                                        [
                                        Expression.LiteralInstance(
                                            IntegerEncoding.EncodeSignedInteger(1)),

                                        Expression.EnvironmentInstance,
                                        ])
                                )
                            ),
                        Expression.EnvironmentInstance),
                    trueBranch:
                    Expression.LiteralInstance(
                        IntegerEncoding.EncodeSignedInteger(17))),

                environment =
                PineValue.List(
                    [
                        IntegerEncoding.EncodeSignedInteger(43),
                        ExpressionEncoding.EncodeExpressionAsValue(
                            Expression.KernelApplicationInstance
                            (
                                function: nameof(KernelFunction.head),
                                input: Expression.EnvironmentInstance
                            ))
                    ]),

                expected = Result<string, PineValue>.ok(
                    IntegerEncoding.EncodeSignedInteger(43))
            },

            new
            {
                expression=
                (Expression)
                Expression.ConditionalInstance(
                    condition:
                    Expression.LiteralInstance(PineValue.Blob([4])),
                    falseBranch:
                    new Expression.ParseAndEval(
                        encoded:
                        Expression.KernelApplicationInstance
                            (
                                function: nameof(KernelFunction.head),
                                input: Expression.EnvironmentInstance
                            ),
                        Expression.EnvironmentInstance),
                    trueBranch:
                    Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(17))),

                environment = PineValue.EmptyList,

                expected = Result<string, PineValue>.ok(
                    IntegerEncoding.EncodeSignedInteger(17))
            },

            new
            {
                expression=
                (Expression)
                Expression.ConditionalInstance(
                    condition:
                    Expression.LiteralInstance(PineValue.Blob([4])),
                    falseBranch:
                    Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(17)),
                    trueBranch:
                    new Expression.ParseAndEval(
                        encoded:
                        Expression.KernelApplicationInstance
                            (
                                function: nameof(KernelFunction.head),
                                input:
                                Expression.KernelApplicationInstance
                                (
                                    function: "skip",
                                    input: Expression.ListInstance(
                                        [
                                        Expression.LiteralInstance(
                                            IntegerEncoding.EncodeSignedInteger(1)),

                                        Expression.EnvironmentInstance,
                                        ])
                                )
                            ),
                        Expression.EnvironmentInstance)),

                environment =
                PineValue.List(
                    [
                        IntegerEncoding.EncodeSignedInteger(41),

                        ExpressionEncoding.EncodeExpressionAsValue(
                            Expression.KernelApplicationInstance
                            (
                                function: nameof(KernelFunction.head),
                                input: Expression.EnvironmentInstance
                            ))
                    ]),

                expected =
                Result<string, PineValue>.ok(IntegerEncoding.EncodeSignedInteger(41))
            },

            // Test Prepend_List_Items: prepend single item to a list
            new
            {
                expression =
                (Expression)
                Expression.KernelApplicationInstance
                (
                    input:
                    Expression.ListInstance(
                        [
                            Expression.ListInstance(
                                [
                                    Expression.LiteralInstance(
                                        IntegerEncoding.EncodeSignedInteger(99)),
                                ]),

                            Expression.EnvironmentInstance,
                        ]),
                    function: "concat"
                ),
                environment =
                PineValue.List(
                    [
                    IntegerEncoding.EncodeSignedInteger(1),
                    IntegerEncoding.EncodeSignedInteger(2),
                    IntegerEncoding.EncodeSignedInteger(3),
                    ]),

                expected = Result<string, PineValue>.ok(PineValue.List(
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
                Expression.KernelApplicationInstance
                (
                    input:
                    Expression.ListInstance(
                        [
                            Expression.ListInstance(
                                [
                                    Expression.LiteralInstance(
                                        IntegerEncoding.EncodeSignedInteger(10)),

                                    Expression.LiteralInstance(
                                        IntegerEncoding.EncodeSignedInteger(20)),

                                    Expression.LiteralInstance(
                                        IntegerEncoding.EncodeSignedInteger(30)),
                                ]),

                            Expression.EnvironmentInstance,
                        ]),
                    function: "concat"
                ),
                environment =
                PineValue.List(
                    [
                    IntegerEncoding.EncodeSignedInteger(1),
                    IntegerEncoding.EncodeSignedInteger(2),
                    ]),

                expected = Result<string, PineValue>.ok(PineValue.List(
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
                Expression.KernelApplicationInstance
                (
                    input:
                    Expression.ListInstance(
                        [
                            Expression.EnvironmentInstance,

                            Expression.ListInstance(
                                [
                                    Expression.LiteralInstance(
                                        IntegerEncoding.EncodeSignedInteger(99)),
                                ]),
                        ]),
                    function: "concat"
                ),
                environment =
                PineValue.List(
                    [
                    IntegerEncoding.EncodeSignedInteger(1),
                    IntegerEncoding.EncodeSignedInteger(2),
                    IntegerEncoding.EncodeSignedInteger(3),
                    ]),

                expected = Result<string, PineValue>.ok(PineValue.List(
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
                Expression.KernelApplicationInstance
                (
                    input:
                    Expression.ListInstance(
                        [
                            Expression.EnvironmentInstance,

                            Expression.ListInstance(
                                [
                                    Expression.LiteralInstance(
                                        IntegerEncoding.EncodeSignedInteger(10)),

                                    Expression.LiteralInstance(
                                        IntegerEncoding.EncodeSignedInteger(20)),

                                    Expression.LiteralInstance(
                                        IntegerEncoding.EncodeSignedInteger(30)),
                                ]),
                        ]),
                    function: "concat"
                ),
                environment =
                PineValue.List(
                    [
                    IntegerEncoding.EncodeSignedInteger(1),
                    IntegerEncoding.EncodeSignedInteger(2),
                    ]),

                expected = Result<string, PineValue>.ok(PineValue.List(
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
