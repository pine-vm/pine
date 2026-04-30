using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
using Pine.Core.Tests.Elm.ElmCompilerInDotnet;
using System.Collections.Immutable;
using System.Linq;
using Xunit;

using PineCodeAnalysis = Pine.Core.CodeAnalysis.CodeAnalysis;

namespace Pine.Core.Tests.CodeAnalysis;

/// <summary>
/// Focused tests verifying that <see cref="StaticProgramParser"/> recognizes the
/// consolidated form produced by the generic-application chain consolidation
/// optimization in
/// <see cref="ReducePineExpression.TryConsolidateGenericFunctionApplicationChain(Expression.ParseAndEval, PineVMParseCache)"/>.
/// </summary>
public class StaticProgramParserConsolidatedFormTests
{
    private static string RenderParseError(StaticProgramParser.ParseError<DeclQualifiedName> parseError)
    {
        var pathString = string.Join("/", parseError.Path.Reverse().Select(i => i.FullName));

        return pathString + ":" + parseError.Message;
    }

    private static Expression BuildParamReference(int paramIndex) =>
        ExpressionBuilder.BuildExpressionForPathInExpression(
            expression: Expression.EnvironmentInstance,
            path: [1 + paramIndex]);

    /// <summary>
    /// Build a 3-parameter callee whose body adds its three params and a constant.
    /// </summary>
    private static PineValue BuildThreeParamCallee()
    {
        return
            FunctionValueBuilder.EmitFunctionValueWithEnvFunctions(
                innerExpression:
                Expression.KernelApplicationInstance(
                    function: nameof(KernelFunction.int_add),
                    input:
                    Expression.ListInstance(
                        [
                            BuildParamReference(0),
                            BuildParamReference(1),
                            BuildParamReference(2),
                            Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(71))
                        ])),
                parameterCount: 3,
                envFunctions: []);
    }

    /// <summary>
    /// Build a 4-parameter callee whose body adds its four params and a constant.
    /// </summary>
    private static PineValue BuildFourParamCallee()
    {
        return
            FunctionValueBuilder.EmitFunctionValueWithEnvFunctions(
                innerExpression:
                Expression.KernelApplicationInstance(
                    function: nameof(KernelFunction.int_add),
                    input:
                    Expression.ListInstance(
                        [
                            BuildParamReference(0),
                            BuildParamReference(1),
                            BuildParamReference(2),
                            BuildParamReference(3),
                            Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(71))
                        ])),
                parameterCount: 4,
                envFunctions: []);
    }

    [Fact]
    public void Parses_consolidated_form_of_2_args_partial_to_3_param_callee()
    {
        var parseCache = new PineVMParseCache();

        var calleeValue = BuildThreeParamCallee();

        // Build the optimized consolidated form for `callee arg0 arg1` (a partial application
        // with 2 of 3 arguments) where arg0 and arg1 are references to outer parameters.
        var arg0 = BuildParamReference(0);
        var arg1 = BuildParamReference(1);

        var chain =
            (Expression.ParseAndEval)PineCodeAnalysis.BuildGenericFunctionApplication(
                Expression.LiteralInstance(calleeValue),
                new Expression[] { arg0, arg1 });

        var consolidated =
            ReducePineExpression.TryConsolidateGenericFunctionApplicationChain(chain, parseCache);

        consolidated.Should().NotBeNull();

        // Wrap the consolidated form as the body of a 2-param outer function.
        var outerFunctionValue =
            FunctionValueBuilder.EmitFunctionValueWithEnvFunctions(
                innerExpression: Expression.ListInstance([consolidated!]),
                parameterCount: 2,
                envFunctions: []);

        var parseConfig =
            BuildStaticProgramParserConfig.Default(
                ImmutableDictionary<DeclQualifiedName, PineValue>.Empty
                .SetItem(DeclQualifiedName.FromString("callee"), calleeValue),
                parseCache);

        var parseResult =
            StaticProgramParser.ParseProgram(
                roots:
                ImmutableDictionary<DeclQualifiedName, PineValue>.Empty
                .SetItem(DeclQualifiedName.FromString("outer"), outerFunctionValue),
                parseConfig: parseConfig,
                parseCache: parseCache)
            .Extract(err => throw new System.Exception(RenderParseError(err)));

        parseResult.Count.Should().Be(2);

        parseResult[DeclQualifiedName.FromString("outer")].BodyExpression.Should().Be(
            StaticExpression<DeclQualifiedName>.ListInstance(
                [
                    StaticExpression<DeclQualifiedName>.FunctionApplicationInstance(
                        functionName: DeclQualifiedName.FromString("callee"),
                        arguments:
                        StaticExpression<DeclQualifiedName>.ListInstance(
                            [
                                StaticExpression<DeclQualifiedName>.ParameterReference(0),
                                StaticExpression<DeclQualifiedName>.ParameterReference(1)
                            ]))
                ]));
    }

    [Fact]
    public void Parses_consolidated_form_of_3_args_partial_to_4_param_callee()
    {
        var parseCache = new PineVMParseCache();

        var calleeValue = BuildFourParamCallee();

        var arg0 = BuildParamReference(0);
        var arg1 = BuildParamReference(1);
        var arg2 = BuildParamReference(2);

        var chain =
            (Expression.ParseAndEval)PineCodeAnalysis.BuildGenericFunctionApplication(
                Expression.LiteralInstance(calleeValue),
                new Expression[] { arg0, arg1, arg2 });

        var consolidated =
            ReducePineExpression.TryConsolidateGenericFunctionApplicationChain(chain, parseCache);

        consolidated.Should().NotBeNull();

        var outerFunctionValue =
            FunctionValueBuilder.EmitFunctionValueWithEnvFunctions(
                innerExpression: Expression.ListInstance([consolidated!]),
                parameterCount: 3,
                envFunctions: []);

        var parseConfig =
            BuildStaticProgramParserConfig.Default(
                ImmutableDictionary<DeclQualifiedName, PineValue>.Empty
                .SetItem(DeclQualifiedName.FromString("callee"), calleeValue),
                parseCache);

        var parseResult =
            StaticProgramParser.ParseProgram(
                roots:
                ImmutableDictionary<DeclQualifiedName, PineValue>.Empty
                .SetItem(DeclQualifiedName.FromString("outer"), outerFunctionValue),
                parseConfig: parseConfig,
                parseCache: parseCache)
            .Extract(err => throw new System.Exception(RenderParseError(err)));

        parseResult.Count.Should().Be(2);

        parseResult[DeclQualifiedName.FromString("outer")].BodyExpression.Should().Be(
            StaticExpression<DeclQualifiedName>.ListInstance(
                [
                    StaticExpression<DeclQualifiedName>.FunctionApplicationInstance(
                        functionName: DeclQualifiedName.FromString("callee"),
                        arguments:
                        StaticExpression<DeclQualifiedName>.ListInstance(
                            [
                                StaticExpression<DeclQualifiedName>.ParameterReference(0),
                                StaticExpression<DeclQualifiedName>.ParameterReference(1),
                                StaticExpression<DeclQualifiedName>.ParameterReference(2)
                            ]))
                ]));
    }

    [Fact]
    public void Parses_consolidated_form_with_literal_arguments()
    {
        // Verify the parser handles the consolidated form when arguments are literal values
        // rather than environment references.
        var parseCache = new PineVMParseCache();

        var calleeValue = BuildThreeParamCallee();

        var arg0 = Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(11));
        var arg1 = Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(13));

        var chain =
            (Expression.ParseAndEval)PineCodeAnalysis.BuildGenericFunctionApplication(
                Expression.LiteralInstance(calleeValue),
                new Expression[] { arg0, arg1 });

        var consolidated =
            ReducePineExpression.TryConsolidateGenericFunctionApplicationChain(chain, parseCache);

        consolidated.Should().NotBeNull();

        var outerFunctionValue =
            FunctionValueBuilder.EmitFunctionValueWithEnvFunctions(
                innerExpression: Expression.ListInstance([consolidated!]),
                parameterCount: 0,
                envFunctions: []);

        var parseConfig =
            BuildStaticProgramParserConfig.Default(
                ImmutableDictionary<DeclQualifiedName, PineValue>.Empty
                .SetItem(DeclQualifiedName.FromString("callee"), calleeValue),
                parseCache);

        var parseResult =
            StaticProgramParser.ParseProgram(
                roots:
                ImmutableDictionary<DeclQualifiedName, PineValue>.Empty
                .SetItem(DeclQualifiedName.FromString("outer"), outerFunctionValue),
                parseConfig: parseConfig,
                parseCache: parseCache)
            .Extract(err => throw new System.Exception(RenderParseError(err)));

        parseResult.Count.Should().Be(2);

        parseResult[DeclQualifiedName.FromString("outer")].BodyExpression.Should().Be(
            StaticExpression<DeclQualifiedName>.ListInstance(
                [
                    StaticExpression<DeclQualifiedName>.FunctionApplicationInstance(
                        functionName: DeclQualifiedName.FromString("callee"),
                        arguments:
                        StaticExpression<DeclQualifiedName>.ListInstance(
                            [
                                StaticExpression<DeclQualifiedName>.LiteralInstance(arg0.Value),
                                StaticExpression<DeclQualifiedName>.LiteralInstance(arg1.Value)
                            ])),
                ]));
    }
}
