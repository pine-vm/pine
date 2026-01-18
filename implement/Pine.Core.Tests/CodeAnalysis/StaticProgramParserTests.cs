using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
using Pine.Core.Tests.Elm.ElmCompilerInDotnet;
using System.Collections.Immutable;
using System.Linq;
using Xunit;

namespace Pine.Core.Tests.CodeAnalysis;

public class StaticProgramParserTests
{
    private static string RenderParseError<T>(
        StaticProgramParser.ParseError<T> parseError,
        System.Func<T, string> renderIdentifier)
    {
        var pathString =
            string.Join("/", parseError.Path.Select(renderIdentifier));

        return pathString + ":" + parseError.Message;
    }

    private static string RenderParseError(
        StaticProgramParser.ParseError<string> parseError)
    {
        return RenderParseError(parseError, i => i);
    }

    private static string RenderParseError(
        StaticProgramParser.ParseError<DeclQualifiedName> parseError)
    {
        return RenderParseError(
            parseError,
            i => i.FullName);
    }

    private static Expression BuildParamReference(int paramIndex) =>
        ExpressionBuilder.BuildExpressionForPathInExpression(
            expression: Expression.EnvironmentInstance,
            path: [1, paramIndex]);

    [Fact]
    public void Zero_roots()
    {
        var parseCache = new PineVMParseCache();

        var parseConfig =
            StaticProgramParserConfig<string>.OptionalNullRequiredThrow();

        var parseResult =
            StaticProgramParser.ParseProgram(
                roots: ImmutableDictionary<string, PineValue>.Empty,
                parseConfig: parseConfig,
                parseCache: parseCache)
            .Extract(err => throw new System.Exception(RenderParseError(err)));

        parseResult.Count.Should().Be(0);
    }

    [Fact]
    public void Trivial_root_literal_integer()
    {
        var parseCache = new PineVMParseCache();

        var parseConfig =
            StaticProgramParserConfig<string>.OptionalNullRequiredThrow();

        var literalValue = IntegerEncoding.EncodeSignedInteger(1234567);

        var parseResult =
            StaticProgramParser.ParseProgram(
                roots:
                ImmutableDictionary<string, PineValue>.Empty
                .SetItem("decl", literalValue),
                parseConfig: parseConfig,
                parseCache: parseCache)
            .Extract(err => throw new System.Exception(RenderParseError(err)));

        parseResult.Count.Should().Be(1);

        parseResult["decl"].BodyExpression.Should().Be(
            StaticExpression<string>.LiteralInstance(literalValue));
    }

    [Fact]
    public void Trivial_root_list_containing_single_literal_integer()
    {
        var parseCache = new PineVMParseCache();

        var parseConfig =
            StaticProgramParserConfig<string>.OptionalNullRequiredThrow();

        var literalValue = IntegerEncoding.EncodeSignedInteger(1234567);

        var parseResult =
            StaticProgramParser.ParseProgram(
                roots:
                ImmutableDictionary<string, PineValue>.Empty
                .SetItem("decl", PineValue.List([literalValue])),
                parseConfig: parseConfig,
                parseCache: parseCache)
            .Extract(err => throw new System.Exception(RenderParseError(err)));

        parseResult.Count.Should().Be(1);

        parseResult["decl"].BodyExpression.Should().Be(
            StaticExpression<string>.LiteralInstance(PineValue.List([literalValue])));
    }

    [Fact]
    public void Function_list_singleton()
    {
        var parseCache = new PineVMParseCache();

        var parseConfig =
            StaticProgramParserConfig<string>.OptionalNullRequiredThrow();

        var functionValue =
            FunctionValueBuilder.EmitFunctionValueWithEnvFunctions(
                innerExpression:
                Expression.ListInstance(
                    [
                    BuildParamReference(0)
                    ]),
                parameterCount: 1,
                envFunctions: []);

        var parseResult =
            StaticProgramParser.ParseProgram(
                roots:
                ImmutableDictionary<string, PineValue>.Empty
                .SetItem("decl", functionValue),
                parseConfig: parseConfig,
                parseCache: parseCache)
            .Extract(err => throw new System.Exception(RenderParseError(err)));

        parseResult.Count.Should().Be(1);

        parseResult["decl"].BodyExpression.Should().Be(
            StaticExpression<string>.ListInstance(
                [
                StaticExpression<string>.ParameterReference(0)
                ]));
    }

    [Fact]
    public void Function_build_tuple()
    {
        var parseCache = new PineVMParseCache();

        var parseConfig =
            StaticProgramParserConfig<string>.OptionalNullRequiredThrow();

        var functionValue =
            FunctionValueBuilder.EmitFunctionValueWithEnvFunctions(
                innerExpression:
                Expression.ListInstance(
                    [
                    BuildParamReference(0),
                    BuildParamReference(1)
                    ]),
                parameterCount: 2,
                envFunctions: []);

        var parseResult =
            StaticProgramParser.ParseProgram(
                roots:
                ImmutableDictionary<string, PineValue>.Empty
                .SetItem("decl", functionValue),
                parseConfig: parseConfig,
                parseCache: parseCache)
            .Extract(err => throw new System.Exception(RenderParseError(err)));

        parseResult.Count.Should().Be(1);

        parseResult["decl"].BodyExpression.Should().Be(
            StaticExpression<string>.ListInstance(
                [
                StaticExpression<string>.ParameterReference(0),
                StaticExpression<string>.ParameterReference(1)
                ]));
    }

    [Fact]
    public void Function_int_add_71()
    {
        var parseCache = new PineVMParseCache();

        var parseConfig =
            StaticProgramParserConfig<string>.OptionalNullRequiredThrow();

        var functionValue =
            FunctionValueBuilder.EmitFunctionValueWithEnvFunctions(
                innerExpression:
                Expression.ListInstance(
                    [
                    Expression.KernelApplicationInstance(
                        function: nameof(KernelFunction.int_add),
                        input:
                        Expression.ListInstance(
                            [
                            BuildParamReference(0),
                            Expression.LiteralInstance(
                                IntegerEncoding.EncodeSignedInteger(71))
                            ])),
                    ]),
                parameterCount: 1,
                envFunctions: []);

        var parseResult =
            StaticProgramParser.ParseProgram(
                roots:
                ImmutableDictionary<string, PineValue>.Empty
                .SetItem("decl", functionValue),
                parseConfig: parseConfig,
                parseCache: parseCache)
            .Extract(err => throw new System.Exception(RenderParseError(err)));

        parseResult.Count.Should().Be(1);

        parseResult["decl"].BodyExpression.Should().Be(
            StaticExpression<string>.ListInstance(
                [
                StaticExpression<string>.KernelApplicationInstance(
                    function: nameof(KernelFunction.int_add),
                    input:
                    StaticExpression<string>.ListInstance(
                        [
                        StaticExpression<string>.ParameterReference(0),
                        StaticExpression<string>.LiteralInstance(
                            IntegerEncoding.EncodeSignedInteger(71))
                        ])),
                ]));
    }

    [Fact]
    public void Minimal_invocation_one_parameter()
    {
        var functionBetaValue =
            FunctionValueBuilder.EmitFunctionValueWithEnvFunctions(
                innerExpression:
                Expression.ListInstance(
                    [
                    Expression.KernelApplicationInstance(
                        function: nameof(KernelFunction.int_add),
                        input:
                        Expression.ListInstance(
                            [
                            BuildParamReference(0),
                            Expression.LiteralInstance(
                                IntegerEncoding.EncodeSignedInteger(71))
                            ])),
                    ]),
                parameterCount: 1,
                envFunctions: []);

        var functionAlfaValue =
            FunctionValueBuilder.EmitFunctionValueWithEnvFunctions(
                innerExpression:
                Expression.ListInstance(
                    [
                    new Expression.ParseAndEval(
                        encoded: Expression.LiteralInstance(functionBetaValue),
                        environment:
                        Expression.ListInstance(
                            [
                            BuildParamReference(0),
                            Expression.LiteralInstance(
                                IntegerEncoding.EncodeSignedInteger(31))
                            ])),
                    ]),
                parameterCount: 1,
                envFunctions: []);

        var parseCache = new PineVMParseCache();

        var parseConfig =
            BuildStaticProgramParserConfig.Default(
                ImmutableDictionary<DeclQualifiedName, PineValue>.Empty
                .SetItem(DeclQualifiedName.FromString("beta"), functionBetaValue),
                parseCache);

        var parseResult =
            StaticProgramParser.ParseProgram(
                roots:
                ImmutableDictionary<DeclQualifiedName, PineValue>.Empty
                .SetItem(DeclQualifiedName.FromString("alfa"), functionAlfaValue),
                parseConfig: parseConfig,
                parseCache: parseCache)
            .Extract(err => throw new System.Exception(RenderParseError(err)));

        parseResult.Count.Should().Be(2);

        parseResult[DeclQualifiedName.FromString("alfa")].BodyExpression.Should().Be(
            StaticExpression<DeclQualifiedName>.ListInstance(
                [
                StaticExpression<DeclQualifiedName>.FunctionApplicationInstance(
                    functionName: DeclQualifiedName.FromString("beta"),
                    arguments:
                    StaticExpression<DeclQualifiedName>.ListInstance(
                        [
                        StaticExpression<DeclQualifiedName>.ListInstance(
                            [
                            StaticExpression<DeclQualifiedName>.ParameterReference(0),
                            StaticExpression<DeclQualifiedName>.LiteralInstance(
                                IntegerEncoding.EncodeSignedInteger(31))
                            ])
                        ])),
                ]));
    }

    [Fact]
    public void Minimal_invocation_two_parameter()
    {
        var functionBetaValue =
            FunctionValueBuilder.EmitFunctionValueWithEnvFunctions(
                innerExpression:
                Expression.ListInstance(
                    [
                    Expression.KernelApplicationInstance(
                        function: nameof(KernelFunction.int_add),
                        input:
                        Expression.ListInstance(
                            [
                            BuildParamReference(0),
                            BuildParamReference(1),
                            ])),
                    ]),
                parameterCount: 2,
                envFunctions: []);

        var functionAlfaValue =
            FunctionValueBuilder.EmitFunctionValueWithEnvFunctions(
                innerExpression:
                Expression.ListInstance(
                    [
                    new Expression.ParseAndEval(
                        encoded:
                        new Expression.ParseAndEval(
                            encoded: Expression.LiteralInstance(functionBetaValue),
                            environment: Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(91))),
                        environment: BuildParamReference(0)),
                    ]),
                parameterCount: 1,
                envFunctions: []);

        var parseCache = new PineVMParseCache();

        var parseConfig =
            BuildStaticProgramParserConfig.Default(
                ImmutableDictionary<DeclQualifiedName, PineValue>.Empty
                .SetItem(DeclQualifiedName.FromString("beta"), functionBetaValue),
                parseCache);

        var parseResult =
            StaticProgramParser.ParseProgram(
                roots:
                ImmutableDictionary<DeclQualifiedName, PineValue>.Empty
                .SetItem(DeclQualifiedName.FromString("alfa"), functionAlfaValue),
                parseConfig: parseConfig,
                parseCache: parseCache)
            .Extract(err => throw new System.Exception(RenderParseError(err)));

        parseResult.Count.Should().Be(2);

        parseResult[DeclQualifiedName.FromString("alfa")].BodyExpression.Should().Be(
            StaticExpression<DeclQualifiedName>.ListInstance(
                [
                StaticExpression<DeclQualifiedName>.FunctionApplicationInstance(
                    functionName: DeclQualifiedName.FromString("beta"),
                    arguments:
                    StaticExpression<DeclQualifiedName>.ListInstance(
                        [
                        StaticExpression<DeclQualifiedName>.LiteralInstance(
                            IntegerEncoding.EncodeSignedInteger(91)),

                        StaticExpression<DeclQualifiedName>.ParameterReference(0),
                        ])),
                ]));
    }
}
