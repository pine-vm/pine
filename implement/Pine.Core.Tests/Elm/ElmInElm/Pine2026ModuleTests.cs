using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Elm.ElmInElm;
using Pine.Core.Encodings;
using Pine.Core.Files;
using Pine.Core.Functional;
using Pine.Core.PineLanguage;
using Pine.Core.Runtime;
using Pine.Core.Tests.Elm.ElmCompilerInDotnet.CoreLibraryModule;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Numerics;
using System.Text;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmInElm;

public class Pine2026ModuleTests
{
    private static readonly Lazy<IReadOnlyDictionary<string, PineValue>> s_functions =
        new(CompileFunctions);

    [Fact]
    public void Elm_Pine_2026_roundtrips_and_evaluates_every_expression_variant()
    {
        var literal = Expression.LitralInst(PineValue.Blob([1, 2, 255]));
        var environment = PineValue.List([PineValue.Blob([17]), PineValue.EmptyList]);
        var inner = Expression.BuiltinInst("head", Expression.EnvironmentInstance);

        Expression[] expressions =
            [
                literal,
                Expression.EmptyList,
                Expression.ListInst([literal, Expression.EnvironmentInstance]),
                Expression.BuiltinInst("length", Expression.EnvironmentInstance),
                Expression.ConditionalInst(
                    Expression.LitralInst(PineKernelValues.TrueValue),
                    Expression.EmptyList,
                    literal),
                Expression.EnvironmentInstance,
                new Expression.Eval(
                    Expression.LitralInst(ExpressionEncoding2026.EncodeExpressionAsValue(inner)),
                    Expression.EnvironmentInstance),
                new Expression.Label(PineValue.List([PineValue.EmptyBlob, environment]), inner)
            ];

        var oracle = DirectInterpreter.WithoutEvalCaching(new PineVMParseCache());

        foreach (var expression in expressions)
        {
            var encoded = ExpressionEncoding2026.EncodeExpressionAsValue(expression);

            Invoke("roundtrip", encoded).Extract(error => throw new Exception(error))
                .Should().Be(encoded);

            Invoke("evaluate", encoded, environment).Extract(error => throw new Exception(error))
                .Should().Be(
                oracle.EvaluateExpression(expression, environment)
                .Extract(error => throw new Exception(error)));
        }
    }

    [Fact]
    public void Elm_Pine_builtins_match_the_DotNet_reference()
    {
        Func<BigInteger, PineValue> integer = IntegerEncoding.EncodeSignedInteger;
        var large = integer(BigInteger.One << 128);
        var sequence = PineValue.Blob([18, 52]);
        var pair = PineValue.List([PineValue.Blob([255, 170]), PineValue.Blob([15])]);

        (string function, PineValue input)[] examples =
            [
                ("equal", pair),
                ("negate", large),
                ("length", sequence),
                ("head", sequence),
                ("skip", PineValue.List([integer(1), sequence])),
                ("take", PineValue.List([integer(1), sequence])),
                ("reverse", sequence),
                ("concat", PineValue.List([PineValue.EmptyList, sequence, sequence])),
                ("int_add", PineValue.List([large, large])),
                ("int_mul", PineValue.List([large, integer(-3)])),
                ("int_is_sorted_asc", PineValue.List([integer(-1), integer(0), large])),
                ("bit_and", pair),
                ("bit_or", pair),
                ("bit_xor", pair),
                ("bit_not", sequence),
                ("bit_shift_left", PineValue.List([integer(4), sequence])),
                ("bit_shift_right", PineValue.List([integer(4), sequence]))
            ];

        foreach (var (function, input) in examples)
        {
            var expression = Expression.BuiltinInst(function, Expression.LitralInst(input));

            Invoke("evaluate", ExpressionEncoding2026.EncodeExpressionAsValue(expression), PineValue.EmptyList)
                .Extract(error => throw new Exception(error))
                .Should().Be(BuiltinFunction.ApplyFunctionGeneric(function, input), function);
        }
    }

    [Fact]
    public void Elm_Pine_rejects_malformed_code_and_keeps_unselected_Eval_lazy()
    {
        PineValue[] malformed =
            [
                PineValue.EmptyBlob,
                PineValue.EmptyList,
                PineValue.List([StringEncoding.ValueFromString("Litral")]),
                PineValue.List([StringEncoding.ValueFromString("Environment"), PineValue.EmptyList]),
                ExpressionEncoding2024.EncodeExpressionAsValue(Expression.EnvironmentInstance)
            ];

        foreach (var encoded in malformed)
            Invoke("roundtrip", encoded).IsErrOrNull().Should().NotBeNull();

        var crash =
            new Expression.Eval(
                Expression.LitralInst(PineValue.EmptyBlob),
                Expression.EnvironmentInstance);

        var lazy =
            Expression.ConditionalInst(
                Expression.LitralInst(PineKernelValues.TrueValue),
                crash,
                Expression.EnvironmentInstance);

        var environment = PineValue.Blob([42]);

        Invoke("evaluate", ExpressionEncoding2026.EncodeExpressionAsValue(lazy), environment)
            .Extract(error => throw new Exception(error))
            .Should().Be(environment);

        Invoke("evaluate", ExpressionEncoding2026.EncodeExpressionAsValue(crash), environment)
            .IsErrOrNull().Should().Contain("Eval decode");
    }

    private static Result<string, PineValue> Invoke(string function, params PineValue[] arguments)
    {
        var result =
            CoreLibraryTestHelper.ApplyGeneric(
                s_functions.Value[function],
                [.. arguments.Select(ElmValueInterop.PineValueEncodedAsInElmCompiler)]);

        return
            ElmValueInterop.ParseElmValueAsCommonResult<Result<string, PineValue>>(
                result,
                err => err is ElmValue.ElmString text
                ?
                text.Value
                :
                throw new InvalidOperationException("Expected an Elm string error."),
                ok => ElmValueInterop.ElmValueDecodedAsInElmCompiler(ok, null, null),
                invalid => throw new InvalidOperationException(invalid));
    }

    private static IReadOnlyDictionary<string, PineValue> CompileFunctions()
    {
        const string adapter =
            """
            module Pine2026TestAdapter exposing (roundtrip, evaluate)
            import Pine

            roundtrip : Pine.Value -> Result String Pine.Value
            roundtrip encoded =
                Pine.parseExpressionFromValue encoded
                    |> Result.map Pine.encodeExpressionAsValue

            evaluate : Pine.Value -> Pine.Value -> Result String Pine.Value
            evaluate encoded environment =
                Pine.parseExpressionFromValue encoded
                    |> Result.andThen (\expression -> Pine.evalExpression expression environment)
            """;

        var sources =
            BundledFiles.CompilerSourceContainerFilesDefault.Value
            .SetNodeAtPathSorted(
                ["src", "Pine2026TestAdapter.elm"],
                FileTree.File(Encoding.UTF8.GetBytes(adapter)));

        var (environment, _) =
            ElmCompiler.CompileInteractiveEnvironment(
                sources,
                [
                    DeclQualifiedName.Create(["Pine2026TestAdapter"], "roundtrip"),
                    DeclQualifiedName.Create(["Pine2026TestAdapter"], "evaluate")
                ])
            .Extract(error => throw new Exception(error));

        return
            ElmInteractiveEnvironment.ParseInteractiveEnvironment(environment)
            .Extract(error => throw new Exception(error))
            .Modules.Single(module => module.moduleName is "Pine2026TestAdapter")
            .moduleContent.FunctionDeclarations.ToDictionary();
    }
}
