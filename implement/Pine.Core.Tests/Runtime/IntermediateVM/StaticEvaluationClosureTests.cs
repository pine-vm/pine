using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.CodeGen.Pine;
using Pine.Core.DotNet;
using Pine.Core.Encodings;
using Pine.Core.Functional;
using Pine.Core.Internal;
using Pine.Core.PineLanguage;
using Pine.Core.Runtime.IntermediateVM;
using System;
using System.Linq;
using Xunit;

namespace Pine.Core.Tests.Runtime.IntermediateVM;

public class StaticEvaluationClosureTests
{
    [Fact]
    public void Known_invalid_eval_targets_lower_to_static_errors_and_round_trip_in_IR()
    {
        var cache = new PineVMParseCache();
        var invalid = PineValue.List([StringEncoding.ValueFromString("UnknownTag")]);
        var expression = new Expression.Eval(Expression.LitralInst(invalid), Expression.EnvironmentInstance);
        var retained = Core.Runtime.InternalExpression.ExpressionConversion.FromCanonicalCached(expression);

        var graph =
            InternalExpressionCompilation.ControlFlowGraphFromExpression(
                retained,
                [],
                null,
                StaticFunctionInterface.Generic,
                cache);

        var instructions = graph.LowerToStackInstructions();
        instructions.Should().NotContain(instruction => PineControlFlowGraph.IsInvocation(instruction.Kind));
        var failure = instructions.Single(instruction => instruction.Kind == StackInstructionKind.Parse_Error_Const);
        failure.ParseError.Should().Be(cache.ParseInternalExpression(invalid).IsErrOrNull());

        var rendered =
            StackInstructionTraceRenderer.RenderInstructions(
                instructions,
                StackInstructionTraceRenderer.RenderingOptions.Persistence);

        StackInstructionTextFormat.ParseInstructions(rendered.Text, hash => rendered.ObservedValues[hash])
            .Should().Equal(instructions);

        using var native = Compile(expression);
        Action execute = () => native.Evaluate(PineValueInProcess.EmptyList);
        execute.Should().Throw<ParseExpressionException>().WithMessage(failure.ParseError);
    }

    [Fact]
    public void Constructed_closure_lowering_evaluates_the_application_environment_before_the_static_call()
    {
        var invalid = PineValue.List([StringEncoding.ValueFromString("Environment"), PineValue.EmptyList]);

        var construction =
            ConstructCall(
                Expression.LitralInst(
                    ExpressionEncoding.EncodeExpressionAsValue(Expression.LitralInst(Number(42)))));

        var expression =
            new Expression.Eval(
                construction,
                new Expression.Eval(Expression.LitralInst(invalid), Expression.EnvironmentInstance));

        using var native = Compile(expression);
        var expectedError = new PineVMParseCache().ParseInternalExpression(invalid).IsErrOrNull()!;
        Action execute = () => native.Evaluate(PineValueInProcess.Create(PineValue.List([Number(5)])));
        execute.Should().Throw<ParseExpressionException>().WithMessage(expectedError);
    }

    [Fact]
    public void Constructed_closures_with_arbitrary_data_captures_compile_without_generic_eval()
    {
        var target = Expression.EnvironmentInstance;
        var construction = ConstructCall(Expression.LitralInst(ExpressionEncoding.EncodeExpressionAsValue(target)));

        var environmentArgument =
            Expression.BuiltinInst(nameof(BuiltinFunction.length), Expression.EnvironmentInstance);

        var expression = new Expression.Eval(construction, environmentArgument);
        using var native = Compile(expression);
        var input = PineValue.List([Number(5), Number(9)]);
        native.Evaluate(PineValueInProcess.Create(input))!.Evaluate().Should().Be(PineValue.List([input]));
    }

    [Fact]
    public void Static_code_dependencies_cross_known_helpers_without_freezing_data_arguments()
    {
        var dynamicBody = new Expression.Eval(Path(0), Path(1));

        var middle =
            new Expression.Eval(
                Expression.LitralInst(ExpressionEncoding.EncodeExpressionAsValue(dynamicBody)),
                Expression.EnvironmentInstance);

        var code = ExpressionEncoding.EncodeExpressionAsValue(Expression.EnvironmentInstance);

        var root =
            new Expression.Eval(
                Expression.LitralInst(ExpressionEncoding.EncodeExpressionAsValue(middle)),
                Expression.ListInst([Expression.LitralInst(code), Path(0)]));

        var compilation =
            PineCSharpCompiler.Compile(root, options: new(EnableCancellation: false))
            .Extract(error => throw new Exception(error.Describe()));

        var helpers =
            compilation.Methods.Where(
                method => method.Function.Expression.Equals(middle) ||
                    method.Function.Expression.Equals(dynamicBody)).ToArray();

        helpers.Should().HaveCount(2);

        helpers.Should().OnlyContain(
            method => method.Function.EnvironmentConstraint!.TryGetValue(new[] { 0 })!.Equals(code) &&
                method.Function.EnvironmentConstraint.TryGetValue(new[] { 1 }) == null);

        using var native =
            PineCSharpAssembly.Load(compilation)
            .Extract(errors => throw new Exception(string.Join("\n", errors)));

        foreach (var value in new[] { Number(5), Number(9), PineValue.List([Number(3), Number(4)]) })
            native.Evaluate(PineValueInProcess.Create(PineValue.List([value])))!.Evaluate().Should().Be(value);
    }

    [Fact]
    public void Constant_dependencies_in_code_construction_are_instantiated_not_left_as_generic_eval()
    {
        var body =
            new Expression.Eval(
                Expression.BuiltinInst(
                    nameof(BuiltinFunction.concat),
                    Expression.ListInst([Text("Invalid expression: "), Path(0)])),
                Expression.EnvironmentInstance);

        var call =
            new Expression.Eval(
                Expression.LitralInst(ExpressionEncoding.EncodeExpressionAsValue(body)),
                Expression.ListInst([Text("message")]));

        var compilation =
            PineCSharpCompiler.Compile(call, options: new(EnableCancellation: false))
            .Extract(error => throw new Exception(error.Describe()));

        compilation.Methods.Should().Contain(
            method => method.Function.Expression.Equals(body) &&
                method.Function.EnvironmentConstraint!.TryGetValue(new[] { 0 })!.Equals(
                    StringEncoding.ValueFromString("message")));

        using var native =
            PineCSharpAssembly.Load(compilation)
            .Extract(errors => throw new Exception(string.Join("\n", errors)));

        Action execute = () => native.Evaluate(PineValueInProcess.EmptyList);
        execute.Should().Throw<ParseExpressionException>();
    }

    private static Expression ConstructCall(Expression code) =>
        Expression.ListInst(
            [
                Text("Eval"),
                Expression.ListInst([Text("Litral"), code]),
                Expression.ListInst(
                    [
                        Text("List"),
                        Expression.ListInst([Text("Litral"), Expression.EnvironmentInstance])
                    ])
            ]);

    private static PineCSharpAssembly Compile(Expression expression) =>
        PineCSharpAssembly.Load(
            PineCSharpCompiler.Compile(expression, options: new(EnableCancellation: false))
            .Extract(error => throw new Exception(error.Describe())))
        .Extract(errors => throw new Exception(string.Join("\n", errors)));

    private static PineValue Number(int value) => IntegerEncoding.EncodeSignedInteger(value);

    private static Expression Text(string text) => Expression.LitralInst(StringEncoding.ValueFromString(text));

    private static Expression Path(params int[] path) =>
        ExpressionBuilder.BuildExpressionForPathInExpression(path, Expression.EnvironmentInstance);
}
