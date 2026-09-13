using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
using Pine.Core.Interpreter;
using Pine.Core.Interpreter.IntermediateVM;
using Pine.Core.Interpreter.IntermediateVM.Backend;
using Pine.Core.Interpreter.IntermediateVM.Frontend;
using Pine.Core.Interpreter.IntermediateVM.Semantic;
using System;
using System.Collections.Immutable;
using System.Linq;
using Xunit;
using Semantic = Pine.Core.Interpreter.IntermediateVM.Semantic;
using VM = Pine.Core.Interpreter.IntermediateVM.PineVM;

namespace Pine.Core.Tests.Interpreter.IntermediateVM;

public class ProjectedGraphBoundaryTests
{
    [Theory]
    [InlineData(false, false)]
    [InlineData(false, true)]
    [InlineData(true, false)]
    [InlineData(true, true)]
    public void Legacy_callers_and_root_entries_share_exact_projected_graph_local_layout(
        bool legacyCaller, bool nested)
    {
        var first = nested ? Project(Project(Expression.EnvironmentInstance, 2), 1) : Project(Expression.EnvironmentInstance, 2);
        var second = Project(Expression.EnvironmentInstance, 0);
        var callee = new Expression.Conditional(
            new Expression.Builtin("equal", new Expression.List([first, second])),
            new Expression.List([first, second, first]),
            new Expression.List([second, first]));
        var root = legacyCaller
            ? (Expression)new Expression.List([
                new Expression.Eval(Literal(ExpressionEncoding.EncodeExpressionAsValue(callee)), Expression.EnvironmentInstance),
                Literal(IntegerEncoding.EncodeSignedInteger(7))])
            : callee;
        var inputs = ImmutableArray.Create(
            PineValue.List([IntegerEncoding.EncodeSignedInteger(3), PineValue.EmptyList,
                nested ? PineValue.List([PineValue.EmptyList, IntegerEncoding.EncodeSignedInteger(9)]) : IntegerEncoding.EncodeSignedInteger(9)]),
            PineValue.List([IntegerEncoding.EncodeSignedInteger(3), PineValue.EmptyList,
                nested ? PineValue.List([PineValue.EmptyList, IntegerEncoding.EncodeSignedInteger(3)]) : IntegerEncoding.EncodeSignedInteger(3)]),
            PineValue.EmptyList);
        var vm = Create(callee);
        foreach (var input in inputs)
        {
            var expected = new DirectInterpreter(new PineVMParseCache(), evalCache: null)
                .EvaluateExpressionDefault(root, input);
            var report = vm.EvaluateExpressionOnCustomStack(root, input, new(100, 100, 10))
                .Extract(error => throw new InvalidOperationException(error.ToString()));
            report.ReturnValue.Evaluate().Should().Be(expected);
            report.InvocationCount.Should().Be(legacyCaller ? 1 : 0);
        }
    }

    [Fact]
    public void Projected_frontend_rejects_unavailable_environment_instead_of_reconstructing_it()
    {
        var signature = new FunctionSignature([new(new([1]))], [Semantic.ValueType.PineValue]);
        var compile = () => ExpressionGraphCompiler.Compile(Expression.EnvironmentInstance, signature);
        compile.Should().Throw<ArgumentException>();
    }

    [Fact]
    public void Projected_frontend_preserves_explicit_order_and_zero_parameter_interfaces()
    {
        foreach (var signature in ImmutableArray.Create(
            new FunctionSignature([new(new([2])), new(new([0]))], [Semantic.ValueType.PineValue]),
            new FunctionSignature([], [Semantic.ValueType.PineValue])))
        {
            var body = signature.Parameters.Count == 0
                ? Literal(IntegerEncoding.EncodeSignedInteger(42))
                : (Expression)new Expression.List([Project(Expression.EnvironmentInstance, 0), Project(Expression.EnvironmentInstance, 2)]);
            var instructions = CompileGraph(body, signature).Generic;
            instructions.Parameters.ParamsPaths.Select(path => string.Join(",", path))
                .Should().Equal(signature.Parameters.Select(parameter => string.Join(",", parameter.Path.Indices)));
            var input = PineValue.List([IntegerEncoding.EncodeSignedInteger(4), PineValue.EmptyList, IntegerEncoding.EncodeSignedInteger(8)]);
            Create(body, signature).EvaluateExpressionOnCustomStack(body, input, new(0, 100, 1))
                .Extract(error => throw new InvalidOperationException(error.ToString()))
                .ReturnValue.Evaluate().Should().Be(
                    new DirectInterpreter(new PineVMParseCache(), evalCache: null).EvaluateExpressionDefault(body, input));
        }
    }

    private static ExpressionCompilation CompileGraph(Expression expression, FunctionSignature? signature = null)
    {
        var parameters = StaticFunctionInterface.FromExpression(expression);
        var graph = ExpressionGraphCompiler.Compile(expression, signature ?? new(
            [.. parameters.ParamsPaths.Select(path => new FunctionParameter(new([.. path])))],
            [Semantic.ValueType.PineValue]));
        var validated = ValidatedFunctionGraph.ValidateGraph(graph, ImmutableDictionary<FunctionId, FunctionSignature>.Empty)
            .Extract(errors => throw new InvalidOperationException(string.Join(", ", errors)));
        var artifact = GraphCompiler.Compile(validated, fuseScalarBuiltins: true, legacyParameterLocals: true, compact: true)
            .Extract(error => throw new InvalidOperationException(error.ToString()));
        return new(GraphVMAdapter.ToStackFrame(artifact), []);
    }

    private static VM Create(Expression callee, FunctionSignature? signature = null) =>
        VM.CreateCustom(
            evalCache: null, evaluationConfigDefault: null, reportFunctionApplication: null,
            compilationEnvClasses: null, disableReductionInCompilation: true, selectPrecompiled: null,
            skipInlineForExpression: _ => true, enableTailRecursionOptimization: false, parseCache: null,
            precompiledLeaves: null, reportEnterPrecompiledLeaf: null, reportExitPrecompiledLeaf: null,
            optimizationParametersSerial: null, cacheFileStore: null,
            disableDirectContinueForSimpleEval: true, disableDirectEvalForSimpleTemplate: true,
            compileExpression: expression => expression == callee
                ? CompileGraph(expression, signature)
                : ExpressionCompilation.CompileExpression(expression, [], new(), true, false, (_, _) => true));

    private static Expression Literal(PineValue value) => new Expression.Litral(value);

    private static Expression Project(Expression source, int index) =>
        new Expression.Builtin("head", new Expression.Builtin("skip", new Expression.List([
            Literal(IntegerEncoding.EncodeSignedInteger(index)), source])));
}
