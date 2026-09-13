using AwesomeAssertions;
using Pine.Core.CommonEncodings;
using Pine.Core.Interpreter.IntermediateVM;
using Pine.Core.Interpreter.IntermediateVM.Frontend;
using Pine.Core.Interpreter.IntermediateVM.Semantic;
using System;
using System.Collections.Immutable;
using System.Linq;
using Xunit;

namespace Pine.Core.Tests.Interpreter.IntermediateVM;

public class GraphValueAnalysisTests
{
    private static LiteralValue Blob(params ImmutableArray<byte> bytes) => new LiteralValue.Blob([.. bytes]);
    private static LiteralValue Integer(int value) => OwnedExpression.CaptureValue(IntegerEncoding.EncodeSignedInteger(value));
    private static Operation Literal(int id, LiteralValue value) => new Operation.Literal(new(new(id)), value);
    private static Operation List(int id, params ImmutableArray<int> items) =>
        new Operation.MakeList(new(new(id)), [.. items.Select(item => new PineVirtualValueId(item))]);
    private static Operation Project(int id, int source, params ImmutableArray<int> path) =>
        new Operation.Project(new(new(id)), new(source), new([.. path]));
    private static Operation Builtin(int id, string name, int argument) => new Operation.Builtin(new(new(id)), name, new(argument));
    private static BasicBlock Block(int id, ImmutableList<int> parameters, ImmutableList<Operation> operations, Terminator terminator) =>
        new(new(id), [.. parameters.Select(parameter => new ValueDefinition(new(parameter)))], operations, terminator);
    private static Edge Edge(int block, params ImmutableArray<int> arguments) =>
        new(new(block), [.. arguments.Select(argument => new PineVirtualValueId(argument))]);
    private static Terminator Return(int value) => new Terminator.Return([new(value)]);
    private static ValidatedFunctionGraph Graph(params ImmutableArray<BasicBlock> blocks) =>
        ValidatedFunctionGraph.ValidateGraph(new(new(0), FunctionSignature.Canonical, blocks[0].Id,
            blocks.ToImmutableDictionary(block => block.Id)), [])
            .Extract(errors => throw new Exception(string.Join(", ", errors)));
    private static GraphValueAnalysisResult Analyze(ValidatedFunctionGraph graph, GraphValueAnalysisOptions? options = null) =>
        GraphValueAnalysis.Analyze(graph, options ?? new());
    private static void Exact(GraphValueAnalysisResult result, int id, LiteralValue literal) =>
        result.ValueOf(new(id)).Should().Be(new GraphValueFact.Exact(literal));

    [Fact]
    public void GraphValue_partial_list_keeps_known_function_target_and_unknown_slots()
    {
        var function = OwnedExpression.CaptureValue(ExpressionEncoding.EncodeExpressionAsValue(Expression.EnvironmentInstance));
        var graph = Graph(Block(0, [0],
            [Literal(1, function), List(2, 0, 1), Project(3, 2, 1), Project(4, 2, 0),
                Project(5, 2, 2), Project(6, 2), List(7, 2), Project(8, 7, 0, 1)], Return(3)));
        var result = Analyze(graph);
        result.BudgetExhausted.Should().BeFalse();
        Exact(result, 3, function);
        Exact(result, 8, function);
        Exact(result, 5, new LiteralValue.List([]));
        result.ValueOf(new(4)).Should().Be(GraphValueFact.Any);
        result.ValueOf(new(6)).Should().Be(result.ValueOf(new(2)));
        result.ValueOf(new(2)).Should().BeOfType<GraphValueFact.List>().Which.Items[0].Should().Be(GraphValueFact.Any);
    }

    [Theory]
    [InlineData("head", 0)]
    [InlineData("head", 1)]
    [InlineData("skip", 0)]
    [InlineData("skip", 1)]
    [InlineData("skip", 2)]
    [InlineData("concat", 0)]
    [InlineData("concat", 1)]
    [InlineData("concat", 2)]
    [InlineData("concat", 3)]
    [InlineData("concat", 4)]
    [InlineData("int_add", 0)]
    [InlineData("int_add", 1)]
    [InlineData("equal", 0)]
    [InlineData("equal", 1)]
    [InlineData("equal", 2)]
    public void GraphValue_safe_builtin_facts_preserve_actual_bytes_and_sequence_kinds(string name, int variant)
    {
        var source = PineValue.Blob([7, 8, 9]);
        var list = PineValue.List([source, PineValue.EmptyList]);
        var input = (name, variant) switch
        {
            ("head", 0) => source,
            ("head", 1) => PineValue.EmptyBlob,
            ("skip", 0) => PineValue.List([PineValue.Blob([4, 0, 1]), source]),
            ("skip", 1) => PineValue.List([IntegerEncoding.EncodeSignedInteger(-2), source]),
            ("skip", 2) => PineValue.List([PineValue.Blob([2, 0]), list]),
            ("concat", 0) => PineValue.List([PineValue.EmptyList, source, PineValue.EmptyList, source]),
            ("concat", 1) => PineValue.List([list, source]),
            ("concat", 2) => PineValue.List([source, list]),
            ("concat", 3) => PineValue.List([list, PineValue.EmptyList, list]),
            ("concat", 4) => PineValue.List([PineValue.EmptyList, PineValue.EmptyBlob]),
            ("int_add", 0) => PineValue.List([PineValue.Blob([4, 0, 1]), PineValue.Blob([2, 0])]),
            ("int_add", 1) => PineValue.EmptyList,
            ("equal", 0) => PineValue.List([PineValue.Blob([4, 0, 1]), PineValue.Blob([4, 1])]),
            ("equal", 1) => PineValue.List([source, source]),
            ("equal", 2) => PineValue.Blob([7, 7]),
            _ => throw new NotImplementedException("GraphValue_safe_builtin_facts_preserve_actual_bytes_and_sequence_kinds: " + name),
        };
        var graph = Graph(Block(0, [0], [Literal(1, OwnedExpression.CaptureValue(input)), Builtin(2, name, 1)], Return(2)));
        var result = Analyze(graph);
        result.BudgetExhausted.Should().BeFalse();
        Exact(result, 2, OwnedExpression.CaptureValue(BuiltinFunction.ApplyFunctionGeneric(name, input)));
    }

    [Fact]
    public void GraphValue_generic_blob_head_differs_from_list_only_projection()
    {
        var result = Analyze(Graph(Block(0, [0],
            [Literal(1, Blob(7, 8)), Builtin(2, "head", 1), Project(3, 1, 0)], Return(3))));
        Exact(result, 2, Blob(7));
        Exact(result, 3, new LiteralValue.List([]));
    }

    [Theory]
    [InlineData(true)]
    [InlineData(false)]
    public void GraphValue_diamond_meets_every_edge_and_retains_only_agreeing_items(bool same)
    {
        var graph = Graph(
            Block(0, [0],
                [Literal(1, Blob(9)), Literal(2, Blob(same ? (byte)9 : (byte)8)),
                    List(3, 1, 0), List(4, 2, 0)],
                new Terminator.Branch(new(0), Blob(4), Edge(1, 3), Edge(1, 4))),
            Block(1, [10], [Project(11, 10, 0), Project(12, 10, 1)], Return(11)));
        var result = Analyze(graph);
        result.BudgetExhausted.Should().BeFalse();
        result.ValueOf(new(11)).Should().Be(same ? new GraphValueFact.Exact(Blob(9)) : GraphValueFact.Any);
        result.ValueOf(new(12)).Should().Be(GraphValueFact.Any);
    }

    [Fact]
    public void GraphValue_switch_includes_duplicate_predecessor_edges_and_default()
    {
        var result = Analyze(Graph(
            Block(0, [0], [Literal(1, Blob(9)), Literal(2, Blob(8))],
                new Terminator.Switch(new(0), [new(Blob(4), Edge(1, 1)), new(Blob(2), Edge(1, 1))], Edge(1, 2))),
            Block(1, [10], [], Return(10))));
        result.ValueOf(new(10)).Should().Be(GraphValueFact.Any);
    }

    [Fact]
    public void GraphValue_invoke_propagates_captures_but_not_return_slots()
    {
        var result = Analyze(Graph(
            Block(0, [0], [Literal(1, Blob(9)), List(2, 1, 0)],
                new Terminator.Invoke(new(new(0), new CallTarget.Dynamic(new(0)), FunctionSignature.Canonical, [new(0)]),
                    new(new(1), [new ContinuationBinding.CallerValue(new(2)), new ContinuationBinding.ReturnedResult(0)]))),
            Block(1, [10, 11], [Project(12, 10, 0)], Return(12))));
        Exact(result, 12, Blob(9));
        result.ValueOf(new(11)).Should().Be(GraphValueFact.Any);
    }

    [Fact]
    public void GraphValue_entry_backedge_cannot_specialize_the_unknown_external_argument()
    {
        var result = Analyze(Graph(
            Block(0, [0], [Literal(1, Blob(9))], new Terminator.Jump(Edge(0, 1)))));
        result.BudgetExhausted.Should().BeFalse();
        result.ValueOf(new(0)).Should().Be(GraphValueFact.Any);
    }

    [Fact]
    public void GraphValue_identity_backedge_stays_conservative_instead_of_seeding_an_invariant()
    {
        var result = Analyze(Graph(
            Block(0, [0], [Literal(1, Blob(9))], new Terminator.Jump(Edge(1, 1))),
            Block(1, [10], [], new Terminator.Jump(Edge(1, 10)))));
        result.BudgetExhausted.Should().BeFalse();
        result.ValueOf(new(10)).Should().Be(GraphValueFact.Any);
    }

    [Fact]
    public void GraphValue_irreducible_nested_cycles_and_recursive_shapes_converge_conservatively()
    {
        var graph = Graph(
            Block(0, [0], [], new Terminator.Branch(new(0), Blob(4), Edge(1, 0), Edge(2, 0))),
            Block(1, [10], [List(11, 10)], new Terminator.Branch(new(10), Blob(4), Edge(1, 11), Edge(2, 11))),
            Block(2, [20], [List(21, 20)], new Terminator.Branch(new(20), Blob(4), Edge(1, 21), Edge(3, 21))),
            Block(3, [30], [List(31, 30)], new Terminator.Branch(new(30), Blob(4), Edge(3, 31), Edge(2, 31))));
        var result = Analyze(graph, new(MaxDepth: 3));
        result.BudgetExhausted.Should().BeFalse();
        result.ValueOf(new(10)).Should().Be(GraphValueFact.Any);
        result.ValueOf(new(20)).Should().Be(GraphValueFact.Any);
        result.WorkUnits.Should().BeLessThan(10_000);
    }

    [Fact]
    public void GraphValue_shape_growth_is_bounded_even_without_an_unknown_external_loop_input()
    {
        var graph = Graph(
            Block(0, [0], [List(1, 0)], new Terminator.Jump(Edge(1, 1))),
            Block(1, [10], [List(11, 10)], new Terminator.Jump(Edge(1, 11))));
        var result = Analyze(graph, new(MaxDepth: 3));
        result.BudgetExhausted.Should().BeFalse();
        result.ValueOf(new(10)).Should().Be(new GraphValueFact.List([GraphValueFact.Any]));
    }

    [Fact]
    public void GraphValue_unreachable_components_publish_no_facts_and_unrelated_literals_do_not_leak()
    {
        var result = Analyze(Graph(
            Block(0, [0], [Literal(1, Blob(9))], new Terminator.Jump(Edge(1, 0))),
            Block(1, [10], [], Return(10)),
            Block(2, [20], [Literal(21, Blob(9))], new Terminator.Jump(Edge(2, 21)))));
        result.ValueOf(new(10)).Should().Be(GraphValueFact.Any);
        result.Facts.ContainsKey(new(20)).Should().BeFalse();
        result.Facts.ContainsKey(new(21)).Should().BeFalse();
    }

    [Theory]
    [InlineData(0)]
    [InlineData(1)]
    [InlineData(20)]
    [InlineData(200)]
    public void GraphValue_budget_exhaustion_publishes_nothing_and_is_deterministic(long budget)
    {
        var graph = Graph(
            Block(0, [0],
                [Literal(1, new LiteralValue.Blob([.. Enumerable.Repeat((byte)7, 1000)])),
                    Literal(2, new LiteralValue.Blob([.. Enumerable.Repeat((byte)7, 1000)]))],
                new Terminator.Branch(new(0), Blob(4), Edge(1, 1), Edge(1, 2))),
            Block(1, [10], [], Return(10)));
        var before = GraphRendering.Render(graph.Graph);
        var cold = Analyze(graph, new(budget));
        var warm = Analyze(graph, new(budget));
        cold.BudgetExhausted.Should().BeTrue();
        cold.Facts.Should().BeEmpty();
        cold.WorkUnits.Should().Be(budget);
        warm.WorkUnits.Should().Be(cold.WorkUnits);
        warm.Facts.Should().BeEmpty();
        GraphRendering.Render(graph.Graph).Should().Be(before);
    }

    [Fact]
    public void GraphValue_unknown_or_failing_primitives_and_unknown_equality_are_not_evaluated()
    {
        var result = Analyze(Graph(Block(0, [0],
            [List(1, 0, 0), Builtin(2, "equal", 1), Literal(3, Integer(-8)), Literal(4, Blob(1)),
                List(5, 3, 4), Builtin(6, "bit_shift_left", 5),
                Literal(7, OwnedExpression.CaptureValue(IntegerEncoding.EncodeSignedInteger((long)int.MaxValue + 1))),
                List(8, 7, 4), Builtin(9, "skip", 8)], Return(2))));
        result.BudgetExhausted.Should().BeFalse();
        result.ValueOf(new(2)).Should().Be(GraphValueFact.Any);
        result.ValueOf(new(6)).Should().Be(GraphValueFact.Any);
        result.ValueOf(new(9)).Should().Be(GraphValueFact.Any);
    }

    [Fact]
    public void GraphValue_real_frontend_head_skip_guard_and_partial_concat_reach_a_known_target()
    {
        var function = ExpressionEncoding.EncodeExpressionAsValue(Expression.EnvironmentInstance);
        var expression = new Expression.Builtin("head", new Expression.Builtin("skip",
            new Expression.List([
                new Expression.Litral(PineValue.Blob([4, 0, 1])),
                new Expression.List([Expression.EnvironmentInstance, new Expression.Litral(function)])])));
        var compiled = ExpressionGraphOptimizer.Compile(
            CompilationRequest.Capture(expression, new(DisableReduction: true)), new(Enabled: false), CompilerMemo.Empty)
            .Extract(errors => throw new Exception(string.Join(", ", errors)));
        var cold = Analyze(compiled.Graph);
        var warm = Analyze(compiled.Graph);
        cold.BudgetExhausted.Should().BeFalse();
        var returned = compiled.Graph.Graph.Blocks.Values.Select(block => block.Terminator).OfType<Terminator.Return>().Single().Values.Single();
        cold.ValueOf(returned).Should().Be(new GraphValueFact.Exact(OwnedExpression.CaptureValue(function)));
        warm.WorkUnits.Should().Be(cold.WorkUnits);
        warm.Facts.Should().Equal(cold.Facts);
    }
}
