using AwesomeAssertions;
using Pine.Core.Interpreter.IntermediateVM;
using Pine.Core.Interpreter.IntermediateVM.Semantic;
using System;
using System.Collections.Immutable;
using System.Linq;
using System.Reflection;
using Xunit;
using ValueType = Pine.Core.Interpreter.IntermediateVM.Semantic.ValueType;

namespace Pine.Core.Tests.Interpreter.IntermediateVM;

public class SemanticGraphValidationTests
{
    private static ImmutableDictionary<FunctionId, FunctionSignature> Signatures =>
        ImmutableDictionary<FunctionId, FunctionSignature>.Empty;

    private static LiteralValue Empty => new LiteralValue.List([]);

    private static FunctionGraph Graph() =>
        new(new(7), FunctionSignature.Canonical, new(90),
            ImmutableDictionary<PineBlockId, BasicBlock>.Empty
            .Add(new(90), new(new(90), [new(new(1))],
                [
                    new Operation.Literal(new(new(2)), new LiteralValue.Blob([255])),
                    new Operation.Project(new(new(3)), new(1), new([0, 2])),
                    new Operation.MakeList(new(new(4)), [new(2), new(3)]),
                    new Operation.Builtin(new(new(5)), "equal", new(4)),
                ],
                new Terminator.Return([new(5)])))
            .Add(new(10), new(new(10), [new(new(10))], [], new Terminator.Return([new(10)]))));

    private static FunctionGraph Replace(FunctionGraph graph, BasicBlock block) =>
        new(graph.Id, graph.Signature, graph.Entry, graph.Blocks.Remove(block.Id).Add(block.Id, block));

    private static FunctionGraph WithTerminator(Terminator terminator) =>
        Replace(Graph(), Graph().Blocks[new(90)] with { Terminator = terminator });

    private static Call DynamicCall() =>
        new(new(8), new CallTarget.Dynamic(new(2)), FunctionSignature.Canonical, [new(1)]);

    private static FunctionGraph InvokeGraph(Call? call = null, InvokeContinuation? continuation = null) =>
        WithTerminator(new Terminator.Invoke(
            call ?? DynamicCall(),
            continuation ?? new(new(10), [new ContinuationBinding.ReturnedResult(0)])));

    private static GraphLocation At(
        int? block = 90,
        int? operation = null,
        int? edge = null,
        int? value = null,
        string member = "") =>
        new(new(7), block is { } b ? new PineBlockId(b) : null,
            operation, edge, value is { } v ? new PineVirtualValueId(v) : null, member);

    private static ImmutableList<GraphDiagnostic> Errors(
        FunctionGraph graph,
        ImmutableDictionary<FunctionId, FunctionSignature>? signatures = null) =>
        ValidatedFunctionGraph.ValidateGraph(graph, signatures ?? Signatures)
        .Should().BeOfType<Result<ImmutableList<GraphDiagnostic>, ValidatedFunctionGraph>.Err>().Subject.Value;

    private static ValidatedFunctionGraph Success(
        FunctionGraph graph,
        ImmutableDictionary<FunctionId, FunctionSignature>? signatures = null) =>
        ValidatedFunctionGraph.ValidateGraph(graph, signatures ?? Signatures)
        .Should().BeOfType<Result<ImmutableList<GraphDiagnostic>, ValidatedFunctionGraph>.Ok>().Subject.Value;

    [Fact]
    public void Success_retains_exact_input_identity_and_cannot_be_constructed_or_copied_as_a_record()
    {
        var graph = Graph();
        var table = Signatures.Add(new(12), FunctionSignature.Canonical);
        var rendering = GraphRendering.Render(graph);
        var hash = graph.GetHashCode();
        var result = Success(graph, table);

        result.Graph.Should().BeSameAs(graph);
        result.KnownFunctionSignatures.Should().BeSameAs(table);
        GraphRendering.Render(graph).Should().Be(rendering);
        graph.GetHashCode().Should().Be(hash);
        typeof(ValidatedFunctionGraph).IsSealed.Should().BeTrue();
        typeof(ValidatedFunctionGraph).GetConstructors().Should().BeEmpty();
        typeof(ValidatedFunctionGraph).GetProperties().Should().OnlyContain(property => property.SetMethod == null);
        typeof(ValidatedFunctionGraph).GetMethod("<Clone>$", BindingFlags.Public | BindingFlags.Instance).Should().BeNull();
        graph.Signature.Should().BeSameAs(FunctionSignature.Canonical);
    }

    [Theory]
    [InlineData("entry", GraphDiagnosticCode.MissingEntry, 99, null, null, null, "entry")]
    [InlineData("id", GraphDiagnosticCode.BlockIdMismatch, 90, null, null, null, "")]
    [InlineData("parameter-duplicate", GraphDiagnosticCode.DuplicateDefinition, 90, null, null, 1, "parameters[0]")]
    [InlineData("result-duplicate", GraphDiagnosticCode.DuplicateDefinition, 90, 0, null, 1, "result")]
    [InlineData("result-global-duplicate", GraphDiagnosticCode.DuplicateDefinition, 90, 0, null, 10, "result")]
    [InlineData("result-result-duplicate", GraphDiagnosticCode.DuplicateDefinition, 90, 1, null, 2, "result")]
    [InlineData("list-use", GraphDiagnosticCode.UndefinedValue, 90, 2, null, 99, "items[1]")]
    [InlineData("project-use", GraphDiagnosticCode.UndefinedValue, 90, 1, null, 99, "source")]
    [InlineData("builtin-use", GraphDiagnosticCode.UndefinedValue, 90, 3, null, 99, "argument")]
    [InlineData("self-use", GraphDiagnosticCode.UndefinedValue, 90, 1, null, 3, "source")]
    [InlineData("forward-use", GraphDiagnosticCode.UndefinedValue, 90, 1, null, 5, "source")]
    [InlineData("cross-block-use", GraphDiagnosticCode.UndefinedValue, 90, 1, null, 10, "source")]
    [InlineData("path", GraphDiagnosticCode.InvalidPath, 90, 1, null, null, "path")]
    [InlineData("builtin-name", GraphDiagnosticCode.InvalidBuiltin, 90, 3, null, null, "name")]
    [InlineData("return-use", GraphDiagnosticCode.UndefinedValue, 90, null, null, 99, "return.values[0]")]
    [InlineData("return-arity", GraphDiagnosticCode.ArityMismatch, 90, null, null, null, "return.values")]
    [InlineData("entry-arity", GraphDiagnosticCode.ArityMismatch, 90, null, null, null, "parameters")]
    [InlineData("jump-use", GraphDiagnosticCode.UndefinedValue, 90, null, 0, 99, "arguments[0]")]
    [InlineData("jump-arity", GraphDiagnosticCode.ArityMismatch, 90, null, 0, null, "arguments")]
    [InlineData("jump-target", GraphDiagnosticCode.MissingTarget, 90, null, 0, null, "")]
    [InlineData("branch-test", GraphDiagnosticCode.UndefinedValue, 90, null, null, 99, "testedValue")]
    [InlineData("branch-equal", GraphDiagnosticCode.UndefinedValue, 90, null, 0, 99, "arguments[0]")]
    [InlineData("branch-not-equal", GraphDiagnosticCode.MissingTarget, 90, null, 1, null, "")]
    [InlineData("switch-selector", GraphDiagnosticCode.UndefinedValue, 90, null, null, 99, "selector")]
    [InlineData("switch-case", GraphDiagnosticCode.MissingTarget, 90, null, 0, null, "")]
    [InlineData("switch-default", GraphDiagnosticCode.MissingTarget, 90, null, 1, null, "default")]
    [InlineData("switch-duplicate", GraphDiagnosticCode.DuplicateCase, 90, null, 1, null, "literal")]
    [InlineData("call-target-use", GraphDiagnosticCode.UndefinedValue, 90, null, null, 99, "call.target")]
    [InlineData("call-argument-use", GraphDiagnosticCode.UndefinedValue, 90, null, null, 99, "call.arguments[0]")]
    [InlineData("call-arity", GraphDiagnosticCode.ArityMismatch, 90, null, null, null, "call.arguments")]
    [InlineData("call-dynamic-signature", GraphDiagnosticCode.SignatureMismatch, 90, null, null, null, "call.signature")]
    [InlineData("call-unknown", GraphDiagnosticCode.UnknownFunction, 90, null, null, null, "call.target")]
    [InlineData("call-recursive-signature", GraphDiagnosticCode.SignatureMismatch, 90, null, null, null, "call.signature")]
    [InlineData("call-duplicate", GraphDiagnosticCode.DuplicateCallSite, 90, null, null, null, "call.site")]
    [InlineData("continuation-target", GraphDiagnosticCode.MissingTarget, 90, null, 0, null, "")]
    [InlineData("continuation-arity", GraphDiagnosticCode.ArityMismatch, 90, null, 0, null, "bindings")]
    [InlineData("continuation-caller", GraphDiagnosticCode.UndefinedValue, 90, null, 0, 10, "bindings[0]")]
    [InlineData("continuation-slot-negative", GraphDiagnosticCode.InvalidReturnedResult, 90, null, 0, null, "bindings[0]")]
    [InlineData("continuation-slot-upper", GraphDiagnosticCode.InvalidReturnedResult, 90, null, 0, null, "bindings[0]")]
    [InlineData("unreachable", GraphDiagnosticCode.UndefinedValue, 10, null, null, 1, "return.values[0]")]
    public void Invalid_invariants_have_exact_codes_and_locations(
        string scenario, GraphDiagnosticCode code, int block, int? operation, int? edge, int? value, string member)
    {
        var graph = InvalidGraph(scenario);
        var rendering = GraphRendering.Render(graph);
        var error = Errors(graph).Should().ContainSingle().Subject;

        error.Code.Should().Be(code);
        error.Location.Should().Be(At(block, operation, edge, value, member));
        error.Reason.Should().NotBeNullOrWhiteSpace();
        GraphRendering.Render(graph).Should().Be(rendering);
    }

    private static FunctionGraph InvalidGraph(string scenario)
    {
        var graph = Graph();
        var block = graph.Blocks[new(90)];
        var validEdge = new Edge(new(10), [new(1)]);
        var projectedSignature = new FunctionSignature([new(new([0]))], [ValueType.PineValue]);

        FunctionGraph Op(int index, Operation operation) =>
            Replace(graph, block with { Operations = block.Operations.SetItem(index, operation) });

        return scenario switch
        {
            "entry" => new(graph.Id, graph.Signature, new(99), graph.Blocks),
            "id" => new(graph.Id, graph.Signature, graph.Entry,
                graph.Blocks.SetItem(new(90), block with { Id = new(91) })),
            "parameter-duplicate" => Replace(graph, graph.Blocks[new(10)] with
            { Parameters = [new(new(1))], Terminator = new Terminator.Return([new(1)]) }),
            "result-duplicate" => Replace(graph, block with
            {
                Operations = block.Operations.SetItem(0, new Operation.Literal(new(new(1)), Empty))
                .SetItem(2, new Operation.MakeList(new(new(4)), [new(1), new(3)])),
            }),
            "result-global-duplicate" => Replace(graph, block with
            {
                Operations = block.Operations.SetItem(0, new Operation.Literal(new(new(10)), Empty))
                .SetItem(2, new Operation.MakeList(new(new(4)), [new(10), new(3)])),
            }),
            "result-result-duplicate" => Replace(graph, block with
            {
                Operations = block.Operations.SetItem(1, new Operation.Project(new(new(2)), new(1), new([])))
                .SetItem(2, new Operation.MakeList(new(new(4)), [new(2), new(2)])),
            }),
            "list-use" => Op(2, new Operation.MakeList(new(new(4)), [new(2), new(99)])),
            "project-use" => Op(1, new Operation.Project(new(new(3)), new(99), new([]))),
            "builtin-use" => Op(3, new Operation.Builtin(new(new(5)), "equal", new(99))),
            "self-use" => Op(1, new Operation.Project(new(new(3)), new(3), new([]))),
            "forward-use" => Op(1, new Operation.Project(new(new(3)), new(5), new([]))),
            "cross-block-use" => Op(1, new Operation.Project(new(new(3)), new(10), new([]))),
            "path" => Op(1, new Operation.Project(new(new(3)), new(1), new([-1]))),
            "builtin-name" => Op(3, new Operation.Builtin(new(new(5)), "Equal", new(4))),
            "return-use" => WithTerminator(new Terminator.Return([new(99)])),
            "return-arity" => WithTerminator(new Terminator.Return([])),
            "entry-arity" => Replace(graph, block with { Parameters = [new(new(1)), new(new(6))] }),
            "jump-use" => WithTerminator(new Terminator.Jump(new(new(10), [new(99)]))),
            "jump-arity" => WithTerminator(new Terminator.Jump(new(new(10), []))),
            "jump-target" => WithTerminator(new Terminator.Jump(new(new(99), [new(1)]))),
            "branch-test" => WithTerminator(new Terminator.Branch(new(99), Empty, validEdge, validEdge)),
            "branch-equal" => WithTerminator(new Terminator.Branch(new(1), Empty, new(new(10), [new(99)]), validEdge)),
            "branch-not-equal" => WithTerminator(new Terminator.Branch(new(1), Empty, validEdge, new(new(99), [new(1)]))),
            "switch-selector" => WithTerminator(new Terminator.Switch(new(99), [new(Empty, validEdge)], validEdge)),
            "switch-case" => WithTerminator(new Terminator.Switch(new(1), [new(Empty, new(new(99), [new(1)]))], validEdge)),
            "switch-default" => WithTerminator(new Terminator.Switch(new(1), [new(Empty, validEdge)], new(new(99), [new(1)]))),
            "switch-duplicate" => WithTerminator(new Terminator.Switch(new(1),
                [new(new LiteralValue.List([Empty]), validEdge), new(new LiteralValue.List([Empty]), validEdge)], validEdge)),
            "call-target-use" => InvokeGraph(DynamicCall() with { Target = new CallTarget.Dynamic(new(99)) }),
            "call-argument-use" => InvokeGraph(DynamicCall() with { Arguments = [new(99)] }),
            "call-arity" => InvokeGraph(DynamicCall() with { Arguments = [] }),
            "call-dynamic-signature" => InvokeGraph(DynamicCall() with { Signature = projectedSignature }),
            "call-unknown" => InvokeGraph(DynamicCall() with { Target = new CallTarget.Known(new(99)) }),
            "call-recursive-signature" => InvokeGraph(DynamicCall() with { Target = new CallTarget.Known(new(7)), Signature = projectedSignature }),
            "call-duplicate" => Replace(InvokeGraph(), graph.Blocks[new(10)] with
            {
                Terminator = new Terminator.TailInvoke(DynamicCall() with
                { Target = new CallTarget.Known(new(7)), Arguments = [new(10)] }),
            }),
            "continuation-target" => InvokeGraph(continuation: new(new(99), [new ContinuationBinding.ReturnedResult(0)])),
            "continuation-arity" => InvokeGraph(continuation: new(new(10), [])),
            "continuation-caller" => InvokeGraph(continuation: new(new(10), [new ContinuationBinding.CallerValue(new(10))])),
            "continuation-slot-negative" => InvokeGraph(continuation: new(new(10), [new ContinuationBinding.ReturnedResult(-1)])),
            "continuation-slot-upper" => InvokeGraph(continuation: new(new(10), [new ContinuationBinding.ReturnedResult(1)])),
            "unreachable" => Replace(graph, graph.Blocks[new(10)] with { Terminator = new Terminator.Return([new(1)]) }),
            _ => throw new ArgumentOutOfRangeException(nameof(scenario)),
        };
    }

    [Theory]
    [InlineData(false, false)]
    [InlineData(false, true)]
    [InlineData(true, false)]
    [InlineData(true, true)]
    public void Dynamic_and_recursive_calls_allow_invoke_and_tail_forms(bool known, bool tail)
    {
        var call = DynamicCall() with { Target = known ? new CallTarget.Known(new(7)) : new CallTarget.Dynamic(new(2)) };
        var graph = tail ? WithTerminator(new Terminator.TailInvoke(call)) : InvokeGraph(call);
        Success(graph);
        Success(graph, Signatures.Add(graph.Id, graph.Signature));
    }

    [Fact]
    public void Known_projected_multi_result_calls_bind_repeated_reordered_slots_and_caller_values()
    {
        var signature = new FunctionSignature([new(new([1])), new(new([0, 2]))], [ValueType.PineValue, ValueType.PineValue]);
        var call = new Call(new(8), new CallTarget.Known(new(20)), signature, [new(3), new(1)]);
        var graph = Replace(
            InvokeGraph(call, new(new(10),
                [new ContinuationBinding.ReturnedResult(1), new ContinuationBinding.CallerValue(new(5)),
                 new ContinuationBinding.ReturnedResult(0), new ContinuationBinding.ReturnedResult(1)])),
            new(new(10), [new(new(10)), new(new(11)), new(new(12)), new(new(13))], [],
                new Terminator.Return([new(13)])));
        Success(graph, Signatures.Add(new(20), signature));
    }

    [Fact]
    public void Known_and_root_table_signatures_must_match_including_projection_paths()
    {
        var signature = new FunctionSignature([new(new([0]))], [ValueType.PineValue]);
        var knownGraph = InvokeGraph(DynamicCall() with { Target = new CallTarget.Known(new(20)) });
        var error = Errors(knownGraph, Signatures.Add(new(20), signature)).Should().ContainSingle().Subject;
        error.Code.Should().Be(GraphDiagnosticCode.SignatureMismatch);
        error.Location.Should().Be(At(member: "call.signature"));

        var rootError = Errors(Graph(), Signatures.Add(new(7), signature)).Should().ContainSingle().Subject;
        rootError.Code.Should().Be(GraphDiagnosticCode.SignatureMismatch);
        rootError.Location.Should().Be(At(block: null, member: "signature"));
    }

    [Fact]
    public void Tail_call_result_signature_must_match_enclosing_return_vector()
    {
        var signature = new FunctionSignature([new(new([]))], []);
        var graph = WithTerminator(new Terminator.TailInvoke(
            DynamicCall() with { Target = new CallTarget.Known(new(20)), Signature = signature }));
        var error = Errors(graph, Signatures.Add(new(20), signature)).Should().ContainSingle().Subject;
        error.Code.Should().Be(GraphDiagnosticCode.SignatureMismatch);
        error.Location.Should().Be(At(member: "call.signature.results"));
    }

    [Theory]
    [InlineData("jump")]
    [InlineData("branch")]
    [InlineData("switch")]
    [InlineData("empty-switch")]
    public void Ordinary_transfers_allow_repeated_values_and_shared_targets(string kind)
    {
        var first = new Edge(new(10), [new(3)]);
        var second = new Edge(new(10), [new(2)]);
        var terminator = kind switch
        {
            "jump" => (Terminator)new Terminator.Jump(first),
            "branch" => new Terminator.Branch(new(5), new LiteralValue.Blob([4]), first, second),
            "switch" => new Terminator.Switch(new(1),
                [new(Empty, first), new(new LiteralValue.Blob([]), second)], first),
            "empty-switch" => new Terminator.Switch(new(1), [], first),
            _ => throw new ArgumentOutOfRangeException(nameof(kind)),
        };
        Success(WithTerminator(terminator));
    }

    [Fact]
    public void Sparse_reordered_nested_irreducible_and_unreachable_cycles_need_no_reachable_return()
    {
        var blocks = ImmutableDictionary<PineBlockId, BasicBlock>.Empty
            .Add(new(900), new(new(900), [new(new(1))], [],
                new Terminator.Branch(new(1), Empty, new(new(20), [new(1)]), new(new(-7), [new(1)]))))
            .Add(new(20), new(new(20), [new(new(2))], [],
                new Terminator.Branch(new(2), Empty, new(new(-7), [new(2)]), new(new(35), [new(2)]))))
            .Add(new(-7), new(new(-7), [new(new(3))], [], new Terminator.Jump(new(new(20), [new(3)]))))
            .Add(new(35), new(new(35), [new(new(4))], [],
                new Terminator.Branch(new(4), Empty, new(new(35), [new(4)]), new(new(20), [new(4)]))))
            .Add(new(4000), new(new(4000), [new(new(5)), new(new(6))], [],
                new Terminator.Jump(new(new(4000), [new(6), new(5)]))));
        var graph = new FunctionGraph(new(7), FunctionSignature.Canonical, new(900), blocks);
        Success(graph);
        Success(new(graph.Id, graph.Signature, graph.Entry, blocks.Reverse().ToImmutableDictionary()));
    }

    [Fact]
    public void All_builtin_names_accept_arbitrary_Pine_data_without_evaluating_it()
    {
        var names = new[]
        {
            "equal", "length", "head", "skip", "take", "concat", "reverse", "negate",
            "int_add", "int_mul", "int_is_sorted_asc", "bit_and", "bit_or", "bit_xor",
            "bit_not", "bit_shift_left", "bit_shift_right",
        };
        foreach (var name in names)
        {
            var graph = Graph();
            Success(Replace(graph, graph.Blocks[new(90)] with
            {
                Operations = graph.Blocks[new(90)].Operations.SetItem(3,
                    new Operation.Builtin(new(new(5)), name, new(2))),
            }));
        }
    }

    [Fact]
    public void Aggregated_diagnostics_are_deterministic_independent_of_map_order_and_leave_inputs_unchanged()
    {
        var graph = InvalidGraph("list-use");
        var invalid = Replace(graph, graph.Blocks[new(10)] with { Terminator = new Terminator.Jump(new(new(99), [new(100)])) });
        var table = Signatures
            .Add(new(44), new([new(new([-1]))], []))
            .Add(new(2), new([], [(ValueType)99]));
        var reversedGraph = new FunctionGraph(invalid.Id, invalid.Signature, invalid.Entry,
            invalid.Blocks.Reverse().ToImmutableDictionary());
        var first = Errors(invalid, table);
        var second = Errors(reversedGraph, table.Reverse().ToImmutableDictionary());

        first.Should().Equal(second);
        first.Should().Equal(
            new[]
            {
                new GraphDiagnostic(GraphDiagnosticCode.InvalidType, new(new(2), Member: "knownFunctions.signature.results[0]"), "Unsupported semantic value type."),
                new GraphDiagnostic(GraphDiagnosticCode.InvalidPath, new(new(44), Member: "knownFunctions.signature.parameters[0].path"), "Projection indices must be nonnegative."),
                new GraphDiagnostic(GraphDiagnosticCode.MissingTarget, At(10, edge: 0), "Edge target block does not exist."),
                new GraphDiagnostic(GraphDiagnosticCode.UndefinedValue, At(10, edge: 0, value: 100, member: "arguments[0]"), "Value is not defined earlier in this block."),
                new GraphDiagnostic(GraphDiagnosticCode.UndefinedValue, At(operation: 2, value: 99, member: "items[1]"), "Value is not defined earlier in this block."),
            });
        table.Count.Should().Be(2);
        table[new(44)].Parameters[0].Path.Indices[0].Should().Be(-1);
        ((Operation.MakeList)invalid.Blocks[new(90)].Operations[2]).Items[1].Should().Be(new PineVirtualValueId(99));
    }

    [Theory]
    [InlineData("signature", null, null, null, "signature")]
    [InlineData("signature-parameters", null, null, null, "signature.parameters")]
    [InlineData("signature-results", null, null, null, "signature.results")]
    [InlineData("signature-parameter", null, null, null, "signature.parameters[0]")]
    [InlineData("signature-path", null, null, null, "signature.parameters[0].path")]
    [InlineData("signature-path-indices", null, null, null, "signature.parameters[0].path")]
    [InlineData("blocks", null, null, null, "blocks")]
    [InlineData("block", 90, null, null, "")]
    [InlineData("parameters", 90, null, null, "parameters")]
    [InlineData("parameter", 90, null, null, "parameters[0]")]
    [InlineData("operations", 90, null, null, "operations")]
    [InlineData("operation", 90, 0, null, "")]
    [InlineData("result", 90, 0, null, "result")]
    [InlineData("literal", 90, 0, null, "literal")]
    [InlineData("blob", 90, 0, null, "literal.bytes")]
    [InlineData("list", 90, 0, null, "literal.items")]
    [InlineData("list-item", 90, 0, null, "literal.items[0]")]
    [InlineData("make-list", 90, 0, null, "items")]
    [InlineData("project-path", 90, 0, null, "path")]
    [InlineData("project-indices", 90, 0, null, "path")]
    [InlineData("terminator", 90, null, null, "terminator")]
    [InlineData("return", 90, null, null, "return.values")]
    [InlineData("jump", 90, null, 0, "")]
    [InlineData("edge-arguments", 90, null, 0, "arguments")]
    [InlineData("branch-literal", 90, null, null, "literal")]
    [InlineData("branch-equal", 90, null, 0, "")]
    [InlineData("branch-not-equal", 90, null, 1, "")]
    [InlineData("switch-cases", 90, null, null, "cases")]
    [InlineData("switch-case", 90, null, 0, "")]
    [InlineData("switch-literal", 90, null, 0, "literal")]
    [InlineData("switch-edge", 90, null, 0, "")]
    [InlineData("switch-default", 90, null, 0, "default")]
    [InlineData("call", 90, null, null, "call")]
    [InlineData("tail-call", 90, null, null, "call")]
    [InlineData("call-target", 90, null, null, "call.target")]
    [InlineData("call-signature", 90, null, null, "call.signature")]
    [InlineData("call-parameters", 90, null, null, "call.signature.parameters")]
    [InlineData("call-results", 90, null, null, "call.signature.results")]
    [InlineData("call-arguments", 90, null, null, "call.arguments")]
    [InlineData("continuation", 90, null, 0, "")]
    [InlineData("bindings", 90, null, 0, "bindings")]
    [InlineData("binding", 90, null, 0, "bindings[0]")]
    public void Null_structural_fields_produce_diagnostics_not_exceptions(
        string scenario, int? blockId, int? operation, int? edge, string member)
    {
        var error = Errors(NullGraph(scenario)).Should().ContainSingle().Subject;
        error.Code.Should().Be(GraphDiagnosticCode.MissingData);
        error.Location.Should().Be(At(blockId, operation, edge, member: member));
    }

    private static FunctionGraph NullGraph(string scenario)
    {
        var signature = new FunctionSignature([new(new([]))], []);
        var block = new BasicBlock(new(90), [new(new(1))],
            [new Operation.Literal(new(new(2)), Empty)], new Terminator.Return([]));
        var graph = new FunctionGraph(new(7), signature, new(90),
            ImmutableDictionary<PineBlockId, BasicBlock>.Empty.Add(block.Id, block));
        var edge = new Edge(new(90), [new(1)]);
        var call = DynamicCall();
        var continuation = new InvokeContinuation(new(90), [new ContinuationBinding.ReturnedResult(0)]);

        FunctionGraph Op(Operation operation) => Replace(graph, block with { Operations = [operation] });
        FunctionGraph End(Terminator terminator) => Replace(graph, block with { Terminator = terminator });
        FunctionGraph Contract(FunctionSignature contract) => new(graph.Id, contract, graph.Entry, graph.Blocks);
        FunctionGraph Invoke(Call contract) => End(new Terminator.Invoke(contract, continuation));

        return scenario switch
        {
            "signature" => Contract(null!),
            "signature-parameters" => Contract(signature with { Parameters = null! }),
            "signature-results" => Contract(signature with { Results = null! }),
            "signature-parameter" => Contract(signature with { Parameters = [null!] }),
            "signature-path" => Contract(signature with { Parameters = [new(null!)] }),
            "signature-path-indices" => Contract(signature with { Parameters = [new(new(null!))] }),
            "blocks" => new(graph.Id, signature, graph.Entry, null!),
            "block" => new(graph.Id, signature, graph.Entry, graph.Blocks.SetItem(block.Id, null!)),
            "parameters" => Replace(graph, block with { Parameters = null! }),
            "parameter" => Replace(graph, block with { Parameters = [null!] }),
            "operations" => Replace(graph, block with { Operations = null! }),
            "operation" => Op(null!),
            "result" => Op(new Operation.Literal(null!, Empty)),
            "literal" => Op(new Operation.Literal(new(new(2)), null!)),
            "blob" => Op(new Operation.Literal(new(new(2)), new LiteralValue.Blob(null!))),
            "list" => Op(new Operation.Literal(new(new(2)), new LiteralValue.List(null!))),
            "list-item" => Op(new Operation.Literal(new(new(2)), new LiteralValue.List([null!]))),
            "make-list" => Op(new Operation.MakeList(new(new(2)), null!)),
            "project-path" => Op(new Operation.Project(new(new(2)), new(1), null!)),
            "project-indices" => Op(new Operation.Project(new(new(2)), new(1), new(null!))),
            "terminator" => End(null!),
            "return" => End(new Terminator.Return(null!)),
            "jump" => End(new Terminator.Jump(null!)),
            "edge-arguments" => End(new Terminator.Jump(edge with { Arguments = null! })),
            "branch-literal" => End(new Terminator.Branch(new(1), null!, edge, edge)),
            "branch-equal" => End(new Terminator.Branch(new(1), Empty, null!, edge)),
            "branch-not-equal" => End(new Terminator.Branch(new(1), Empty, edge, null!)),
            "switch-cases" => End(new Terminator.Switch(new(1), null!, edge)),
            "switch-case" => End(new Terminator.Switch(new(1), [null!], edge)),
            "switch-literal" => End(new Terminator.Switch(new(1), [new(null!, edge)], edge)),
            "switch-edge" => End(new Terminator.Switch(new(1), [new(Empty, null!)], edge)),
            "switch-default" => End(new Terminator.Switch(new(1), [], null!)),
            "call" => Invoke(null!),
            "tail-call" => End(new Terminator.TailInvoke(null!)),
            "call-target" => Invoke(call with { Target = null! }),
            "call-signature" => Invoke(call with { Signature = null! }),
            "call-parameters" => Invoke(call with { Signature = call.Signature with { Parameters = null! } }),
            "call-results" => Invoke(call with { Signature = call.Signature with { Results = null! } }),
            "call-arguments" => Invoke(call with { Arguments = null! }),
            "continuation" => End(new Terminator.Invoke(call, null!)),
            "bindings" => End(new Terminator.Invoke(call, continuation with { Bindings = null! })),
            "binding" => End(new Terminator.Invoke(call, continuation with { Bindings = [null!] })),
            _ => throw new ArgumentOutOfRangeException(nameof(scenario)),
        };
    }

    [Fact]
    public void Null_graph_and_signature_table_are_diagnosed()
    {
        var graphError = Errors(null!).Should().ContainSingle().Subject;
        graphError.Code.Should().Be(GraphDiagnosticCode.MissingData);
        graphError.Location.Should().Be(new GraphLocation(null));

        var tableError = ValidatedFunctionGraph.ValidateGraph(Graph(), null!)
            .Should().BeOfType<Result<ImmutableList<GraphDiagnostic>, ValidatedFunctionGraph>.Err>()
            .Subject.Value.Should().ContainSingle().Subject;
        tableError.Code.Should().Be(GraphDiagnosticCode.MissingData);
        tableError.Location.Should().Be(At(block: null, member: "knownFunctions"));

        var signatureError = Errors(Graph(), Signatures.Add(new(8), null!)).Should().ContainSingle().Subject;
        signatureError.Code.Should().Be(GraphDiagnosticCode.MissingData);
        signatureError.Location.Should().Be(new GraphLocation(new(8), Member: "knownFunctions.signature"));
    }

    [Theory]
    [InlineData(null)]
    [InlineData("")]
    [InlineData("unknown")]
    public void Invalid_builtin_names_are_graph_errors(string? name)
    {
        var graph = Graph();
        var error = Errors(Replace(graph, graph.Blocks[new(90)] with
        {
            Operations = graph.Blocks[new(90)].Operations.SetItem(3, new Operation.Builtin(new(new(5)), name!, new(4))),
        })).Should().ContainSingle().Subject;
        error.Code.Should().Be(GraphDiagnosticCode.InvalidBuiltin);
        error.Location.Should().Be(At(operation: 3, member: "name"));
    }

    [Theory]
    [InlineData("literal")]
    [InlineData("list")]
    [InlineData("project")]
    [InlineData("builtin")]
    public void Every_operation_result_requires_a_supported_Pine_type(string kind)
    {
        var invalid = new ValueDefinition(new(2), (ValueType)99);
        var operation = kind switch
        {
            "literal" => (Operation)new Operation.Literal(invalid, Empty),
            "list" => new Operation.MakeList(invalid, [new(1)]),
            "project" => new Operation.Project(invalid, new(1), new([])),
            "builtin" => new Operation.Builtin(invalid, "head", new(1)),
            _ => throw new ArgumentOutOfRangeException(nameof(kind)),
        };
        var graph = NullGraph("blob");
        var errors = Errors(Replace(graph, graph.Blocks[new(90)] with { Operations = [operation] }));
        errors.Select(error => error.Code).Should().Equal(GraphDiagnosticCode.InvalidType, GraphDiagnosticCode.TypeMismatch);
        var expectedLocation = At(operation: 0, value: 2, member: "result");
        errors.Select(error => error.Location).Should().OnlyContain(location => location == expectedLocation);
    }

    [Theory]
    [InlineData("return", "return.values[0]", null)]
    [InlineData("edge", "arguments[0]", 0)]
    [InlineData("call", "call.arguments[0]", null)]
    [InlineData("continuation", "bindings[0]", 0)]
    [InlineData("branch", "testedValue", null)]
    [InlineData("switch", "selector", null)]
    [InlineData("dynamic", "call.target", null)]
    [InlineData("list", "items[0]", null)]
    [InlineData("project", "source", null)]
    [InlineData("builtin", "argument", null)]
    public void Operand_types_are_checked_at_every_use(string scenario, string member, int? edge)
    {
        var graph = Graph();
        var block = graph.Blocks[new(90)] with
        {
            Parameters = [new(new(1), (ValueType)99)],
            Operations = [new Operation.Literal(new(new(2)), Empty)],
        };
        var target = new Edge(new(10), [new(2)]);
        var terminator = scenario switch
        {
            "return" => (Terminator)new Terminator.Return([new(1)]),
            "edge" => new Terminator.Jump(new(new(10), [new(1)])),
            "call" => new Terminator.Invoke(DynamicCall(), new(new(10), [new ContinuationBinding.ReturnedResult(0)])),
            "continuation" => new Terminator.Invoke(DynamicCall() with { Arguments = [new(2)] },
                new(new(10), [new ContinuationBinding.CallerValue(new(1))])),
            "branch" => new Terminator.Branch(new(1), Empty, target, target),
            "switch" => new Terminator.Switch(new(1), [], target),
            "dynamic" => new Terminator.TailInvoke(DynamicCall() with { Target = new CallTarget.Dynamic(new(1)), Arguments = [new(2)] }),
            "list" or "project" or "builtin" => new Terminator.Return([new(2)]),
            _ => throw new ArgumentOutOfRangeException(nameof(scenario)),
        };
        var operations = scenario switch
        {
            "list" => block.Operations.Add(new Operation.MakeList(new(new(3)), [new(1)])),
            "project" => block.Operations.Add(new Operation.Project(new(new(3)), new(1), new([]))),
            "builtin" => block.Operations.Add(new Operation.Builtin(new(new(3)), "equal", new(1))),
            _ => block.Operations,
        };
        var errors = Errors(Replace(graph, block with { Terminator = terminator, Operations = operations }));
        errors.Select(error => error.Code).Should().Equal(
            GraphDiagnosticCode.InvalidType, GraphDiagnosticCode.TypeMismatch, GraphDiagnosticCode.TypeMismatch);
        errors[0].Location.Should().Be(At(value: 1, member: "parameters[0]"));
        errors[1].Location.Should().Be(At(value: 1, member: "parameters[0]"));
        errors[2].Location.Should().Be(At(operation: operations.Count == 2 ? 1 : null, edge: edge, value: 1, member: member));
    }

    [Fact]
    public void Invalid_signature_types_and_paths_are_rejected_even_without_calls()
    {
        var graph = Graph();
        var signature = new FunctionSignature([new(new([-1]), (ValueType)99)], [(ValueType)99]);
        var errors = Errors(new(graph.Id, signature, graph.Entry, graph.Blocks));
        errors.Select(error => error.Code).Should().Equal(
            GraphDiagnosticCode.InvalidType, GraphDiagnosticCode.InvalidPath, GraphDiagnosticCode.InvalidType,
            GraphDiagnosticCode.TypeMismatch, GraphDiagnosticCode.TypeMismatch, GraphDiagnosticCode.TypeMismatch);
        errors.Take(3).Select(error => error.Location).Should().Equal(
            At(block: null, member: "signature.parameters[0]"),
            At(block: null, member: "signature.parameters[0].path"),
            At(block: null, member: "signature.results[0]"));
    }
}
