using Pine.Core.Interpreter.IntermediateVM.Semantic;
using Pine.Core.PineVM;
using System;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.Core.Interpreter.IntermediateVM.Frontend;

/// <summary>
/// Direct expression-to-block-local-SSA compilation. Every lexical value crossing a block boundary
/// is passed explicitly; no instruction stream is constructed or inspected by this frontend.
/// </summary>
public static class ExpressionGraphCompiler
{
    private sealed record Cursor(GraphBuildState State, ImmutableList<PineVirtualValueId> Live);

    /// <summary>Compiles without optional preparation rewrites, preserving the source evaluation order.</summary>
    public static FunctionGraph Compile(Expression expression) =>
        CompileOwned(OwnedExpression.Capture(expression), new(0));

    /// <summary>Compiles a prepared body; all graph optimizations remain disabled.</summary>
    public static FunctionGraph CompileExpressionToGraph(PreparedFunction prepared) =>
        CompileOwned(prepared.Body, prepared.Request.Id);

    /// <summary>Validates the directly constructed graph and returns updated explicit graph memoization.</summary>
    public static (Result<ImmutableList<GraphDiagnostic>, ValidatedFunctionGraph> Graph, CompilerMemo Memo)
        CompileExpressionToGraph(PreparedFunction prepared, CompilerMemo memo)
    {
        var graph =
            memo.Graphs.TryGetValue(prepared, out var cached)
            ? cached
            : CompileExpressionToGraph(prepared);

        return (
            ValidatedFunctionGraph.ValidateGraph(graph, ImmutableDictionary<FunctionId, FunctionSignature>.Empty),
            memo with { Graphs = memo.Graphs.SetItem(prepared, graph) });
    }

    private static FunctionGraph CompileOwned(OwnedExpression root, FunctionId id)
    {
        var (entryState, entry) = GraphBuildState.Empty.AllocateBlock();
        var (environmentState, environment) = entryState.AllocateValue();
        var initial = environmentState.OpenBlock(entry, [new(environment)]);
        var completed = CompileTail(root, new(initial, [environment]));
        return new(id, FunctionSignature.Canonical, entry, completed.CompletedBlocks);
    }

    private static GraphBuildState CompileTail(OwnedExpression expression, Cursor cursor) =>
        expression switch
        {
            OwnedExpression.Label label => CompileTail(label.Tagged, cursor),
            OwnedExpression.Eval eval => TailCall(eval, cursor),
            OwnedExpression.Conditional conditional => TailConditional(conditional, cursor),
            OwnedExpression.Literal or OwnedExpression.List or OwnedExpression.Builtin or OwnedExpression.Environment =>
                ReturnValue(CompileNode(expression, cursor)),
            _ => throw new NotImplementedException("Unknown expression variant: " + expression.GetType().Name),
        };

    private static GraphBuildState ReturnValue(Cursor cursor) =>
        cursor.State.CompleteBlock(new Terminator.Return([cursor.Live[^1]]));

    // Live[0] is always the environment. CompileNode appends exactly one result while preserving
    // and, across transfers, rebinding every incoming live value.
    private static Cursor CompileNode(OwnedExpression expression, Cursor cursor) =>
        expression switch
        {
            OwnedExpression.Literal literal => Define(cursor, result => new Operation.Literal(result, literal.Value)),
            OwnedExpression.Environment => cursor with { Live = cursor.Live.Add(cursor.Live[0]) },
            OwnedExpression.Builtin builtin => CompileBuiltin(builtin, cursor),
            OwnedExpression.List list => CompileList(list, cursor),
            OwnedExpression.Conditional conditional => CompileConditional(conditional, cursor),
            OwnedExpression.Eval eval => CompileCall(eval, cursor),
            OwnedExpression.Label label => CompileNode(label.Tagged, cursor),
            _ => throw new NotImplementedException("Unknown expression variant: " + expression.GetType().Name),
        };

    private static Cursor Define(Cursor cursor, Func<ValueDefinition, Operation> operation)
    {
        var (state, value) = cursor.State.AllocateValue();
        return new(state.AppendOperation(operation(new(value))), cursor.Live.Add(value));
    }

    private static Cursor CompileBuiltin(OwnedExpression.Builtin builtin, Cursor cursor)
    {
        return builtin is
        {
            Name: "head",
            Input: OwnedExpression.Builtin
            {
                Name: "skip",
                Input: OwnedExpression.List { Items: [var count, var source] },
            },
        }
            ? CompileListOnlyHeadSkip(builtin, count, source, cursor)
            : CompileBuiltinGeneric(builtin, cursor);
    }

    private static Cursor CompileBuiltinGeneric(OwnedExpression.Builtin builtin, Cursor cursor)
    {
        var input = CompileNode(builtin.Input, cursor);
        var defined = Define(input, result => new Operation.Builtin(result, builtin.Name, input.Live[^1]));
        return defined with { Live = defined.Live.RemoveAt(cursor.Live.Count) };
    }

    private static Cursor CompileListOnlyHeadSkip(
        OwnedExpression.Builtin builtin, OwnedExpression count, OwnedExpression source, Cursor cursor)
    {
        // DirectInterpreter gives this syntactic form list-only semantics. Its unsuccessful
        // integer probe falls back to generic evaluation, evaluating the count a second time.
        var counted = CompileNode(count, cursor);
        var normalizedInput = Define(counted, result => new Operation.MakeList(result, [counted.Live[^1]]));
        var normalized = Define(normalizedInput, result => new Operation.Builtin(result, "int_add", normalizedInput.Live[^1]));
        var (invalidState, invalidBlock) = normalized.State.AllocateBlock();
        var (validState, validBlock) = invalidState.AllocateBlock();
        var (joinState, joinBlock) = validState.AllocateBlock();
        var branched = joinState.CompleteBlock(new Terminator.Branch(
            normalized.Live[^1], new LiteralValue.List([]),
            new(invalidBlock, counted.Live), new(validBlock, counted.Live)));

        var invalid = Open(branched, invalidBlock, counted.Live.Count);
        var fallback = CompileBuiltinGeneric(builtin, invalid with { Live = invalid.Live.RemoveAt(cursor.Live.Count) });
        var afterFallback = fallback.State.CompleteBlock(new Terminator.Jump(new(joinBlock, fallback.Live)));

        var valid = Open(afterFallback, validBlock, counted.Live.Count);
        var sourced = CompileNode(source, valid);
        // A nonempty list prefix forces concat to reject blobs. Dropping that prefix gives the
        // original list, or [] for every blob, without type speculation or duplicate evaluation.
        var prefix = Define(sourced, result => new Operation.Literal(result, new LiteralValue.List([new LiteralValue.List([])])));
        var concatenationInput = Define(prefix, result => new Operation.MakeList(result, [prefix.Live[^1], sourced.Live[^1]]));
        var concatenated = Define(concatenationInput, result => new Operation.Builtin(result, "concat", concatenationInput.Live[^1]));
        var one = Define(concatenated, result => new Operation.Literal(result,
            OwnedExpression.CaptureValue(CommonEncodings.IntegerEncoding.EncodeSignedInteger(1))));
        var dropPrefixInput = Define(one, result => new Operation.MakeList(result, [one.Live[^1], concatenated.Live[^1]]));
        var list = Define(dropPrefixInput, result => new Operation.Builtin(result, "skip", dropPrefixInput.Live[^1]));
        var skipInput = Define(list, result => new Operation.MakeList(result, [sourced.Live[cursor.Live.Count], list.Live[^1]]));
        var skipped = Define(skipInput, result => new Operation.Builtin(result, "skip", skipInput.Live[^1]));
        var head = Define(skipped, result => new Operation.Builtin(result, "head", skipped.Live[^1]));
        var resultValues = head.Live.Take(cursor.Live.Count).Append(head.Live[^1]).ToImmutableList();
        var afterValid = head.State.CompleteBlock(new Terminator.Jump(new(joinBlock, resultValues)));
        return Open(afterValid, joinBlock, cursor.Live.Count + 1);
    }

    private static Cursor CompileList(OwnedExpression.List list, Cursor cursor)
    {
        var items = list.Items.Aggregate(cursor, (state, item) => CompileNode(item, state));
        var defined = Define(items, result => new Operation.MakeList(result, items.Live.Skip(cursor.Live.Count).ToImmutableList()));
        return defined with { Live = defined.Live.Take(cursor.Live.Count).Append(defined.Live[^1]).ToImmutableList() };
    }

    private static (GraphBuildState State, ImmutableList<ValueDefinition> Parameters) AllocateParameters(
        GraphBuildState state, int count) =>
        Enumerable.Range(0, count).Aggregate(
            (State: state, Parameters: ImmutableList<ValueDefinition>.Empty),
            (acc, _) =>
            {
                var (next, value) = acc.State.AllocateValue();
                return (next, acc.Parameters.Add(new(value)));
            });

    private static Cursor Open(GraphBuildState state, PineBlockId block, int liveCount)
    {
        var (allocated, parameters) = AllocateParameters(state, liveCount);
        return new(allocated.OpenBlock(block, parameters), parameters.Select(parameter => parameter.Id).ToImmutableList());
    }

    private static (GraphBuildState State, PineBlockId True, PineBlockId False, ImmutableList<PineVirtualValueId> Live)
        Branch(OwnedExpression.Conditional conditional, Cursor cursor)
    {
        var condition = CompileNode(conditional.Condition, cursor);
        var (trueState, trueBlock) = condition.State.AllocateBlock();
        var (falseState, falseBlock) = trueState.AllocateBlock();
        var live = condition.Live.RemoveAt(condition.Live.Count - 1);
        return (
            falseState.CompleteBlock(new Terminator.Branch(
                condition.Live[^1], OwnedExpression.CaptureValue(PineKernelValues.TrueValue),
                new(trueBlock, live), new(falseBlock, live))),
            trueBlock, falseBlock, live);
    }

    private static GraphBuildState TailConditional(OwnedExpression.Conditional conditional, Cursor cursor)
    {
        var branch = Branch(conditional, cursor);
        var trueState = CompileTail(conditional.TrueBranch, Open(branch.State, branch.True, branch.Live.Count));
        return CompileTail(conditional.FalseBranch, Open(trueState, branch.False, branch.Live.Count));
    }

    private static Cursor CompileConditional(OwnedExpression.Conditional conditional, Cursor cursor)
    {
        var branch = Branch(conditional, cursor);
        var (state, join) = branch.State.AllocateBlock();
        var trueArm = CompileNode(conditional.TrueBranch, Open(state, branch.True, branch.Live.Count));
        var afterTrue = trueArm.State.CompleteBlock(new Terminator.Jump(new(join, trueArm.Live)));
        var falseArm = CompileNode(conditional.FalseBranch, Open(afterTrue, branch.False, branch.Live.Count));
        var afterFalse = falseArm.State.CompleteBlock(new Terminator.Jump(new(join, falseArm.Live)));
        return Open(afterFalse, join, branch.Live.Count + 1);
    }

    private static (Cursor Cursor, Call Call) CallOperands(OwnedExpression.Eval eval, Cursor cursor)
    {
        var environment = CompileNode(eval.InvocationEnvironment, cursor);
        var encoded = CompileNode(eval.Encoded, environment);
        var (state, site) = encoded.State.AllocateCallSite();
        return (encoded with { State = state },
            new(site, new CallTarget.Dynamic(encoded.Live[^1]), FunctionSignature.Canonical, [encoded.Live[^2]]));
    }

    private static GraphBuildState TailCall(OwnedExpression.Eval eval, Cursor cursor)
    {
        var operands = CallOperands(eval, cursor);
        return operands.Cursor.State.CompleteBlock(new Terminator.TailInvoke(operands.Call));
    }

    private static Cursor CompileCall(OwnedExpression.Eval eval, Cursor cursor)
    {
        var operands = CallOperands(eval, cursor);
        var (state, continuation) = operands.Cursor.State.AllocateBlock();
        var bindings = operands.Cursor.Live.Take(cursor.Live.Count)
            .Select(value => (ContinuationBinding)new ContinuationBinding.CallerValue(value))
            .Append(new ContinuationBinding.ReturnedResult(0)).ToImmutableList();
        var completed = state.CompleteBlock(new Terminator.Invoke(operands.Call, new(continuation, bindings)));
        return Open(completed, continuation, cursor.Live.Count + 1);
    }
}
