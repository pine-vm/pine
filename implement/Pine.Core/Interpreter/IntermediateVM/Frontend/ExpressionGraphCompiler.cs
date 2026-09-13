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
    private sealed record Cursor(
        GraphBuildState State, ImmutableList<PineVirtualValueId> Live,
        FunctionSignature Signature);
    private sealed record EqualityCase(LiteralValue Literal, OwnedExpression Body);
    private sealed record EqualitySwitch(OwnedExpression Selector, ImmutableList<EqualityCase> Cases, OwnedExpression Default);

    /// <summary>Compiles without optional preparation rewrites, preserving the source evaluation order.</summary>
    public static FunctionGraph Compile(Expression expression) =>
        CompileOwned(OwnedExpression.Capture(expression), new(0), FunctionSignature.Canonical);

    /// <summary>Compiles against ordered input projections without reconstructing omitted fields.</summary>
    public static FunctionGraph Compile(Expression expression, FunctionSignature signature) =>
        CompileOwned(OwnedExpression.Capture(expression), new(0), signature);

    /// <summary>Compiles a prepared body; all graph optimizations remain disabled.</summary>
    public static FunctionGraph CompileExpressionToGraph(PreparedFunction prepared) =>
        CompileOwned(prepared.Body, prepared.Request.Id, FunctionSignature.Canonical);

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

    private static FunctionGraph CompileOwned(OwnedExpression root, FunctionId id, FunctionSignature signature)
    {
        var (entryState, entry) = GraphBuildState.Empty.AllocateBlock();
        var (parameterState, parameters) = AllocateParameters(entryState, signature.Parameters.Count);
        var initial = parameterState.OpenBlock(entry, parameters);
        var completed = CompileTail(root, new(initial, [.. parameters.Select(parameter => parameter.Id)], signature));
        return new(id, signature, entry, completed.CompletedBlocks);
    }

    private static GraphBuildState CompileTail(OwnedExpression expression, Cursor cursor) =>
        expression switch
        {
            OwnedExpression.Label label => CompileTail(label.Tagged, cursor),
            OwnedExpression.Eval eval => TailCall(eval, cursor),
            OwnedExpression.Conditional conditional => TailConditional(conditional, cursor),
            OwnedExpression.Literal or OwnedExpression.List or OwnedExpression.Builtin or OwnedExpression.Environment =>
                ReturnValue(CompileNode(expression, cursor)),
            _ => throw new NotImplementedException("CompileTail does not handle expression variant: " + expression.GetType().Name),
        };

    private static GraphBuildState ReturnValue(Cursor cursor) =>
        cursor.State.CompleteBlock(new Terminator.Return([cursor.Live[^1]]));

    // The first live values are the ordered input projections. CompileNode appends exactly one result while preserving
    // and, across transfers, rebinding every incoming live value.
    private static Cursor CompileNode(OwnedExpression expression, Cursor cursor)
    {
        if (cursor.Signature != FunctionSignature.Canonical &&
            CodeAnalysis.CodeAnalysis.TryParseExprAsPathInEnv(expression.ToExpression()) is { } path)
        {
            var parameterIndex = cursor.Signature.Parameters.FindIndex(parameter =>
                path.Count >= parameter.Path.Indices.Count &&
                path.Take(parameter.Path.Indices.Count).SequenceEqual(parameter.Path.Indices));
            if (parameterIndex < 0)
                throw new ArgumentException("CompileNode environment reference is not covered by the declared signature.");
            var parameter = cursor.Signature.Parameters[parameterIndex];
            var remaining = path.Skip(parameter.Path.Indices.Count).ToImmutableList();
            return remaining.Count == 0
                ? cursor with { Live = cursor.Live.Add(cursor.Live[parameterIndex]) }
                : Define(cursor, result => new Operation.Project(result, cursor.Live[parameterIndex], new(remaining)));
        }
        return expression switch
        {
            OwnedExpression.Literal literal => Define(cursor, result => new Operation.Literal(result, literal.Value)),
            OwnedExpression.Environment => cursor with { Live = cursor.Live.Add(cursor.Live[0]) },
            OwnedExpression.Builtin builtin => CompileBuiltin(builtin, cursor),
            OwnedExpression.List list => CompileList(list, cursor),
            OwnedExpression.Conditional conditional => CompileConditional(conditional, cursor),
            OwnedExpression.Eval eval => CompileCall(eval, cursor),
            OwnedExpression.Label label => CompileNode(label.Tagged, cursor),
            _ => throw new NotImplementedException("CompileNode does not handle expression variant: " + expression.GetType().Name),
        };
    }

    private static Cursor Define(Cursor cursor, Func<ValueDefinition, Operation> operation)
    {
        var (state, value) = cursor.State.AllocateValue();
        return new(state.AppendOperation(operation(new(value))), cursor.Live.Add(value), cursor.Signature);
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
        if (count is OwnedExpression.Literal literal &&
            BuiltinFunction.SignedIntegerFromValueRelaxed(OwnedExpression.ToValue(literal.Value)) is { } index)
        {
            var directSource = CompileNode(source, cursor);
            var projected = Define(directSource, result => index > int.MaxValue
                ? new Operation.Literal(result, new LiteralValue.List([]))
                : new Operation.Project(result, directSource.Live[^1], new([index < 0 ? 0 : (int)index])));
            return projected with { Live = projected.Live.RemoveAt(cursor.Live.Count) };
        }

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

        var invalid = Open(branched, invalidBlock, counted.Live.Count, cursor.Signature);
        var fallback = CompileBuiltinGeneric(builtin, invalid with { Live = invalid.Live.RemoveAt(cursor.Live.Count) });
        var afterFallback = fallback.State.CompleteBlock(new Terminator.Jump(new(joinBlock, fallback.Live)));

        var valid = Open(afterFallback, validBlock, counted.Live.Count, cursor.Signature);
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
        return Open(afterValid, joinBlock, cursor.Live.Count + 1, cursor.Signature);
    }

    private static Cursor CompileList(OwnedExpression.List list, Cursor cursor)
    {
        var items = list.Items.Aggregate(cursor, (state, item) => CompileNode(item, state));
        var defined = Define(items, result => new Operation.MakeList(result, [.. items.Live.Skip(cursor.Live.Count)]));
        return defined with { Live = [.. defined.Live.Take(cursor.Live.Count), defined.Live[^1]] };
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

    private static Cursor Open(GraphBuildState state, PineBlockId block, int liveCount, FunctionSignature signature)
    {
        var (allocated, parameters) = AllocateParameters(state, liveCount);
        return new(allocated.OpenBlock(block, parameters), [.. parameters.Select(parameter => parameter.Id)], signature);
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
        if (ParseSwitch(conditional) is { } selection)
            return CompileSwitch(selection, cursor, tail: true).State;
        var branch = Branch(conditional, cursor);
        var trueState = CompileTail(conditional.TrueBranch, Open(branch.State, branch.True, branch.Live.Count, cursor.Signature));
        return CompileTail(conditional.FalseBranch, Open(trueState, branch.False, branch.Live.Count, cursor.Signature));
    }

    private static Cursor CompileConditional(OwnedExpression.Conditional conditional, Cursor cursor)
    {
        if (ParseSwitch(conditional) is { } selection)
            return CompileSwitch(selection, cursor, tail: false);
        var branch = Branch(conditional, cursor);
        var (state, join) = branch.State.AllocateBlock();
        var trueArm = CompileNode(conditional.TrueBranch, Open(state, branch.True, branch.Live.Count, cursor.Signature));
        var afterTrue = trueArm.State.CompleteBlock(new Terminator.Jump(new(join, trueArm.Live)));
        var falseArm = CompileNode(conditional.FalseBranch, Open(afterTrue, branch.False, branch.Live.Count, cursor.Signature));
        var afterFalse = falseArm.State.CompleteBlock(new Terminator.Jump(new(join, falseArm.Live)));
        return Open(afterFalse, join, branch.Live.Count + 1, cursor.Signature);
    }

    private static EqualitySwitch? ParseSwitch(OwnedExpression.Conditional root)
    {
        return Parse();

        EqualitySwitch? Parse()
        {
            OwnedExpression current = root;
            OwnedExpression? selector = null;
            var cases = ImmutableList<EqualityCase>.Empty;
            var literals = ImmutableHashSet<LiteralValue>.Empty;
            while (current is OwnedExpression.Conditional conditional &&
                conditional.Condition is OwnedExpression.Builtin { Name: "equal", Input: OwnedExpression.List { Items.Count: 2 } arguments })
            {
                var literal = arguments.Items[0] as OwnedExpression.Literal ?? arguments.Items[1] as OwnedExpression.Literal;
                if (literal is null)
                    break;
                var compared = arguments.Items[0] == literal ? arguments.Items[1] : arguments.Items[0];
                if (selector is not null && compared != selector)
                    break;
                selector = compared;
                if (!literals.Contains(literal.Value))
                {
                    cases = cases.Add(new(literal.Value, conditional.TrueBranch));
                    literals = literals.Add(literal.Value);
                }
                current = conditional.FalseBranch;
            }
            return selector is not null && cases.Count > 1 &&
                !Expression.EnumerateSelfAndDescendants(selector.ToExpression()).Any(expression => expression is Expression.Eval)
                ? new(selector, cases, current) : null;
        }
    }

    private static Cursor CompileSwitch(EqualitySwitch selection, Cursor cursor, bool tail)
    {
        return CompileBranches();

        Cursor CompileBranches()
        {
            var tested = CompileNode(selection.Selector, cursor);
            var state = tested.State;
            var targets = ImmutableList<PineBlockId>.Empty;
            foreach (var unused in selection.Cases)
            {
                var allocated = state.AllocateBlock();
                state = allocated.State;
                targets = targets.Add(allocated.Id);
            }
            var (defaultState, defaultBlock) = state.AllocateBlock();
            var (joinState, join) = defaultState.AllocateBlock();
            var live = tested.Live.RemoveAt(tested.Live.Count - 1);
            state = joinState.CompleteBlock(new Terminator.Switch(tested.Live[^1],
                [.. selection.Cases.Select((@case, index) => new SwitchCase(@case.Literal, new(targets[index], live)))],
                new(defaultBlock, live)));
            foreach (var (body, target) in selection.Cases.Select((@case, index) => (@case.Body, targets[index]))
                .Append((selection.Default, defaultBlock)))
            {
                var branch = Open(state, target, live.Count, cursor.Signature);
                if (tail)
                    state = CompileTail(body, branch);
                else
                {
                    var result = CompileNode(body, branch);
                    state = result.State.CompleteBlock(new Terminator.Jump(new(join, result.Live)));
                }
            }
            return tail ? new(state, live, cursor.Signature) : Open(state, join, cursor.Live.Count + 1, cursor.Signature);
        }
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
        return Open(completed, continuation, cursor.Live.Count + 1, cursor.Signature);
    }
}
