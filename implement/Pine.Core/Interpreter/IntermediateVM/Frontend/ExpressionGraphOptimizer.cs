using Pine.Core.CommonEncodings;
using Pine.Core.Interpreter.IntermediateVM.Semantic;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.Core.Interpreter.IntermediateVM.Frontend;

/// <summary>
/// Opt-in graph policy. Inlining and analysis bounds apply cumulatively per Optimize invocation, including speculative
/// work on refused candidates. Work units count encoded nodes/bytes and owned AST nodes.
/// Depth and per-body limits bound recursive parsing and frontend construction before either runs.
/// Scalar and guard limits are separate per-pass/body bounds; the cumulative candidate limit also
/// bounds how many such passes can run. They do not consume the inliner's expansion allowance.
/// </summary>
public sealed record GraphOptimizerOptions(
    bool Enabled = true,
    bool InlineLiteralCalls = true,
    bool SelfTailLoops = true,
    int MaxCandidates = 32,
    long MaxExpansionUnits = 20_000,
    long MaxWorkUnits = 65_536,
    int MaxBodyNodes = 256,
    int MaxDepth = 64,
    long MaxAnalysisWorkUnits = 100_000,
    int MaxAnalysisDepth = 32,
    int MaxAnalysisListItems = 256,
    bool ScalarReplacement = true,
    bool GuardDynamicSelfTailCalls = true,
    long MaxScalarWorkUnits = 200_000,
    long MaxScalarExpansionUnits = 20_000,
    long MaxGuardWorkUnits = 65_536,
    long MaxGuardExpansionUnits = 20_000);

/// <summary>A conservative reason to retain the original dynamic call.</summary>
public enum GraphOptimizationDeclineCode
{
    /// <summary>No exact literal or sound graph value fact defines the dynamic target.</summary>
    UnknownTarget,
    /// <summary>A cumulative limit prevents further speculative work.</summary>
    BudgetExhausted,
    /// <summary>The encoding exceeds the bounded traversal/depth limit.</summary>
    EncodingLimit,
    /// <summary>The literal is not a valid encoded expression.</summary>
    InvalidEncoding,
    /// <summary>The parsed body exceeds the frontend construction limit.</summary>
    BodyLimit,
    /// <summary>The frontend cannot produce a validated callee.</summary>
    UnsupportedBody,
    /// <summary>No collision-free representable function ID remains.</summary>
    IdSpaceExhausted,
    /// <summary>The temporary known-call graph or inline expansion was refused.</summary>
    InlineRefused,
}

/// <summary>A deterministic call-site refusal; no temporary known call escapes.</summary>
public sealed record GraphOptimizationDecline(
    CallSiteId Site, GraphOptimizationDeclineCode Code, GraphInliningDeclineCode? InlineReason = null);

/// <summary>Logical work counts are identical on cold and warm memos, not cache-hit counters.</summary>
public sealed record GraphOptimizationStats(
    int Candidates, int InlinedCalls, int SelfTailCalls, long WorkUnits, long GraphGrowthUnits,
    ImmutableList<GraphOptimizationDecline> Declines,
    bool BudgetLimitReached = false,
    long AnalysisWorkUnits = 0,
    int AnalysisPasses = 0)
{
    /// <summary>No optimization work.</summary>
    public static GraphOptimizationStats Empty { get; } = new(0, 0, 0, 0, 0, []);

    /// <inheritdoc/>
    public bool Equals(GraphOptimizationStats? other) =>
        other is not null && Candidates == other.Candidates && InlinedCalls == other.InlinedCalls &&
        SelfTailCalls == other.SelfTailCalls && WorkUnits == other.WorkUnits &&
        GraphGrowthUnits == other.GraphGrowthUnits && BudgetLimitReached == other.BudgetLimitReached &&
        AnalysisWorkUnits == other.AnalysisWorkUnits && AnalysisPasses == other.AnalysisPasses &&
        Declines.SequenceEqual(other.Declines);

    /// <inheritdoc/>
    public override int GetHashCode() =>
        HashCode.Combine(
            HashCode.Combine(Candidates, InlinedCalls, SelfTailCalls, WorkUnits, GraphGrowthUnits, ModelEquality.SequenceHash(Declines), BudgetLimitReached),
            AnalysisWorkUnits, AnalysisPasses);
}

/// <summary>The validated output and explicit persistent compiler memo; optimized graphs are not memoized.</summary>
public sealed record GraphOptimizationResult(ValidatedFunctionGraph Graph, CompilerMemo Memo, GraphOptimizationStats Stats);

/// <summary>
/// Bounded expression-graph integration using only structurally proven, owned target values.
/// Every speculative body uses reduction-disabled preparation, never legacy expression inlining.
/// </summary>
public static class ExpressionGraphOptimizer
{
    /// <summary>Pure inspection entrypoint without creating a VM or any sequential instructions.</summary>
    public static Result<ImmutableList<GraphDiagnostic>, GraphOptimizationResult> Compile(
        CompilationRequest request, GraphOptimizerOptions options, CompilerMemo memo)
    {
        var prepared = FunctionPreparation.PrepareFunction(request, memo);
        var compiled = ExpressionGraphCompiler.CompileExpressionToGraph(prepared.Function, prepared.Memo);
        return compiled.Graph.Map(graph =>
        {
            var guarded = options.Enabled && options.SelfTailLoops && options.GuardDynamicSelfTailCalls &&
                options.MaxGuardWorkUnits > 0 && options.MaxGuardExpansionUnits > 0 &&
                options.MaxCandidates > 0 && options.MaxWorkUnits > 0 && options.MaxExpansionUnits > 0 &&
                options.MaxBodyNodes > 0 && options.MaxDepth > 0 &&
                prepared.Function.Request.Specialization is null &&
                prepared.Function.Body == prepared.Function.Source
                ? ExpressionSelfTailLoops.Compile(prepared.Function,
                    OwnedExpression.CaptureValue(ExpressionEncoding.EncodeExpressionAsValue(prepared.Function.Source.ToExpression())),
                    options.MaxGuardWorkUnits, options.MaxGuardExpansionUnits, options.MaxDepth)
                : (graph, 0);
            var optimized = Optimize(guarded.Item1, options, compiled.Memo);
            return optimized with { Stats = optimized.Stats with { SelfTailCalls = optimized.Stats.SelfTailCalls + guarded.Item2 } };
        });
    }

    /// <summary>
    /// Discovers exact operands in block-ID order, revisiting introduced calls and newly exposed facts.
    /// A persistent visited set and strictly cumulative candidate/work/growth limits bound expansion.
    /// Disabled policy returns the exact input evidence and memo without inspecting the graph.
    /// </summary>
    public static GraphOptimizationResult Optimize(
        ValidatedFunctionGraph input, GraphOptimizerOptions options, CompilerMemo memo)
    {
        return !options.Enabled ? new(input, memo, GraphOptimizationStats.Empty) : Run();

        GraphOptimizationResult Run()
        {
            if (options.MaxCandidates <= 0 || options.MaxWorkUnits <= 0 || options.MaxExpansionUnits <= 0 ||
                options.MaxBodyNodes <= 0 || options.MaxDepth <= 0)
                return new(input, memo, GraphOptimizationStats.Empty with { BudgetLimitReached = true });
            // Equivalent immutable fold: (graph, memo, visited sites, stats) -> next state.
            var loops = options.SelfTailLoops ? GraphSelfTailLoops.Rewrite(input) : (input, 0);
            var current = options.ScalarReplacement
                ? GraphScalarReplacement.Rewrite(loops.Item1, maxWorkUnits: options.MaxScalarWorkUnits,
                    maxExpansionUnits: options.MaxScalarExpansionUnits) : loops.Item1;
            var currentMemo = memo;
            var stats = GraphOptimizationStats.Empty with { SelfTailCalls = loops.Item2 };
            var visited = ImmutableHashSet<CallSiteId>.Empty;
            var unknownVisited = ImmutableHashSet<CallSiteId>.Empty;
            GraphValueAnalysisResult? analysis = null;
            if (!options.InlineLiteralCalls)
                return new(current, currentMemo, stats);

            while (true)
            {
                if (stats.Candidates >= options.MaxCandidates || stats.WorkUnits >= options.MaxWorkUnits ||
                    stats.GraphGrowthUnits >= options.MaxExpansionUnits || options.MaxBodyNodes <= 0 || options.MaxDepth <= 0)
                    return new(current, currentMemo, stats with { BudgetLimitReached = true });

                var block = current.Graph.Blocks.Values.OrderBy(block => block.Id.Value).FirstOrDefault(block =>
                    CallOf(block.Terminator) is { } call && IsDynamic(call.Target) && !visited.Contains(call.Site));
                if (block is null)
                    return new(current, currentMemo, stats);
                var call = CallOf(block.Terminator)!;
                visited = visited.Add(call.Site);
                stats = stats with { Candidates = stats.Candidates + 1 };
                var literal = block.Operations.Select(LiteralOf).FirstOrDefault(operation =>
                    operation?.Result.Id == ((CallTarget.Dynamic)call.Target).EncodedExpression);
                // Keep the original literal fast path and its work counts. Run analysis at most
                // once for each graph version, only when an extended target actually needs it.
                var target = literal?.Value ?? ResolveFact(((CallTarget.Dynamic)call.Target).EncodedExpression);
                if (target is null)
                {
                    unknownVisited = unknownVisited.Add(call.Site);
                    Decline(analysis?.BudgetExhausted is true
                        ? GraphOptimizationDeclineCode.BudgetExhausted : GraphOptimizationDeclineCode.UnknownTarget);
                    continue;
                }

                var encoding = MeasureLiteral(target, options.MaxWorkUnits - stats.WorkUnits, options.MaxDepth);
                stats = stats with { WorkUnits = stats.WorkUnits + encoding.Units };
                if (!encoding.Fits)
                {
                    Decline(GraphOptimizationDeclineCode.EncodingLimit);
                    continue;
                }
                var parsed = FunctionPreparation.ParseExpression(target, currentMemo);
                currentMemo = parsed.Memo;
                if (parsed.Result.Expression is not { } body)
                {
                    Decline(GraphOptimizationDeclineCode.InvalidEncoding);
                    continue;
                }
                var size = MeasureBody(body, Math.Min(options.MaxBodyNodes, options.MaxWorkUnits - stats.WorkUnits), options.MaxDepth);
                stats = stats with { WorkUnits = stats.WorkUnits + size.Units };
                if (!size.Fits)
                {
                    Decline(GraphOptimizationDeclineCode.BodyLimit);
                    continue;
                }
                var id = FreshFunctionId(current);
                if (id is null)
                {
                    Decline(GraphOptimizationDeclineCode.IdSpaceExhausted);
                    continue;
                }
                var request = new CompilationRequest(id.Value, body, new(DisableReduction: true), null, []);
                (PreparedFunction Function, CompilerMemo Memo) preparation;
                try
                {
                    preparation = FunctionPreparation.PrepareFunction(request, currentMemo);
                }
                catch (OverflowException)
                {
                    // Legacy parameter-path inference can reject otherwise valid encoded
                    // bodies. Speculative preparation must not reject an unselected call.
                    Decline(GraphOptimizationDeclineCode.UnsupportedBody);
                    continue;
                }
                var compilation = ExpressionGraphCompiler.CompileExpressionToGraph(preparation.Function, preparation.Memo);
                currentMemo = compilation.Memo;
                if (compilation.Graph.IsOkOrNull() is not { } callee)
                {
                    Decline(GraphOptimizationDeclineCode.UnsupportedBody);
                    continue;
                }
                var calleeLoops = options.SelfTailLoops && options.GuardDynamicSelfTailCalls &&
                    options.MaxGuardWorkUnits > 0 && options.MaxGuardExpansionUnits > 0
                    ? ExpressionSelfTailLoops.Compile(preparation.Function, target,
                        options.MaxGuardWorkUnits, options.MaxGuardExpansionUnits, options.MaxDepth) : (callee, 0);
                callee = calleeLoops.Item1;
                var known = call with { Target = new CallTarget.Known(id.Value) };
                var terminator = block.Terminator switch
                {
                    Terminator.Invoke invoke => (Terminator)(invoke with { Call = known }),
                    Terminator.TailInvoke tail => tail with { Call = known },
                    Terminator.Return or Terminator.Jump or Terminator.Branch or Terminator.Switch =>
                        throw new InvalidOperationException("Run expected a call terminator, not: " + block.Terminator.GetType().Name),
                    _ => throw new NotImplementedException("Run does not handle terminator variant: " + block.Terminator.GetType().Name),
                };
                var temporary = ValidatedFunctionGraph.ValidateGraph(
                    new(current.Graph.Id, current.Graph.Signature, current.Graph.Entry,
                        current.Graph.Blocks.SetItem(block.Id, block with { Terminator = terminator })),
                    current.KnownFunctionSignatures.Add(id.Value, callee.Graph.Signature));
                if (temporary.IsOkOrNull() is not { } candidate)
                {
                    Decline(GraphOptimizationDeclineCode.InlineRefused);
                    continue;
                }
                var expanded = GraphInliner.Inline(candidate, callee, call.Site,
                    new(options.MaxExpansionUnits - stats.GraphGrowthUnits));
                if (expanded.IsOkOrNull() is not { } success)
                {
                    Decline(GraphOptimizationDeclineCode.InlineRefused, expanded.IsErrOrNull()!.Code);
                    continue;
                }
                var growth = Math.Max(0, GraphUnits(success.Graph) - GraphUnits(current.Graph));
                if (growth > options.MaxExpansionUnits - stats.GraphGrowthUnits)
                {
                    Decline(GraphOptimizationDeclineCode.BudgetExhausted);
                    continue;
                }
                // Inlining exposes caller environments and successful return operands. Reconsider
                // unknown refusals, including sites visited before their defining call was inlined.
                visited = visited.Remove(call.Site).Except(unknownVisited);
                unknownVisited = [];
                analysis = null;
                current = options.ScalarReplacement
                    ? GraphScalarReplacement.Rewrite(success, maxWorkUnits: options.MaxScalarWorkUnits,
                        maxExpansionUnits: options.MaxScalarExpansionUnits) : success;
                stats = stats with
                {
                    InlinedCalls = stats.InlinedCalls + 1,
                    SelfTailCalls = stats.SelfTailCalls + calleeLoops.Item2,
                    GraphGrowthUnits = stats.GraphGrowthUnits + growth,
                };

                void Decline(GraphOptimizationDeclineCode reason, GraphInliningDeclineCode? inlineReason = null) =>
                    stats = stats with { Declines = stats.Declines.Add(new(call.Site, reason, inlineReason)) };
            }

            LiteralValue? ResolveFact(PineVirtualValueId value)
            {
                if (analysis is null)
                {
                    analysis = GraphValueAnalysis.Analyze(current, new(
                        Math.Max(0, options.MaxAnalysisWorkUnits - stats.AnalysisWorkUnits),
                        options.MaxAnalysisDepth, options.MaxAnalysisListItems));
                    stats = stats with
                    {
                        AnalysisWorkUnits = stats.AnalysisWorkUnits + analysis.WorkUnits,
                        AnalysisPasses = stats.AnalysisPasses + 1,
                        BudgetLimitReached = stats.BudgetLimitReached || analysis.BudgetExhausted,
                    };
                }
                return analysis.ValueOf(value) switch
                {
                    GraphValueFact.Exact exact => exact.Value,
                    GraphValueFact.Unknown or GraphValueFact.List => null,
                    _ => throw new NotImplementedException("ResolveFact does not handle fact variant: " + analysis.ValueOf(value).GetType().Name),
                };
            }
        }
    }

    private static FunctionId? FreshFunctionId(ValidatedFunctionGraph input)
    {
        return Allocate();
        FunctionId? Allocate()
        {
            // The memo is not a runtime function table. Reusing request IDs there is necessary
            // for cold/warm equivalence; reserve all identities in this graph's validation context.
            long id = 0;
            while (id <= int.MaxValue && (input.Graph.Id.Value == id || input.KnownFunctionSignatures.ContainsKey(new((int)id))))
                ++id;
            return id <= int.MaxValue ? new((int)id) : null;
        }
    }

    internal static (bool Fits, long Units) MeasureLiteral(LiteralValue root, long limit, int depthLimit)
    {
        return Measure();
        (bool, long) Measure()
        {
            var pending = new Stack<(LiteralValue Value, int Depth)>();
            pending.Push((root, 1));
            long units = 0;
            while (pending.TryPop(out var item))
            {
                if (units >= limit || item.Depth > depthLimit)
                    return (false, units);
                ++units;
                switch (item.Value)
                {
                    case LiteralValue.Blob blob:
                        if (blob.Bytes.Count > limit - units)
                            return (false, limit);
                        units += blob.Bytes.Count;
                        break;
                    case LiteralValue.List list:
                        if (list.Items.Count > limit - units)
                            return (false, limit);
                        foreach (var child in list.Items.Reverse())
                            pending.Push((child, item.Depth + 1));
                        break;
                    default:
                        throw new NotImplementedException("MeasureLiteral does not handle literal variant: " + item.Value.GetType().Name);
                }
            }
            return (true, units);
        }
    }

    private static (bool Fits, long Units) MeasureBody(OwnedExpression root, long limit, int depthLimit)
    {
        return Measure();
        (bool, long) Measure()
        {
            var pending = new Stack<(OwnedExpression Expression, int Depth)>();
            pending.Push((root, 1));
            long units = 0;
            while (pending.TryPop(out var item))
            {
                if (units >= limit || item.Depth > depthLimit)
                    return (false, units);
                ++units;
                var children = item.Expression switch
                {
                    OwnedExpression.Literal or OwnedExpression.Environment => ImmutableList<OwnedExpression>.Empty,
                    OwnedExpression.List list => list.Items,
                    // The frontend's list-only head/skip form builds a generic fallback and a
                    // specialized arm. Charge duplicated subtrees before constructing that graph.
                    OwnedExpression.Builtin
                    {
                        Name: "head",
                        Input: OwnedExpression.Builtin
                        {
                            Name: "skip",
                            Input: OwnedExpression.List { Items: [var count, var source] },
                        },
                    } => [count, count, source, source],
                    OwnedExpression.Builtin builtin => [builtin.Input],
                    OwnedExpression.Conditional conditional => [conditional.Condition, conditional.FalseBranch, conditional.TrueBranch],
                    OwnedExpression.Eval eval => [eval.InvocationEnvironment, eval.Encoded],
                    OwnedExpression.Label label => [label.Tagged],
                    _ => throw new NotImplementedException("MeasureBody does not handle expression variant: " + item.Expression.GetType().Name),
                };
                if (children.Count > limit - units)
                    return (false, limit);
                foreach (var child in children.Reverse())
                    pending.Push((child, item.Depth + 1));
            }
            return (true, units);
        }
    }

    private static Call? CallOf(Terminator terminator) => terminator switch
    {
        Terminator.Return or Terminator.Jump or Terminator.Branch or Terminator.Switch => null,
        Terminator.Invoke invoke => invoke.Call,
        Terminator.TailInvoke tail => tail.Call,
        _ => throw new NotImplementedException("CallOf does not handle terminator variant: " + terminator.GetType().Name),
    };

    private static bool IsDynamic(CallTarget target) => target switch
    {
        CallTarget.Dynamic => true,
        CallTarget.Known => false,
        _ => throw new NotImplementedException("IsDynamic does not handle target variant: " + target.GetType().Name),
    };

    private static Operation.Literal? LiteralOf(Operation operation) => operation switch
    {
        Operation.Literal literal => literal,
        Operation.MakeList or Operation.Project or Operation.Builtin => null,
        _ => throw new NotImplementedException("LiteralOf does not handle operation variant: " + operation.GetType().Name),
    };

    internal static long GraphUnits(FunctionGraph graph) => graph.Blocks.Values.Sum(block =>
        1L + block.Parameters.Count + block.Operations.Sum(operation => operation switch
        {
            Operation.Literal => 2L,
            Operation.MakeList list => 2L + list.Items.Count,
            Operation.Project project => 3L + project.Path.Indices.Count,
            Operation.Builtin => 3L,
            _ => throw new NotImplementedException("GraphUnits does not handle operation variant: " + operation.GetType().Name),
        }) + (block.Terminator switch
        {
            Terminator.Return ret => 1L + ret.Values.Count,
            Terminator.Jump jump => 2L + jump.Edge.Arguments.Count,
            Terminator.Branch branch => 4L + branch.IfEqual.Arguments.Count + branch.IfNotEqual.Arguments.Count,
            Terminator.Switch selection => 3L + selection.Default.Arguments.Count + selection.Cases.Sum(item => 2L + item.Edge.Arguments.Count),
            Terminator.Invoke invoke => 4L + invoke.Call.Arguments.Count + invoke.Continuation.Bindings.Count,
            Terminator.TailInvoke tail => 3L + tail.Call.Arguments.Count,
            _ => throw new NotImplementedException("GraphUnits does not handle terminator variant: " + block.Terminator.GetType().Name),
        }));
}
