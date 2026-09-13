using System;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.Core.Interpreter.IntermediateVM.Semantic;

/// <summary>
/// Bounds one expansion, not recursive discovery. Units count cloned blocks, definitions,
/// operations, transfers, calls, edges and operand/binding slots, including capture plumbing.
/// Shared immutable literals and signatures are not copied or charged.
/// </summary>
public sealed record GraphInliningBudget(long MaxExpansionUnits);

/// <summary>Reasons an explicit call-site expansion leaves the caller unchanged.</summary>
public enum GraphInliningDeclineCode
{
    /// <summary>The caller contains no call with the requested identity.</summary>
    MissingCallSite,
    /// <summary>A dynamic target has no established graph identity.</summary>
    DynamicTarget,
    /// <summary>The supplied body has a different identity from the selected target.</summary>
    TargetMismatch,
    /// <summary>The selected call and supplied body declare different contracts.</summary>
    SignatureMismatch,
    /// <summary>The two validation contexts disagree about a function contract.</summary>
    SignatureConflict,
    /// <summary>Distinct bodies or specializations sharing the root identity are not proven equivalent.</summary>
    RecursiveIdentity,
    /// <summary>The requested expansion exceeds its immutable budget.</summary>
    BudgetExhausted,
    /// <summary>No fresh representable identity remains.</summary>
    IdSpaceExhausted,
    /// <summary>The constructed graph did not pass structural validation.</summary>
    InvalidRewrite,
}

/// <summary>A structured refusal retaining the exact, unchanged validation evidence.</summary>
public sealed record GraphInliningDecline(
    GraphInliningDeclineCode Code,
    ValidatedFunctionGraph UnchangedCaller,
    ImmutableList<GraphDiagnostic> Diagnostics);

/// <summary>
/// Explicit graph substitution only: no target discovery, specialization, runtime changes or
/// recursive expansion. The caller must authorize the supplied body as the definition of its
/// known function identity and contract. Structural validation and signature agreement alone
/// do not prove specialized-body applicability; preparation specialization facts are not
/// represented by FunctionGraph and are neither inferred nor consumed here.
/// </summary>
public static class GraphInliner
{
    /// <summary>
    /// Expands exactly one known call, including cyclic bodies and entry backedges. Residual calls
    /// retain their original function identities. Success certifies exactly the returned graph.
    /// </summary>
    public static Result<GraphInliningDecline, ValidatedFunctionGraph> Inline(
        ValidatedFunctionGraph caller,
        ValidatedFunctionGraph callee,
        CallSiteId site,
        GraphInliningBudget budget)
    {
        return Rewrite();

        Result<GraphInliningDecline, ValidatedFunctionGraph> Rewrite()
        {
            var source = caller.Graph.Blocks.Values.OrderBy(block => block.Id.Value)
                .FirstOrDefault(block => CallOf(block.Terminator)?.Site == site);
            if (source is null)
                return Decline(GraphInliningDeclineCode.MissingCallSite);

            var call = CallOf(source.Terminator)!;
            switch (call.Target)
            {
                case CallTarget.Dynamic:
                    return Decline(GraphInliningDeclineCode.DynamicTarget);
                case CallTarget.Known known:
                    if (known.Function != callee.Graph.Id)
                        return Decline(GraphInliningDeclineCode.TargetMismatch);
                    break;
                default:
                    throw new NotImplementedException("Inline does not handle target variant: " + call.Target.GetType().Name);
            }
            if (call.Signature != callee.Graph.Signature)
                return Decline(GraphInliningDeclineCode.SignatureMismatch);
            if (caller.Graph.Id == callee.Graph.Id)
                return Decline(GraphInliningDeclineCode.RecursiveIdentity);

            // A root contract may be absent from its validation table. Once cloned, callee
            // self references are no longer root references and must resolve explicitly.
            var signatures = caller.KnownFunctionSignatures.SetItem(caller.Graph.Id, caller.Graph.Signature);
            foreach (var dependency in callee.KnownFunctionSignatures
                .SetItem(callee.Graph.Id, callee.Graph.Signature).OrderBy(pair => pair.Key.Value))
            {
                if (signatures.TryGetValue(dependency.Key, out var previous) && previous != dependency.Value)
                    return Decline(GraphInliningDeclineCode.SignatureConflict);
                signatures = signatures.SetItem(dependency.Key, dependency.Value);
            }

            var continuation = source.Terminator is Terminator.Invoke invoke ? invoke.Continuation : null;
            var captured = (continuation?.Bindings ?? []).SelectMany(binding => binding switch
            {
                ContinuationBinding.CallerValue value => ImmutableList.Create(value.Value),
                ContinuationBinding.ReturnedResult => [],
                _ => throw new NotImplementedException("Inline does not handle binding variant: " + binding.GetType().Name),
            }).Distinct().OrderBy(value => value.Value).ToImmutableList();
            var captureIndices = captured.Select((value, index) => (value, index))
                .ToImmutableDictionary(pair => pair.value, pair => pair.index);
            var ordered = callee.Graph.Blocks.Values.OrderBy(block => block.Id.Value).ToImmutableList();
            var expansionUnits = 2L + call.Arguments.Count + captured.Count +
                ordered.Sum(block => 1L + block.Parameters.Count + captured.Count +
                    block.Operations.Sum(OperationUnits) + TerminatorUnits(block.Terminator));
            if (budget.MaxExpansionUnits < expansionUnits)
                return Decline(GraphInliningDeclineCode.BudgetExhausted);

            try
            {
                var allBlocks = caller.Graph.Blocks.Values.Concat(ordered).ToImmutableList();
                var reservedBlocks = allBlocks.Select(block => block.Id.Value).ToImmutableHashSet();
                var reservedValues = allBlocks.SelectMany(Definitions).Select(value => value.Id.Value).ToImmutableHashSet();
                var reservedCalls = allBlocks.Select(block => CallOf(block.Terminator))
                    .OfType<Call>().Select(item => item.Site.Value).ToImmutableHashSet();
                long nextBlock = 0;
                long nextValue = 0;
                long nextCall = 0;
                var blocks = ordered.ToImmutableDictionary(block => block.Id, _ => new PineBlockId(AllocateBlock()));
                var values = ordered.SelectMany(Definitions)
                    .ToImmutableDictionary(value => value.Id, value => new ValueDefinition(new(AllocateValue()), value.Type));
                var calls = ordered.Select(block => CallOf(block.Terminator)).OfType<Call>()
                    .ToImmutableDictionary(item => item.Site, _ => new CallSiteId(AllocateCall()));
                var captures = ordered.ToImmutableDictionary(block => block.Id,
                    _ => captured.Select(_ => new ValueDefinition(new(AllocateValue()))).ToImmutableList());
                var rewritten = caller.Graph.Blocks.ToBuilder();

                foreach (var block in ordered)
                {
                    var localCaptures = captures[block.Id].Select(value => value.Id).ToImmutableList();
                    rewritten.Add(blocks[block.Id], new(
                        blocks[block.Id],
                        block.Parameters.Select(value => values[value.Id]).ToImmutableList().AddRange(captures[block.Id]),
                        block.Operations.Select(CloneOperation).ToImmutableList(),
                        CloneTerminator(block.Terminator, localCaptures)));
                }
                rewritten[source.Id] = source with
                {
                    Terminator = new Terminator.Jump(new(blocks[callee.Graph.Entry], call.Arguments.AddRange(captured))),
                };
                var graph = new FunctionGraph(caller.Graph.Id, caller.Graph.Signature, caller.Graph.Entry, rewritten.ToImmutable());
                return ValidatedFunctionGraph.ValidateGraph(graph, signatures)
                    .MapError(errors => new GraphInliningDecline(GraphInliningDeclineCode.InvalidRewrite, caller, errors));

                int AllocateBlock()
                {
                    while (nextBlock <= int.MaxValue && reservedBlocks.Contains((int)nextBlock))
                        ++nextBlock;
                    return checked((int)nextBlock++);
                }
                int AllocateValue()
                {
                    while (nextValue <= int.MaxValue && reservedValues.Contains((int)nextValue))
                        ++nextValue;
                    return checked((int)nextValue++);
                }
                int AllocateCall()
                {
                    while (nextCall <= int.MaxValue && reservedCalls.Contains((int)nextCall))
                        ++nextCall;
                    return checked((int)nextCall++);
                }
                PineVirtualValueId Value(PineVirtualValueId value) => values[value].Id;
                ImmutableList<PineVirtualValueId> Values(ImmutableList<PineVirtualValueId> operands) =>
                    operands.Select(Value).ToImmutableList();
                Operation CloneOperation(Operation operation) => operation switch
                {
                    Operation.Literal literal => new Operation.Literal(values[literal.Result.Id], literal.Value),
                    Operation.MakeList list => new Operation.MakeList(values[list.Result.Id], Values(list.Items)),
                    Operation.Project project => new Operation.Project(values[project.Result.Id], Value(project.Source), project.Path),
                    Operation.Builtin builtin => new Operation.Builtin(values[builtin.Result.Id], builtin.Name, Value(builtin.Argument)),
                    _ => throw new NotImplementedException("CloneOperation does not handle operation variant: " + operation.GetType().Name),
                };
                Call CloneCall(Call original) => new(calls[original.Site], original.Target switch
                {
                    CallTarget.Dynamic dynamic => new CallTarget.Dynamic(Value(dynamic.EncodedExpression)),
                    CallTarget.Known known => new CallTarget.Known(known.Function),
                    _ => throw new NotImplementedException("CloneCall does not handle target variant: " + original.Target.GetType().Name),
                }, original.Signature, Values(original.Arguments));
                Terminator ReturnToCaller(ImmutableList<PineVirtualValueId> results, ImmutableList<PineVirtualValueId> localCaptures) =>
                    continuation is null ? new Terminator.Return(results) :
                    new Terminator.Jump(new(continuation.Target, continuation.Bindings.Select(binding => binding switch
                    {
                        ContinuationBinding.CallerValue value => localCaptures[captureIndices[value.Value]],
                        ContinuationBinding.ReturnedResult result => results[result.Index],
                        _ => throw new NotImplementedException("ReturnToCaller does not handle binding variant: " + binding.GetType().Name),
                    }).ToImmutableList()));
                Terminator CloneTerminator(Terminator terminator, ImmutableList<PineVirtualValueId> localCaptures)
                {
                    switch (terminator)
                    {
                        case Terminator.Return ret:
                            return ReturnToCaller(Values(ret.Values), localCaptures);
                        case Terminator.Jump jump:
                            return new Terminator.Jump(CloneEdge(jump.Edge));
                        case Terminator.Branch branch:
                            return new Terminator.Branch(Value(branch.TestedValue), branch.Literal,
                                CloneEdge(branch.IfEqual), CloneEdge(branch.IfNotEqual));
                        case Terminator.Switch selection:
                            return new Terminator.Switch(Value(selection.Selector),
                                selection.Cases.Select(item => new SwitchCase(item.Value, CloneEdge(item.Edge))).ToImmutableList(),
                                CloneEdge(selection.Default));
                        case Terminator.Invoke inner:
                            return new Terminator.Invoke(CloneCall(inner.Call), new(blocks[inner.Continuation.Target],
                                inner.Continuation.Bindings.Select(binding => binding switch
                                {
                                    ContinuationBinding.CallerValue value => (ContinuationBinding)new ContinuationBinding.CallerValue(Value(value.Value)),
                                    ContinuationBinding.ReturnedResult result => new ContinuationBinding.ReturnedResult(result.Index),
                                    _ => throw new NotImplementedException("CloneTerminator does not handle binding variant: " + binding.GetType().Name),
                                }).ToImmutableList().AddRange(localCaptures.Select(value => new ContinuationBinding.CallerValue(value)))));
                        case Terminator.TailInvoke tail:
                            if (continuation is null)
                                return new Terminator.TailInvoke(CloneCall(tail.Call));
                            var returnBlock = new PineBlockId(AllocateBlock());
                            var returned = tail.Call.Signature.Results.Select(type => new ValueDefinition(new(AllocateValue()), type)).ToImmutableList();
                            var preserved = localCaptures.Select(_ => new ValueDefinition(new(AllocateValue()))).ToImmutableList();
                            rewritten.Add(returnBlock, new(returnBlock, returned.AddRange(preserved), [],
                                ReturnToCaller(returned.Select(value => value.Id).ToImmutableList(), preserved.Select(value => value.Id).ToImmutableList())));
                            return new Terminator.Invoke(CloneCall(tail.Call), new(returnBlock,
                                returned.Select((_, index) => (ContinuationBinding)new ContinuationBinding.ReturnedResult(index)).ToImmutableList()
                                    .AddRange(localCaptures.Select(value => new ContinuationBinding.CallerValue(value)))));
                        default:
                            throw new NotImplementedException("CloneTerminator does not handle terminator variant: " + terminator.GetType().Name);
                    }

                    Edge CloneEdge(Edge edge) => new(blocks[edge.Target], Values(edge.Arguments).AddRange(localCaptures));
                }
            }
            catch (OverflowException)
            {
                return Decline(GraphInliningDeclineCode.IdSpaceExhausted);
            }

            long EdgeUnits(Edge edge) => 1L + edge.Arguments.Count + captured.Count;
            long TerminatorUnits(Terminator terminator) => terminator switch
            {
                Terminator.Return ret => 1L + (continuation is null ? ret.Values.Count : 1L + continuation.Bindings.Count),
                Terminator.Jump jump => 1L + EdgeUnits(jump.Edge),
                Terminator.Branch branch => 2L + EdgeUnits(branch.IfEqual) + EdgeUnits(branch.IfNotEqual),
                Terminator.Switch selection => 2L + selection.Cases.Sum(item => 1L + EdgeUnits(item.Edge)) + EdgeUnits(selection.Default),
                Terminator.Invoke inner => 2L + CallUnits(inner.Call) + inner.Continuation.Bindings.Count + captured.Count,
                Terminator.TailInvoke tail => 1L + CallUnits(tail.Call) + (continuation is null ? 0 :
                    4L + 2L * (tail.Call.Signature.Results.Count + captured.Count) + continuation.Bindings.Count),
                _ => throw new NotImplementedException("TerminatorUnits does not handle terminator variant: " + terminator.GetType().Name),
            };
        }

        Result<GraphInliningDecline, ValidatedFunctionGraph> Decline(GraphInliningDeclineCode code) =>
            Result<GraphInliningDecline, ValidatedFunctionGraph>.err(new(code, caller, []));
    }

    private static Call? CallOf(Terminator terminator) => terminator switch
    {
        Terminator.Return => null,
        Terminator.Jump => null,
        Terminator.Branch => null,
        Terminator.Switch => null,
        Terminator.Invoke invoke => invoke.Call,
        Terminator.TailInvoke tail => tail.Call,
        _ => throw new NotImplementedException("CallOf does not handle terminator variant: " + terminator.GetType().Name),
    };

    private static ImmutableList<ValueDefinition> Definitions(BasicBlock block) =>
        block.Parameters.AddRange(block.Operations.Select(operation => operation switch
        {
            Operation.Literal literal => literal.Result,
            Operation.MakeList list => list.Result,
            Operation.Project project => project.Result,
            Operation.Builtin builtin => builtin.Result,
            _ => throw new NotImplementedException("Definitions does not handle operation variant: " + operation.GetType().Name),
        }));

    private static long OperationUnits(Operation operation) => operation switch
    {
        Operation.Literal => 2,
        Operation.MakeList list => 2L + list.Items.Count,
        Operation.Project => 3,
        Operation.Builtin => 3,
        _ => throw new NotImplementedException("OperationUnits does not handle operation variant: " + operation.GetType().Name),
    };

    private static long CallUnits(Call call) => 2L + call.Arguments.Count + (call.Target switch
    {
        CallTarget.Dynamic => 1,
        CallTarget.Known => 0,
        _ => throw new NotImplementedException("CallUnits does not handle target variant: " + call.Target.GetType().Name),
    });

}
