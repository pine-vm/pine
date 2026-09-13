using System;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.Core.Interpreter.IntermediateVM.Semantic;

/// <summary>Identity-based self-tail elimination, without expression specialization or runtime changes.</summary>
public static class GraphSelfTailLoops
{
    /// <summary>
    /// Replaces only known, contract-matching root tail calls with simultaneous entry bindings.
    /// All operations remain in place. Backend edge stubs retain cancellation and layout-sensitive
    /// negative-backjump quota accounting; this pass adds no runtime safety metadata.
    /// </summary>
    public static (ValidatedFunctionGraph Graph, int RewrittenCalls) Rewrite(ValidatedFunctionGraph input)
    {
        var replacements = input.Graph.Blocks.Values.Where(block => IsSelfTail(block.Terminator, input.Graph)).ToImmutableList();
        if (replacements.Count == 0)
            return (input, 0);

        var blocks = replacements.Aggregate(input.Graph.Blocks, (current, block) =>
            current.SetItem(block.Id, block with
            {
                Terminator = new Terminator.Jump(new(input.Graph.Entry, ((Terminator.TailInvoke)block.Terminator).Call.Arguments)),
            }));
        var validated = ValidatedFunctionGraph.ValidateGraph(
            new(input.Graph.Id, input.Graph.Signature, input.Graph.Entry, blocks), input.KnownFunctionSignatures)
            .Extract(errors => throw new InvalidOperationException(string.Join(", ", errors)));
        return (validated, replacements.Count);
    }

    private static bool IsSelfTail(Terminator terminator, FunctionGraph graph) => terminator switch
    {
        Terminator.Return or Terminator.Jump or Terminator.Branch or Terminator.Switch or Terminator.Invoke => false,
        Terminator.TailInvoke tail => tail.Call.Target switch
        {
            CallTarget.Dynamic => false,
            CallTarget.Known known => known.Function == graph.Id && tail.Call.Signature == graph.Signature,
            _ => throw new NotImplementedException("IsSelfTail does not handle target variant: " + tail.Call.Target.GetType().Name),
        },
        _ => throw new NotImplementedException("IsSelfTail does not handle terminator variant: " + terminator.GetType().Name),
    };
}
