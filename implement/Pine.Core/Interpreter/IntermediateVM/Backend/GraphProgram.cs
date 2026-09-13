using Pine.Core.Interpreter.IntermediateVM.Semantic;
using System;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.Core.Interpreter.IntermediateVM.Backend;

/// <summary>
/// Immutable ID-based publication boundary. Recursion requires no mutable instruction links.
/// Known calls accept exactly the declared projected arguments, not a reconstructed environment.
/// </summary>
public sealed record GraphProgram
{
    /// <summary>Functions indexed by their semantic identity.</summary>
    public ImmutableDictionary<FunctionId, GraphFunction> Functions { get; }

    /// <summary>Checks all known call references and ABI contracts before publication.</summary>
    public GraphProgram(ImmutableDictionary<FunctionId, GraphFunction> functions)
    {
        Functions = functions.WithComparers(System.Collections.Generic.EqualityComparer<FunctionId>.Default,
            System.Collections.Generic.EqualityComparer<GraphFunction>.Default);
        if (Functions.Any(pair => pair.Key != pair.Value.Id))
            throw new ArgumentException("Function table keys must match function identities.", nameof(functions));
        if (Functions.Values.SelectMany(function => function.Blocks).SelectMany(block => Calls(block.Terminator))
            .Any(call => !Compatible(call, Functions)))
            throw new ArgumentException("Known call target is absent or has an incompatible signature.", nameof(functions));
    }

    private static ImmutableList<SelectedCall> Calls(SelectedTerminator terminator) =>
        terminator switch
        {
            SelectedTerminator.Return => [],
            SelectedTerminator.Jump => [],
            SelectedTerminator.Match => [],
            SelectedTerminator.Invoke invoke => [invoke.Call],
            SelectedTerminator.TailInvoke invoke => [invoke.Call],
            _ => throw new NotImplementedException("Calls does not handle terminator variant: " + terminator.GetType().Name),
        };

    private static bool Compatible(SelectedCall call, ImmutableDictionary<FunctionId, GraphFunction> functions) =>
        call.Target switch
        {
            SelectedCallTarget.Dynamic => true,
            SelectedCallTarget.Known known => functions.ContainsKey(known.Function) &&
                functions[known.Function].Signature == call.Signature,
            _ => throw new NotImplementedException("Compatible does not handle target variant: " + call.Target.GetType().Name),
        };

    /// <inheritdoc/>
    public bool Equals(GraphProgram? other) =>
        other is not null && Functions.Count == other.Functions.Count &&
        Functions.All(pair => other.Functions.ContainsKey(pair.Key) && other.Functions[pair.Key] == pair.Value);
    /// <inheritdoc/>
    public override int GetHashCode() =>
        ModelEquality.SequenceHash(Functions.OrderBy(pair => pair.Key.Value).ToImmutableList());
}

/// <summary>Runtime call metadata containing only an immutable identity and ABI shape.</summary>
public sealed record GraphInvocation(FunctionId Function, int ArgumentCount, bool IsTail);
