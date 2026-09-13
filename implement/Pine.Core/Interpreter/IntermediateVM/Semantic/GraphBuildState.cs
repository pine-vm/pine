using System;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.Core.Interpreter.IntermediateVM.Semantic;

/// <summary>
/// An incomplete block: no terminator and no implicit successor. It is deliberately not a BasicBlock.
/// </summary>
public sealed record BlockFragment(
    PineBlockId Id,
    ImmutableList<ValueDefinition> Parameters,
    ImmutableList<Operation> Operations)
{
    /// <inheritdoc/>
    public bool Equals(BlockFragment? other) =>
        other is not null && Id == other.Id &&
        Parameters.SequenceEqual(other.Parameters) && Operations.SequenceEqual(other.Operations);

    /// <inheritdoc/>
    public override int GetHashCode() =>
        HashCode.Combine(Id, ModelEquality.SequenceHash(Parameters), ModelEquality.SequenceHash(Operations));
}

/// <summary>
/// Persistent construction state. Each transition returns a new state; a completed block is still
/// unvalidated. This API neither creates a validated graph nor accepts arbitrary preallocated blocks.
/// </summary>
public sealed record GraphBuildState
{
    /// <summary>Next block identity in this function.</summary>
    public int NextBlockId { get; private init; }

    /// <summary>Next value identity in this function.</summary>
    public int NextValueId { get; private init; }

    /// <summary>Next call occurrence identity in this function.</summary>
    public int NextCallSiteId { get; private init; }

    /// <summary>Completed block shapes, not validated blocks.</summary>
    public ImmutableDictionary<PineBlockId, BasicBlock> CompletedBlocks { get; private init; } =
        [];

    /// <summary>The optional open, unterminated block.</summary>
    public BlockFragment? Fragment { get; private init; }

    private GraphBuildState() { }

    /// <summary>An empty function-local allocation namespace.</summary>
    public static GraphBuildState Empty { get; } = new();

    /// <summary>Reserves a block identity, including for a forward edge.</summary>
    public (GraphBuildState State, PineBlockId Id) AllocateBlock() =>
        (this with { NextBlockId = checked(NextBlockId + 1) }, new(NextBlockId));

    /// <summary>Reserves a value definition identity.</summary>
    public (GraphBuildState State, PineVirtualValueId Id) AllocateValue() =>
        (this with { NextValueId = checked(NextValueId + 1) }, new(NextValueId));

    /// <summary>Reserves an invocation occurrence identity.</summary>
    public (GraphBuildState State, CallSiteId Id) AllocateCallSite() =>
        (this with { NextCallSiteId = checked(NextCallSiteId + 1) }, new(NextCallSiteId));

    /// <summary>Opens a reserved block. Misuse of the construction protocol is rejected.</summary>
    public GraphBuildState OpenBlock(PineBlockId id, ImmutableList<ValueDefinition> parameters) =>
        Fragment is not null
        ? throw new InvalidOperationException("OpenBlock requires completing the current fragment first.")
        : id.Value < 0 || id.Value >= NextBlockId || CompletedBlocks.ContainsKey(id)
        ? throw new InvalidOperationException("OpenBlock requires a reserved, uncompleted block ID.")
        : this with { Fragment = new(id, parameters, []) };

    /// <summary>Appends one operation without changing the supplied state or fragment.</summary>
    public GraphBuildState AppendOperation(Operation operation) =>
        Fragment is { } fragment
        ? this with { Fragment = fragment with { Operations = fragment.Operations.Add(operation) } }
        : throw new InvalidOperationException("AppendOperation requires an open fragment.");

    /// <summary>Closes a fragment with an explicit terminator, without validating its contents.</summary>
    public GraphBuildState CompleteBlock(Terminator terminator) =>
        Fragment is { } fragment
        ? this with
        {
            CompletedBlocks = CompletedBlocks.Add(
                fragment.Id, new(fragment.Id, fragment.Parameters, fragment.Operations, terminator)),
            Fragment = null,
        }
        : throw new InvalidOperationException("CompleteBlock requires an open fragment.");

    /// <inheritdoc/>
    public bool Equals(GraphBuildState? other) =>
        other is not null && NextBlockId == other.NextBlockId && NextValueId == other.NextValueId &&
        NextCallSiteId == other.NextCallSiteId && Fragment == other.Fragment &&
        ModelEquality.BlocksEqual(CompletedBlocks, other.CompletedBlocks);

    /// <inheritdoc/>
    public override int GetHashCode() =>
        HashCode.Combine(NextBlockId, NextValueId, NextCallSiteId, Fragment,
            ModelEquality.BlocksHash(CompletedBlocks));
}
