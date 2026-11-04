using System;

namespace Pine.Core.Elm.Testing;

/// <summary>Wall-clock phase durations, with optional nested discovery timings and summed VM calls.</summary>
public sealed record ElmTestRunDurations
{
    /// <summary>Source loading, test-root identification, parsing and dependency resolution.</summary>
    public TimeSpan? SourceResolution { get; init; }

    /// <summary>Compilation of the complete environment for compiler/PineVM discovery only.</summary>
    public TimeSpan? EnvironmentCompilation { get; init; }

    /// <summary>Test construction, runner seeding, inventory projection and selection.</summary>
    public TimeSpan? Discovery { get; init; }

    /// <summary>Canonicalization, interpreter preparation and source-observer setup within discovery.</summary>
    public TimeSpan? DeclarationPreparation { get; init; }

    /// <summary>Root interpretation, test-tree traversal and runner seeding within discovery.</summary>
    public TimeSpan? TestConstruction { get; init; }

    /// <summary>Global selection, selectors, source navigation and inventory indexing within discovery.</summary>
    public TimeSpan? InventoryProjection { get; init; }

    /// <summary>Export and compilation of selected callbacks and captures to Pine, including reseeding when needed.</summary>
    public TimeSpan? RunnerCompilation { get; init; }

    /// <summary>Wall-clock time spent running selected tests and decoding results.</summary>
    public TimeSpan? Execution { get; init; }

    /// <summary>Sum of PineVM calls during compiler-based discovery; absent for syntax discovery.</summary>
    public TimeSpan? DiscoveryVm { get; init; }

    /// <summary>Sum of PineVM calls during execution; parallel calls can overlap.</summary>
    public TimeSpan? ExecutionVm { get; init; }

    /// <summary>Sum of nonoverlapping top-level phases, excluding nested timings and VM sums.</summary>
    public TimeSpan Total =>
        (SourceResolution ?? TimeSpan.Zero) + (EnvironmentCompilation ?? TimeSpan.Zero) +
        (Discovery ?? TimeSpan.Zero) + (RunnerCompilation ?? TimeSpan.Zero) + (Execution ?? TimeSpan.Zero);
}
