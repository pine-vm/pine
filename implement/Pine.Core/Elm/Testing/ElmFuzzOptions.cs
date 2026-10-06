using System;
using System.Collections.Generic;
using System.Security.Cryptography;

namespace Pine.Core.Elm.Testing;

/// <summary>elm-test-rs compatible execution options, independent of dependency resolution and compilation.</summary>
public sealed record ElmFuzzOptions
{
    /// <summary>Number of iterations per fuzz test. Test.fuzzWith may explicitly override this value.</summary>
    public uint Runs { get; init; } = 100;

    /// <summary>Initial unsigned 32-bit seed; null chooses a new seed once per invocation.</summary>
    public uint? Seed { get; init; }

    internal ElmTestExecutionSettings Resolve()
    {
        if (Runs == 0)
        {
            throw new ArgumentOutOfRangeException(
                nameof(Runs),
                "The --fuzz value must be a positive unsigned 32-bit integer.");
        }

        return new(Seed ?? BitConverter.ToUInt32(RandomNumberGenerator.GetBytes(sizeof(uint))), Runs);
    }
}

/// <summary>The effective, reproducible invocation settings.</summary>
public sealed record ElmTestExecutionSettings(uint Seed, uint FuzzRuns);

/// <summary>Execution information retained for a fuzz property, including the random-choice tapes used in shrinking.</summary>
public sealed record ElmFuzzResult(
    ElmTestExecutionSettings Settings,
    string EffectiveSeedState,
    uint RunsRequested)
{
    /// <summary>Number of generated examples tested, when the engine returned normally.</summary>
    public uint? RunsElapsed { get; init; }

    /// <summary>One-based failing iteration, or null for passing/invalid-generator/evaluation-error results.</summary>
    public uint? FailingIteration { get; init; }

    /// <summary>Original failing value in Elm notation.</summary>
    public string? OriginalInput { get; init; }

    /// <summary>Final failing value after upstream simplification, not a claim of a global minimum.</summary>
    public string? ShrunkInput { get; init; }

    /// <summary>Recorded choices that generated the original failing example.</summary>
    public IReadOnlyList<long> OriginalChoices { get; init; } = [];

    /// <summary>Recorded choices that replay the final simplified example.</summary>
    public IReadOnlyList<long> ShrunkChoices { get; init; } = [];

    /// <summary>True only when the shrinking engine completed normally for a failing example.</summary>
    public bool ShrinkingCompleted { get; init; }

    /// <summary>Underlying VM error, including resource exhaustion, distinct from an expectation failure.</summary>
    public string? EvaluationError { get; init; }

    /// <summary>Upstream failure category, including InvalidFuzzer and distribution failures.</summary>
    public string? FailureReason { get; init; }

    /// <summary>Full upstream distribution metadata in Elm notation.</summary>
    public string? DistributionReport { get; init; }
}
