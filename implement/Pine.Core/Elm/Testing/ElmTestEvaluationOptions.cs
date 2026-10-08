using System;

namespace Pine.Core.Elm.Testing;

/// <summary>Shared command-wide work limits and cooperative timeout for Elm test execution.</summary>
public record ElmTestEvaluationOptions
{
    /// <summary>Command-wide invocation limit, or null for unbounded invocation work.</summary>
    public int? InvocationBudget { get; init; }

    /// <summary>Command-wide backward-jump limit, or null for unbounded loop work.</summary>
    public int? LoopBudget { get; init; }

    /// <summary>Cooperative execution deadline, or null for no deadline.</summary>
    public TimeSpan? Timeout { get; init; }

    /// <summary>Maximum VM stack depth, independent of command-wide work budgets.</summary>
    public int StackDepthLimit { get; init; } = 100_000;

    /// <summary>Whether these limits differ from uncapped execution with the default stack guard.</summary>
    public bool HasCustomLimits =>
        InvocationBudget is not null || LoopBudget is not null || Timeout is not null || StackDepthLimit != 100_000;

    /// <summary>Rejects nonpositive work limits and deadlines outside the timer's supported range.</summary>
    public virtual void Validate()
    {
        if (InvocationBudget is <= 0 || LoopBudget is <= 0)
            throw new ArgumentException("Invocation and loop budgets must be positive.");

        if (Timeout is { } timeout && (timeout <= TimeSpan.Zero || timeout.TotalMilliseconds > uint.MaxValue - 1))
            throw new ArgumentException("Timeout must be positive and fit the timer range.");

        if (StackDepthLimit <= 0)
            throw new ArgumentException("Stack depth must be positive.");
    }

    /// <summary>Copies the shared limits into options for profiling or nonprofiling budget tracking.</summary>
    public ElmTestInstrumentationOptions ToInstrumentationOptions() =>
        new()
        {
            InvocationBudget = InvocationBudget,
            LoopBudget = LoopBudget,
            Timeout = Timeout,
            StackDepthLimit = StackDepthLimit,
        };
}
