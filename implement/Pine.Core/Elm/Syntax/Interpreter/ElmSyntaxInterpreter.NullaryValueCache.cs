using Pine.Core.CodeAnalysis;
using Pine.Core.Internal;
using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Runtime.CompilerServices;
using System.Threading;

namespace Pine.Core.Elm.Syntax.Interpreter;

public partial class ElmSyntaxInterpreter
{
    // Only interpreter-owned resolver compositions carry this capability. An arbitrary delegate
    // (including a composition with custom resolvers) is not assumed to be referentially transparent.
    private sealed class DefaultApplicationResolver(
        Func<Application, ApplicationResolution> resolve,
        Prepared program,
        bool enableDefaultBuiltins = true)
    {
        public Prepared Program { get; } = program;

        public bool EnableDefaultBuiltins { get; } = enableDefaultBuiltins;

        public ApplicationResolution Resolve(Application application) => resolve(application);
    }

    private readonly record struct NullaryDeclarationKey(
        DeclQualifiedName Name,
        PreparedFunctionImplementation Implementation)
    {
        public bool Equals(NullaryDeclarationKey other) =>
            Name == other.Name && ReferenceEquals(Implementation, other.Implementation);

        public override int GetHashCode() =>
            HashCode.Combine(Name, RuntimeHelpers.GetHashCode(Implementation));
    }

    private sealed record CompletedNullaryValue(
        PineValueInProcess Value,
        NullaryObservation[] Observations);

    private readonly record struct NullaryObservation(
        Action<PineValueInProcess>? Observer,
        PineValueInProcess? Value,
        CompletedNullaryValue? Replayed);

    private sealed class NullaryValueStores
    {
        public NullaryOperatorStores WithBuiltins { get; } = new();

        public NullaryOperatorStores WithoutBuiltins { get; } = new();
    }

    private sealed class NullaryOperatorStores
    {
        public NullaryObserverStores WithoutOperators { get; } = new();

        public ConditionalWeakTable<IReadOnlyDictionary<string, DeclQualifiedName>, NullaryObserverStores> WithOperators { get; } =
            new();
    }

    private sealed class NullaryObserverStores
    {
        public ConcurrentDictionary<NullaryDeclarationKey, CompletedNullaryValue> WithoutObservers { get; } = new();

        public ConditionalWeakTable<IReadOnlyDictionary<PreparedExpression, Action<PineValueInProcess>>,
            ConcurrentDictionary<NullaryDeclarationKey, CompletedNullaryValue>> WithObservers
        { get; } = new();
    }

    private static readonly ConditionalWeakTable<IReadOnlyDictionary<DeclQualifiedName, PreparedDeclaration>, NullaryValueStores>
        s_nullaryValueStores = new();

    private static NullaryValueCache? CreateNullaryValueCacheExecution(
        Func<Application, ApplicationResolution> resolver,
        IReadOnlyDictionary<string, DeclQualifiedName>? operators,
        EvaluationConfig config)
    {
        if (!config.CacheNullaryValues || config.ApplicationCache is not null ||
            resolver.Target is not DefaultApplicationResolver owner ||
            resolver.GetInvocationList().Length is not 1)
        {
            return null;
        }

        var stores =
            s_nullaryValueStores.GetValue(GetDeclarationSnapshot(owner.Program.Declarations), static _ => new());

        var operatorStores = owner.EnableDefaultBuiltins ? stores.WithBuiltins : stores.WithoutBuiltins;

        var observerStores =
            operators is null
            ?
            operatorStores.WithoutOperators
            :
            operatorStores.WithOperators.GetValue(operators, static _ => new());

        var values =
            config.ExpressionObservers is null
            ?
            observerStores.WithoutObservers
            :
            observerStores.WithObservers.GetValue(config.ExpressionObservers, static _ => new());

        return new(values, config.CancellationToken);
    }

    private sealed class NullaryValueCache(
        ConcurrentDictionary<NullaryDeclarationKey, CompletedNullaryValue> completed,
        CancellationToken cancellationToken)
    {
        private readonly Dictionary<NullaryDeclarationKey, Entry> _entries = [];

        private readonly List<Entry> _pending = [];

        public sealed class Entry
        {
            public required NullaryDeclarationKey Key { get; init; }

            public CompletedNullaryValue? Completed { get; set; }

            public List<NullaryObservation> Observations { get; } = [];
        }

        public Entry? Find(ApplicationResolution.ContinueWithFunction resolution)
        {
            var key = new NullaryDeclarationKey(resolution.ResolvedName!, resolution.Function);

            if (_entries.TryGetValue(key, out var entry))
                return entry;

            if (!completed.TryGetValue(key, out var value))
                return null;

            entry = new() { Key = key, Completed = value };
            _entries.Add(key, entry);
            return entry;
        }

        public Entry Begin(ApplicationResolution.ContinueWithFunction resolution)
        {
            var entry = new Entry() { Key = new(resolution.ResolvedName!, resolution.Function) };
            _entries.Add(entry.Key, entry);
            _pending.Add(entry);
            return entry;
        }

        public void Complete(Entry entry, PineValueInProcess value)
        {
            if (!ReferenceEquals(_pending[^1], entry))
                throw new InvalidOperationException("Nullary evaluation completed out of order.");

            _pending.RemoveAt(_pending.Count - 1);

            cancellationToken.ThrowIfCancellationRequested();
            entry.Completed = new(value, [.. entry.Observations]);
            entry.Observations.Clear();
            // Publish only completed immutable snapshots. Concurrent misses may compute independently;
            // sharing pending entries would deadlock mutually recursive declarations across evaluations.
            completed.TryAdd(entry.Key, entry.Completed);
        }

        public void Observe(Action<PineValueInProcess> observer, PineValueInProcess value)
        {
            for (var index = 0; index < _pending.Count; index++)
            {
                var entry = _pending[index];

                entry.Observations.Add(new(observer, value, null));
            }

            observer(value);
        }

        public void Replay(CompletedNullaryValue value)
        {
            cancellationToken.ThrowIfCancellationRequested();

            if (value.Observations.Length is 0)
                return;

            // Retain a shared replay segment instead of copying every observation into each caller.
            for (var index = 0; index < _pending.Count; ++index)
            {
                _pending[index].Observations.Add(new(null, null, value));
            }

            var stack = new Stack<(NullaryObservation[] Observations, int Index)>();
            var observations = value.Observations;
            var next = 0;

            while (true)
            {
                cancellationToken.ThrowIfCancellationRequested();

                if (next == observations.Length)
                {
                    if (!stack.TryPop(out var parent))
                        break;

                    (observations, next) = parent;
                    continue;
                }

                var observation = observations[next++];

                if (observation.Replayed is { } replayed)
                {
                    stack.Push((observations, next));
                    observations = replayed.Observations;
                    next = 0;
                }
                else
                {
                    observation.Observer!(observation.Value!);
                }
            }
        }

    }
}
