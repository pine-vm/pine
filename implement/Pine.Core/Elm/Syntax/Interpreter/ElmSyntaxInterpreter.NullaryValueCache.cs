using Pine.Core.CodeAnalysis;
using Pine.Core.Internal;
using System;
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

    private sealed class NullaryValueCache(CancellationToken cancellationToken)
    {
        private const int MaxObservationsPerValue = 256;

        private const int MaxEntries = 4096;

        private const int MaxScalarBytes = 64 * 1024;

        private const int MaxRetainedScalarBytes = 16 * 1024 * 1024;

        private readonly Dictionary<DeclarationKey, Entry> _entries = [];

        private readonly List<Entry> _pending = [];

        private int _retainedScalarBytes;

        private readonly record struct DeclarationKey(
            DeclQualifiedName Name,
            PreparedFunctionImplementation Implementation)
        {
            public bool Equals(DeclarationKey other) =>
                Name == other.Name && ReferenceEquals(Implementation, other.Implementation);

            public override int GetHashCode() =>
                HashCode.Combine(Name, RuntimeHelpers.GetHashCode(Implementation));
        }

        public sealed class Entry
        {
            public PineValueInProcess? Value { get; set; }

            public bool Eligible { get; set; } = true;

            public int RetainedScalarBytes { get; set; }

            public List<(Action<PineValueInProcess> Observer, PineValueInProcess Value)> Observations { get; } = [];
        }

        public Entry? Find(ApplicationResolution.ContinueWithFunction resolution)
        {
            var key = new DeclarationKey(resolution.ResolvedName!, resolution.Function);
            return _entries.GetValueOrDefault(key);
        }

        public Entry? Begin(ApplicationResolution.ContinueWithFunction resolution)
        {
            if (_entries.Count >= MaxEntries)
                return null;

            var entry = new Entry();
            _entries.Add(new(resolution.ResolvedName!, resolution.Function), entry);
            _pending.Add(entry);
            return entry;
        }

        public void Complete(Entry entry, PineValueInProcess value)
        {
            if (!ReferenceEquals(_pending[^1], entry))
                throw new InvalidOperationException("Nullary evaluation completed out of order.");

            _pending.RemoveAt(_pending.Count - 1);

            // Scalar blobs cannot contain closures, test nodes or mutable lexical environments.
            // Do not materialize compound values merely to decide whether to cache them.
            if (entry.Eligible && !IsOpaque(value) && value.IsBlob() && TryRetain(entry, value))
                entry.Value = value;

            else
                Reject(entry);
        }

        public void Observe(Action<PineValueInProcess> observer, PineValueInProcess value)
        {
            for (var index = 0; index < _pending.Count; index++)
            {
                var entry = _pending[index];

                if (!entry.Eligible)
                    continue;

                if (IsOpaque(value) || !value.IsBlob() ||
                    entry.Observations.Count >= MaxObservationsPerValue ||
                    !TryRetain(entry, value))
                {
                    Reject(entry);
                    continue;
                }

                entry.Observations.Add((observer, value));
            }

            observer(value);
        }

        public void Replay(Entry entry)
        {
            for (var index = 0; index < entry.Observations.Count; index++)
            {
                cancellationToken.ThrowIfCancellationRequested();
                var observation = entry.Observations[index];
                Observe(observation.Observer, observation.Value);
            }
        }

        public void RejectPending()
        {
            for (var index = 0; index < _pending.Count; index++)
                Reject(_pending[index]);
        }

        private bool TryRetain(Entry entry, PineValueInProcess value)
        {
            var bytes = value.GetLength();

            if (bytes > MaxScalarBytes || bytes > MaxRetainedScalarBytes - _retainedScalarBytes)
                return false;

            entry.RetainedScalarBytes += bytes;
            _retainedScalarBytes += bytes;
            return true;
        }

        private void Reject(Entry entry)
        {
            entry.Eligible = false;
            entry.Observations.Clear();
            _retainedScalarBytes -= entry.RetainedScalarBytes;
            entry.RetainedScalarBytes = 0;
        }
    }
}
