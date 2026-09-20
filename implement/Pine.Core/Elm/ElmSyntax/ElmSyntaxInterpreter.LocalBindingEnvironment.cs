using Pine.Core.Internal;
using System.Collections;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.Core.Elm.ElmSyntax;

public partial class ElmSyntaxInterpreter
{
    /// <summary>
    /// Linked local-binding environment used by the interpreter runtime. Each layer carries only
    /// the bindings introduced at one scope boundary and points at its parent for older bindings.
    /// Lookup therefore avoids copying all parent bindings when entering <c>let</c>, <c>case</c>,
    /// or function-call scopes.
    /// </summary>
    internal sealed class LocalBindingEnvironment : IReadOnlyDictionary<string, PineValueInProcess>
    {
        private readonly LocalBindingEnvironment? parent;

        private readonly IReadOnlyDictionary<string, PineValueInProcess> localBindings;

        private readonly bool localBindingsMayMutate;

        public static LocalBindingEnvironment Empty { get; } =
            new(
                parent: null,
                localBindings: ImmutableDictionary<string, PineValueInProcess>.Empty,
                localBindingsMayMutate: false);

        private LocalBindingEnvironment(
            LocalBindingEnvironment? parent,
            IReadOnlyDictionary<string, PineValueInProcess> localBindings,
            bool localBindingsMayMutate)
        {
            this.parent = parent;
            this.localBindings = localBindings;
            this.localBindingsMayMutate = localBindingsMayMutate;
        }

        public IEnumerable<string> Keys =>
            this.Select(static binding => binding.Key);

        public IEnumerable<PineValueInProcess> Values =>
            this.Select(static binding => binding.Value);

        public int Count =>
            parent is null
            ?
            localBindings.Count
            :
            CountVisibleBindings();

        public PineValueInProcess this[string key] =>
            TryGetValue(key, out var value)
            ?
            value
            :
            throw new KeyNotFoundException("The given key '" + key + "' was not present in the environment.");

        public static LocalBindingEnvironment FromBindings(
            IReadOnlyDictionary<string, PineValueInProcess>? bindings)
        {
            if (bindings is null || bindings.Count is 0)
                return Empty;

            if (bindings is LocalBindingEnvironment environment)
                return environment;

            return
                new LocalBindingEnvironment(
                    parent: Empty,
                    localBindings: SnapshotLayer(bindings),
                    localBindingsMayMutate: false);
        }

        internal LocalBindingEnvironment CreateChild(
            IReadOnlyDictionary<string, PineValueInProcess> bindings)
        {
            if (bindings.Count is 0)
                return this;

            return
                new LocalBindingEnvironment(
                    parent: this,
                    localBindings: bindings,
                    localBindingsMayMutate: false);
        }

        public LocalBindingEnvironment CreateMutableChild(
            Dictionary<string, PineValueInProcess> bindings) =>
            new(
                parent: this,
                localBindings: bindings,
                localBindingsMayMutate: true);

        public LocalBindingEnvironment Snapshot()
        {
            if (ReferenceEquals(this, Empty))
                return this;

            var snapshottedParent =
                parent?.Snapshot()
                ??
                Empty;

            if (!localBindingsMayMutate && ReferenceEquals(snapshottedParent, parent))
                return this;

            return
                new LocalBindingEnvironment(
                    parent: snapshottedParent,
                    localBindings:
                    localBindingsMayMutate
                    ?
                    SnapshotLayer(localBindings)
                    :
                    localBindings,
                    localBindingsMayMutate: false);
        }

        public bool ContainsKey(string key) =>
            TryGetValue(key, out _);

        public bool TryGetValue(string key, out PineValueInProcess value)
        {
            if (localBindings.TryGetValue(key, out var localValue))
            {
                value = localValue;
                return true;
            }

            if (parent is null)
            {
                value = default!;
                return false;
            }

            return parent.TryGetValue(key, out value);
        }

        public IEnumerator<KeyValuePair<string, PineValueInProcess>> GetEnumerator()
        {
            if (parent is null)
                return localBindings.GetEnumerator();

            return EnumerateVisibleBindings().GetEnumerator();
        }

        IEnumerator IEnumerable.GetEnumerator() =>
            GetEnumerator();

        private int CountVisibleBindings()
        {
            var seen = new HashSet<string>(System.StringComparer.Ordinal);
            var count = 0;

            foreach (var layer in EnumerateLayersNewestFirst())
            {
                foreach (var binding in layer.localBindings)
                {
                    if (seen.Add(binding.Key))
                        ++count;
                }
            }

            return count;
        }

        private IEnumerable<KeyValuePair<string, PineValueInProcess>> EnumerateVisibleBindings()
        {
            var seen = new HashSet<string>(System.StringComparer.Ordinal);

            foreach (var layer in EnumerateLayersNewestFirst())
            {
                foreach (var binding in layer.localBindings)
                {
                    if (seen.Add(binding.Key))
                        yield return binding;
                }
            }
        }

        private IEnumerable<LocalBindingEnvironment> EnumerateLayersNewestFirst()
        {
            for (var current = this; current is not null; current = current.parent)
            {
                yield return current;
            }
        }

        private static IReadOnlyDictionary<string, PineValueInProcess> SnapshotLayer(
            IReadOnlyDictionary<string, PineValueInProcess> bindings)
        {
            if (bindings.Count is 0)
                return ImmutableDictionary<string, PineValueInProcess>.Empty;

            if (bindings is ImmutableDictionary<string, PineValueInProcess> immutableBindings)
                return immutableBindings;

            return bindings.ToImmutableDictionary();
        }
    }
}
