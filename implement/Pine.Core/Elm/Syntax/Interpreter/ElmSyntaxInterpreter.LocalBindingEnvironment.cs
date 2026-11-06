using Pine.Core.Internal;
using System.Collections;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.Core.Elm.Syntax.Interpreter;

public partial class ElmSyntaxInterpreter
{
    /// <summary>
    /// Linked local-binding environment used by the interpreter runtime. Each layer carries only
    /// the bindings introduced at one scope boundary and points at its parent for older bindings.
    /// Lookup therefore avoids copying all parent bindings when entering <c>let</c>, <c>case</c>,
    /// or function-call scopes. Parameter layers share a prepared name layout and own only
    /// their values; mutable <c>let</c> layers retain their dictionary storage.
    /// </summary>
    internal sealed class LocalBindingEnvironment : IReadOnlyDictionary<string, PineValueInProcess>
    {
        private readonly LocalBindingEnvironment? _parent;

        private readonly IReadOnlyDictionary<string, PineValueInProcess>? _localBindings;

        private readonly PreparedParameterBindingLayout? _bindingLayout;

        private readonly PineValueInProcess[]? _bindingValues;

        private readonly bool _localBindingsMayMutate;

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
            _parent = parent;
            _localBindings = localBindings;
            _localBindingsMayMutate = localBindingsMayMutate;
        }

        private LocalBindingEnvironment(
            LocalBindingEnvironment parent,
            PreparedParameterBindingLayout bindingLayout,
            PineValueInProcess[] bindingValues)
        {
            _parent = parent;
            _bindingLayout = bindingLayout;
            _bindingValues = bindingValues;
        }

        public IEnumerable<string> Keys =>
            this.Select(static binding => binding.Key);

        public IEnumerable<PineValueInProcess> Values =>
            this.Select(static binding => binding.Value);

        public int Count =>
            _parent is null
            ?
            LocalBindingCount
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

        internal LocalBindingEnvironment CreateChild(
            PreparedParameterBindingLayout bindingLayout,
            PineValueInProcess[] bindingValues) =>
            bindingLayout.BindingCount is 0
            ?
            this
            :
            new LocalBindingEnvironment(
                parent: this,
                bindingLayout: bindingLayout,
                bindingValues: bindingValues);

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
                _parent?.Snapshot()
                ??
                Empty;

            if (!_localBindingsMayMutate && ReferenceEquals(snapshottedParent, _parent))
                return this;

            if (_bindingLayout is { } layout)
            {
                return
                    new LocalBindingEnvironment(
                        parent: snapshottedParent,
                        bindingLayout: layout,
                        bindingValues: _bindingValues!);
            }

            return
                new LocalBindingEnvironment(
                    parent: snapshottedParent,
                    localBindings:
                    _localBindingsMayMutate
                    ?
                    SnapshotLayer(_localBindings!)
                    :
                    _localBindings!,
                    localBindingsMayMutate: false);
        }

        public bool ContainsKey(string key) =>
            TryGetValue(key, out _);

        public bool TryGetValue(string key, out PineValueInProcess value)
        {
            if (_bindingLayout is { } layout)
            {
                if (layout.TryGetSlot(key, out var slot))
                {
                    value = _bindingValues![slot];
                    return true;
                }
            }
            else if (_localBindings!.TryGetValue(key, out var localValue))
            {
                value = localValue;
                return true;
            }

            if (_parent is null)
            {
                value = default!;
                return false;
            }

            return _parent.TryGetValue(key, out value);
        }

        public IEnumerator<KeyValuePair<string, PineValueInProcess>> GetEnumerator()
        {
            if (_parent is null)
                return EnumerateLocalBindings().GetEnumerator();

            return EnumerateVisibleBindings().GetEnumerator();
        }

        IEnumerator IEnumerable.GetEnumerator() =>
            GetEnumerator();

        private int LocalBindingCount =>
            _bindingLayout?.BindingCount ?? _localBindings!.Count;

        private IEnumerable<KeyValuePair<string, PineValueInProcess>> EnumerateLocalBindings()
        {
            if (_bindingLayout is { } layout)
            {
                for (var slot = 0; slot < layout.BindingCount; ++slot)
                {
                    yield return
                        new KeyValuePair<string, PineValueInProcess>(
                            layout.NameAt(slot),
                            _bindingValues![slot]);
                }
            }
            else
            {
                foreach (var binding in _localBindings!)
                    yield return binding;
            }
        }

        private int CountVisibleBindings()
        {
            var seen = new HashSet<string>(System.StringComparer.Ordinal);
            var count = 0;

            foreach (var layer in EnumerateLayersNewestFirst())
            {
                foreach (var binding in layer.EnumerateLocalBindings())
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
                foreach (var binding in layer.EnumerateLocalBindings())
                {
                    if (seen.Add(binding.Key))
                        yield return binding;
                }
            }
        }

        private IEnumerable<LocalBindingEnvironment> EnumerateLayersNewestFirst()
        {
            for (var current = this; current is not null; current = current._parent)
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
