using Pine.Core.Elm.Syntax.Abstract;
using Pine.Core.Internal;
using System;
using System.Collections.Frozen;
using System.Collections.Generic;
using System.Runtime.CompilerServices;

namespace Pine.Core.Elm.Syntax.Interpreter;

public partial class ElmSyntaxInterpreter
{
    /// <summary>
    /// Immutable parameter metadata shared across calls. Pattern lists are treated as immutable,
    /// just like the prepared syntax containing them; neither this cache nor its layouts hold
    /// values from an invocation.
    /// </summary>
    internal sealed class PreparedParameterBindingLayout
    {
        private static readonly ConditionalWeakTable<
            IReadOnlyList<Pattern>,
            PreparedParameterBindingLayout>
            s_layouts = [];

        private readonly IReadOnlyList<Pattern> _patterns;

        private readonly string[] _names;

        private readonly FrozenDictionary<string, int> _slotsByName;

        private readonly int[] _writeSlots;

        private PreparedParameterBindingLayout(IReadOnlyList<Pattern> patterns)
        {
            _patterns = patterns;

            var names = new List<string>();
            var slots = new Dictionary<string, int>(StringComparer.Ordinal);
            var writeSlots = new List<int>();

            for (var index = 0; index < patterns.Count; ++index)
                CollectWrites(patterns[index], names, slots, writeSlots);

            _names = [.. names];
            _slotsByName = slots.ToFrozenDictionary(StringComparer.Ordinal);
            _writeSlots = [.. writeSlots];
        }

        internal static PreparedParameterBindingLayout ForPatterns(IReadOnlyList<Pattern> patterns) =>
            s_layouts.GetValue(patterns, static source => new PreparedParameterBindingLayout(source));

        internal int BindingCount =>
            _names.Length;

        internal string NameAt(int slot) =>
            _names[slot];

        internal bool TryGetSlot(string name, out int slot) =>
            _slotsByName.TryGetValue(name, out slot);

        internal LocalBindingEnvironment BindArguments(
            LocalBindingEnvironment parent,
            IReadOnlyList<PineValueInProcess> arguments)
        {
            var values =
                BindingCount is 0
                ?
                Array.Empty<PineValueInProcess>()
                :
                new PineValueInProcess[BindingCount];

            var sink = new SlotBindingSink(this, values);

            for (var index = 0; index < _patterns.Count; ++index)
                BindPattern(_patterns[index], arguments[index], ref sink);

            return parent.CreateChild(this, values);
        }

        private static void CollectWrites(
            Pattern pattern,
            List<string> names,
            Dictionary<string, int> slots,
            List<int> writeSlots)
        {
            switch (pattern)
            {
                case Pattern.VarPattern variable:
                    AddWrite(variable.Name, names, slots, writeSlots);
                    break;

                case Pattern.AllPattern:
                case Pattern.UnitPattern:
                case Pattern.CharPattern:
                case Pattern.StringPattern:
                case Pattern.IntPattern:
                case Pattern.FloatPattern:
                    break;

                case Pattern.AsPattern alias:
                    CollectWrites(alias.Pattern, names, slots, writeSlots);
                    AddWrite(alias.Name, names, slots, writeSlots);
                    break;

                case Pattern.TuplePattern tuple:
                    for (var index = 0; index < tuple.Elements.Count; ++index)
                        CollectWrites(tuple.Elements[index], names, slots, writeSlots);

                    break;

                case Pattern.ListPattern list:
                    for (var index = 0; index < list.Elements.Count; ++index)
                        CollectWrites(list.Elements[index], names, slots, writeSlots);

                    break;

                case Pattern.UnConsPattern cons:
                    CollectWrites(cons.Head, names, slots, writeSlots);
                    CollectWrites(cons.Tail, names, slots, writeSlots);
                    break;

                case Pattern.RecordPattern record:
                    foreach (var field in record.Fields)
                        AddWrite(field.FieldName, names, slots, writeSlots);

                    break;

                case Pattern.NamedPattern named:

                    // BindPattern intentionally ignores even argument patterns on these tags.
                    if (named.Name.Name is "True" or "False")
                        break;

                    for (var index = 0; index < named.Arguments.Count; ++index)
                        CollectWrites(named.Arguments[index], names, slots, writeSlots);

                    break;

                default:
                    throw new NotImplementedException(
                        "CollectWrites does not handle pattern variant: " + pattern.GetType().Name);
            }
        }

        private static void AddWrite(
            string name,
            List<string> names,
            Dictionary<string, int> slots,
            List<int> writeSlots)
        {
            if (!slots.TryGetValue(name, out var slot))
            {
                slot = names.Count;
                names.Add(name);
                slots.Add(name, slot);
            }

            writeSlots.Add(slot);
        }

        private struct SlotBindingSink(
            PreparedParameterBindingLayout layout,
            PineValueInProcess[] values) : IPatternBindingSink
        {
            private int _nextWrite;

            public void Bind(string name, PineValueInProcess value)
            {
                var slot = layout._writeSlots[_nextWrite++];

                System.Diagnostics.Debug.Assert(layout._names[slot] == name);

                values[slot] = value;
            }
        }
    }

    private interface IPatternBindingSink
    {
        void Bind(string name, PineValueInProcess value);
    }

    private readonly struct DictionaryPatternBindingSink(
        IDictionary<string, PineValueInProcess> bindings) : IPatternBindingSink
    {
        public void Bind(string name, PineValueInProcess value) =>
            bindings[name] = value;
    }
}
