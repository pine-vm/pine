using Pine.Core.Encodings;
using Pine.Core.Functional;
using Pine.Core.PineLanguage;
using System;
using System.Collections.Generic;
using System.Linq;

namespace Pine.Core.DotNet;

internal enum PineCSharpLocalType
{
    Pine,
    Integer,
    OptionalInteger,
    Boolean,
    OptionalBoolean,
    IntegerWithOriginal
}

/// <summary>Plans storage for locals, independently of any numeric interpretation of an arbitrary Pine value.</summary>
internal sealed class PineCSharpLocalPlan
{
    [Flags]
    internal enum Kind
    {
        Empty = 1,
        Integer = 2,
        Boolean = 4,
        Pine = 8
    }

    private readonly Dictionary<string, Kind> _kinds = [];

    private readonly Dictionary<string, HashSet<string>> _copies = [];

    private readonly Dictionary<string, HashSet<string>> _aliases = [];

    private readonly HashSet<string> _pineConsumers = [];

    public static Kind LiteralKind(PineValue literal) =>
        literal.Equals(PineValue.EmptyList)
        ?
        Kind.Empty
        :
        literal.Equals(PineKernelValues.TrueValue) || literal.Equals(PineKernelValues.FalseValue)
        ?
        Kind.Boolean
        :
        IntegerEncoding.ParseSignedIntegerStrict(literal).IsOkOrNullable() is not null
        ?
        Kind.Integer
        :
        Kind.Pine;

    public void Seed(string variable, Kind kind) =>
        _kinds[variable] = _kinds.GetValueOrDefault(variable) | kind;

    public void Copy(string target, string source)
    {
        Add(_copies, source, target);
        Add(_aliases, source, target);
        Add(_aliases, target, source);
    }

    private static void Add(Dictionary<string, HashSet<string>> edges, string source, string target)
    {
        if (!edges.TryGetValue(source, out var targets))
            edges[source] = targets = [];

        targets.Add(target);
    }

    public void ObservePine(string variable) => _pineConsumers.Add(variable);

    public IReadOnlyDictionary<string, PineCSharpLocalType> Finish()
    {
        var pending = new Queue<string>(_kinds.Keys);

        while (pending.TryDequeue(out var source))
        {
            if (!_copies.TryGetValue(source, out var targets))
                continue;

            foreach (var target in targets)
            {
                var before = _kinds.GetValueOrDefault(target);
                var after = before | _kinds[source];

                if (after != before)
                {
                    _kinds[target] = after;
                    pending.Enqueue(target);
                }
            }
        }

        // Keep one shared Pine object when aliases can escape to an operation that stores Pine values.
        pending = new Queue<string>(_pineConsumers);

        while (pending.TryDequeue(out var source))
        {
            if (!_aliases.TryGetValue(source, out var aliases))
                continue;

            foreach (var alias in aliases)
            {
                if (_pineConsumers.Add(alias))
                    pending.Enqueue(alias);
            }
        }

        return
            _kinds.ToDictionary(
                pair => pair.Key,
                pair =>
                _pineConsumers.Contains(pair.Key)
                ?
                PineCSharpLocalType.Pine
                :
                pair.Value switch
                {
                    Kind.Integer => PineCSharpLocalType.Integer,
                    Kind.Integer | Kind.Empty => PineCSharpLocalType.OptionalInteger,
                    Kind.Boolean => PineCSharpLocalType.Boolean,
                    Kind.Boolean | Kind.Empty => PineCSharpLocalType.OptionalBoolean,

                    // Retain an arbitrary incoming value until an integer operation replaces it.
                    // In particular a loop must not decode or re-encode an untouched argument.
                    Kind.Integer | Kind.Pine or Kind.Integer | Kind.Empty | Kind.Pine =>
                    PineCSharpLocalType.IntegerWithOriginal,

                    _ =>
                    PineCSharpLocalType.Pine
                });
    }
}
