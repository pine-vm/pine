using System;
using System.Collections.Generic;
using System.Collections.Immutable;

namespace Pine.Core.Elm.ElmCompilerInDotnet;

/// <summary>
/// Structural type model for later compilation stages.
/// Types are identified by their structure (shape) rather than by their declared name.
/// Supports non-specific types as they appear in source declarations: type variables,
/// open records, constrained type variables (comparable, appendable, number), etc.
/// </summary>
public abstract record StructuralType
{
    private StructuralType() { }

    /// <summary>Integer type.</summary>
    public sealed record IntType : StructuralType
    {
        /// <summary>Singleton instance.</summary>
        public static readonly IntType Instance = new();

        private readonly int _hashCode = typeof(IntType).GetHashCode();

        /// <inheritdoc/>
        public override int GetHashCode() => _hashCode;
    }

    /// <summary>Float type.</summary>
    public sealed record FloatType : StructuralType
    {
        /// <summary>Singleton instance.</summary>
        public static readonly FloatType Instance = new();

        private readonly int _hashCode = typeof(FloatType).GetHashCode();

        /// <inheritdoc/>
        public override int GetHashCode() => _hashCode;
    }

    /// <summary>String type.</summary>
    public sealed record StringType : StructuralType
    {
        /// <summary>Singleton instance.</summary>
        public static readonly StringType Instance = new();

        private readonly int _hashCode = typeof(StringType).GetHashCode();

        /// <inheritdoc/>
        public override int GetHashCode() => _hashCode;
    }

    /// <summary>Char type.</summary>
    public sealed record CharType : StructuralType
    {
        /// <summary>Singleton instance.</summary>
        public static readonly CharType Instance = new();

        private readonly int _hashCode = typeof(CharType).GetHashCode();

        /// <inheritdoc/>
        public override int GetHashCode() => _hashCode;
    }

    /// <summary>Boolean type.</summary>
    public sealed record BoolType : StructuralType
    {
        /// <summary>Singleton instance.</summary>
        public static readonly BoolType Instance = new();

        private readonly int _hashCode = typeof(BoolType).GetHashCode();

        /// <inheritdoc/>
        public override int GetHashCode() => _hashCode;
    }

    /// <summary>
    /// A type variable representing a polymorphic type parameter.
    /// Uses a zero-based index: 0 corresponds to "a", 1 to "b", etc. in printed form.
    /// </summary>
    public sealed record TypeVariable(int Index) : StructuralType
    {
        private readonly int _hashCode = HashCode.Combine(typeof(TypeVariable), Index);

        /// <inheritdoc/>
        public override int GetHashCode() => _hashCode;
    }

    /// <summary>
    /// Identifies the kind of constraint on a constrained type variable.
    /// </summary>
    public enum TypeConstraint
    {
        /// <summary>Can be Int or Float.</summary>
        Number,

        /// <summary>Can be Int, Float, Char, String, List of comparable, or tuple of comparables.</summary>
        Comparable,

        /// <summary>Can be String or List a.</summary>
        Appendable,
    }

    /// <summary>
    /// A constrained type variable, representing Elm's built-in type classes:
    /// number, comparable, appendable.
    /// Uses a zero-based index for identity, same as <see cref="TypeVariable"/>.
    /// </summary>
    public sealed record ConstrainedVariable(
        int Index,
        TypeConstraint Constraint) : StructuralType
    {
        private readonly int _hashCode = HashCode.Combine(typeof(ConstrainedVariable), Index, Constraint);

        /// <inheritdoc/>
        public override int GetHashCode() => _hashCode;
    }

    /// <summary>
    /// Function type with argument type and return type.
    /// </summary>
    public sealed record FunctionType(
        StructuralType ArgumentType,
        StructuralType ReturnType) : StructuralType
    {
        private readonly int _hashCode = HashCode.Combine(typeof(FunctionType), ArgumentType, ReturnType);

        /// <inheritdoc/>
        public override int GetHashCode() => _hashCode;
    }

    /// <summary>
    /// List type with element type.
    /// </summary>
    public sealed record ListType(StructuralType ElementType) : StructuralType
    {
        private readonly int _hashCode = HashCode.Combine(typeof(ListType), ElementType);

        /// <inheritdoc/>
        public override int GetHashCode() => _hashCode;
    }

    /// <summary>
    /// Tuple type with element types.
    /// Type variable names are canonicalized to a, b, c, ... based on first-appearance
    /// order when constructed via the public <see cref="Create"/> factory method.
    /// </summary>
    public sealed record TupleType : StructuralType
    {
        public ImmutableList<StructuralType> ElementTypes { get; }

        private readonly int _hashCode;

        internal TupleType(ImmutableList<StructuralType> elementTypes)
        {
            ElementTypes = elementTypes;
            _hashCode = ComputeHash(elementTypes);
        }

        /// <summary>
        /// Creates a tuple type, canonicalizing any type variable indices to 0, 1, 2, ...
        /// based on first-appearance order in the element types.
        /// </summary>
        public static TupleType Create(ImmutableList<StructuralType> elementTypes)
        {
            var mapping = BuildCanonicalIndexMapping(elementTypes);

            if (mapping is null)
                return new TupleType(elementTypes);

            return SubstituteTuple(new TupleType(elementTypes), mapping);
        }

        /// <inheritdoc/>
        public bool Equals(TupleType? other)
        {
            if (other is null)
                return false;

            if (ElementTypes.Count != other.ElementTypes.Count)
                return false;

            for (var i = 0; i < ElementTypes.Count; i++)
            {
                if (!ElementTypes[i].Equals(other.ElementTypes[i]))
                    return false;
            }

            return true;
        }

        /// <inheritdoc/>
        public override int GetHashCode() => _hashCode;

        private static int ComputeHash(ImmutableList<StructuralType> elements)
        {
            var hash = new HashCode();
            hash.Add(typeof(TupleType));

            for (var i = 0; i < elements.Count; i++)
                hash.Add(elements[i]);

            return hash.ToHashCode();
        }
    }

    /// <summary>
    /// A closed record type with a fixed set of fields.
    /// Fields are stored in an ImmutableDictionary keyed by field name.
    /// Type variable indices are canonicalized to 0, 1, 2, ... based on first-appearance
    /// order (fields visited in sorted name order) when constructed via <see cref="Create"/>.
    /// </summary>
    public sealed record ClosedRecord : StructuralType
    {
        public ImmutableDictionary<string, StructuralType> Fields { get; }

        private readonly int _hashCode;

        internal ClosedRecord(ImmutableDictionary<string, StructuralType> fields)
        {
            Fields = fields;
            _hashCode = ComputeHash(fields);
        }

        /// <summary>
        /// Creates a closed record type, canonicalizing any type variable indices to 0, 1, 2, ...
        /// based on first-appearance order (fields visited in sorted name order).
        /// </summary>
        public static ClosedRecord Create(ImmutableDictionary<string, StructuralType> fields)
        {
            var sortedKeys = GetSortedKeys(fields);
            var typeSequence = new List<StructuralType>(sortedKeys.Length);

            for (var i = 0; i < sortedKeys.Length; i++)
                typeSequence.Add(fields[sortedKeys[i]]);

            var mapping = BuildCanonicalIndexMapping(typeSequence);

            if (mapping is null)
                return new ClosedRecord(fields);

            return SubstituteClosedRecord(new ClosedRecord(fields), mapping);
        }

        /// <inheritdoc/>
        public bool Equals(ClosedRecord? other)
        {
            if (other is null)
                return false;

            if (Fields.Count != other.Fields.Count)
                return false;

            var sortedKeys = GetSortedKeys(Fields);

            for (var i = 0; i < sortedKeys.Length; i++)
            {
                if (!other.Fields.TryGetValue(sortedKeys[i], out var otherValue))
                    return false;

                if (!Fields[sortedKeys[i]].Equals(otherValue))
                    return false;
            }

            return true;
        }

        /// <inheritdoc/>
        public override int GetHashCode() => _hashCode;

        private static int ComputeHash(ImmutableDictionary<string, StructuralType> fields)
        {
            var hash = new HashCode();
            hash.Add(typeof(ClosedRecord));

            var sortedKeys = GetSortedKeys(fields);

            for (var i = 0; i < sortedKeys.Length; i++)
            {
                hash.Add(sortedKeys[i]);
                hash.Add(fields[sortedKeys[i]]);
            }

            return hash.ToHashCode();
        }
    }

    /// <summary>
    /// An open (extensible) record type that has at least the given fields,
    /// plus a row variable representing additional unknown fields.
    /// Corresponds to Elm's <c>{ a | name : String }</c> syntax.
    /// The row variable uses a zero-based index like <see cref="TypeVariable"/>.
    /// </summary>
    public sealed record OpenRecord(
        ImmutableDictionary<string, StructuralType> Fields,
        int RowVariableIndex) : StructuralType
    {
        private readonly int _hashCode = ComputeHash(Fields, RowVariableIndex);

        /// <inheritdoc/>
        public bool Equals(OpenRecord? other)
        {
            if (other is null)
                return false;

            if (RowVariableIndex != other.RowVariableIndex)
                return false;

            if (Fields.Count != other.Fields.Count)
                return false;

            var sortedKeys = GetSortedKeys(Fields);

            for (var i = 0; i < sortedKeys.Length; i++)
            {
                if (!other.Fields.TryGetValue(sortedKeys[i], out var otherValue))
                    return false;

                if (!Fields[sortedKeys[i]].Equals(otherValue))
                    return false;
            }

            return true;
        }

        /// <inheritdoc/>
        public override int GetHashCode() => _hashCode;

        private static int ComputeHash(
            ImmutableDictionary<string, StructuralType> fields,
            int rowVariableIndex)
        {
            var hash = new HashCode();
            hash.Add(typeof(OpenRecord));
            hash.Add(rowVariableIndex);

            var sortedKeys = GetSortedKeys(fields);

            for (var i = 0; i < sortedKeys.Length; i++)
            {
                hash.Add(sortedKeys[i]);
                hash.Add(fields[sortedKeys[i]]);
            }

            return hash.ToHashCode();
        }
    }

    /// <summary>
    /// A choice (union/sum) type identified structurally by its set of tags
    /// and their field types. Tags are stored in an ImmutableDictionary keyed by tag name.
    /// Type variable indices are canonicalized to 0, 1, 2, ... based on first-appearance
    /// order (tags visited in sorted name order) when constructed via <see cref="Create"/>.
    /// </summary>
    public sealed record ChoiceType : StructuralType
    {
        public ImmutableDictionary<string, ImmutableList<StructuralType>> Tags { get; }

        private readonly int _hashCode;

        internal ChoiceType(ImmutableDictionary<string, ImmutableList<StructuralType>> tags)
        {
            Tags = tags;
            _hashCode = ComputeHash(tags);
        }

        /// <summary>
        /// Creates a choice type, canonicalizing any type variable indices to 0, 1, 2, ...
        /// based on first-appearance order (tags visited in sorted name order,
        /// then each tag's argument types in order).
        /// </summary>
        public static ChoiceType Create(ImmutableDictionary<string, ImmutableList<StructuralType>> tags)
        {
            var sortedKeys = GetSortedKeys(tags);
            var typeSequence = new List<StructuralType>();

            for (var i = 0; i < sortedKeys.Length; i++)
            {
                var fields = tags[sortedKeys[i]];

                for (var j = 0; j < fields.Count; j++)
                    typeSequence.Add(fields[j]);
            }

            var mapping = BuildCanonicalIndexMapping(typeSequence);

            if (mapping is null)
                return new ChoiceType(tags);

            return SubstituteChoiceType(new ChoiceType(tags), mapping);
        }

        /// <inheritdoc/>
        public bool Equals(ChoiceType? other)
        {
            if (other is null)
                return false;

            if (Tags.Count != other.Tags.Count)
                return false;

            var sortedKeys = GetSortedKeys(Tags);

            for (var i = 0; i < sortedKeys.Length; i++)
            {
                if (!other.Tags.TryGetValue(sortedKeys[i], out var otherFields))
                    return false;

                var thisFields = Tags[sortedKeys[i]];

                if (thisFields.Count != otherFields.Count)
                    return false;

                for (var j = 0; j < thisFields.Count; j++)
                {
                    if (!thisFields[j].Equals(otherFields[j]))
                        return false;
                }
            }

            return true;
        }

        /// <inheritdoc/>
        public override int GetHashCode() => _hashCode;

        private static int ComputeHash(
            ImmutableDictionary<string, ImmutableList<StructuralType>> tags)
        {
            var hash = new HashCode();
            hash.Add(typeof(ChoiceType));

            var sortedKeys = GetSortedKeys(tags);

            for (var i = 0; i < sortedKeys.Length; i++)
            {
                hash.Add(sortedKeys[i]);

                var fields = tags[sortedKeys[i]];

                for (var j = 0; j < fields.Count; j++)
                    hash.Add(fields[j]);
            }

            return hash.ToHashCode();
        }
    }

    /// <summary>
    /// Self-reference for recursive types.
    /// Represents "the enclosing type being defined," analogous to the μ (mu) binder
    /// in type theory. This allows modelling recursive types as finite trees without
    /// circular references or mutation.
    /// </summary>
    public sealed record Self : StructuralType
    {
        /// <summary>Singleton instance.</summary>
        public static readonly Self Instance = new();

        private readonly int _hashCode = typeof(Self).GetHashCode();

        /// <inheritdoc/>
        public override int GetHashCode() => _hashCode;
    }

    /// <summary>
    /// A concrete instance of a generic (parametric) type, formed by applying a
    /// generic type to one or more type arguments.
    /// For example, <c>Maybe Int</c> is represented as
    /// <c>TypeApplication(ChoiceType(Just a | Nothing), [IntType])</c>.
    /// <para>
    /// The order of type parameters is inferred from the first appearance of each
    /// type variable in the generic type's structure (depth-first, left-to-right).
    /// </para>
    /// </summary>
    public sealed record TypeApplication(
        StructuralType GenericType,
        ImmutableList<StructuralType> Arguments) : StructuralType
    {
        private readonly int _hashCode = ComputeHash(GenericType, Arguments);

        /// <inheritdoc/>
        public bool Equals(TypeApplication? other)
        {
            if (other is null)
                return false;

            if (!GenericType.Equals(other.GenericType))
                return false;

            if (Arguments.Count != other.Arguments.Count)
                return false;

            for (var i = 0; i < Arguments.Count; i++)
            {
                if (!Arguments[i].Equals(other.Arguments[i]))
                    return false;
            }

            return true;
        }

        /// <inheritdoc/>
        public override int GetHashCode() => _hashCode;

        private static int ComputeHash(
            StructuralType genericType,
            ImmutableList<StructuralType> arguments)
        {
            var hash = new HashCode();
            hash.Add(typeof(TypeApplication));
            hash.Add(genericType);

            for (var i = 0; i < arguments.Count; i++)
                hash.Add(arguments[i]);

            return hash.ToHashCode();
        }
    }

    /// <summary>
    /// Checks if <paramref name="specific"/> is assignable to <paramref name="general"/>.
    /// This is the "fits into" / subsumption check used to determine if a concrete type
    /// satisfies the constraints of a more general type — e.g., whether a value of the
    /// specific type can be used where the general type is expected.
    /// Examples: Int fits into number; a closed record fits into an open record
    /// that requires a subset of its fields.
    /// </summary>
    public static bool IsAssignableTo(StructuralType specific, StructuralType general)
    {
        if (specific.Equals(general))
            return true;

        switch (general)
        {
            case TypeVariable:

                // A type variable accepts any type.
                return true;

            case ConstrainedVariable constrained:
                return SpecificSatisfiesConstraint(specific, constrained.Constraint);

            case OpenRecord openRecord:
                return SpecificFitsOpenRecord(specific, openRecord);

            case FunctionType generalFunc when specific is FunctionType specificFunc:

                // Covariant in return, contravariant in argument.
                return
                    IsAssignableTo(generalFunc.ArgumentType, specificFunc.ArgumentType) &&
                    IsAssignableTo(specificFunc.ReturnType, generalFunc.ReturnType);

            case ListType generalList when specific is ListType specificList:
                return IsAssignableTo(specificList.ElementType, generalList.ElementType);

            case TupleType generalTuple when specific is TupleType specificTuple:
                {
                    if (generalTuple.ElementTypes.Count != specificTuple.ElementTypes.Count)
                        return false;

                    for (var i = 0; i < generalTuple.ElementTypes.Count; i++)
                    {
                        if (!IsAssignableTo(specificTuple.ElementTypes[i], generalTuple.ElementTypes[i]))
                            return false;
                    }

                    return true;
                }

            case ChoiceType generalChoice when specific is ChoiceType specificChoice:
                return SpecificFitsChoiceType(specificChoice, generalChoice);

            default:
                return false;
        }
    }

    private static bool SpecificSatisfiesConstraint(
        StructuralType specific,
        TypeConstraint constraint)
    {
        switch (constraint)
        {
            case TypeConstraint.Number:
                return
                    specific is IntType or FloatType ||
                    (specific is ConstrainedVariable cvNum && cvNum.Constraint is TypeConstraint.Number);

            case TypeConstraint.Comparable:
                {
                    if (specific is IntType or FloatType or CharType or StringType)
                        return true;

                    if (specific is ListType list)
                        return SpecificSatisfiesConstraint(list.ElementType, TypeConstraint.Comparable);

                    if (specific is TupleType tuple)
                    {
                        for (var i = 0; i < tuple.ElementTypes.Count; i++)
                        {
                            if (!SpecificSatisfiesConstraint(tuple.ElementTypes[i], TypeConstraint.Comparable))
                                return false;
                        }

                        return true;
                    }

                    if (specific is ConstrainedVariable cvComp)
                        return cvComp.Constraint is TypeConstraint.Comparable or TypeConstraint.Number;

                    return false;
                }

            case TypeConstraint.Appendable:
                return
                    specific is StringType or ListType ||
                    (specific is ConstrainedVariable cvApp && cvApp.Constraint is TypeConstraint.Appendable);

            default:
                return false;
        }
    }

    private static bool SpecificFitsOpenRecord(
        StructuralType specific,
        OpenRecord openRecord)
    {
        var specificFields =
            specific switch
            {
                ClosedRecord closed => closed.Fields,
                OpenRecord open => open.Fields,

                _ =>
                null,
            };

        if (specificFields is null)
            return false;

        // The specific type must have at least all fields required by the open record,
        // and each field type must be assignable.
        var requiredKeys = GetSortedKeys(openRecord.Fields);

        for (var i = 0; i < requiredKeys.Length; i++)
        {
            if (!specificFields.TryGetValue(requiredKeys[i], out var specificFieldType))
                return false;

            if (!IsAssignableTo(specificFieldType, openRecord.Fields[requiredKeys[i]]))
                return false;
        }

        return true;
    }

    private static bool SpecificFitsChoiceType(
        ChoiceType specific,
        ChoiceType general)
    {
        // For structural choice type assignability, the specific type's tags must
        // be a subset of or equal to the general type's tags, and each tag's fields
        // must be assignable.
        var specificKeys = GetSortedKeys(specific.Tags);

        for (var i = 0; i < specificKeys.Length; i++)
        {
            if (!general.Tags.TryGetValue(specificKeys[i], out var generalFields))
                return false;

            var specificFields = specific.Tags[specificKeys[i]];

            if (specificFields.Count != generalFields.Count)
                return false;

            for (var j = 0; j < specificFields.Count; j++)
            {
                if (!IsAssignableTo(specificFields[j], generalFields[j]))
                    return false;
            }
        }

        return true;
    }

    /// <summary>
    /// Substitutes type variables in <paramref name="type"/> according to
    /// <paramref name="substitutions"/>. Returns a new type with variables replaced.
    /// Used when instantiating a polymorphic function for a specific call site
    /// (e.g., replacing type variable 0 with <c>Int</c>).
    /// </summary>
    public static StructuralType Substitute(
        StructuralType type,
        IReadOnlyDictionary<int, StructuralType> substitutions)
    {
        return type switch
        {
            TypeVariable tv =>
            substitutions.TryGetValue(tv.Index, out var replacement)
            ?
            replacement
            :
            tv,

            ConstrainedVariable cv =>
            substitutions.TryGetValue(cv.Index, out var replacement)
            ?
            replacement
            :
            cv,

            FunctionType func =>
            new FunctionType(
                Substitute(func.ArgumentType, substitutions),
                Substitute(func.ReturnType, substitutions)),

            ListType list =>
            new ListType(Substitute(list.ElementType, substitutions)),

            TupleType tuple =>
            SubstituteTuple(tuple, substitutions),

            ClosedRecord record =>
            SubstituteClosedRecord(record, substitutions),

            OpenRecord openRec =>
            SubstituteOpenRecord(openRec, substitutions),

            ChoiceType choice =>
            SubstituteChoiceType(choice, substitutions),

            TypeApplication app =>
            new TypeApplication(
                Substitute(app.GenericType, substitutions),
                SubstituteList(app.Arguments, substitutions)),

            // Primitives and Self are unchanged by substitution.
            _ =>
            type,
        };
    }

    private static TupleType SubstituteTuple(
        TupleType tuple,
        IReadOnlyDictionary<int, StructuralType> substitutions)
    {
        var builder = ImmutableList.CreateBuilder<StructuralType>();

        for (var i = 0; i < tuple.ElementTypes.Count; i++)
            builder.Add(Substitute(tuple.ElementTypes[i], substitutions));

        return new TupleType(builder.ToImmutable());
    }

    private static ClosedRecord SubstituteClosedRecord(
        ClosedRecord record,
        IReadOnlyDictionary<int, StructuralType> substitutions)
    {
        var builder = ImmutableDictionary.CreateBuilder<string, StructuralType>();

        var keys = GetSortedKeys(record.Fields);

        for (var i = 0; i < keys.Length; i++)
            builder[keys[i]] = Substitute(record.Fields[keys[i]], substitutions);

        return new ClosedRecord(builder.ToImmutable());
    }

    private static StructuralType SubstituteOpenRecord(
        OpenRecord openRec,
        IReadOnlyDictionary<int, StructuralType> substitutions)
    {
        if (substitutions.TryGetValue(openRec.RowVariableIndex, out var rowReplacement))
        {
            if (rowReplacement is ClosedRecord closedReplacement)
            {
                // Merge the fields into a closed record.
                var builder = ImmutableDictionary.CreateBuilder<string, StructuralType>();

                var replacementKeys = GetSortedKeys(closedReplacement.Fields);

                for (var i = 0; i < replacementKeys.Length; i++)
                    builder[replacementKeys[i]] = closedReplacement.Fields[replacementKeys[i]];

                var openRecKeys = GetSortedKeys(openRec.Fields);

                for (var i = 0; i < openRecKeys.Length; i++)
                    builder[openRecKeys[i]] = Substitute(openRec.Fields[openRecKeys[i]], substitutions);

                return new ClosedRecord(builder.ToImmutable());
            }
        }

        var fieldBuilder = ImmutableDictionary.CreateBuilder<string, StructuralType>();

        var fieldKeys = GetSortedKeys(openRec.Fields);

        for (var i = 0; i < fieldKeys.Length; i++)
            fieldBuilder[fieldKeys[i]] = Substitute(openRec.Fields[fieldKeys[i]], substitutions);

        // Remap the row variable index if the substitution maps it to another variable
        var newRowIndex = openRec.RowVariableIndex;

        if (substitutions.TryGetValue(openRec.RowVariableIndex, out var rowSub) && rowSub is TypeVariable tv)
            newRowIndex = tv.Index;

        return new OpenRecord(fieldBuilder.ToImmutable(), newRowIndex);
    }

    private static ChoiceType SubstituteChoiceType(
        ChoiceType choice,
        IReadOnlyDictionary<int, StructuralType> substitutions)
    {
        var tagBuilder = ImmutableDictionary.CreateBuilder<string, ImmutableList<StructuralType>>();

        var tagKeys = GetSortedKeys(choice.Tags);

        for (var i = 0; i < tagKeys.Length; i++)
        {
            var fields = choice.Tags[tagKeys[i]];
            var fieldBuilder = ImmutableList.CreateBuilder<StructuralType>();

            for (var j = 0; j < fields.Count; j++)
                fieldBuilder.Add(Substitute(fields[j], substitutions));

            tagBuilder[tagKeys[i]] = fieldBuilder.ToImmutable();
        }

        return new ChoiceType(tagBuilder.ToImmutable());
    }

    private static ImmutableList<StructuralType> SubstituteList(
        ImmutableList<StructuralType> types,
        IReadOnlyDictionary<int, StructuralType> substitutions)
    {
        var builder = ImmutableList.CreateBuilder<StructuralType>();

        for (var i = 0; i < types.Count; i++)
            builder.Add(Substitute(types[i], substitutions));

        return builder.ToImmutable();
    }

    /// <summary>
    /// Builds a substitution mapping that remaps type variable indices to canonical
    /// sequential indices (0, 1, 2, ...) based on first-appearance order in the given
    /// sequence of types. Returns null if the indices are already canonical or if
    /// there are no variables.
    /// </summary>
    internal static Dictionary<int, StructuralType>? BuildCanonicalIndexMapping(
        IEnumerable<StructuralType> typeSequence)
    {
        var seen = new HashSet<int>();
        var orderedIndices = new List<int>();
        var variableInstances = new Dictionary<int, StructuralType>();

        foreach (var type in typeSequence)
            CollectVariableIndicesForCanonicalization(type, seen, orderedIndices, variableInstances);

        if (orderedIndices.Count is 0)
            return null;

        // Check if already canonical (indices are 0, 1, 2, ...)
        var alreadyCanonical = true;

        for (var i = 0; i < orderedIndices.Count; i++)
        {
            if (orderedIndices[i] != i)
            {
                alreadyCanonical = false;
                break;
            }
        }

        if (alreadyCanonical)
            return null;

        var mapping = new Dictionary<int, StructuralType>();

        for (var i = 0; i < orderedIndices.Count; i++)
        {
            var oldIndex = orderedIndices[i];
            var original = variableInstances[oldIndex];

            mapping[oldIndex] =
                original switch
                {
                    ConstrainedVariable cv => new ConstrainedVariable(i, cv.Constraint),

                    _ =>
                    new TypeVariable(i),
                };
        }

        return mapping;
    }

    private static void CollectVariableIndicesForCanonicalization(
        StructuralType type,
        HashSet<int> seen,
        List<int> orderedIndices,
        Dictionary<int, StructuralType> variableInstances)
    {
        switch (type)
        {
            case TypeVariable tv:
                if (seen.Add(tv.Index))
                {
                    orderedIndices.Add(tv.Index);
                    variableInstances[tv.Index] = tv;
                }

                break;

            case ConstrainedVariable cv:
                if (seen.Add(cv.Index))
                {
                    orderedIndices.Add(cv.Index);
                    variableInstances[cv.Index] = cv;
                }

                break;

            case FunctionType func:
                CollectVariableIndicesForCanonicalization(func.ArgumentType, seen, orderedIndices, variableInstances);
                CollectVariableIndicesForCanonicalization(func.ReturnType, seen, orderedIndices, variableInstances);
                break;

            case ListType list:
                CollectVariableIndicesForCanonicalization(list.ElementType, seen, orderedIndices, variableInstances);
                break;

            case TupleType tuple:
                for (var i = 0; i < tuple.ElementTypes.Count; i++)
                    CollectVariableIndicesForCanonicalization(tuple.ElementTypes[i], seen, orderedIndices, variableInstances);

                break;

            case ClosedRecord record:
                {
                    var recordKeys = GetSortedKeys(record.Fields);

                    for (var i = 0; i < recordKeys.Length; i++)
                        CollectVariableIndicesForCanonicalization(record.Fields[recordKeys[i]], seen, orderedIndices, variableInstances);

                    break;
                }

            case OpenRecord openRec:
                {
                    if (seen.Add(openRec.RowVariableIndex))
                    {
                        orderedIndices.Add(openRec.RowVariableIndex);
                        variableInstances[openRec.RowVariableIndex] = new TypeVariable(openRec.RowVariableIndex);
                    }

                    var openRecKeys = GetSortedKeys(openRec.Fields);

                    for (var i = 0; i < openRecKeys.Length; i++)
                        CollectVariableIndicesForCanonicalization(openRec.Fields[openRecKeys[i]], seen, orderedIndices, variableInstances);

                    break;
                }

            case ChoiceType choice:
                {
                    var choiceKeys = GetSortedKeys(choice.Tags);

                    for (var i = 0; i < choiceKeys.Length; i++)
                    {
                        var fields = choice.Tags[choiceKeys[i]];

                        for (var j = 0; j < fields.Count; j++)
                            CollectVariableIndicesForCanonicalization(fields[j], seen, orderedIndices, variableInstances);
                    }

                    break;
                }

            case TypeApplication app:
                CollectVariableIndicesForCanonicalization(app.GenericType, seen, orderedIndices, variableInstances);

                for (var i = 0; i < app.Arguments.Count; i++)
                    CollectVariableIndicesForCanonicalization(app.Arguments[i], seen, orderedIndices, variableInstances);

                break;
        }
    }

    internal static string VariableIndexToName(int index) =>
        ((char)('a' + index)).ToString();

    private static string[] GetSortedKeys<TValue>(ImmutableDictionary<string, TValue> dict)
    {
        var keys = new string[dict.Count];
        var index = 0;

        var enumerator = dict.GetEnumerator();

        while (enumerator.MoveNext())
            keys[index++] = enumerator.Current.Key;

        Array.Sort(keys, StringComparer.Ordinal);
        return keys;
    }
}
