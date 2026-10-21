using System;
using System.Collections.Generic;
using System.Linq;

namespace Pine.Core.Elm.ElmCompilerInDotnet.CompilerModel;

/// <summary>Deterministic structural ordering for layouts. No identity is serialized to a name.</summary>
public sealed class IdentityComparer : IComparer<FunctionId>, IComparer<BindingId>
{
    /// <summary>Shared stateless comparer.</summary>
    public static IdentityComparer Instance { get; } = new();

    /// <inheritdoc/>
    public int Compare(FunctionId? x, FunctionId? y)
    {
        if (ReferenceEquals(x, y))
            return 0;

        if (x is null)
            return -1;

        if (y is null)
            return 1;

        var rank = FunctionRank(x).CompareTo(FunctionRank(y));

        if (rank is not 0)
            return rank;

        return x switch
        {
            FunctionId.Source source => source.Name.CompareTo(((FunctionId.Source)y).Name),

            FunctionId.Lifted lifted =>
            Then(
                Compare(lifted.Owner, ((FunctionId.Lifted)y).Owner),
                () => lifted.Site.CompareTo(((FunctionId.Lifted)y).Site)),

            FunctionId.Specialized specialized =>
            Then(
                Compare(specialized.Original, ((FunctionId.Specialized)y).Original),
                () => Sequence(specialized.Constraints, ((FunctionId.Specialized)y).Constraints, Constraint)),

            _ =>
            throw new NotImplementedException($"{nameof(Compare)} does not handle identity variant: {x.GetType().Name}")
        };
    }

    /// <inheritdoc/>
    public int Compare(BindingId? x, BindingId? y)
    {
        if (ReferenceEquals(x, y))
            return 0;

        if (x is null)
            return -1;

        if (y is null)
            return 1;

        var rank = BindingRank(x).CompareTo(BindingRank(y));

        if (rank is not 0)
            return rank;

        return x switch
        {
            BindingId.Local local =>
            Then(
                Compare(local.Owner, ((BindingId.Local)y).Owner),
                () => local.Slot.CompareTo(((BindingId.Local)y).Slot)),

            BindingId.Temporary temporary =>
            Then(
                Compare(temporary.Owner, ((BindingId.Temporary)y).Owner),
                () => temporary.Slot.CompareTo(((BindingId.Temporary)y).Slot)),

            _ =>
            throw new NotImplementedException($"{nameof(Compare)} does not handle binding variant: {x.GetType().Name}")
        };
    }

    private static int BindingRank(BindingId binding) =>
        binding switch
        {
            BindingId.Local => 0,
            BindingId.Temporary => 1,

            _ =>
            throw new NotImplementedException(
                $"{nameof(BindingRank)} does not handle binding variant: {binding.GetType().Name}")
        };

    private static int FunctionRank(FunctionId id) =>
        id switch
        {
            FunctionId.Source => 0,
            FunctionId.Lifted => 1,
            FunctionId.Specialized => 2,

            _ =>
            throw new NotImplementedException($"{nameof(FunctionRank)} does not handle identity variant: {id.GetType().Name}")
        };

    private int Constraint(ParameterConstraint x, ParameterConstraint y)
    {
        var slot = x.Parameter.CompareTo(y.Parameter);

        if (slot is not 0)
            return slot;

        return x switch
        {
            ParameterConstraint.KnownFunction function =>
            y is ParameterConstraint.KnownFunction other
            ?
            Compare(function.Function, other.Function)
            :
            -1,

            ParameterConstraint.ClosedRecord record =>
            y is ParameterConstraint.ClosedRecord other
            ?
            Type(record.Type, other.Type)
            :
            1,

            _ =>
            throw new NotImplementedException($"{nameof(Constraint)} does not handle constraint variant: {x.GetType().Name}")
        };
    }

    private int Type(StructuralType x, StructuralType y)
    {
        var rank = TypeRank(x).CompareTo(TypeRank(y));

        if (rank is not 0)
            return rank;

        return x switch
        {
            StructuralType.IntType => 0,
            StructuralType.FloatType => 0,
            StructuralType.StringType => 0,
            StructuralType.CharType => 0,
            StructuralType.BoolType => 0,
            StructuralType.Self => 0,
            StructuralType.TypeVariable variable => variable.Index.CompareTo(((StructuralType.TypeVariable)y).Index),

            StructuralType.ConstrainedVariable variable =>
            Then(
                variable.Index.CompareTo(((StructuralType.ConstrainedVariable)y).Index),
                () => variable.Constraint.CompareTo(((StructuralType.ConstrainedVariable)y).Constraint)),

            StructuralType.FunctionType function =>
            Then(
                Type(function.ArgumentType, ((StructuralType.FunctionType)y).ArgumentType),
                () => Type(function.ReturnType, ((StructuralType.FunctionType)y).ReturnType)),

            StructuralType.ListType list => Type(list.ElementType, ((StructuralType.ListType)y).ElementType),

            StructuralType.TupleType tuple =>
            Sequence(tuple.ElementTypes, ((StructuralType.TupleType)y).ElementTypes, Type),

            StructuralType.ClosedRecord record => Fields(record.Fields, ((StructuralType.ClosedRecord)y).Fields),

            StructuralType.OpenRecord record =>
            Then(
                record.RowVariableIndex.CompareTo(((StructuralType.OpenRecord)y).RowVariableIndex),
                () => Fields(record.Fields, ((StructuralType.OpenRecord)y).Fields)),

            StructuralType.ChoiceType choice =>
            Sequence(
                choice.Tags.OrderBy(entry => entry.Key, StringComparer.Ordinal),
                ((StructuralType.ChoiceType)y).Tags.OrderBy(entry => entry.Key, StringComparer.Ordinal),
                (a, b) => Then(StringComparer.Ordinal.Compare(a.Key, b.Key), () => Sequence(a.Value, b.Value, Type))),

            StructuralType.TypeApplication application =>
            Then(
                Type(application.GenericType, ((StructuralType.TypeApplication)y).GenericType),
                () => Sequence(application.Arguments, ((StructuralType.TypeApplication)y).Arguments, Type)),

            _ =>
            throw new NotImplementedException($"{nameof(Type)} does not handle type variant: {x.GetType().Name}")
        };
    }

    private int Fields(IReadOnlyDictionary<string, StructuralType> x, IReadOnlyDictionary<string, StructuralType> y) =>
        Sequence(
            x.OrderBy(entry => entry.Key, StringComparer.Ordinal),
            y.OrderBy(entry => entry.Key, StringComparer.Ordinal),
            (a, b) => Then(StringComparer.Ordinal.Compare(a.Key, b.Key), () => Type(a.Value, b.Value)));

    private static int TypeRank(StructuralType type) =>
        type switch
        {
            StructuralType.IntType => 0,
            StructuralType.FloatType => 1,
            StructuralType.StringType => 2,
            StructuralType.CharType => 3,
            StructuralType.BoolType => 4,
            StructuralType.TypeVariable => 5,
            StructuralType.ConstrainedVariable => 6,
            StructuralType.FunctionType => 7,
            StructuralType.ListType => 8,
            StructuralType.TupleType => 9,
            StructuralType.ClosedRecord => 10,
            StructuralType.OpenRecord => 11,
            StructuralType.ChoiceType => 12,
            StructuralType.Self => 13,
            StructuralType.TypeApplication => 14,

            _ =>
            throw new NotImplementedException($"{nameof(TypeRank)} does not handle type variant: {type.GetType().Name}")
        };

    private static int Then(int first, Func<int> next) => first is 0 ? next() : first;

    private static int Sequence<T>(IEnumerable<T> x, IEnumerable<T> y, Func<T, T, int> compare)
    {
        using var a = x.GetEnumerator();
        using var b = y.GetEnumerator();

        while (a.MoveNext())
        {
            if (!b.MoveNext())
                return 1;

            var result = compare(a.Current, b.Current);

            if (result is not 0)
                return result;
        }

        return b.MoveNext() ? -1 : 0;
    }
}
