using System;
using System.Text.Json.Serialization;

namespace Pine.Core;

/// <summary>
/// A choice type that represents values that may or may not exist.
/// The only two variants are <see cref="Nothing"/> and <see cref="Just"/>.
/// </summary>
[JsonConverter(typeof(Json.JsonConverterForChoiceType))]
public abstract record Maybe<JustT>
{
    /// <summary>
    /// Explicit constructor for the 'Nothing' case.
    /// </summary>
    public static Maybe<JustT> nothing() => new Nothing();

    /// <summary>
    /// Explicit constructor for the 'Just' case.
    /// </summary>
    public static Maybe<JustT> just(JustT just) => new Just(just);

    /// <summary>
    /// Implicit constructor for the 'Just' case.
    /// </summary>
    public static implicit operator Maybe<JustT>(JustT just) =>
        Maybe<JustT>.just(just);

    /// <summary>
    /// A <see cref="Maybe{JustT}"/> that is Nothing.
    /// </summary>
    public record Nothing : Maybe<JustT>;

    /// <summary>
    /// A <see cref="Maybe{JustT}"/> that is a Just.
    /// </summary>
    public record Just(JustT Value) : Maybe<JustT>;

    /// <summary>
    /// Transform a Maybe value with a given function.
    /// </summary>
    public Maybe<MappedT> Map<MappedT>(Func<JustT, MappedT> map) =>
        this switch
        {
            Nothing _ =>
            Maybe<MappedT>.nothing(),

            Just just =>
            Maybe<MappedT>.just(map(just.Value)),

            _ =>
            throw new NotImplementedException()
        };

    /// <summary>
    /// Chain a Maybe value with a given method.
    /// The <paramref name="map"/> method is only invoked if the current instance is a <see cref="Just"/>.
    /// Returns <see cref="Nothing" /> if either the current instance is <see cref="Nothing" /> or the <paramref name="map"/> method returns <see cref="Nothing" />.
    /// </summary>
    public Maybe<MappedT> AndThen<MappedT>(
        Func<JustT, Maybe<MappedT>> map) =>
        this switch
        {
            Nothing =>
            Maybe<MappedT>.nothing(),

            Just just =>
            map(just.Value),

            _ =>
            throw new NotImplementedException()
        };

    /// <summary>
    /// Provide a default value, turning an optional value into a normal value.
    /// </summary>
    public JustT WithDefault(JustT defaultIfNothing) =>
        WithDefaultBuilder(() => defaultIfNothing);

    /// <summary>
    /// Provide a default value, turning an optional value into a normal value.
    /// </summary>
    public JustT WithDefaultBuilder(Func<JustT> buildDefault) =>
        this switch
        {
            Just just => just.Value,
            _ => buildDefault()
        };
}

/// <summary>
/// Namespace for static generic functions to work with <see cref="Maybe{JustT}"/>.
/// </summary>
public static class Maybe
{
    /// <summary>
    /// Convert a .NET nullable reference type to a <see cref="Maybe{JustT}"/>.
    /// </summary>
    public static Maybe<ClassJustT> NothingFromNull<ClassJustT>(ClassJustT? maybeNull)
        where ClassJustT : class =>
        maybeNull is { } notNull ?
        Maybe<ClassJustT>.just(notNull) :
        Maybe<ClassJustT>.nothing();

    /// <summary>
    /// Convert a .NET nullable value type to a <see cref="Maybe{JustT}"/>.
    /// </summary>
    public static Maybe<StructJustT> NothingFromNull<StructJustT>(StructJustT? maybeNull)
        where StructJustT : struct =>
        maybeNull is { } notNull ?
        Maybe<StructJustT>.just(notNull) :
        Maybe<StructJustT>.nothing();
}
