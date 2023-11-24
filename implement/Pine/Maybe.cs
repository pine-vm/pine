using System;
using System.Text.Json.Serialization;

namespace Pine;

/// <summary>
/// A choice type that represents values that may or may not exist.
/// The only two possible variants are <see cref="Nothing"/> and <see cref="Just"/>.
/// </summary>
[JsonConverter(typeof(Json.JsonConverterForChoiceType))]
public abstract record Maybe<JustT>
{
    public static Maybe<JustT> nothing() => new Nothing();

    public static Maybe<JustT> just(JustT just) => new Just(just);

    public bool IsNothing() =>
        this switch
        {
            Nothing _ => true,
            _ => false
        };

    public bool IsJust() =>
        this switch
        {
            Just _ => true,
            _ => false
        };

    /// <summary>
    /// A <see cref="Maybe{JustT}"/> that is Nothing.
    /// </summary>
    public record Nothing : Maybe<JustT>;

    /// <summary>
    /// A <see cref="Maybe{JustT}"/> that is a Just.
    /// </summary>
    public record Just(JustT Value) : Maybe<JustT>;

    public Maybe<MappedT> Map<MappedT>(Func<JustT, MappedT> map) =>
        this switch
        {
            Nothing _ => Maybe<MappedT>.nothing(),
            Just just => Maybe<MappedT>.just(map(just.Value)),
            _ => throw new NotImplementedException()
        };

    public Maybe<MappedT> AndThen<MappedT>(Func<JustT, Maybe<MappedT>> map) =>
        this switch
        {
            Nothing _ => Maybe<MappedT>.nothing(),
            Just just => map(just.Value),
            _ => throw new NotImplementedException()
        };

    public Result<ErrT, JustT> ToResult<ErrT>(ErrT defaultErr) =>
        this switch
        {
            Nothing _ => Result<ErrT, JustT>.err(defaultErr),
            Just just => Result<ErrT, JustT>.ok(just.Value),
            _ => throw new NotImplementedException()
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

public static class Maybe
{
    public static Maybe<ClassJustT> NothingFromNull<ClassJustT>(ClassJustT? maybeNull)
        where ClassJustT : class =>
        maybeNull is { } notNull ? Maybe<ClassJustT>.just(notNull) : Maybe<ClassJustT>.nothing();

    public static Maybe<StructJustT> NothingFromNull<StructJustT>(StructJustT? maybeNull)
        where StructJustT : struct =>
        maybeNull is { } notNull ? Maybe<StructJustT>.just(notNull) : Maybe<StructJustT>.nothing();
}
