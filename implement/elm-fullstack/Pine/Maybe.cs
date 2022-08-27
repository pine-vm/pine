using System;
using System.Text.Json.Serialization;

namespace Pine;

[JsonConverter(typeof(Json.JsonConverterForDiscriminatedUnionType))]
/// <summary>
/// A discriminated union type that represents values that may or may not exist.
/// The only two possible variants are <see cref="Nothing"/> and <see cref="Just"/>.
/// </summary>
public abstract record Maybe<JustT>
{
    static public Maybe<JustT> nothing() => new Nothing();

    static public Maybe<JustT> just(JustT just) => new Just(just);

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
    public record Nothing() : Maybe<JustT>;

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

    /// <summary>
    /// Provide a default value, turning an optional value into a normal value.
    /// </summary>
    public JustT WithDefault(Func<JustT> getDefault) =>
        this switch
        {
            Just just => just.Value,
            _ => getDefault()
        };
}
