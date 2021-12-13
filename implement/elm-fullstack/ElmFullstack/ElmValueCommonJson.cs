using System;
using System.Collections.Generic;
using System.Collections.Immutable;

namespace ElmFullstack.ElmValueCommonJson;

public record Maybe<JustT>
{
    [Newtonsoft.Json.JsonProperty(NullValueHandling = Newtonsoft.Json.NullValueHandling.Ignore)]
    public IReadOnlyList<object>? Nothing { init; get; }

    [Newtonsoft.Json.JsonProperty(NullValueHandling = Newtonsoft.Json.NullValueHandling.Ignore)]
    public IReadOnlyList<JustT>? Just { init; get; }

    static public Maybe<JustT> just(JustT j) =>
        new() { Just = ImmutableList.Create(j) };

    static public Maybe<JustT> nothing() =>
        new() { Nothing = ImmutableList<object>.Empty };

    static public Maybe<JustT> NothingFromNull(JustT maybeNull) =>
        maybeNull == null
        ?
        nothing()
        :
        new Maybe<JustT> { Just = ImmutableList.Create(maybeNull) };
}

public record Result<ErrT, OkT>
{
    [Newtonsoft.Json.JsonProperty(NullValueHandling = Newtonsoft.Json.NullValueHandling.Ignore)]
    public IReadOnlyList<ErrT>? Err { init; get; }

    [Newtonsoft.Json.JsonProperty(NullValueHandling = Newtonsoft.Json.NullValueHandling.Ignore)]
    public IReadOnlyList<OkT>? Ok { init; get; }

    static public Result<ErrT, OkT> ok(OkT ok) =>
        new() { Ok = ImmutableList.Create(ok) };

    static public Result<ErrT, OkT> err(ErrT err) =>
        new() { Err = ImmutableList.Create(err) };

    public Result<ErrT, NewOkT> map<NewOkT>(Func<OkT, NewOkT> mapOk)
    {
        if (0 < Ok?.Count)
            return Result<ErrT, NewOkT>.ok(mapOk(Ok[0]));

        if (Err == null)
            throw new NullReferenceException();

        return Result<ErrT, NewOkT>.err(Err[0]);
    }
}
