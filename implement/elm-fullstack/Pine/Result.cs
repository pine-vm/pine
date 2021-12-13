using System;

namespace Pine;

public record Result<ErrT, OkT>(ErrT? Err = default, OkT? Ok = default)
{
    static public Result<ErrT, OkT> err(ErrT err) =>
        new() { Err = err };

    static public Result<ErrT, OkT> ok(OkT ok) =>
        new() { Ok = ok };

    public Result<ErrT, MappedOkT> map<MappedOkT>(Func<OkT, MappedOkT> okMap)
    {
        if (Ok == null)
            return Result<ErrT, MappedOkT>.err(Err!);

        return Result<ErrT, MappedOkT>.ok(okMap(Ok));
    }

    public Result<MappedErrT, OkT> mapError<MappedErrT>(Func<ErrT, MappedErrT> errMap)
    {
        if (Ok == null)
            return Result<MappedErrT, OkT>.err(errMap(Err!));

        return Result<MappedErrT, OkT>.ok(Ok);
    }

    public Result<ErrT, MappedOkT> andThen<MappedOkT>(Func<OkT, Result<ErrT, MappedOkT>> okMap)
    {
        if (Ok == null)
            return Result<ErrT, MappedOkT>.err(Err!);

        return okMap(Ok);
    }
}
