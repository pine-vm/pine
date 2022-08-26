using System;
using System.Collections.Generic;

namespace Pine;

public static class ResultExtension
{
    public static Result<ErrT, IReadOnlyList<OkT>> ListCombine<ErrT, OkT>(this IReadOnlyList<Result<ErrT, OkT>> list)
    {
        var okList = new List<OkT>();

        foreach (var item in list)
        {
            if (item is Result<ErrT, OkT>.Err error)
                return new Result<ErrT, IReadOnlyList<OkT>>.Err(error.Value);

            if (item is not Result<ErrT, OkT>.Ok ok)
                throw new NotImplementedException();

            okList.Add(ok.Value);
        }

        return new Result<ErrT, IReadOnlyList<OkT>>.Ok(okList);
    }
}
