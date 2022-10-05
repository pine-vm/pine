using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

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

    static public Result<IImmutableList<ErrT>, OkT> FirstOkOrErrors<ErrT, OkT>(
        this IEnumerable<Func<Result<ErrT, OkT>>> candidates) =>
        candidates.Aggregate(
            seed: Result<IImmutableList<ErrT>, OkT>.err(ImmutableList<ErrT>.Empty),
            func: (accumulate, candidateFunc) =>
                accumulate.Unpack(
                    fromErr: previousErrors =>
                        candidateFunc()
                        .Unpack(
                            fromErr: newErr => Result<IImmutableList<ErrT>, OkT>.err(previousErrors.Add(newErr)),
                            fromOk: success => Result<IImmutableList<ErrT>, OkT>.ok(success)),
                    fromOk: _ => accumulate));
}
