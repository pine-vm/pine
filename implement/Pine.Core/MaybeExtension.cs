using System;

namespace Pine.Core;

/// <summary>
/// Extension methods for <see cref="Maybe{JustT}"/>.
/// </summary>
public static class MaybeExtension
{
    /// <summary>
    /// Convert a <see cref="Maybe{JustT}"/> to a <see cref="Result{ErrT, JustT}"/> with a default error value.
    /// </summary>
    /// <param name="self"><see cref="Maybe{JustT}"/> to convert.</param>"
    /// <param name="defaultErr">Error value to use if the Maybe is Nothing.</param>
    public static Result<ErrT, JustT> ToResult<JustT, ErrT>(
        this Maybe<JustT> self,
        ErrT defaultErr) =>
        self switch
        {
            Maybe<JustT>.Nothing =>
            Result<ErrT, JustT>.err(defaultErr),

            Maybe<JustT>.Just just =>
            Result<ErrT, JustT>.ok(just.Value),

            _ =>
            throw new NotImplementedException()
        };
}
