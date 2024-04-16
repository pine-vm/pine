using System.Collections.Generic;

namespace Pine;

public static class MaybeExtension
{
    public static Maybe<IReadOnlyList<JustT>> ListCombine<JustT>(this IEnumerable<Maybe<JustT>> list)
    {
        var justList = new List<JustT>();

        foreach (var item in list)
        {
            if (item is Maybe<JustT>.Nothing)
                return Maybe<IReadOnlyList<JustT>>.nothing();

            if (item is not Maybe<JustT>.Just just)
                return Maybe<IReadOnlyList<JustT>>.nothing();

            justList.Add(just.Value);
        }

        return Maybe<IReadOnlyList<JustT>>.just(justList);
    }
}
