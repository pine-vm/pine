using System.Collections.Generic;
using System.Collections.Immutable;

namespace Kalmit.ElmJsonStructure
{
    public class Maybe<JustT>
    {
        [Newtonsoft.Json.JsonProperty(NullValueHandling = Newtonsoft.Json.NullValueHandling.Ignore)]
        public IReadOnlyList<object> Nothing;

        [Newtonsoft.Json.JsonProperty(NullValueHandling = Newtonsoft.Json.NullValueHandling.Ignore)]
        public IReadOnlyList<JustT> Just;

        static public Maybe<JustT> just(JustT j) =>
            new Maybe<JustT> { Just = ImmutableList.Create(j) };

        static public Maybe<JustT> nothing() =>
            new Maybe<JustT> { Nothing = ImmutableList<object>.Empty };

        static public Maybe<JustT> NothingFromNull(JustT maybeNull) =>
            maybeNull == null
            ?
            nothing()
            :
            new Maybe<JustT> { Just = ImmutableList.Create(maybeNull) };
    }
}

