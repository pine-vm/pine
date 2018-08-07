using System;
using System.Collections.Generic;

namespace PersistentAppFromElmCode.Common
{
    public interface IApp<RequestT, ResponseT>
    {
        ResponseT Request(RequestT serializedRequest);
    }

    public interface IAppWithCustomSerialization : IApp<string, string>
    {
    }

    public class PersistentAppFromElmCode
    {
        static public IAppWithCustomSerialization WithCustomSerialization(
            IReadOnlyCollection<(string, byte[])> elmCodeFiles,
            string pathToSerializedRequestFunction,
            string pathToSerializeStateFunction,
            string pathToDeserializeStateFunction)
        {
            throw new NotImplementedException();
        }
    }
}
