using System;
using System.Linq;

namespace Kalmit
{
    public class CommonConversion
    {
        public static byte[] ByteArrayFromStringBase16(string base16) =>
            Enumerable.Range(0, base16.Length / 2)
            .Select(octetIndex => Convert.ToByte(base16.Substring(octetIndex * 2, 2), 16))
            .ToArray();
    }
}