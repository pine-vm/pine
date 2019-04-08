using System;
using System.Linq;
using System.Security.Cryptography;

namespace Kalmit
{
    public class CommonConversion
    {
        static public byte[] ByteArrayFromStringBase16(string base16) =>
            Enumerable.Range(0, base16.Length / 2)
            .Select(octetIndex => Convert.ToByte(base16.Substring(octetIndex * 2, 2), 16))
            .ToArray();

        static public string StringBase16FromByteArray(byte[] array) =>
            BitConverter.ToString(array).Replace("-", "").ToUpperInvariant();

        static public byte[] HashSHA256(byte[] input)
        {
            using (var hasher = new SHA256Managed())
                return hasher.ComputeHash(input);
        }
    }
}