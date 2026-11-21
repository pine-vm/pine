using Microsoft.CodeAnalysis.CSharp;
using Pine.Core.Addressing;
using Pine.Core.Encodings;
using Pine.Core.Functional;
using Pine.Core.PineLanguage;
using System;
using System.Collections.Generic;
using System.Globalization;
using System.Linq;
using System.Numerics;

namespace Pine.Core.DotNet;

internal sealed class PineCSharpLiteralNaming(ConcurrentPineValueHashCache hashCache)
{
    private readonly Dictionary<string, PineValue> _declarations = new(StringComparer.Ordinal);

    public string ValueName(PineValue value)
    {
        var integer = IntegerEncoding.ParseSignedIntegerStrict(value).IsOkOrNullable();

        if (integer is { } number)
        {
            var magnitude = BigInteger.Abs(number);
            var hexadecimal = magnitude.ToString("x", CultureInfo.InvariantCulture).TrimStart('0');

            return
                Register(
                    "Value_int_" + (number.Sign < 0 ? "neg" : "pos") + "_" +
                    magnitude.ToString(CultureInfo.InvariantCulture) +
                    (magnitude < 16 ? "" : "_0x" + hexadecimal),
                    value);
        }

        var hash = ShortHash(value);

        if (value is PineValue.BlobValue blob && blob.Bytes.Length % 4 == 0 &&
            StringEncoding.StringFromBlobValue(blob.Bytes).IsOkOrNull() is { } text &&
            text.All(character => char.IsLetterOrDigit(character) || character == '_'))
        {
            var name = "Value_string_" + text + "_" + hash;

            if (SyntaxFacts.IsValidIdentifier(name))
                return Register(name, value);
        }

        return Register("Value_" + hash, value);
    }

    public string TableName(string prefix, PineValue contents) =>
        Register(prefix + "_" + ShortHash(contents), contents);

    private string ShortHash(PineValue value) =>
        Convert.ToHexStringLower(hashCache.GetHash(value).Span[..5]);

    private string Register(string name, PineValue value)
    {
        if (_declarations.TryGetValue(name, out var previous) && !previous.Equals(value))
            throw new InvalidOperationException("Distinct literals have the same generated C# name: " + name);

        _declarations[name] = value;
        return name;
    }
}
