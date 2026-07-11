using Pine.Core;
using Pine.Core.Elm;
using System.Collections.Generic;

namespace Pine.Elm;

public class ElmCompilerCache
{

    private readonly Dictionary<PineValue, PineValue> encodedForCompilerCache = [];

    private readonly Dictionary<ElmValue, PineValue> elmValueAsPineValueCache = [];

    private readonly Dictionary<PineValue, ElmValue> pineValueDecodedAsElmValueCache = [];

    private readonly Dictionary<PineValue, ElmValue> pineValueEncodedAsInElmCompilerCache = [];

    private readonly Dictionary<ElmValue, PineValue> elmValueDecodedAsInElmCompilerCache = [];

    private readonly Dictionary<ElmValue, Expression> elmValueDecodedAsExpressionElmCompilerCache = [];

    private readonly System.Threading.Lock cachesLock = new();


    public PineValue EncodeValueForCompiler(PineValue pineValue)
    {
        lock (cachesLock)
        {
            if (encodedForCompilerCache.TryGetValue(pineValue, out var encoded))
            {
                return encoded;
            }

            encoded =
                ElmValueAsPineValue(
                    ElmValueInterop.PineValueEncodedAsInElmCompiler(
                        pineValue,
                        additionalReusableEncodings:
                        pineValueEncodedAsInElmCompilerCache,
                        reportNewEncoding:
                        (pineValue, encoding) => pineValueEncodedAsInElmCompilerCache.TryAdd(pineValue, encoding)));

            encodedForCompilerCache.TryAdd(pineValue, encoded);

            return encoded;
        }
    }

    public PineValue ElmValueAsPineValue(ElmValue elmValue)
    {
        lock (cachesLock)
        {
            if (elmValueAsPineValueCache.TryGetValue(elmValue, out var encoded))
            {
                return encoded;
            }

            encoded =
                ElmValueEncoding.ElmValueAsPineValue(
                    elmValue,
                    additionalReusableEncodings:
                    elmValueAsPineValueCache,
                    reportNewEncoding:
                    (elmValue, encoded) => elmValueAsPineValueCache.TryAdd(elmValue, encoded));

            elmValueAsPineValueCache.TryAdd(elmValue, encoded);

            return encoded;
        }
    }

    public Result<string, ElmValue> PineValueDecodedAsElmValue(PineValue pineValue)
    {
        lock (cachesLock)
        {
            if (pineValueDecodedAsElmValueCache.TryGetValue(pineValue, out var decoded))
            {
                return decoded;
            }

            var decodeResult =
                ElmValueEncoding.PineValueAsElmValue(
                    pineValue,
                    additionalReusableDecodings:
                    pineValueDecodedAsElmValueCache,
                    reportNewDecoding:
                    (pineValue, decoding) => pineValueDecodedAsElmValueCache.TryAdd(pineValue, decoding));

            if (decodeResult.IsOkOrNull() is { } decodedOk)
            {
                pineValueDecodedAsElmValueCache.TryAdd(pineValue, decodedOk);
            }

            return decodeResult;
        }
    }

    public Result<string, PineValue> DecodeElmValueFromCompiler(ElmValue elmValue)
    {
        lock (cachesLock)
        {
            if (elmValueDecodedAsInElmCompilerCache.TryGetValue(elmValue, out var decoded))
            {
                return decoded;
            }

            var decodeResult =
                ElmValueInterop.ElmValueDecodedAsInElmCompiler(
                    elmValue,
                    additionalReusableDecodings:
                    elmValueDecodedAsInElmCompilerCache,
                    reportNewDecoding:
                    (elmValue, decoded) => elmValueDecodedAsInElmCompilerCache.TryAdd(elmValue, decoded));

            if (decodeResult.IsOkOrNull() is { } decodedOk)
            {
                elmValueDecodedAsInElmCompilerCache.TryAdd(elmValue, decodedOk);
            }

            return decodeResult;
        }
    }

    public Result<string, Expression> DecodeExpressionFromElmValue(
        ElmValue exprEncoded)
    {
        lock (cachesLock)
        {
            return
                ElmValueInterop.ElmValueFromCompilerDecodedAsExpression(
                    exprEncoded,
                    additionalReusableDecodings:
                    elmValueDecodedAsExpressionElmCompilerCache,
                    reportNewDecoding:
                    (elmValue, decoding) =>
                    elmValueDecodedAsExpressionElmCompilerCache.TryAdd(elmValue, decoding),
                    literalAdditionalReusableDecodings:
                    elmValueDecodedAsInElmCompilerCache,
                    literalReportNewDecoding:
                    (elmValue, decoding) =>
                    elmValueDecodedAsInElmCompilerCache.TryAdd(elmValue, decoding));
        }
    }
}
