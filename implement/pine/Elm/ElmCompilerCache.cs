using Pine.Core;
using Pine.Core.Elm;
using System.Collections.Generic;

namespace Pine.Elm;

public class ElmCompilerCache
{

    private readonly Dictionary<PineValue, PineValue> _encodedForCompilerCache = [];

    private readonly Dictionary<ElmValue, PineValue> _elmValueAsPineValueCache = [];

    private readonly Dictionary<PineValue, ElmValue> _pineValueDecodedAsElmValueCache = [];

    private readonly Dictionary<PineValue, ElmValue> _pineValueEncodedAsInElmCompilerCache = [];

    private readonly Dictionary<ElmValue, PineValue> _elmValueDecodedAsInElmCompilerCache = [];

    private readonly Dictionary<ElmValue, Expression> _elmValueDecodedAsExpressionElmCompilerCache = [];

    private readonly System.Threading.Lock _cachesLock = new();


    public PineValue EncodeValueForCompiler(PineValue pineValue)
    {
        lock (_cachesLock)
        {
            if (_encodedForCompilerCache.TryGetValue(pineValue, out var encoded))
            {
                return encoded;
            }

            encoded =
                ElmValueAsPineValue(
                    ElmValueInterop.PineValueEncodedAsInElmCompiler(
                        pineValue,
                        additionalReusableEncodings:
                        _pineValueEncodedAsInElmCompilerCache,
                        reportNewEncoding:
                        (pineValue, encoding) => _pineValueEncodedAsInElmCompilerCache.TryAdd(pineValue, encoding)));

            _encodedForCompilerCache.TryAdd(pineValue, encoded);

            return encoded;
        }
    }

    public PineValue ElmValueAsPineValue(ElmValue elmValue)
    {
        lock (_cachesLock)
        {
            if (_elmValueAsPineValueCache.TryGetValue(elmValue, out var encoded))
            {
                return encoded;
            }

            encoded =
                ElmValueEncoding.ElmValueAsPineValue(
                    elmValue,
                    additionalReusableEncodings:
                    _elmValueAsPineValueCache,
                    reportNewEncoding:
                    (elmValue, encoded) => _elmValueAsPineValueCache.TryAdd(elmValue, encoded));

            _elmValueAsPineValueCache.TryAdd(elmValue, encoded);

            return encoded;
        }
    }

    public Result<string, ElmValue> PineValueDecodedAsElmValue(PineValue pineValue)
    {
        lock (_cachesLock)
        {
            if (_pineValueDecodedAsElmValueCache.TryGetValue(pineValue, out var decoded))
            {
                return decoded;
            }

            var decodeResult =
                ElmValueEncoding.PineValueAsElmValue(
                    pineValue,
                    additionalReusableDecodings:
                    _pineValueDecodedAsElmValueCache,
                    reportNewDecoding:
                    (pineValue, decoding) => _pineValueDecodedAsElmValueCache.TryAdd(pineValue, decoding));

            if (decodeResult.IsOkOrNull() is { } decodedOk)
            {
                _pineValueDecodedAsElmValueCache.TryAdd(pineValue, decodedOk);
            }

            return decodeResult;
        }
    }

    public Result<string, PineValue> DecodeElmValueFromCompiler(ElmValue elmValue)
    {
        lock (_cachesLock)
        {
            if (_elmValueDecodedAsInElmCompilerCache.TryGetValue(elmValue, out var decoded))
            {
                return decoded;
            }

            var decodeResult =
                ElmValueInterop.ElmValueDecodedAsInElmCompiler(
                    elmValue,
                    additionalReusableDecodings:
                    _elmValueDecodedAsInElmCompilerCache,
                    reportNewDecoding:
                    (elmValue, decoded) => _elmValueDecodedAsInElmCompilerCache.TryAdd(elmValue, decoded));

            if (decodeResult.IsOkOrNull() is { } decodedOk)
            {
                _elmValueDecodedAsInElmCompilerCache.TryAdd(elmValue, decodedOk);
            }

            return decodeResult;
        }
    }

    public Result<string, Expression> DecodeExpressionFromElmValue(
        ElmValue exprEncoded)
    {
        lock (_cachesLock)
        {
            return
                ElmValueInterop.ElmValueFromCompilerDecodedAsExpression(
                    exprEncoded,
                    additionalReusableDecodings:
                    _elmValueDecodedAsExpressionElmCompilerCache,
                    reportNewDecoding:
                    (elmValue, decoding) =>
                    _elmValueDecodedAsExpressionElmCompilerCache.TryAdd(elmValue, decoding),
                    literalAdditionalReusableDecodings:
                    _elmValueDecodedAsInElmCompilerCache,
                    literalReportNewDecoding:
                    (elmValue, decoding) =>
                    _elmValueDecodedAsInElmCompilerCache.TryAdd(elmValue, decoding));
        }
    }
}
