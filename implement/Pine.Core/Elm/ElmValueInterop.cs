using Pine.Core.PopularEncodings;
using System;
using System.Collections.Generic;

namespace Pine.Core.Elm;

/// <summary>
/// Helpers to convert between <see cref="PineValue"/>, <see cref="ElmValue"/>, and compiler-facing encodings.
/// </summary>
/// <remarks>
/// This type provides utilities to encode/decode values as produced or consumed by the Elm compiler
/// and to parse common Elm ADTs (Maybe, Result) from Pine/Elm representations.
/// </remarks>
public class ElmValueInterop
{
    /// <summary>
    /// Pine-encoded string value for the Elm constructor name "Nothing".
    /// </summary>
    public static readonly PineValue String_Nothing_Value =
        StringEncoding.ValueFromString("Nothing");

    /// <summary>
    /// Pine-encoded string value for the Elm constructor name "Just".
    /// </summary>
    public static readonly PineValue String_Just_Value =
        StringEncoding.ValueFromString("Just");

    /// <summary>
    /// Pine-encoded string value for the Elm constructor name "Err".
    /// </summary>
    public static readonly PineValue String_Err_Value =
        StringEncoding.ValueFromString("Err");

    /// <summary>
    /// Pine-encoded string value for the Elm constructor name "Ok".
    /// </summary>
    public static readonly PineValue String_Ok_Value =
        StringEncoding.ValueFromString("Ok");

    /// <summary>
    /// Encodes a <see cref="PineValue"/> to an <see cref="ElmValue"/> using the same scheme as the Elm compiler.
    /// </summary>
    /// <param name="pineValue">The value to encode.</param>
    /// <returns>The encoded <see cref="ElmValue"/>.</returns>
    public static ElmValue PineValueEncodedAsInElmCompiler(
        PineValue pineValue) =>
        PineValueEncodedAsInElmCompiler(
            pineValue,
            additionalReusableEncodings: null,
            reportNewEncoding: null);

    /// <summary>
    /// Encode as in https://github.com/pine-vm/pine/blob/ef26bed9aa54397e476545d9e30821565139d821/implement/pine/ElmTime/compile-elm-program/src/Pine.elm#L75-L77
    /// </summary>
    /// <param name="pineValue">The value to encode.</param>
    /// <param name="additionalReusableEncodings">Optional cache of precomputed encodings to reuse.</param>
    /// <param name="reportNewEncoding">Optional callback invoked when a new encoding is produced.</param>
    /// <returns>The encoded <see cref="ElmValue"/>.</returns>
    public static ElmValue PineValueEncodedAsInElmCompiler(
        PineValue pineValue,
        IReadOnlyDictionary<PineValue, ElmValue>? additionalReusableEncodings,
        Action<PineValue, ElmValue>? reportNewEncoding)
    {
        {
            if (additionalReusableEncodings?.TryGetValue(pineValue, out var reused) ?? false)
            {
                return reused;
            }
        }

        {
            if (ReusedInstances.Instance.ElmValueEncodedAsInElmCompiler?.TryGetValue(pineValue, out var reused) ?? false)
            {
                return reused;
            }
        }

        switch (pineValue)
        {
            case PineValue.BlobValue blobValue:
                {
                    var byteItems = new ElmValue[blobValue.Bytes.Length];

                    for (var byteIndex = 0; byteIndex < byteItems.Length; byteIndex++)
                    {
                        var byteValue = blobValue.Bytes.Span[byteIndex];

                        byteItems[byteIndex] = ElmValue.Integer(byteValue);
                    }

                    var encoded =
                        ElmValue.TagInstance("BlobValue", [ElmValue.ListInstance(byteItems)]);

                    reportNewEncoding?.Invoke(pineValue, encoded);

                    return encoded;
                }
            case PineValue.ListValue listValue:
                {
                    var elementsSpan = listValue.Items.Span;

                    var items = new ElmValue[elementsSpan.Length];

                    for (var i = 0; i < elementsSpan.Length; i++)
                    {
                        var item = elementsSpan[i];

                        var itemEncoded =
                            PineValueEncodedAsInElmCompiler(
                                item,
                                additionalReusableEncodings,
                                reportNewEncoding);

                        items[i] = itemEncoded;
                    }

                    var encoded =
                        ElmValue.TagInstance("ListValue", [ElmValue.ListInstance(items)]);

                    reportNewEncoding?.Invoke(pineValue, encoded);

                    return encoded;
                }

            default:
                throw new NotImplementedException(
                    "Unsupported PineValue: " + pineValue.GetType().FullName);
        }
    }

    /// <summary>
    /// Decodes an <see cref="ElmValue"/> that was encoded using <see cref="PineValueEncodedAsInElmCompiler(PineValue, IReadOnlyDictionary{PineValue, ElmValue}?, Action{PineValue, ElmValue}?)"/> back into <see cref="PineValue"/>.
    /// </summary>
    /// <param name="elmValue">The encoded value.</param>
    /// <param name="additionalReusableDecodings">Optional cache of precomputed decodings to reuse.</param>
    /// <param name="reportNewDecoding">Optional callback invoked when a new decoding is produced.</param>
    /// <returns>A <see cref="Result{ErrT, OkT}"/> with the decoded <see cref="PineValue"/> or an error message.</returns>
    public static Result<string, PineValue> ElmValueDecodedAsInElmCompiler(
        ElmValue elmValue,
        IReadOnlyDictionary<ElmValue, PineValue>? additionalReusableDecodings,
        Action<ElmValue, PineValue>? reportNewDecoding)
    {
        if (ReusedInstances.Instance.ElmValueDecodedAsInElmCompiler?.TryGetValue(elmValue, out var pineValue) ?? false)
        {
            return pineValue;
        }

        if (additionalReusableDecodings?.TryGetValue(elmValue, out pineValue) ?? false)
        {
            return pineValue;
        }

        var decodeResult =
            elmValue switch
            {
                ElmValue.ElmTag tag =>
                tag.TagName switch
                {
                    "BlobValue" =>
                    tag.Arguments switch
                    {
                        [ElmValue.ElmList firstArgument] =>
                            ElmValueBlobValueDecodedAsInElmCompiler(firstArgument.Items),

                        _ =>
                            "Invalid arguments for BlobValue tag"
                    },

                    "ListValue" =>
                    tag.Arguments switch
                    {
                        [ElmValue.ElmList firstArgument] =>
                            ElmValueListValueDecodedAsInElmCompiler(
                                firstArgument.Items,
                                additionalReusableDecodings,
                                reportNewDecoding),

                        _ =>
                            "Invalid arguments for ListValue tag"
                    },

                    _ =>
                    "Unsupported tag: " + tag.TagName
                },

                _ =>
                "Unsupported ElmValue: " + elmValue.GetType().FullName
            };

        if (decodeResult.IsOkOrNull() is { } decodeOk)
        {
            reportNewDecoding?.Invoke(elmValue, decodeOk);
        }

        return decodeResult;
    }

    /// <summary>
    /// Decodes an Elm tag "BlobValue" with a list of integers (0..255) into a Pine blob.
    /// </summary>
    /// <param name="byteItems">The list of byte values as <see cref="ElmValue"/> integers.</param>
    /// <returns>A <see cref="Result{ErrT, OkT}"/> with the decoded <see cref="PineValue"/> or an error message.</returns>
    public static Result<string, PineValue> ElmValueBlobValueDecodedAsInElmCompiler(
        IReadOnlyList<ElmValue> byteItems)
    {
        if (byteItems.Count is 0)
            return PineValue.EmptyBlob;

        var bytes = new byte[byteItems.Count];

        for (var i = 0; i < byteItems.Count; i++)
        {
            var itemValue = byteItems[i];

            if (itemValue is not ElmValue.ElmInteger byteElement)
            {
                return
                    "Invalid item under BlobValue tag at index " + i + " of " + byteItems.Count +
                    ": " +
                    itemValue.ToString();
            }

            if (byteElement.Value < 0 || byteElement.Value > 0xff)
            {
                return
                    "Invalid item under BlobValue tag at index " + i + " of " + byteItems.Count +
                    ": Integer out of range: " +
                    byteElement.Value;
            }

            bytes[i] = (byte)byteElement.Value;
        }

        return PineValue.Blob(bytes);
    }

    /// <summary>
    /// Decodes an Elm tag "ListValue" with an item list into a Pine list value, recursively decoding nested items.
    /// </summary>
    /// <param name="listItems">The list of Elm items to decode.</param>
    /// <param name="additionalReusableEncodings">Optional cache of precomputed decodings to reuse.</param>
    /// <param name="reportNewEncoding">Optional callback invoked when a new decoding is produced.</param>
    /// <returns>A <see cref="Result{ErrT, OkT}"/> with the decoded <see cref="PineValue"/> or an error message.</returns>
    public static Result<string, PineValue> ElmValueListValueDecodedAsInElmCompiler(
        IReadOnlyList<ElmValue> listItems,
        IReadOnlyDictionary<ElmValue, PineValue>? additionalReusableEncodings,
        Action<ElmValue, PineValue>? reportNewEncoding)
    {
        if (listItems.Count is 0)
            return PineValue.EmptyList;

        var items = new PineValue[listItems.Count];

        for (var i = 0; i < listItems.Count; i++)
        {
            var itemResult =
                ElmValueDecodedAsInElmCompiler(
                    listItems[i],
                    additionalReusableEncodings,
                    reportNewEncoding);

            if (itemResult.IsOkOrNull() is { } itemOk)
            {
                items[i] = itemOk;
                continue;
            }

            string? tagName = null;

            if (items.Length is 2 && i is 1)
            {
                tagName = StringEncoding.StringFromValue(items[0]).IsOkOrNull();
            }

            if (itemResult.IsErrOrNull() is { } itemErr)
            {
                return
                    "Error decoding list item at index " + i + " of " + listItems.Count +
                    (tagName is null ? null : " (after tag '" + tagName + "')") +
                    ": " + itemErr;
            }

            throw new NotImplementedException(
                "Unexpected result type for list item: " + itemResult.GetType().FullName);
        }

        return PineValue.List(items);
    }

    /// <summary>
    /// Decodes an Elm compiler representation of an expression into the internal <see cref="Expression"/> model.
    /// </summary>
    /// <param name="elmValue">The Elm value representing the expression.</param>
    /// <param name="additionalReusableDecodings">Optional cache for expression decodings.</param>
    /// <param name="reportNewDecoding">Optional callback invoked when a new expression decoding is produced.</param>
    /// <param name="literalAdditionalReusableDecodings">Optional cache for literal value decodings.</param>
    /// <param name="literalReportNewDecoding">Optional callback invoked when a new literal value decoding is produced.</param>
    /// <returns>A <see cref="Result{ErrT, OkT}"/> with the decoded <see cref="Expression"/> or an error message.</returns>
    public static Result<string, Expression> ElmValueFromCompilerDecodedAsExpression(
        ElmValue elmValue,
        IReadOnlyDictionary<ElmValue, Expression>? additionalReusableDecodings,
        Action<ElmValue, Expression>? reportNewDecoding,
        IReadOnlyDictionary<ElmValue, PineValue>? literalAdditionalReusableDecodings,
        Action<ElmValue, PineValue>? literalReportNewDecoding)
    {
        if (elmValue is not ElmValue.ElmTag tag)
        {
            return "Unexpected type of ElmValue: " + elmValue.GetType().FullName;
        }

        if (additionalReusableDecodings?.TryGetValue(elmValue, out var reusedDecoding) ?? false &&
            reusedDecoding is not null)
        {
            return reusedDecoding;
        }

        Result<string, Expression> WithoutReport()
        {
            if (tag.TagName is "LiteralExpression")
            {
                if (tag.Arguments.Count is not 1)
                {
                    return "Invalid arguments for tag " + tag.TagName + ": " + tag;
                }

                var firstArgument = tag.Arguments[0];

                var literalValueResult =
                    ElmValueDecodedAsInElmCompiler(
                        firstArgument,
                        literalAdditionalReusableDecodings,
                        literalReportNewDecoding);

                if (literalValueResult.IsOkOrNull() is { } literalValueOk)
                {
                    return Expression.LiteralInstance(literalValueOk);
                }

                if (literalValueResult.IsErrOrNull() is { } literalValueErr)
                {
                    return literalValueErr;
                }

                throw new NotImplementedException(
                    "Unexpected result type for literal value: " + literalValueResult.GetType().FullName);
            }

            if (tag.TagName is "ListExpression")
            {
                if (tag.Arguments.Count is not 1)
                {
                    return "Invalid arguments for tag " + tag.TagName + ": " + tag;
                }

                var firstArgument = tag.Arguments[0];

                if (firstArgument is not ElmValue.ElmList firstList)
                {
                    return "Invalid arguments for tag " + tag.TagName + ": " + tag;
                }

                var items = new Expression[firstList.Items.Count];

                for (var i = 0; i < firstList.Items.Count; i++)
                {
                    var itemResult =
                        ElmValueFromCompilerDecodedAsExpression(
                            firstList.Items[i],
                            additionalReusableDecodings,
                            reportNewDecoding,
                            literalAdditionalReusableDecodings,
                            literalReportNewDecoding);

                    if (itemResult.IsOkOrNull() is { } itemOk)
                    {
                        items[i] = itemOk;
                        continue;
                    }

                    if (itemResult.IsErrOrNull() is { } itemErr)
                    {
                        return "Failed for list item [" + i + "]: " + itemErr;
                    }

                    throw new NotImplementedException(
                        "Unexpected result type for list item: " + itemResult.GetType().FullName);
                }

                return Expression.ListInstance(items);
            }

            if (tag.TagName is "ParseAndEvalExpression")
            {
                if (tag.Arguments.Count is not 2)
                {
                    return "Invalid arguments for tag " + tag.TagName + ": " + tag;
                }

                var encoded = tag.Arguments[0];
                var environment = tag.Arguments[1];

                var encodedResult =
                    ElmValueFromCompilerDecodedAsExpression(
                        encoded,
                        additionalReusableDecodings,
                        reportNewDecoding,
                        literalAdditionalReusableDecodings,
                        literalReportNewDecoding);

                if (encodedResult.IsOkOrNull() is not { } encodedOk)
                {
                    if (encodedResult.IsErrOrNull() is { } encodedErr)
                    {
                        return "Failed for parse and eval encoded: " + encodedErr;
                    }

                    throw new NotImplementedException(
                        "Unexpected result type for encoded: " + encodedResult.GetType().FullName);
                }

                var environmentResult =
                    ElmValueFromCompilerDecodedAsExpression(
                        environment,
                        additionalReusableDecodings,
                        reportNewDecoding,
                        literalAdditionalReusableDecodings,
                        literalReportNewDecoding);

                if (environmentResult.IsOkOrNull() is not { } environmentOk)
                {
                    if (environmentResult.IsErrOrNull() is { } environmentErr)
                    {
                        return "Failed for parse and eval environment: " + environmentErr;
                    }

                    throw new NotImplementedException(
                        "Unexpected result type for environment: " + environmentResult.GetType().FullName);
                }

                return new Expression.ParseAndEval(encodedOk, environmentOk);
            }

            if (tag.TagName is "KernelApplicationExpression")
            {
                if (tag.Arguments.Count is not 2)
                {
                    return "Invalid arguments for tag " + tag.TagName + ": " + tag;
                }

                var function = tag.Arguments[0];
                var environment = tag.Arguments[1];

                if (function is not ElmValue.ElmString functionString)
                {
                    return "Invalid arguments for tag " + tag.TagName + ": " + tag;
                }

                var environmentResult =
                    ElmValueFromCompilerDecodedAsExpression(
                        environment,
                        additionalReusableDecodings,
                        reportNewDecoding,
                        literalAdditionalReusableDecodings,
                        literalReportNewDecoding);

                if (environmentResult.IsErrOrNull() is { } environmentErr)
                {
                    return "Failed for kernel application environment: " + environmentErr;
                }

                if (environmentResult.IsOkOrNull() is not { } environmentOk)
                {
                    throw new NotImplementedException(
                        "Unexpected result type for environment: " + environmentResult.GetType().FullName);
                }

                return Expression.KernelApplicationInstance(functionString.Value, environmentOk);
            }

            if (tag.TagName is "ConditionalExpression")
            {
                if (tag.Arguments.Count is not 3)
                {
                    return "Invalid arguments for tag " + tag.TagName + ": " + tag;
                }

                var condition = tag.Arguments[0];
                var falseBranch = tag.Arguments[1];
                var trueBranch = tag.Arguments[2];

                var conditionResult =
                    ElmValueFromCompilerDecodedAsExpression(
                        condition,
                        additionalReusableDecodings,
                        reportNewDecoding,
                        literalAdditionalReusableDecodings,
                        literalReportNewDecoding);

                if (conditionResult.IsErrOrNull() is { } conditionErr)
                {
                    return "Failed for conditional condition: " + conditionErr;
                }

                if (conditionResult.IsOkOrNull() is not { } conditionOk)
                {
                    throw new NotImplementedException(
                        "Unexpected result type for condition: " + conditionResult.GetType().FullName);
                }

                var falseBranchResult =
                    ElmValueFromCompilerDecodedAsExpression(
                        falseBranch,
                        additionalReusableDecodings,
                        reportNewDecoding,
                        literalAdditionalReusableDecodings,
                        literalReportNewDecoding);

                if (falseBranchResult.IsErrOrNull() is { } falseBranchErr)
                {
                    return "Failed for conditional false branch: " + falseBranchErr;
                }

                if (falseBranchResult.IsOkOrNull() is not { } falseBranchOk)
                {
                    throw new NotImplementedException(
                        "Unexpected result type for false branch: " + falseBranchResult.GetType().FullName);
                }

                var trueBranchResult =
                    ElmValueFromCompilerDecodedAsExpression(
                        trueBranch,
                        additionalReusableDecodings,
                        reportNewDecoding,
                        literalAdditionalReusableDecodings,
                        literalReportNewDecoding);

                if (trueBranchResult.IsErrOrNull() is { } trueBranchErr)
                {
                    return "Failed for conditional true branch: " + trueBranchErr;
                }

                if (trueBranchResult.IsOkOrNull() is not { } trueBranchOk)
                {
                    throw new NotImplementedException(
                        "Unexpected result type for true branch: " + trueBranchResult.GetType().FullName);
                }

                return Expression.ConditionalInstance(
                    condition: conditionOk,
                    falseBranch: falseBranchOk,
                    trueBranch: trueBranchOk);
            }

            if (tag.TagName is "EnvironmentExpression")
            {
                return Expression.EnvironmentInstance;
            }

            if (tag.TagName is "StringTagExpression")
            {
                if (tag.Arguments.Count is not 2)
                {
                    return "Invalid arguments for tag " + tag.TagName + ": " + tag;
                }

                var stringtag = tag.Arguments[0];
                var stringTagged = tag.Arguments[1];

                if (stringtag is not ElmValue.ElmString stringtagString)
                {
                    return "Invalid arguments for tag " + tag.TagName + ": " + tag;
                }

                var stringTaggedResult =
                    ElmValueFromCompilerDecodedAsExpression(
                        stringTagged,
                        additionalReusableDecodings,
                        reportNewDecoding,
                        literalAdditionalReusableDecodings,
                        literalReportNewDecoding);

                if (stringTaggedResult.IsErrOrNull() is { } stringTaggedErr)
                {
                    return "Failed for string tag stringTagged: " + stringTaggedErr;
                }

                if (stringTaggedResult.IsOkOrNull() is not { } stringTaggedOk)
                {
                    throw new NotImplementedException(
                        "Unexpected result type for stringTagged: " + stringTaggedResult.GetType().FullName);
                }

                return new Expression.StringTag(stringtagString.Value, stringTaggedOk);
            }

            return "Unexpected tag name: " + tag.TagName;
        }

        var decodeResult = WithoutReport();

        if (reportNewDecoding is not null && decodeResult.IsOkOrNull() is { } decodeOk)
        {
            reportNewDecoding(elmValue, decodeOk);
        }

        return decodeResult;
    }

    /// <summary>
    /// Parses a Pine value representing an Elm <c>Maybe a</c>.
    /// </summary>
    /// <typeparam name="T">The result type returned by the caller-provided handlers.</typeparam>
    /// <param name="pineValue">The Pine value to parse.</param>
    /// <param name="nothing">Handler invoked when the value is <c>Nothing</c>.</param>
    /// <param name="just">Handler invoked with the contained value when the value is <c>Just x</c>.</param>
    /// <param name="invalid">Handler invoked with an error message when the structure is invalid.</param>
    /// <returns>The value returned by one of the provided handlers.</returns>
    public static T ParseElmMaybeValue<T>(
        PineValue pineValue,
        Func<T> nothing,
        Func<PineValue, T> just,
        Func<string, T> invalid)
    {
        if (pineValue is not PineValue.ListValue listValue)
        {
            return invalid("Root is not a list");
        }

        if (listValue.Items.Length is not 2)
        {
            return invalid("Root list does not have 2 elements");
        }

        var tagValue = listValue.Items.Span[0];

        if (tagValue == String_Nothing_Value)
        {
            return nothing();
        }

        if (tagValue == String_Just_Value)
        {
            var tagArgumentsValue = listValue.Items.Span[1];

            if (tagArgumentsValue is not PineValue.ListValue tagArgumentsListValue)
            {
                return invalid("Just tag arguments is not a list");
            }

            if (tagArgumentsListValue.Items.Length is not 1)
            {
                return invalid("Just tag arguments does not have 1 element");
            }

            return just(tagArgumentsListValue.Items.Span[0]);
        }

        return invalid("Not tagged with Nothing or Just");
    }

    /// <summary>
    /// Parses a Pine value representing an Elm <c>Result e a</c>.
    /// </summary>
    /// <typeparam name="T">The result type returned by the caller-provided handlers.</typeparam>
    /// <param name="pineValue">The Pine value to parse.</param>
    /// <param name="err">Handler invoked with the contained error value when the value is <c>Err e</c>.</param>
    /// <param name="ok">Handler invoked with the contained value when the value is <c>Ok a</c>.</param>
    /// <param name="invalid">Handler invoked with an error message when the structure is invalid.</param>
    /// <returns>The value returned by one of the provided handlers.</returns>
    public static T ParseElmResultValue<T>(
        PineValue pineValue,
        Func<PineValue, T> err,
        Func<PineValue, T> ok,
        Func<string, T> invalid)
    {
        if (pineValue is not PineValue.ListValue listValue)
        {
            return invalid("Root is not a list");
        }

        if (listValue.Items.Length is not 2)
        {
            return invalid("Root list does not have 2 elements");
        }

        var tagValue = listValue.Items.Span[0];

        if (tagValue == String_Err_Value)
        {
            var tagArgumentsValue = listValue.Items.Span[1];

            if (tagArgumentsValue is not PineValue.ListValue tagArgumentsListValue)
            {
                return invalid("Tag 'Err' arguments is not a list");
            }

            if (tagArgumentsListValue.Items.Length is not 1)
            {
                return invalid(
                    "Tag 'Err' arguments does not have 1 item, but " +
                    tagArgumentsListValue.Items.Length);
            }

            return err(tagArgumentsListValue.Items.Span[0]);
        }

        if (tagValue == String_Ok_Value)
        {
            var tagArgumentsValue = listValue.Items.Span[1];

            if (tagArgumentsValue is not PineValue.ListValue tagArgumentsListValue)
            {
                return invalid("Tag 'Ok' arguments is not a list");
            }

            if (tagArgumentsListValue.Items.Length is not 1)
            {
                return invalid(
                    "Tag 'Ok' arguments does not have 1 item, but " +
                    tagArgumentsListValue.Items.Length);
            }

            return ok(tagArgumentsListValue.Items.Span[0]);
        }

        return invalid("Not tagged with Err or Ok");
    }


    /// <summary>
    /// Parses an <see cref="ElmValue"/> representing an Elm <c>Result e a</c> into one of the caller-provided handlers.
    /// </summary>
    /// <typeparam name="T">The result type returned by the caller-provided handlers.</typeparam>
    /// <param name="elmValue">The Elm value to parse.</param>
    /// <param name="err">Handler invoked when encountering <c>Err e</c>.</param>
    /// <param name="ok">Handler invoked when encountering <c>Ok a</c>.</param>
    /// <param name="invalid">Handler invoked with an error message when the structure is invalid.</param>
    /// <returns>The value returned by one of the provided handlers.</returns>
    public static T ParseElmValueAsCommonResult<T>(
        ElmValue elmValue,
        Func<ElmValue, T> err,
        Func<ElmValue, T> ok,
        Func<string, T> invalid)
    {
        if (elmValue is not ElmValue.ElmTag resultTag)
        {
            return invalid("Unexpected value type: No tag: " + elmValue.GetType());
        }

        if (resultTag.TagName is "Err")
        {
            if (resultTag.Arguments.Count is not 1)
            {
                return invalid(
                    "Tag arguments does not have 1 item, but " +
                    resultTag.Arguments.Count);
            }

            return err(resultTag.Arguments[0]);
        }

        if (resultTag.TagName is "Ok")
        {
            if (resultTag.Arguments.Count is not 1)
            {
                return invalid(
                    "Tag arguments does not have 1 item, but " +
                    resultTag.Arguments.Count);
            }

            return ok(resultTag.Arguments[0]);
        }

        return invalid("Tag name is neither 'Err' nor 'Ok', but " + resultTag.TagName);
    }
}
