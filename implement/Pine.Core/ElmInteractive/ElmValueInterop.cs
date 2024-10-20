using Pine.Core;
using System;
using System.Collections.Generic;
using System.Linq;

namespace Pine.ElmInteractive;

public class ElmValueInterop
{
    public static readonly PineValue String_Nothing_Value =
        PineValueAsString.ValueFromString("Nothing");

    public static readonly PineValue String_Just_Value =
        PineValueAsString.ValueFromString("Just");

    public static ElmValue PineValueEncodedAsInElmCompiler(
        PineValue pineValue) =>
        PineValueEncodedAsInElmCompiler(
            pineValue,
            additionalReusableEncodings: null,
            reportNewEncoding: null);

    /// <summary>
    /// Encode as in https://github.com/pine-vm/pine/blob/ef26bed9aa54397e476545d9e30821565139d821/implement/pine/ElmTime/compile-elm-program/src/Pine.elm#L75-L77
    /// </summary>
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

        var encoded =
            pineValue switch
            {
                PineValue.BlobValue blobValue =>
                ElmValue.TagInstance(
                    "BlobValue",
                    [new ElmValue.ElmList([.. blobValue.Bytes.ToArray().Select(byteInt => ElmValue.Integer(byteInt))])]),

                PineValue.ListValue listValue =>
                ElmValue.TagInstance(
                    "ListValue",
                    [new ElmValue.ElmList(
                        [.. listValue.Elements
                        .Select(item => PineValueEncodedAsInElmCompiler(item, additionalReusableEncodings, reportNewEncoding))])]),

                _ =>
                throw new NotImplementedException(
                    "Unsupported PineValue: " + pineValue.GetType().FullName)
            };

        reportNewEncoding?.Invoke(pineValue, encoded);

        return encoded;
    }

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
                        ElmValueBlobValueDecodedAsInElmCompiler(firstArgument.Elements),

                        _ =>
                            "Invalid arguments for BlobValue tag"
                    },

                    "ListValue" =>
                    tag.Arguments switch
                    {
                    [ElmValue.ElmList firstArgument] =>
                        ElmValueListValueDecodedAsInElmCompiler(
                            firstArgument.Elements,
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
                return "Invalid element in BlobValue tag at index " + i + ": " + itemValue.ToString();

            bytes[i] = (byte)byteElement.Value;
        }

        return PineValue.Blob(bytes);
    }

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

            if (itemResult is Result<string, PineValue>.Ok itemOk)
            {
                items[i] = itemOk.Value;
                continue;
            }

            if (itemResult is Result<string, PineValue>.Err itemErr)
            {
                return "Error decoding list item at index " + i + ": " + itemErr.Value;
            }

            throw new NotImplementedException(
                "Unexpected result type for list item: " + itemResult.GetType().FullName);
        }

        return PineValue.List(items);
    }

    public static Result<string, Expression> ElmValueFromCompilerDecodedAsExpression(
        ElmValue elmValue,
        IReadOnlyDictionary<ElmValue, PineValue>? literalAdditionalReusableEncodings,
        Action<ElmValue, PineValue>? literalReportNewEncoding)
    {
        if (elmValue is ElmValue.ElmTag tag)
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
                        literalAdditionalReusableEncodings,
                        literalReportNewEncoding);

                if (literalValueResult is Result<string, PineValue>.Ok literalValueOk)
                {
                    return Expression.LiteralInstance(literalValueOk.Value);
                }

                if (literalValueResult is Result<string, PineValue>.Err literalValueErr)
                {
                    return literalValueErr.Value;
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

                var items = new Expression[firstList.Elements.Count];

                for (var i = 0; i < firstList.Elements.Count; i++)
                {
                    var itemResult =
                        ElmValueFromCompilerDecodedAsExpression(
                            firstList.Elements[i],
                            literalAdditionalReusableEncodings,
                            literalReportNewEncoding);

                    if (itemResult is Result<string, Expression>.Ok itemOk)
                    {
                        items[i] = itemOk.Value;
                        continue;
                    }

                    if (itemResult is Result<string, Expression>.Err itemErr)
                    {
                        return "Failed for list item [" + i + "]: " + itemErr.Value;
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
                        literalAdditionalReusableEncodings,
                        literalReportNewEncoding);

                if (encodedResult is Result<string, Expression>.Err encodedErr)
                {
                    return "Failed for parse and eval encoded: " + encodedErr.Value;
                }

                if (encodedResult is not Result<string, Expression>.Ok encodedOk)
                {
                    throw new NotImplementedException(
                        "Unexpected result type for encoded: " + encodedResult.GetType().FullName);
                }

                var environmentResult =
                    ElmValueFromCompilerDecodedAsExpression(
                        environment,
                        literalAdditionalReusableEncodings,
                        literalReportNewEncoding);

                if (environmentResult is Result<string, Expression>.Err environmentErr)
                {
                    return "Failed for parse and eval environment: " + environmentErr.Value;
                }

                if (environmentResult is not Result<string, Expression>.Ok environmentOk)
                {
                    throw new NotImplementedException(
                        "Unexpected result type for environment: " + environmentResult.GetType().FullName);
                }

                return new Expression.ParseAndEval(encodedOk.Value, environmentOk.Value);
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
                        literalAdditionalReusableEncodings,
                        literalReportNewEncoding);

                if (environmentResult is Result<string, Expression>.Err environmentErr)
                {
                    return "Failed for kernel application environment: " + environmentErr.Value;
                }

                if (environmentResult is not Result<string, Expression>.Ok environmentOk)
                {
                    throw new NotImplementedException(
                        "Unexpected result type for environment: " + environmentResult.GetType().FullName);
                }

                return new Expression.KernelApplication(functionString.Value, environmentOk.Value);
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
                        literalAdditionalReusableEncodings,
                        literalReportNewEncoding);

                if (conditionResult is Result<string, Expression>.Err conditionErr)
                {
                    return "Failed for conditional condition: " + conditionErr.Value;
                }

                if (conditionResult is not Result<string, Expression>.Ok conditionOk)
                {
                    throw new NotImplementedException(
                        "Unexpected result type for condition: " + conditionResult.GetType().FullName);
                }

                var falseBranchResult =
                    ElmValueFromCompilerDecodedAsExpression(
                        falseBranch,
                        literalAdditionalReusableEncodings,
                        literalReportNewEncoding);

                if (falseBranchResult is Result<string, Expression>.Err falseBranchErr)
                {
                    return "Failed for conditional false branch: " + falseBranchErr.Value;
                }

                if (falseBranchResult is not Result<string, Expression>.Ok falseBranchOk)
                {
                    throw new NotImplementedException(
                        "Unexpected result type for false branch: " + falseBranchResult.GetType().FullName);
                }

                var trueBranchResult =
                    ElmValueFromCompilerDecodedAsExpression(
                        trueBranch,
                        literalAdditionalReusableEncodings,
                        literalReportNewEncoding);

                if (trueBranchResult is Result<string, Expression>.Err trueBranchErr)
                {
                    return "Failed for conditional true branch: " + trueBranchErr.Value;
                }

                if (trueBranchResult is not Result<string, Expression>.Ok trueBranchOk)
                {
                    throw new NotImplementedException(
                        "Unexpected result type for true branch: " + trueBranchResult.GetType().FullName);
                }

                return Expression.ConditionalInstance(
                    condition: conditionOk.Value,
                    falseBranch: falseBranchOk.Value,
                    trueBranch: trueBranchOk.Value);
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
                        literalAdditionalReusableEncodings,
                        literalReportNewEncoding);

                if (stringTaggedResult is Result<string, Expression>.Err stringTaggedErr)
                {
                    return "Failed for string tag stringTagged: " + stringTaggedErr.Value;
                }

                if (stringTaggedResult is not Result<string, Expression>.Ok stringTaggedOk)
                {
                    throw new NotImplementedException(
                        "Unexpected result type for stringTagged: " + stringTaggedResult.GetType().FullName);
                }

                return new Expression.StringTag(stringtagString.Value, stringTaggedOk.Value);
            }
        }

        throw new NotImplementedException(
            "Unsupported ElmValue: " + elmValue.GetType().FullName);
    }

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

        if (listValue.Elements.Count is not 2)
        {
            return invalid("Root list does not have 2 elements");
        }

        var tagValue = listValue.Elements[0];

        if (tagValue == String_Nothing_Value)
        {
            return nothing();
        }

        if (tagValue == String_Just_Value)
        {
            var tagArgumentsValue = listValue.Elements[1];

            if (tagArgumentsValue is not PineValue.ListValue tagArgumentsListValue)
            {
                return invalid("Just tag arguments is not a list");
            }

            if (tagArgumentsListValue.Elements.Count is not 1)
            {
                return invalid("Just tag arguments does not have 1 element");
            }

            return just(tagArgumentsListValue.Elements[0]);
        }

        return invalid("Not tagged with Nothing or Just");
    }
}
