using Pine.Core;
using System;
using System.Collections.Generic;
using System.Linq;

namespace Pine.ElmInteractive;

public class ElmValueInterop
{
    public static ElmValue PineValueEncodedAsInElmCompiler(
        PineValue pineValue) =>
        PineValueEncodedAsInElmCompiler(
            pineValue,
            ReusedInstances.Instance.ElmValueEncodedAsInElmCompiler);

    /// <summary>
    /// Encode as in https://github.com/pine-vm/pine/blob/ef26bed9aa54397e476545d9e30821565139d821/implement/pine/ElmTime/compile-elm-program/src/Pine.elm#L75-L77
    /// </summary>
    public static ElmValue PineValueEncodedAsInElmCompiler(
        PineValue pineValue,
        IReadOnlyDictionary<PineValue, ElmValue>? reusableEncodings)
    {
        if (reusableEncodings?.TryGetValue(pineValue, out var reused) ?? false)
        {
            return reused;
        }

        return
            pineValue switch
            {
                PineValue.BlobValue blobValue =>
                ElmValue.TagInstance(
                    "BlobValue",
                    [new ElmValue.ElmList([.. blobValue.Bytes.ToArray().Select(byteInt => ElmValue.Integer(byteInt))])]),

                PineValue.ListValue listValue =>
                ElmValue.TagInstance(
                    "ListValue",
                    [new ElmValue.ElmList([.. listValue.Elements.Select(item => PineValueEncodedAsInElmCompiler(item, reusableEncodings))])]),

                _ =>
                throw new NotImplementedException(
                    "Unsupported PineValue: " + pineValue.GetType().FullName)
            };
    }

    public static Result<string, PineValue> ElmValueDecodedAsInElmCompiler(ElmValue elmValue)
    {
        if (ReusedInstances.Instance.ElmValueDecodedAsInElmCompiler?.TryGetValue(elmValue, out var pineValue) ?? false)
        {
            return pineValue;
        }

        return
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
                        ElmValueListValueDecodedAsInElmCompiler(firstArgument.Elements),

                        _ =>
                            "Invalid arguments for ListValue tag"
                    },

                    _ =>
                    "Unsupported tag: " + tag.TagName
                },

                _ =>
                "Unsupported ElmValue: " + elmValue.GetType().FullName
            };
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
        IReadOnlyList<ElmValue> listItems)
    {
        if (listItems.Count is 0)
            return PineValue.EmptyList;

        var items = new PineValue[listItems.Count];

        for (var i = 0; i < listItems.Count; i++)
        {
            var itemResult = ElmValueDecodedAsInElmCompiler(listItems[i]);

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
}
