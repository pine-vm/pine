using System;
using System.Linq;

namespace Pine.ElmInteractive;

public class ElmValueInterop
{
    /// <summary>
    /// Encode as in https://github.com/pine-vm/pine/blob/ef26bed9aa54397e476545d9e30821565139d821/implement/pine/ElmTime/compile-elm-program/src/Pine.elm#L75-L77
    /// </summary>
    public static ElmValue PineValueEncodedAsInElmCompiler(PineValue pineValue) =>
        pineValue switch
        {
            PineValue.BlobValue blobValue =>
            new ElmValue.ElmTag(
                "BlobValue",
                [new ElmValue.ElmList([.. blobValue.Bytes.ToArray().Select(byteInt => ElmValue.Integer(byteInt))])]),

            PineValue.ListValue listValue =>
            new ElmValue.ElmTag(
                "ListValue",
                [new ElmValue.ElmList([.. listValue.Elements.Select(PineValueEncodedAsInElmCompiler)])]),

            _ =>
            throw new NotImplementedException("Unsupported PineValue: " + pineValue.GetType().FullName)
        };

    public static Result<string, PineValue> ElmValueDecodedAsInElmCompiler(ElmValue elmValue) =>
        elmValue switch
        {
            ElmValue.ElmTag tag =>
            tag.TagName switch
            {
                "BlobValue" =>
                tag.Arguments switch
                {
                [ElmValue.ElmList firstArgument] =>
                ResultExtension.ListCombine(firstArgument.Elements.Select(elmValue =>
                elmValue switch
                {
                    ElmValue.ElmInteger integer =>
                    (Result<string, byte>)(byte)integer.Value,
                    _ =>
                    "Invalid element in BlobValue tag"
                }))
                .Map(bytes => PineValue.Blob([.. bytes])),

                    _ =>
                    "Invalid arguments for BlobValue tag"
                },

                "ListValue" =>
                tag.Arguments switch
                {
                [ElmValue.ElmList firstArgument] =>
                ResultExtension.ListCombine(firstArgument.Elements.Select(ElmValueDecodedAsInElmCompiler))
                .Map(PineValue.List),

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
