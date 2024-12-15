using Pine.Core;
using Pine.ElmInteractive;
using System.Collections.Generic;
using System.Linq;

namespace Pine.Elm.LanguageServiceInterface;


/*

type Response
    = WorkspaceSummaryResponse
    | ProvideHoverResponse (List String)
    | ProvideCompletionItemsResponse (List Frontend.MonacoEditor.MonacoCompletionItem)
    | ProvideDefinitionResponse (List LocationUnderFilePath)

 * */

public abstract record Response
{
    public record WorkspaceSummaryResponse
        : Response;

    public record ProvideHoverResponse(
        IReadOnlyList<string> Strings)
        : Response;

    public record ProvideCompletionItemsResponse(
        IReadOnlyList<MonacoEditor.MonacoCompletionItem> CompletionItems)
        : Response;

    public record ProvideDefinitionResponse(
        IReadOnlyList<LocationUnderFilePath> Locations)
        : Response;
}

/*

type alias LocationUnderFilePath =
    { filePath : List String
    , range : Frontend.MonacoEditor.MonacoRange
    }
 * */

public record LocationUnderFilePath(
    IReadOnlyList<string> FilePath,
    MonacoEditor.MonacoRange Range);

public static class ResponseEncoding
{
    public static Result<string, Response> Decode(PineValue pineValue)
    {
        var elmValueResult =
            ElmValueEncoding.PineValueAsElmValue(pineValue, null, null);

        {
            if (elmValueResult.IsErrOrNull() is { } err)
            {
                return "Failed decoding as Elm value: " + err;
            }
        }

        if (elmValueResult.IsOkOrNull() is not { } elmValue)
        {
            throw new System.NotImplementedException
                ("Unexpected result type: " + elmValueResult.GetType());
        }

        if (elmValue is not ElmValue.ElmTag responseTag)
        {
            return "Expected Elm tag, got: " + elmValue.GetType();
        }

        if (responseTag.TagName is "WorkspaceSummaryResponse")
        {
            if (responseTag.Arguments.Count is not 0)
            {
                return
                    "Unexpected response tag arguments count: " +
                    responseTag.Arguments.Count;
            }

            return new Response.WorkspaceSummaryResponse();
        }

        if (responseTag.TagName is "ProvideHoverResponse")
        {
            if (responseTag.Arguments.Count is not 1)
            {
                return
                    "Unexpected response tag arguments count: " +
                    responseTag.Arguments.Count;
            }

            if (responseTag.Arguments[0] is not ElmValue.ElmList responseList)
            {
                return
                    "Unexpected response tag argument type: " +
                    responseTag.Arguments[0].GetType();
            }

            var plainStrings = new string[responseList.Elements.Count];

            for (var i = 0; i < responseList.Elements.Count; i++)
            {
                if (responseList.Elements[i] is not ElmValue.ElmString plainString)
                {
                    return
                        "Unexpected response tag element type: " +
                        responseList.Elements[i].GetType();
                }

                plainStrings[i] = plainString.Value;
            }

            return new Response.ProvideHoverResponse(plainStrings);
        }

        if (responseTag.TagName is "ProvideCompletionItemsResponse")
        {
            if (responseTag.Arguments.Count is not 1)
            {
                return
                    "Unexpected response tag arguments count: " +
                    responseTag.Arguments.Count;
            }

            if (responseTag.Arguments[0] is not ElmValue.ElmList responseList)
            {
                return
                    "Unexpected response tag argument type: " +
                    responseTag.Arguments[0].GetType();
            }

            var completionItems = new MonacoEditor.MonacoCompletionItem[responseList.Elements.Count];

            for (var i = 0; i < responseList.Elements.Count; i++)
            {
                var completionItemResult =
                    MonacoEditor.CompletionItemEncoding.Decode(responseList.Elements[i]);

                {
                    if (completionItemResult.IsErrOrNull() is { } err)
                    {
                        return "Failed decoding completion item: " + err;
                    }
                }

                if (completionItemResult.IsOkOrNull() is not { } completionItem)
                {
                    throw new System.NotImplementedException
                        ("Unexpected result type: " + completionItemResult.GetType());
                }

                completionItems[i] = completionItem;
            }

            return new Response.ProvideCompletionItemsResponse(completionItems);
        }

        if (responseTag.TagName is "ProvideDefinitionResponse")
        {
            if (responseTag.Arguments.Count is not 1)
            {
                return
                    "Unexpected response tag arguments count: " +
                    responseTag.Arguments.Count;
            }

            if (responseTag.Arguments[0] is not ElmValue.ElmList responseList)
            {
                return
                    "Unexpected response tag argument type: " +
                    responseTag.Arguments[0].GetType();
            }

            var locations = new LocationUnderFilePath[responseList.Elements.Count];

            for (var i = 0; i < responseList.Elements.Count; i++)
            {
                var locationResult =
                    LocationUnderFilePathEncoding.Decode(responseList.Elements[i]);

                {
                    if (locationResult.IsErrOrNull() is { } err)
                    {
                        return "Failed decoding location at index [" + i + "]: " + err;
                    }
                }

                if (locationResult.IsOkOrNull() is not { } location)
                {
                    throw new System.NotImplementedException
                        ("Unexpected result type: " + locationResult.GetType());
                }

                locations[i] = location;
            }
            return new Response.ProvideDefinitionResponse(locations);
        }

        return
            "Unexpected response tag: " +
            responseTag.TagName;
    }
}

public static class LocationUnderFilePathEncoding
{
    public static Result<string, LocationUnderFilePath> Decode(PineValue pineValue)
    {
        var decodeElmValueResult = ElmValueEncoding.PineValueAsElmValue(pineValue, null, null);

        if (decodeElmValueResult.IsErrOrNull() is { } err)
        {
            return "Failed decoding as Elm value: " + err;
        }

        if (decodeElmValueResult.IsOkOrNull() is not { } elmValue)
        {
            throw new System.NotImplementedException
                ("Unexpected result type: " + decodeElmValueResult.GetType());
        }

        return Decode(elmValue);
    }

    public static Result<string, LocationUnderFilePath> Decode(ElmValue elmValue)
    {
        if (elmValue is not ElmValue.ElmRecord record)
        {
            return "Expected Elm record, got: " + elmValue.GetType();
        }

        // We expect exactly 2 fields: filePath, range
        if (record.Fields.Count is not 2)
        {
            return "Expected 2 fields, got: " + record.Fields.Count;
        }

        if (record["filePath"] is not { } filePathValue)
        {
            return
                "Expected field 'filePath' to be present, got: " +
                string.Join(", ", record.Fields.Select(f => f.FieldName));
        }

        if (filePathValue is not ElmValue.ElmList filePathListValue)
        {
            return "Expected field 'filePath' to be a list, got: " + filePathValue.GetType();
        }

        // Decode filePath as a list of strings
        var filePathArray = new string[filePathListValue.Elements.Count];
        for (var i = 0; i < filePathListValue.Elements.Count; i++)
        {
            if (filePathListValue.Elements[i] is not ElmValue.ElmString strVal)
            {
                return "Expected element of 'filePath' to be a string, got: " + filePathListValue.Elements[i].GetType();
            }
            filePathArray[i] = strVal.Value;
        }

        if (record["range"] is not { } rangeValue)
        {
            return
                "Expected field 'range' to be present, got: " +
                string.Join(", ", record.Fields.Select(f => f.FieldName));
        }

        var rangeDecodeResult =
            MonacoEditor.MonacoRangeEncoding.Decode(rangeValue);

        if (rangeDecodeResult.IsErrOrNull() is { } rangeDecodeErr)
        {
            return "Failed decoding range: " + rangeDecodeErr;
        }

        if (rangeDecodeResult.IsOkOrNull() is not { } range)
        {
            throw new System.NotImplementedException
                ("Unexpected result type: " + rangeDecodeResult.GetType());
        }

        return new LocationUnderFilePath(filePathArray, range);
    }
}

