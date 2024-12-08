using Pine.Core;
using Pine.ElmInteractive;
using System.Collections.Generic;

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
}

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

        return
            "Unexpected response tag: " +
            responseTag.TagName;
    }
}
