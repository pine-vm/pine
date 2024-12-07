using Pine.Core;
using Pine.ElmInteractive;
using System.Collections.Generic;
using System.Linq;

namespace Pine.Elm.LanguageServiceInterface;

public record RequestInWorkspace(
    FileTreeNode<FileTreeBlobNode> Workspace,
    Request Request);


public record FileTreeBlobNode(
    string AsBase64,
    string? AsText);


/*

type Request
    = ProvideHoverRequest ProvideHoverRequestStruct
    | ProvideCompletionItemsRequest ProvideCompletionItemsRequestStruct
    | ProvideDefinitionRequest ProvideHoverRequestStruct

 * */
public abstract record Request
{
    public record ProvideHoverRequest(ProvideHoverRequestStruct Request)
        : Request;

    public record ProvideCompletionItemsRequest(ProvideCompletionItemsRequestStruct Request)
        : Request;
}

public record ProvideHoverRequestStruct(
    IReadOnlyList<string> FilePathOpenedInEditor,
    int PositionLineNumber,
    int PositionColumn,
    string LineText);

/*

type alias ProvideCompletionItemsRequestStruct =
    { filePathOpenedInEditor : List String
    , cursorLineNumber : Int
    , textUntilPosition : String
    }

 * */

public record ProvideCompletionItemsRequestStruct(
    IReadOnlyList<string> FilePathOpenedInEditor,
    int CursorLineNumber,
    string TextUntilPosition);

public static class RequestEncoding
{
    public static PineValue Encode(Request request)
    {
        return request switch
        {
            Request.ProvideHoverRequest provideHoverRequest =>
                ElmValueEncoding.TagAsPineValue(
                    "ProvideHoverRequest",
                    [
                        Encode(provideHoverRequest.Request)
                    ]),

            Request.ProvideCompletionItemsRequest provideCompletionItemsRequest =>
                ElmValueEncoding.TagAsPineValue(
                    "ProvideCompletionItemsRequest",
                    [
                        Encode(provideCompletionItemsRequest.Request)
                    ]),

            _ =>
            throw new System.NotImplementedException(
                "Unexpected request type: " + request.GetType())
        };
    }

    public static PineValue Encode(ProvideHoverRequestStruct provideHoverRequest)
    {
        return
            ElmValueEncoding.ElmRecordAsPineValue(
                [
                    ("filePathOpenedInEditor",
                    PineValue.List(
                        [..provideHoverRequest.FilePathOpenedInEditor
                        .Select(ElmValueEncoding.StringAsPineValue)
                        ])),
                    ("positionLineNumber",
                    PineValueAsInteger.ValueFromSignedInteger(provideHoverRequest.PositionLineNumber)),
                    ("positionColumn",
                    PineValueAsInteger.ValueFromSignedInteger(provideHoverRequest.PositionColumn)),
                    ("lineText",
                    ElmValueEncoding.StringAsPineValue(provideHoverRequest.LineText))
                ]);
    }

    public static PineValue Encode(ProvideCompletionItemsRequestStruct provideCompletionItemsRequest)
    {
        return
            ElmValueEncoding.ElmRecordAsPineValue(
                [
                    ("filePathOpenedInEditor",
                    PineValue.List(
                        [..provideCompletionItemsRequest.FilePathOpenedInEditor
                        .Select(ElmValueEncoding.StringAsPineValue)
                        ])),
                    ("cursorLineNumber",
                    PineValueAsInteger.ValueFromSignedInteger(provideCompletionItemsRequest.CursorLineNumber)),
                    ("textUntilPosition",
                    ElmValueEncoding.StringAsPineValue(provideCompletionItemsRequest.TextUntilPosition))
                ]);
    }
}