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
    = AddFileRequest (List String) FileTreeBlobNode
    | DeleteFileRequest (List String)
    | ProvideHoverRequest ProvideHoverRequestStruct
    | ProvideCompletionItemsRequest ProvideCompletionItemsRequestStruct
    | ProvideDefinitionRequest ProvideDefinitionRequestStruct
    | TextDocumentSymbolRequest (List String)
    | TextDocumentReferencesRequest ProvideReferencesRequestStruct
    | TextDocumentRenameRequest RenameParams

 * */
public abstract record Request
{
    public record AddFileRequest(
        IReadOnlyList<string> FilePath,
        FileTreeBlobNode Blob)
        : Request;

    public record DeleteFileRequest(IReadOnlyList<string> FilePath)
        : Request;

    public record ProvideHoverRequest(ProvideHoverRequestStruct Request)
        : Request;

    public record ProvideCompletionItemsRequest(ProvideCompletionItemsRequestStruct Request)
        : Request;

    public record ProvideDefinitionRequest(ProvideHoverRequestStruct Request)
        : Request;

    public record TextDocumentSymbolRequest(IReadOnlyList<string> FilePath)
        : Request;

    public record TextDocumentReferencesRequest(ProvideHoverRequestStruct Request)
        : Request;

    public record TextDocumentRenameRequest(RenameParams Request)
        : Request;
}

public record ProvideHoverRequestStruct(
    IReadOnlyList<string> FilePathOpenedInEditor,
    int PositionLineNumber,
    int PositionColumn);

/*

type alias ProvideCompletionItemsRequestStruct =
    { filePathOpenedInEditor : List String
    , cursorLineNumber : Int
    , cursorColumn : Int
    }

 * */

public record ProvideCompletionItemsRequestStruct(
    IReadOnlyList<string> FilePathOpenedInEditor,
    int CursorLineNumber,
    int CursorColumn);

/*
type alias RenameParams =
    { filePath : List String
    , positionLineNumber : Int
    , positionColumn : Int
    , newName : String
    }

*/

public record RenameParams(
    IReadOnlyList<string> FilePath,
    int PositionLineNumber,
    int PositionColumn,
    string NewName);

public static class RequestEncoding
{
    public static PineValue Encode(Request request)
    {
        return request switch
        {
            Request.AddFileRequest addFileRequest =>
                ElmValueEncoding.TagAsPineValue(
                    "AddFileRequest",
                    [
                        PineValue.List(
                            [..addFileRequest.FilePath
                            .Select(ElmValueEncoding.StringAsPineValue)
                            ]),
                        ElmValueEncoding.ElmValueAsPineValue(
                            new ElmValue.ElmRecord(
                                [
                                ("asBase64",
                                new ElmValue.ElmString(addFileRequest.Blob.AsBase64)),

                                ("asText",
                                    addFileRequest.Blob.AsText is { } asText
                                    ?
                                    ElmValue.TagInstance("Just", [new ElmValue.ElmString(asText)])
                                    :
                                    ElmValue.TagInstance("Nothing", [])
                                    )
                                ]))
                    ]),

            Request.DeleteFileRequest deleteFileRequest =>
                ElmValueEncoding.TagAsPineValue(
                    "DeleteFileRequest",
                    [
                        PineValue.List(
                            [..deleteFileRequest.FilePath
                            .Select(ElmValueEncoding.StringAsPineValue)
                            ])
                    ]),

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

            Request.ProvideDefinitionRequest provideDefinitionRequest =>
                ElmValueEncoding.TagAsPineValue(
                    "ProvideDefinitionRequest",
                    [
                        Encode(provideDefinitionRequest.Request)
                    ]),

            Request.TextDocumentSymbolRequest textDocumentSymbolRequest =>
                ElmValueEncoding.TagAsPineValue(
                    "TextDocumentSymbolRequest",
                    [
                        PineValue.List(
                            [..textDocumentSymbolRequest.FilePath
                            .Select(ElmValueEncoding.StringAsPineValue)
                            ])
                    ]),

            Request.TextDocumentReferencesRequest textDocumentReferenceRequest =>
                ElmValueEncoding.TagAsPineValue(
                    "TextDocumentReferencesRequest",
                    [
                        Encode(textDocumentReferenceRequest.Request)
                    ]),

            Request.TextDocumentRenameRequest textDocumentRenameRequest =>
                ElmValueEncoding.TagAsPineValue(
                    "TextDocumentRenameRequest",
                    [
                        Encode(textDocumentRenameRequest.Request)
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
                    ("cursorColumn",
                    PineValueAsInteger.ValueFromSignedInteger(provideCompletionItemsRequest.CursorColumn))
                ]);
    }

    public static PineValue Encode(RenameParams renameParams)
    {
        return
            ElmValueEncoding.ElmRecordAsPineValue(
                [
                    ("filePath",
                    PineValue.List(
                        [..renameParams.FilePath
                        .Select(ElmValueEncoding.StringAsPineValue)
                        ])),
                    ("positionLineNumber",
                    PineValueAsInteger.ValueFromSignedInteger(renameParams.PositionLineNumber)),
                    ("positionColumn",
                    PineValueAsInteger.ValueFromSignedInteger(renameParams.PositionColumn)),
                    ("newName",
                    ElmValueEncoding.StringAsPineValue(renameParams.NewName))
                ]);
    }
}