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
    = AddWorkspaceFileRequest String FileTreeBlobNode
    | AddElmPackageVersionRequest ElmPackageVersionIdentifer (List ( List String, FileTreeBlobNode ))
    | DeleteWorkspaceFileRequest String
    | ProvideHoverRequest ProvideHoverRequestStruct
    | ProvideCompletionItemsRequest ProvideCompletionItemsRequestStruct
    | ProvideDefinitionRequest ProvideDefinitionRequestStruct
    | TextDocumentSymbolRequest String
    | TextDocumentReferencesRequest ProvideReferencesRequestStruct
    | TextDocumentRenameRequest RenameParams

 * */

public abstract record Request
{
    public record AddWorkspaceFileRequest(
        string FilePath,
        FileTreeBlobNode Blob)
        : Request;

    public record DeleteWorkspaceFileRequest(string FilePath)
        : Request;

    public record AddElmPackageVersionRequest(
        ElmPackageVersion019Identifer ElmPackageVersionIdentifer,
        List<(IReadOnlyList<string> ModulePath, FileTreeBlobNode Blob)> ModulePathsAndBlobs)
        : Request;

    public record ProvideHoverRequest(ProvideHoverRequestStruct Request)
        : Request;

    public record ProvideCompletionItemsRequest(ProvideCompletionItemsRequestStruct Request)
        : Request;

    public record ProvideDefinitionRequest(ProvideHoverRequestStruct Request)
        : Request;

    public record TextDocumentSymbolRequest(string FilePath)
        : Request;

    public record TextDocumentReferencesRequest(ProvideHoverRequestStruct Request)
        : Request;

    public record TextDocumentRenameRequest(RenameParams Request)
        : Request;
}

/*

type alias ProvideHoverRequestStruct =
    { fileLocation : FileLocation
    , positionLineNumber : Int
    , positionColumn : Int
    }

 * */


public record ProvideHoverRequestStruct(
    FileLocation FileLocation,
    int PositionLineNumber,
    int PositionColumn);

/*

type alias ProvideCompletionItemsRequestStruct =
    { filePathOpenedInEditor : String
    , cursorLineNumber : Int
    , cursorColumn : Int
    }

 * */

public record ProvideCompletionItemsRequestStruct(
    string FilePathOpenedInEditor,
    int CursorLineNumber,
    int CursorColumn);

/*
type alias RenameParams =
    { filePath : String
    , positionLineNumber : Int
    , positionColumn : Int
    , newName : String
    }

*/

public record RenameParams(
    string FilePath,
    int PositionLineNumber,
    int PositionColumn,
    string NewName);

public static class RequestEncoding
{
    public static PineValue Encode(Request request)
    {
        return request switch
        {
            Request.AddWorkspaceFileRequest addFileRequest =>
                ElmValueEncoding.TagAsPineValue(
                    "AddWorkspaceFileRequest",
                    [
                        ElmValueEncoding.StringAsPineValue(addFileRequest.FilePath),
                        Encode(addFileRequest.Blob)
                    ]),

            Request.DeleteWorkspaceFileRequest deleteFileRequest =>
                ElmValueEncoding.TagAsPineValue(
                    "DeleteWorkspaceFileRequest",
                    [
                        ElmValueEncoding.StringAsPineValue(deleteFileRequest.FilePath),
                    ]),

            Request.AddElmPackageVersionRequest addElmPackageRequest =>
                ElmValueEncoding.TagAsPineValue(
                    "AddElmPackageVersionRequest",
                    [
                        ElmValueEncoding.ElmValueAsPineValue(
                            ElmPackageVersionIdentiferEncoding.Encode(addElmPackageRequest.ElmPackageVersionIdentifer)),

                        PineValue.List(
                            [..addElmPackageRequest.ModulePathsAndBlobs.Select(
                                modulePathAndBlob =>
                                PineValue.List(
                                    [
                                    PineValue.List(
                                        [..modulePathAndBlob.ModulePath.Select(ElmValueEncoding.StringAsPineValue)]),
                                    Encode(modulePathAndBlob.Blob)
                                    ]))
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
                        ElmValueEncoding.StringAsPineValue(textDocumentSymbolRequest.FilePath)
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

    public static PineValue Encode(FileTreeBlobNode fileTreeBlobNode)
    {
        return
            ElmValueEncoding.ElmValueAsPineValue(
                new ElmValue.ElmRecord(
                    [
                        ("asBase64",
                        ElmValue.StringInstance(fileTreeBlobNode.AsBase64)),
                        ("asText",
                            fileTreeBlobNode.AsText is { } asText
                            ?
                            ElmValue.TagInstance("Just", [ElmValue.StringInstance(asText)])
                            :
                            ElmValue.TagInstance("Nothing", [])
                            )
                        ]));
    }

    public static PineValue Encode(ProvideHoverRequestStruct provideHoverRequest)
    {
        return
            ElmValueEncoding.ElmRecordAsPineValue(
                [
                    ("fileLocation",
                    ElmValueEncoding.ElmValueAsPineValue(
                        FileLocationEncoding.EncodeAsElmValue(provideHoverRequest.FileLocation))),
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
                    ElmValueEncoding.StringAsPineValue(provideCompletionItemsRequest.FilePathOpenedInEditor)),
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
                    ElmValueEncoding.StringAsPineValue(renameParams.FilePath)),
                    ("positionLineNumber",
                    PineValueAsInteger.ValueFromSignedInteger(renameParams.PositionLineNumber)),
                    ("positionColumn",
                    PineValueAsInteger.ValueFromSignedInteger(renameParams.PositionColumn)),
                    ("newName",
                    ElmValueEncoding.StringAsPineValue(renameParams.NewName))
                ]);
    }
}