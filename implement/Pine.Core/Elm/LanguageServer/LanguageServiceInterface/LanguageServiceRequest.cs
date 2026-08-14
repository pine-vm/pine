using Pine.Core.CommonEncodings;
using System.Collections.Generic;
using System.Linq;

namespace Pine.Core.Elm.LanguageServer.LanguageServiceInterface;

/// <summary>
/// Combines a language service request with its workspace.
/// </summary>
public record RequestInWorkspace(
    FileTreeNode<FileTreeBlobNode> Workspace,
    Request Request);


/// <summary>
/// Contains the encoded content of a file tree blob.
/// </summary>
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

/// <summary>
/// Represents a request to the Elm language service.
/// </summary>
public abstract record Request
{
    /// <summary>
    /// Requests adding or replacing a workspace file.
    /// </summary>
    public record AddWorkspaceFileRequest(
        string FilePath,
        FileTreeBlobNode Blob)
        : Request;

    /// <summary>
    /// Requests deleting a workspace file.
    /// </summary>
    public record DeleteWorkspaceFileRequest(string FilePath)
        : Request;

    /// <summary>
    /// Requests adding an Elm package version.
    /// </summary>
    public record AddElmPackageVersionRequest(
        ElmPackageVersion019Identifer ElmPackageVersionIdentifer,
        List<(IReadOnlyList<string> ModulePath, FileTreeBlobNode Blob)> ModulePathsAndBlobs)
        : Request;

    /// <summary>
    /// Requests hover information.
    /// </summary>
    public record ProvideHoverRequest(ProvideHoverRequestStruct Request)
        : Request;

    /// <summary>
    /// Requests completion items.
    /// </summary>
    public record ProvideCompletionItemsRequest(ProvideCompletionItemsRequestStruct Request)
        : Request;

    /// <summary>
    /// Requests definition locations.
    /// </summary>
    public record ProvideDefinitionRequest(ProvideHoverRequestStruct Request)
        : Request;

    /// <summary>
    /// Requests document symbols.
    /// </summary>
    public record TextDocumentSymbolRequest(string FilePath)
        : Request;

    /// <summary>
    /// Requests document references.
    /// </summary>
    public record TextDocumentReferencesRequest(ProvideHoverRequestStruct Request)
        : Request;

    /// <summary>
    /// Requests renaming a document symbol.
    /// </summary>
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


/// <summary>
/// Identifies a source position for a hover request.
/// </summary>
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

/// <summary>
/// Identifies a source position for a completion request.
/// </summary>
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

/// <summary>
/// Describes a symbol rename request.
/// </summary>
public record RenameParams(
    string FilePath,
    int PositionLineNumber,
    int PositionColumn,
    string NewName);

/// <summary>
/// Encodes language service requests as Pine values.
/// </summary>
public static class RequestEncoding
{
    /// <summary>
    /// Encodes a language service request.
    /// </summary>
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
                [
                ..addElmPackageRequest.ModulePathsAndBlobs.Select(
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

    /// <summary>
    /// Encodes a file tree blob.
    /// </summary>
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
                        ElmValue.TagInstance("Nothing", []))
                    ]));
    }

    /// <summary>
    /// Encodes a hover request.
    /// </summary>
    public static PineValue Encode(ProvideHoverRequestStruct provideHoverRequest)
    {
        return
            ElmValueEncoding.ElmRecordAsPineValue(
                [
                    ("fileLocation",
                    ElmValueEncoding.ElmValueAsPineValue(
                        FileLocationEncoding.EncodeAsElmValue(provideHoverRequest.FileLocation))),
                    ("positionLineNumber",
                    IntegerEncoding.EncodeSignedInteger(provideHoverRequest.PositionLineNumber)),
                    ("positionColumn",
                    IntegerEncoding.EncodeSignedInteger(provideHoverRequest.PositionColumn)),
                ]);
    }

    /// <summary>
    /// Encodes a completion request.
    /// </summary>
    public static PineValue Encode(ProvideCompletionItemsRequestStruct provideCompletionItemsRequest)
    {
        return
            ElmValueEncoding.ElmRecordAsPineValue(
                [
                    ("filePathOpenedInEditor",
                    ElmValueEncoding.StringAsPineValue(provideCompletionItemsRequest.FilePathOpenedInEditor)),
                    ("cursorLineNumber",
                    IntegerEncoding.EncodeSignedInteger(provideCompletionItemsRequest.CursorLineNumber)),
                    ("cursorColumn",
                    IntegerEncoding.EncodeSignedInteger(provideCompletionItemsRequest.CursorColumn))
                ]);
    }

    /// <summary>
    /// Encodes a rename request.
    /// </summary>
    public static PineValue Encode(RenameParams renameParams)
    {
        return
            ElmValueEncoding.ElmRecordAsPineValue(
                [
                    ("filePath",
                    ElmValueEncoding.StringAsPineValue(renameParams.FilePath)),
                    ("positionLineNumber",
                    IntegerEncoding.EncodeSignedInteger(renameParams.PositionLineNumber)),
                    ("positionColumn",
                    IntegerEncoding.EncodeSignedInteger(renameParams.PositionColumn)),
                    ("newName",
                    ElmValueEncoding.StringAsPineValue(renameParams.NewName))
                ]);
    }
}
