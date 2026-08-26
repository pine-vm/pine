using Pine.Core.CodeAnalysis;
using Pine.Core.Elm.LanguageServer.LanguageServiceInterface;
using Pine.Core.Elm.LanguageServer.MonacoEditor;
using Pine.Core.Files;
using Pine.Core.IO;
using Pine.Core.PineVM;
using System.Collections.Generic;
using System.Linq;

namespace Pine.Core.Elm.LanguageServer;

/// <summary>
/// Session of the Elm language service, backed by the language service program running in a Pine VM.
/// </summary>
public class LanguageServiceState(
    LanguageServiceInterfaceStruct languageServiceInterface,
    PineValue initState,
    IPineVM pineVM)
    : ILanguageServiceSession
{
    private static readonly PineVMParseCache s_parseCache = new();

    private PineValue _state = initState;

    internal sealed record LanguageServiceProgram(
        LanguageServiceInterfaceStruct Interface,
        PineValue InitialState);

    internal sealed record LanguageServiceTransition(
        Result<string, Response> Response,
        PineValue State);

    /// <summary>
    /// Compiles the language service program and initializes a new session on the given VM.
    /// </summary>
    /// <param name="pineVM">VM used to run the language service program.</param>
    /// <param name="compilationCache">
    /// Optional store to cache the compiled environment between sessions and processes.
    /// </param>
    /// <param name="logDelegate">Optional delegate receiving progress reports from compilation.</param>
    internal static Result<string, LanguageServiceProgram> CompileLanguageServiceProgram(
        IPineVM pineVM,
        IFileStore? compilationCache,
        System.Action<string>? logDelegate = null)
    {
        var sourceTree =
            LanguageServiceCompilation.BuildLanguageServiceSourceTree();

        var compileResult =
            LanguageServiceCompilation.CompileLanguageServiceEnv(
                sourceTree,
                cache: compilationCache,
                logDelegate: logDelegate);

        {
            if (compileResult.IsErrOrNull() is { } err)
            {
                return err;
            }
        }

        if (compileResult.IsOkOrNull() is not { } compiledEnv)
        {
            throw new System.NotImplementedException(
                "Unexpected result type: " + compileResult.GetType());
        }

        var parseInitStateResult =
            ElmInteractiveEnvironment.ParseFunctionFromElmModule(
                interactiveEnvironment: compiledEnv,
                moduleName: "LanguageService",
                declarationName: "initLanguageServiceState",
                s_parseCache);

        {
            if (parseInitStateResult.IsErrOrNull() is { } err)
            {
                return
                    "Failed parsing initLanguageServiceState from compiled language service environment: " +
                    err;
            }
        }

        if (parseInitStateResult.IsOkOrNullable() is not { } parseInitOk)
        {
            throw new System.NotImplementedException(
                "Unexpected result type: " + parseInitStateResult.GetType());
        }

        var parseHandleRequestResult =
            ElmInteractiveEnvironment.ParseFunctionFromElmModule(
                interactiveEnvironment: compiledEnv,
                moduleName: "LanguageService",
                declarationName: "handleRequestInCurrentWorkspace",
                s_parseCache);

        {
            if (parseHandleRequestResult.IsErrOrNull() is { } err)
            {
                return
                    "Failed parsing handleRequestInCurrentWorkspace from compiled language service environment: " +
                    err;
            }
        }

        if (parseHandleRequestResult.IsOkOrNullable() is not { } parseHandleRequestOk)
        {
            throw new System.NotImplementedException(
                "Unexpected result type: " + parseHandleRequestResult.GetType());
        }

        var languageServiceInterface =
            new LanguageServiceInterfaceStruct(
                parseInitOk.functionRecord,
                parseHandleRequestOk.functionRecord);

        var elmCoreModulesSourceList =
            PineValue.EmptyList;

        var initResult =
            ElmInteractiveEnvironment.ApplyFunction(
                pineVM,
                languageServiceInterface.InitState,
                [elmCoreModulesSourceList]);

        {
            if (initResult.IsErrOrNull() is { } err)
            {
                throw new System.Exception("Failed to initialize language service: " + err);
            }
        }

        if (initResult.IsOkOrNull() is not { } initOk)
        {
            throw new System.NotImplementedException(
                "Unexpected init result type: " + initResult.GetType());
        }

        return
            new LanguageServiceProgram(
                languageServiceInterface,
                initOk);
    }

    /// <summary>
    /// Compiles the language service program and initializes a new session on the given VM.
    /// </summary>
    public static Result<string, LanguageServiceState> InitLanguageServiceState(
        IPineVM pineVM,
        IFileStore? compilationCache,
        System.Action<string>? logDelegate = null)
    {
        var programResult =
            CompileLanguageServiceProgram(
                pineVM,
                compilationCache,
                logDelegate);

        if (programResult.IsErrOrNull() is { } err)
        {
            return err;
        }

        if (programResult.IsOkOrNull() is not { } program)
        {
            throw new System.NotImplementedException(
                "Unexpected language service program result type: " + programResult.GetType());
        }

        return
            new LanguageServiceState(
                program.Interface,
                program.InitialState,
                pineVM);
    }

    /// <summary>
    /// Removes a file from the language service workspace.
    /// </summary>
    public Result<string, Response.WorkspaceSummaryResponse>
        DeleteFile(
        string fileUri)
    {
        var genericRequestResult =
            HandleRequest(new Request.DeleteWorkspaceFileRequest(fileUri));

        if (genericRequestResult.IsErrOrNull() is { } err)
        {
            return err;
        }

        if (genericRequestResult.IsOkOrNull() is not { } requestOk)
        {
            throw new System.NotImplementedException(
                "Unexpected request result type: " + genericRequestResult.GetType());
        }

        if (requestOk is not Response.WorkspaceSummaryResponse workspaceSummary)
        {
            throw new System.NotImplementedException(
                "Unexpected request result type: " + requestOk.GetType());
        }

        return workspaceSummary;
    }

    /// <summary>
    /// Adds or replaces a file in the language service workspace.
    /// </summary>
    public Result<string, Response.WorkspaceSummaryResponse>
        AddFile(
        string fileUri,
        string fileContentAsText)
    {
        var asBase64 =
            System.Convert.ToBase64String(
                System.Text.Encoding.UTF8.GetBytes(fileContentAsText));

        var genericRequestResult =
            HandleRequest(
                new Request.AddWorkspaceFileRequest(
                    fileUri,
                    new FileTreeBlobNode(AsBase64: asBase64, AsText: fileContentAsText)));

        if (genericRequestResult.IsErrOrNull() is { } err)
        {
            return err;
        }

        if (genericRequestResult.IsOkOrNull() is not { } requestOk)
        {
            throw new System.NotImplementedException(
                "Unexpected request result type: " + genericRequestResult.GetType());
        }

        if (requestOk is not Response.WorkspaceSummaryResponse workspaceSummary)
        {
            throw new System.NotImplementedException(
                "Unexpected request result type: " + requestOk.GetType());
        }

        return workspaceSummary;
    }

    /// <summary>
    /// Adds an Elm package to the language service workspace.
    /// </summary>
    public Result<string, Response.WorkspaceSummaryResponse>
        AddElmPackage(
        ElmPackageVersion019Identifer packageVersionId,
        IReadOnlyList<KeyValuePair<IReadOnlyList<string>, string>> filesContentsAsText)
    {
        var genericRequestResult =
            HandleRequest(
                new Request.AddElmPackageVersionRequest(
                    packageVersionId,
                    [
                    .. filesContentsAsText.Select(
                        e =>
                        (e.Key,
                        new FileTreeBlobNode(
                            AsBase64: System.Convert.ToBase64String(System.Text.Encoding.UTF8.GetBytes(e.Value)),
                            AsText: e.Value)))
                    ]));

        if (genericRequestResult.IsErrOrNull() is { } err)
        {
            return err;
        }

        if (genericRequestResult.IsOkOrNull() is not { } requestOk)
        {
            throw new System.NotImplementedException(
                "Unexpected request result type: " + genericRequestResult.GetType());
        }

        if (requestOk is not Response.WorkspaceSummaryResponse workspaceSummary)
        {
            throw new System.NotImplementedException(
                "Unexpected request result type: " + requestOk.GetType());
        }

        return workspaceSummary;
    }

    /// <summary>
    /// Provides hover information for a source position.
    /// </summary>
    public Result<string, IReadOnlyList<string>> ProvideHover(
        ProvideHoverRequestStruct provideHoverRequest)
    {
        var genericRequestResult =
            HandleRequest(
                new Request.ProvideHoverRequest(provideHoverRequest));

        if (genericRequestResult.IsErrOrNull() is { } err)
        {
            return err;
        }

        if (genericRequestResult.IsOkOrNull() is not { } requestOk)
        {
            throw new System.NotImplementedException(
                "Unexpected request result type: " + genericRequestResult.GetType());
        }

        if (requestOk is not Response.ProvideHoverResponse provideHoverResponse)
        {
            throw new System.NotImplementedException(
                "Unexpected request result type: " + requestOk.GetType());
        }

        return Result<string, IReadOnlyList<string>>.ok(provideHoverResponse.Strings);
    }

    /// <summary>
    /// Provides completion items for a source position.
    /// </summary>
    public Result<string, IReadOnlyList<MonacoCompletionItem>>
        ProvideCompletionItems(
        ProvideCompletionItemsRequestStruct provideCompletionItemsRequest)
    {
        var genericRequestResult =
            HandleRequest(
                new Request.ProvideCompletionItemsRequest(provideCompletionItemsRequest));

        if (genericRequestResult.IsErrOrNull() is { } err)
        {
            return err;
        }

        if (genericRequestResult.IsOkOrNull() is not { } requestOk)
        {
            throw new System.NotImplementedException(
                "Unexpected request result type: " + genericRequestResult.GetType());
        }

        if (requestOk is not Response.ProvideCompletionItemsResponse provideCompletionItemsResponse)
        {
            throw new System.NotImplementedException(
                "Unexpected request result type: " + requestOk.GetType());
        }

        return
            Result<string, IReadOnlyList<MonacoCompletionItem>>.ok(
                provideCompletionItemsResponse.CompletionItems);
    }

    /// <summary>
    /// Handles a request in the current language service workspace.
    /// </summary>
    public Result<string, Response> HandleRequest(
        Request request)
    {
        lock (pineVM)
        {
            var transition =
                ApplyRequest(
                    new LanguageServiceProgram(languageServiceInterface, _state),
                    pineVM,
                    _state,
                    request);

            _state = transition.State;

            return transition.Response;
        }
    }

    internal static LanguageServiceTransition ApplyRequest(
        LanguageServiceProgram program,
        IPineVM pineVM,
        PineValue state,
        Request request,
        System.Threading.CancellationToken cancellationToken = default)
    {
        var requestEncoded =
            RequestEncoding.Encode(request);

        var handleRequestResult =
            ElmInteractiveEnvironment.ApplyFunction(
                pineVM,
                program.Interface.HandleRequestInCurrentWorkspace,
                [
                requestEncoded,
                state,
                ],
                cancellationToken);

        {
            if (handleRequestResult.IsErrOrNull() is { } requestError)
            {
                throw new System.Exception("Failed to handle request: " + requestError);
            }
        }

        if (handleRequestResult.IsOkOrNull() is not { } handleRequestOk)
        {
            throw new System.NotImplementedException(
                "Unexpected handle request result type: " + handleRequestResult.GetType());
        }

        if (handleRequestOk is not PineValue.ListValue handleRequestOkList)
        {
            throw new System.NotImplementedException(
                "Unexpected handle request result type: " + handleRequestOk.GetType());
        }

        if (handleRequestOkList.Items.Length is not 2)
        {
            throw new System.NotImplementedException(
                "Unexpected handle request result length: " + handleRequestOkList.Items.Length);
        }

        var requestResponseResultValue =
            handleRequestOkList.Items.Span[0];

        var langServiceStateValue =
            handleRequestOkList.Items.Span[1];

        var langServiceResponseOk =
            ElmValueInterop.ParseElmResultValue(
                requestResponseResultValue,
                err: err =>
                {
                    throw new System.Exception("Failed to parse request response result: " + err);
                },
                ok:
                ok => ok,
                invalid:
                err => throw new System.Exception("Invalid form: " + err));


        var langServiceResponseOkElmValue =
            ElmValueEncoding.PineValueAsElmValue(langServiceResponseOk, null, null)
            .Extract(err => throw new System.Exception("Failed to parse request response result: " + err));

        if (langServiceResponseOkElmValue is not ElmValue.ElmTag responseTag)
        {
            throw new System.NotImplementedException(
                "Unexpected response type: " + langServiceResponseOkElmValue.GetType());
        }

        var decodeResponseResult =
            ResponseEncoding.Decode(langServiceResponseOk);

        Result<string, Response> response;

        if (decodeResponseResult.IsErrOrNull() is { } err)
        {
            response = "Failed to decode response: " + err;
        }
        else
        {
            if (decodeResponseResult.IsOkOrNull() is not { } decodedResponse)
            {
                throw new System.NotImplementedException(
                    "Unexpected response type: " + decodeResponseResult.GetType());
            }

            response = decodedResponse;
        }

        return
            new LanguageServiceTransition(
                response,
                langServiceStateValue);
    }

    /// <summary>
    /// Encodes a language service file tree as a Pine value.
    /// </summary>
    public static PineValue EncodeFileTreeNodeAsPineValue(
        FileTreeNode<FileTreeBlobNode> node)
    {
        return
            EncodeFileTreeNodeAsPineValue(
                node,
                blob =>
                ElmValueEncoding.ElmValueAsPineValue(
                    new ElmValue.ElmRecord(
                        [
                        ("asBase64", ElmValue.StringInstance(blob.AsBase64)),
                        ("asText",
                        blob.AsText is null
                        ?
                        ElmValue.TagInstance("Nothing", [])
                        :
                        ElmValue.TagInstance("Just", [ElmValue.StringInstance(blob.AsText)]))
                        ])));
    }

    /// <summary>
    /// Encodes a file tree as a Pine value.
    /// </summary>
    public static PineValue EncodeFileTreeNodeAsPineValue<BlobT>(
        FileTreeNode<BlobT> node,
        System.Func<BlobT, PineValue> encodeBlob)
    {
        if (node is FileTreeNode<BlobT>.BlobNode blobNode)
        {
            return
                ElmValueEncoding.TagAsPineValue(
                    "BlobNode",
                    [encodeBlob(blobNode.Blob)]);
        }

        if (node is FileTreeNode<BlobT>.TreeNode treeNode)
        {
            return
                ElmValueEncoding.TagAsPineValue(
                    "TreeNode",
                    [
                        PineValue.List(
                            [
                            ..treeNode.Children
                            .Select(
                                e =>
                                PineValue.List(
                                    [
                                    ElmValueEncoding.StringAsPineValue(e.name),
                                    EncodeFileTreeNodeAsPineValue(e.node, encodeBlob)
                                    ]))
                            ])
                    ]);
        }

        throw new System.NotImplementedException(
            "Unexpected node type: " + node.GetType());
    }

    /// <summary>
    /// Converts a file tree to the language service representation.
    /// </summary>
    public static FileTreeNode<FileTreeBlobNode>
        Workspace(FileTree workspace)
    {
        if (workspace is FileTree.FileNode blobNode)
        {
            string? asText = null;

            try
            {
                asText = System.Text.Encoding.UTF8.GetString(blobNode.Bytes.Span);
            }
            catch (System.Exception e)
            {
                System.Console.WriteLine("Failed to decode blob as text: " + e);
            }

            return
                new FileTreeNode<FileTreeBlobNode>.BlobNode(
                    new FileTreeBlobNode(
                        AsBase64: System.Convert.ToBase64String(blobNode.Bytes.Span),
                        AsText: asText));
        }

        if (workspace is FileTree.DirectoryNode treeNode)
        {
            return
                new FileTreeNode<FileTreeBlobNode>.TreeNode(
                    [
                    ..treeNode.Items.Select(
                        e =>
                        (e.name, Workspace(e.component)))
                    ]);
        }

        throw new System.NotImplementedException(
            "Unexpected node type: " + workspace.GetType());
    }
}
