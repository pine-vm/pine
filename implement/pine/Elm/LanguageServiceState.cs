using Interface = Pine.Elm.LanguageServiceInterface;
using Pine.Core;
using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
using Pine.Core.Elm;
using Pine.Core.Files;
using Pine.Core.PineVM;
using System.Collections.Generic;
using System.Linq;

namespace Pine.Elm;

public class LanguageServiceState(
    ElmCompiler.LanguageServiceInterfaceStruct languageServiceInterface,
    PineValue initState,
    IPineVM pineVM)
{
    private PineValue _state = initState;

    public static Result<string, LanguageServiceState> InitLanguageServiceState(
        IPineVM pineVM)
    {
        var compilerSourceFiles =
            ElmCompiler.CompilerSourceFilesDefault.Value;

        var combinedSourceFiles =
            ElmCompiler.ElmCompilerFileTreeFromBundledFileTree(compilerSourceFiles);

        var elmCompilerFromBundleValue =
            BundledElmEnvironments.BundledElmEnvironmentFromFileTree(combinedSourceFiles);

        if (elmCompilerFromBundleValue is null)
        {
            return "Failed loading Elm compiler from bundle";
        }

        var parseElmCompilerResult =
            ElmCompiler.ElmCompilerFromEnvValue(elmCompilerFromBundleValue);

        {
            if (parseElmCompilerResult.IsErrOrNull() is { } err)
            {
                return
                    "Failed parsing Elm compiler from bundled value: " +
                    err;
            }
        }

        if (parseElmCompilerResult.IsOkOrNull() is not { } elmCompiler)
        {
            throw new System.NotImplementedException(
                "Unexpected result type: " + parseElmCompilerResult.GetType());
        }

        if (elmCompiler.LanguageServiceInterface is not { } languageServiceInterface)
        {
            return "Language service interface not available in bundled Elm compiler";
        }

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


        var initElmValue =
            ElmValueEncoding.PineValueAsElmValue(initOk, null, null)
            .Extract(err => throw new System.Exception("Failed to parse init result: " + err));

        var initExpr =
            initElmValue.ToString();

        return new LanguageServiceState(
            languageServiceInterface,
            initOk,
            pineVM);
    }

    public static Result<string, LanguageServiceState> InitLanguageServiceState()
    {
        // TODO: Use overlap and warmup to reduce response delays.

        var pineVM =
            PineVM.PineVMResettingCache.Create(
                resetCacheEntriesThresholdDefault: 10_000);

        return InitLanguageServiceState(pineVM);
    }

    public Result<string, Interface.Response.WorkspaceSummaryResponse>
        DeleteFile(
        string fileUri)
    {
        var genericRequestResult =
            HandleRequest(new Interface.Request.DeleteWorkspaceFileRequest(fileUri));

        if (genericRequestResult.IsErrOrNull() is { } err)
        {
            return err;
        }

        if (genericRequestResult.IsOkOrNull() is not { } requestOk)
        {
            throw new System.NotImplementedException(
                "Unexpected request result type: " + genericRequestResult.GetType());
        }

        if (requestOk is not Interface.Response.WorkspaceSummaryResponse workspaceSummary)
        {
            throw new System.NotImplementedException(
                "Unexpected request result type: " + requestOk.GetType());
        }

        return workspaceSummary;
    }

    public Result<string, Interface.Response.WorkspaceSummaryResponse>
        AddFile(
        string fileUri,
        string fileContentAsText)
    {
        var asBase64 =
            System.Convert.ToBase64String(
                System.Text.Encoding.UTF8.GetBytes(fileContentAsText));

        var genericRequestResult =
            HandleRequest(
                new Interface.Request.AddWorkspaceFileRequest(
                    fileUri,
                    new Interface.FileTreeBlobNode(AsBase64: asBase64, AsText: fileContentAsText)));

        if (genericRequestResult.IsErrOrNull() is { } err)
        {
            return err;
        }

        if (genericRequestResult.IsOkOrNull() is not { } requestOk)
        {
            throw new System.NotImplementedException(
                "Unexpected request result type: " + genericRequestResult.GetType());
        }

        if (requestOk is not Interface.Response.WorkspaceSummaryResponse workspaceSummary)
        {
            throw new System.NotImplementedException(
                "Unexpected request result type: " + requestOk.GetType());
        }

        return workspaceSummary;
    }

    public Result<string, Interface.Response.WorkspaceSummaryResponse>
        AddElmPackage(
        Interface.ElmPackageVersion019Identifer packageVersionId,
        IReadOnlyList<KeyValuePair<IReadOnlyList<string>, string>> filesContentsAsText)
    {
        var genericRequestResult =
            HandleRequest(
                new Interface.Request.AddElmPackageVersionRequest(
                    packageVersionId,
                    [.. filesContentsAsText.Select(e =>
                    (e.Key,
                    new Interface.FileTreeBlobNode(
                        AsBase64: System.Convert.ToBase64String(System.Text.Encoding.UTF8.GetBytes(e.Value)), AsText: e.Value)))]));

        if (genericRequestResult.IsErrOrNull() is { } err)
        {
            return err;
        }

        if (genericRequestResult.IsOkOrNull() is not { } requestOk)
        {
            throw new System.NotImplementedException(
                "Unexpected request result type: " + genericRequestResult.GetType());
        }

        if (requestOk is not Interface.Response.WorkspaceSummaryResponse workspaceSummary)
        {
            throw new System.NotImplementedException(
                "Unexpected request result type: " + requestOk.GetType());
        }

        return workspaceSummary;
    }

    public Result<string, IReadOnlyList<string>> ProvideHover(
        Interface.ProvideHoverRequestStruct provideHoverRequest)
    {
        var genericRequestResult =
            HandleRequest(
                new Interface.Request.ProvideHoverRequest(provideHoverRequest));

        if (genericRequestResult.IsErrOrNull() is { } err)
        {
            return err;
        }

        if (genericRequestResult.IsOkOrNull() is not { } requestOk)
        {
            throw new System.NotImplementedException(
                "Unexpected request result type: " + genericRequestResult.GetType());
        }

        if (requestOk is not Interface.Response.ProvideHoverResponse provideHoverResponse)
        {
            throw new System.NotImplementedException(
                "Unexpected request result type: " + requestOk.GetType());
        }

        return Result<string, IReadOnlyList<string>>.ok(provideHoverResponse.Strings);
    }

    public Result<string, IReadOnlyList<MonacoEditor.MonacoCompletionItem>>
        ProvideCompletionItems(
            Interface.ProvideCompletionItemsRequestStruct provideCompletionItemsRequest)
    {
        var genericRequestResult =
            HandleRequest(
                new Interface.Request.ProvideCompletionItemsRequest(provideCompletionItemsRequest));

        if (genericRequestResult.IsErrOrNull() is { } err)
        {
            return err;
        }

        if (genericRequestResult.IsOkOrNull() is not { } requestOk)
        {
            throw new System.NotImplementedException(
                "Unexpected request result type: " + genericRequestResult.GetType());
        }

        if (requestOk is not Interface.Response.ProvideCompletionItemsResponse provideCompletionItemsResponse)
        {
            throw new System.NotImplementedException(
                "Unexpected request result type: " + requestOk.GetType());
        }

        return Result<string, IReadOnlyList<MonacoEditor.MonacoCompletionItem>>.ok(
            provideCompletionItemsResponse.CompletionItems);
    }

    public Result<string, Interface.Response> HandleRequest(
        Interface.Request request)
    {
        var requestEncoded =
            Interface.RequestEncoding.Encode(request);

        lock (pineVM)
        {
            var handleRequestResult =
                ElmInteractiveEnvironment.ApplyFunction(
                    pineVM,
                    languageServiceInterface.HandleRequestInCurrentWorkspace,
                    [
                        requestEncoded,
                        _state,
                    ]);

            {
                if (handleRequestResult.IsErrOrNull() is { } err)
                {
                    throw new System.Exception("Failed to handle request: " + err);
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

            _state = langServiceStateValue;

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
                Interface.ResponseEncoding.Decode(langServiceResponseOk);

            {
                if (decodeResponseResult.IsErrOrNull() is { } err)
                {
                    return
                        "Failed to decode response: " + err;
                }
            }

            if (decodeResponseResult.IsOkOrNull() is not { } decodedResponse)
            {
                throw new System.NotImplementedException(
                    "Unexpected response type: " + decodeResponseResult.GetType());
            }

            return decodedResponse;
        }
    }

    public static PineValue EncodeFileTreeNodeAsPineValue(
        Interface.FileTreeNode<Interface.FileTreeBlobNode> node)
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

    public static PineValue EncodeFileTreeNodeAsPineValue<BlobT>(
        Interface.FileTreeNode<BlobT> node,
        System.Func<BlobT, PineValue> encodeBlob)
    {
        if (node is Interface.FileTreeNode<BlobT>.BlobNode blobNode)
        {
            return PineValue.List(
                [
                    StringEncoding.ValueFromString("BlobNode"),
                    PineValue.List(
                        [
                            encodeBlob(blobNode.Blob)
                        ])
                ]);
        }

        if (node is Interface.FileTreeNode<BlobT>.TreeNode treeNode)
        {
            return PineValue.List(
                [
                    StringEncoding.ValueFromString("TreeNode"),
                    PineValue.List(
                        [
                            PineValue.List(
                                [..treeNode.Children
                                .Select(e =>
                                    PineValue.List(
                                        [
                                            ElmValueEncoding.StringAsPineValue(e.name),
                                            EncodeFileTreeNodeAsPineValue(e.node, encodeBlob)
                                        ]))
                                ])
                        ])
                ]);
        }

        throw new System.NotImplementedException(
            "Unexpected node type: " + node.GetType());
    }

    public static Interface.FileTreeNode<Interface.FileTreeBlobNode>
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
                new Interface.FileTreeNode<Interface.FileTreeBlobNode>.BlobNode(
                    new Interface.FileTreeBlobNode(
                        AsBase64: System.Convert.ToBase64String(blobNode.Bytes.Span),
                        AsText: asText));
        }

        if (workspace is FileTree.DirectoryNode treeNode)
        {
            return
                new Interface.FileTreeNode<Interface.FileTreeBlobNode>.TreeNode(
                    [..treeNode.Items.Select(e =>
                        (e.name, Workspace(e.component))
                    )]);
        }

        throw new System.NotImplementedException(
            "Unexpected node type: " + workspace.GetType());
    }


    readonly ElmCompilerCache inspectionElmCompilerCache = new();
}
