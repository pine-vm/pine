using Microsoft.VisualStudio.TestTools.UnitTesting;
using Pine;
using Pine.Core;
using Pine.Core.PineVM;
using Pine.Elm;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text;

namespace TestElmTime;

[TestClass]
public class ElmLanguageServiceTests
{
    [TestMethod]
    public void Language_service_provides_hover()
    {
        var elmModuleText =
            """
            module Main exposing (State)


            name = i👈🚁nit


            init : State
            init =
                0

            """;


        var compilerSourceFiles =
            ElmCompiler.CompilerSourceFilesDefault.Value;

        var combinedSourceFiles =
            ElmCompiler.ElmCompilerFileTreeFromBundledFileTree(compilerSourceFiles);

        var elmCompilerFromBundleValue =
            Pine.Core.Elm.BundledElmEnvironments.BundledElmEnvironmentFromFileTree(combinedSourceFiles);

        Assert.IsNotNull(elmCompilerFromBundleValue);

        var elmCompiler =
            ElmCompiler.ElmCompilerFromEnvValue(elmCompilerFromBundleValue)
            .Extract(err => throw new Exception(err));

        var pineVMCache =
            new Pine.PineVM.PineVMCache();

        var pineVM =
            new Pine.PineVM.PineVM(evalCache: pineVMCache.EvalCache);

        AssertHover(
            workspace: TreeNodeWithStringPath.EmptyTree,
            filePathOpenedInEditor: ["src", "Main.elm"],
            elmModuleText,
            expectedHoverText: "    init : State",
            pineVM);
    }

    [TestMethod]
    public void Language_service_provides_completion_items()
    {
        var elmModuleTextAlfa =
            """
            module Alfa exposing (..)

            {-| Documentation comment on module Alfa
            -}

            from_alfa = 123
            
            """;

        var elmModuleTextMainBefore =
            """
            module Main exposing (..)

            import Alfa

            name = Alfa.test

            init : State
            init =
                0

            """;

        var elmModuleTextMain =
            """
            module Main exposing (..)

            import Alfa

            name = Alfa.✂➕

            init : State
            init =
                0

            """;


        var compilerSourceFiles =
            ElmCompiler.CompilerSourceFilesDefault.Value;

        var combinedSourceFiles =
            ElmCompiler.ElmCompilerFileTreeFromBundledFileTree(compilerSourceFiles);

        var elmCompilerFromBundleValue =
            Pine.Core.Elm.BundledElmEnvironments.BundledElmEnvironmentFromFileTree(combinedSourceFiles);

        Assert.IsNotNull(elmCompilerFromBundleValue);

        var elmCompiler =
            ElmCompiler.ElmCompilerFromEnvValue(elmCompilerFromBundleValue)
            .Extract(err => throw new Exception(err));

        var pineVMCache =
            new Pine.PineVM.PineVMCache();

        var pineVM =
            new Pine.PineVM.PineVM(evalCache: pineVMCache.EvalCache);

        var workspace =
            TreeNodeWithStringPath.EmptyTree
            .SetNodeAtPathSorted(
                ["src", "Alfa.elm"],
                new TreeNodeWithStringPath.BlobNode(Encoding.UTF8.GetBytes(elmModuleTextAlfa)));

        AssertCompletionItems(
            workspace: workspace,
            filePathOpenedInEditor: ["src", "Main.elm"],
            elmModuleTextBefore: elmModuleTextMainBefore,
            elmModuleTextWithSplitSymbol: elmModuleTextMain,
            expectedCompletionItems:
            [
                new Pine.Elm.MonacoEditor.MonacoCompletionItem(
                    Label: "from_alfa",
                    Kind: new Pine.Elm.MonacoEditor.CompletionItemKind.FunctionCompletionItemKind(),
                    Documentation: "    from_alfa",
                    InsertText: "from_alfa")
            ],
            pineVM);
    }

    private static void AssertHover(
        TreeNodeWithStringPath workspace,
        IReadOnlyList<string> filePathOpenedInEditor,
        string elmModuleTextWithSplitSymbol,
        string expectedHoverText,
        IPineVM pineVM)
    {
        var split = elmModuleTextWithSplitSymbol.Split("👈🚁");

        if (split.Length is not 2)
            throw new Exception("Expected exactly one '👈🚁' in the Elm module text");

        var (beforeCursor, afterCursor) = (split[0], split[1]);

        var elmModuleText =
            beforeCursor + afterCursor;

        var elmModuleTextLines =
            ElmTime.ElmSyntax.ElmModule.ModuleLines(elmModuleText)
            .ToImmutableArray();

        var beforeCursorLines =
            ElmTime.ElmSyntax.ElmModule.ModuleLines(beforeCursor)
            .ToImmutableArray();

        var afterCursorLines =
            ElmTime.ElmSyntax.ElmModule.ModuleLines(afterCursor)
            .ToImmutableArray();

        var lineText =
            beforeCursorLines.Last() + afterCursorLines.First();

        var mergedWorkspace =
            workspace
            .SetNodeAtPathSorted(
                filePathOpenedInEditor,
                new TreeNodeWithStringPath.BlobNode(Encoding.UTF8.GetBytes(elmModuleText)));

        var languageService =
            LanguageServiceState.InitLanguageServiceState(pineVM)
            .Extract(err => throw new Exception(err));

        MutateServiceAddingFiles(mergedWorkspace, languageService, pineVM);

        var hoverRequest =
            new Pine.Elm.LanguageServiceInterface.ProvideHoverRequestStruct(
                FilePathOpenedInEditor: filePathOpenedInEditor,
                PositionLineNumber: beforeCursorLines.Length,
                PositionColumn: beforeCursorLines.Last().Length);

        var hoverResponse =
            languageService.ProvideHover(
                hoverRequest,
                pineVM)
            .Extract(err => throw new Exception(err));

        Assert.AreEqual(
            expectedHoverText,
            string.Concat(hoverResponse));
    }

    private static void MutateServiceAddingFiles(
        TreeNodeWithStringPath fileTree,
        LanguageServiceState languageServiceState,
        IPineVM pineVM)
    {

        foreach (var file in fileTree.EnumerateBlobsTransitive())
        {
            var asText = Encoding.UTF8.GetString(file.blobContent.Span);

            var asBase64 = Convert.ToBase64String(file.blobContent.Span);

            var addFileResult =
                languageServiceState.HandleRequest(
                new Pine.Elm.LanguageServiceInterface.Request.AddFileRequest(
                    FilePath: file.path,
                    Blob: new Pine.Elm.LanguageServiceInterface.FileTreeBlobNode(
                        AsBase64: asBase64,
                        AsText: asText)),
                pineVM);

            if (addFileResult.IsErrOrNull() is { } err)
                throw new Exception(err);
        }
    }

    private static void AssertCompletionItems(
        TreeNodeWithStringPath workspace,
        IReadOnlyList<string> filePathOpenedInEditor,
        string elmModuleTextBefore,
        string elmModuleTextWithSplitSymbol,
        IReadOnlyList<Pine.Elm.MonacoEditor.MonacoCompletionItem> expectedCompletionItems,
        IPineVM pineVM)
    {
        var split =
            elmModuleTextWithSplitSymbol.Split("✂➕");

        if (split.Length is not 2)
            throw new Exception("Expected exactly one '✂➕' in the Elm module text");

        var (beforeCursor, afterCursor) = (split[0], split[1]);

        var languageService =
            LanguageServiceState.InitLanguageServiceState(pineVM)
            .Extract(err => throw new Exception(err));

        var mergedWorkspaceBefore =
            workspace
            .SetNodeAtPathSorted(
                filePathOpenedInEditor,
                new TreeNodeWithStringPath.BlobNode(Encoding.UTF8.GetBytes(elmModuleTextBefore)));

        MutateServiceAddingFiles(mergedWorkspaceBefore, languageService, pineVM);

        var elmModuleText =
            beforeCursor + afterCursor;

        var elmModuleTextLines =
            ElmTime.ElmSyntax.ElmModule.ModuleLines(elmModuleText)
            .ToImmutableArray();

        var beforeCursorLines =
            ElmTime.ElmSyntax.ElmModule.ModuleLines(beforeCursor)
            .ToImmutableArray();

        var afterCursorLines =
            ElmTime.ElmSyntax.ElmModule.ModuleLines(afterCursor)
            .ToImmutableArray();

        var lineText =
            beforeCursorLines.Last() + afterCursorLines.First();

        var mergedWorkspace =
            workspace
            .SetNodeAtPathSorted(
                filePathOpenedInEditor,
                new TreeNodeWithStringPath.BlobNode(Encoding.UTF8.GetBytes(elmModuleText)));

        var completionItemsRequest =
            new Pine.Elm.LanguageServiceInterface.ProvideCompletionItemsRequestStruct(
                FilePathOpenedInEditor: filePathOpenedInEditor,
                CursorLineNumber: beforeCursorLines.Length,
                CursorColumn: beforeCursorLines.Last().Length + 1);

        var completionItemsResponse =
            languageService.ProvideCompletionItems(
                completionItemsRequest,
                pineVM)
            .Extract(err => throw new Exception(err));

        if (completionItemsResponse.Count != expectedCompletionItems.Count)
        {
            Console.WriteLine(
                "Expected completion items:\n" +
                string.Join("\n", expectedCompletionItems));

            Console.WriteLine(
                "Actual completion items:\n" +
                string.Join("\n", completionItemsResponse));

            throw new Exception(
                "Actual completion items count (" + completionItemsResponse.Count +
                ") does not match expected completion items count (" + expectedCompletionItems.Count + ")");
        }

        for (int i = 0; i < expectedCompletionItems.Count; i++)
        {
            var expectedItem = expectedCompletionItems[i];

            var actualItem = completionItemsResponse[i];

            if (!actualItem.Equals(expectedItem))
            {
                Console.WriteLine(
                    "Expected completion item:\n" +
                    expectedItem);

                Console.WriteLine(
                    "Actual completion item:\n" +
                    actualItem);

                throw new Exception(
                    "Expected completion item does not match actual completion item");
            }
        }
    }
}