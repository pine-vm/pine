using AwesomeAssertions;
using Pine.Core;
using Pine.Elm;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text;
using Xunit;

namespace Pine.IntegrationTests;

public class ElmLanguageServiceTests
{
    [Fact]
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
            Core.Elm.BundledElmEnvironments.BundledElmEnvironmentFromFileTree(combinedSourceFiles);

        elmCompilerFromBundleValue.Should().NotBeNull();

        var elmCompiler =
            ElmCompiler.ElmCompilerFromEnvValue(elmCompilerFromBundleValue)
            .Extract(err => throw new Exception(err));

        AssertHover(
            workspace: BlobTreeWithStringPath.EmptyTree,
            filePathOpenedInEditor: ["src", "Main.elm"],
            elmModuleText,
            expectedHoverText: "    init : State");
    }

    [Fact]
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
            Core.Elm.BundledElmEnvironments.BundledElmEnvironmentFromFileTree(combinedSourceFiles);

        elmCompilerFromBundleValue.Should().NotBeNull();

        var elmCompiler =
            ElmCompiler.ElmCompilerFromEnvValue(elmCompilerFromBundleValue)
            .Extract(err => throw new Exception(err));

        var pineVMCache =
            new PineVM.PineVMCache();

        var pineVM =
            new PineVM.PineVM(evalCache: pineVMCache.EvalCache);

        var workspace =
            BlobTreeWithStringPath.EmptyTree
            .SetNodeAtPathSorted(
                ["src", "Alfa.elm"],
                new BlobTreeWithStringPath.BlobNode(Encoding.UTF8.GetBytes(elmModuleTextAlfa)));

        AssertCompletionItems(
            workspace: workspace,
            filePathOpenedInEditor: ["src", "Main.elm"],
            elmModuleTextBefore: elmModuleTextMainBefore,
            elmModuleTextWithSplitSymbol: elmModuleTextMain,
            expectedCompletionItems:
            [
                new Elm.MonacoEditor.MonacoCompletionItem(
                    Label: "from_alfa",
                    Kind: new Elm.MonacoEditor.CompletionItemKind.FunctionCompletionItemKind(),
                    Documentation: "    from_alfa",
                    InsertText: "from_alfa")
            ]);
    }

    private static void AssertHover(
        BlobTreeWithStringPath workspace,
        IReadOnlyList<string> filePathOpenedInEditor,
        string elmModuleTextWithSplitSymbol,
        string expectedHoverText)
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
                new BlobTreeWithStringPath.BlobNode(Encoding.UTF8.GetBytes(elmModuleText)));

        var languageService =
            LanguageServiceState.InitLanguageServiceState()
            .Extract(err => throw new Exception(err));

        MutateServiceAddingFiles(mergedWorkspace, languageService);

        var hoverRequest =
            new Elm.LanguageServiceInterface.ProvideHoverRequestStruct(
                FileLocation:
                new Elm.LanguageServiceInterface.FileLocation.WorkspaceFileLocation(
                    string.Join("/", filePathOpenedInEditor)),
                PositionLineNumber: beforeCursorLines.Length,
                PositionColumn: beforeCursorLines.Last().Length);

        var hoverResponse =
            languageService.ProvideHover(hoverRequest)
            .Extract(err => throw new Exception(err));

        string.Concat(hoverResponse)
            .Should()
            .Be(expectedHoverText);
    }

    private static void MutateServiceAddingFiles(
        BlobTreeWithStringPath fileTree,
        LanguageServiceState languageServiceState)
    {

        foreach (var file in fileTree.EnumerateBlobsTransitive())
        {
            var asText = Encoding.UTF8.GetString(file.blobContent.Span);

            var asBase64 = Convert.ToBase64String(file.blobContent.Span);

            var addFileResult =
                languageServiceState.HandleRequest(
                new Elm.LanguageServiceInterface.Request.AddWorkspaceFileRequest(
                    FilePath: string.Join("/", file.path),
                    Blob: new Elm.LanguageServiceInterface.FileTreeBlobNode(
                        AsBase64: asBase64,
                        AsText: asText)));

            if (addFileResult.IsErrOrNull() is { } err)
                throw new Exception(err);
        }
    }

    private static void AssertCompletionItems(
        BlobTreeWithStringPath workspace,
        IReadOnlyList<string> filePathOpenedInEditor,
        string elmModuleTextBefore,
        string elmModuleTextWithSplitSymbol,
        IReadOnlyList<Elm.MonacoEditor.MonacoCompletionItem> expectedCompletionItems)
    {
        var split =
            elmModuleTextWithSplitSymbol.Split("✂➕");

        if (split.Length is not 2)
            throw new Exception("Expected exactly one '✂➕' in the Elm module text");

        var (beforeCursor, afterCursor) = (split[0], split[1]);

        var languageService =
            LanguageServiceState.InitLanguageServiceState()
            .Extract(err => throw new Exception(err));

        var mergedWorkspaceBefore =
            workspace
            .SetNodeAtPathSorted(
                filePathOpenedInEditor,
                new BlobTreeWithStringPath.BlobNode(Encoding.UTF8.GetBytes(elmModuleTextBefore)));

        MutateServiceAddingFiles(mergedWorkspaceBefore, languageService);

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
                new BlobTreeWithStringPath.BlobNode(Encoding.UTF8.GetBytes(elmModuleText)));

        var completionItemsRequest =
            new Elm.LanguageServiceInterface.ProvideCompletionItemsRequestStruct(
                FilePathOpenedInEditor: string.Join("/", filePathOpenedInEditor),
                CursorLineNumber: beforeCursorLines.Length,
                CursorColumn: beforeCursorLines.Last().Length + 1);

        var completionItemsResponse =
            languageService.ProvideCompletionItems(completionItemsRequest)
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

        for (var i = 0; i < expectedCompletionItems.Count; i++)
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
