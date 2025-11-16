using AwesomeAssertions;
using Pine.Core;
using Pine.Core.Files;
using Pine.Core.Interpreter.IntermediateVM;
using Pine.Elm;
using Pine.Elm.MonacoEditor;
using Pine.IntermediateVM;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text;
using Xunit;

using LanguageServiceInterface = Pine.Elm.LanguageServiceInterface;

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
            workspace: FileTree.EmptyTree,
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

        var pineVMCache = new InvocationCache();

        var pineVM =
            SetupVM.Create(evalCache: pineVMCache);

        var workspace =
            FileTree.EmptyTree
            .SetNodeAtPathSorted(
                ["src", "Alfa.elm"],
                new FileTree.FileNode(Encoding.UTF8.GetBytes(elmModuleTextAlfa)));

        AssertCompletionItems(
            workspace: workspace,
            filePathOpenedInEditor: ["src", "Main.elm"],
            elmModuleTextBefore: elmModuleTextMainBefore,
            elmModuleTextWithSplitSymbol: elmModuleTextMain,
            expectedCompletionItems:
            [
                new MonacoCompletionItem(
                    Label: "from_alfa",
                    Kind: new CompletionItemKind.FunctionCompletionItemKind(),
                    Documentation: "    from_alfa",
                    InsertText: "from_alfa")
            ]);
    }

    private static void AssertHover(
        FileTree workspace,
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
            Core.Elm.ElmSyntax.ElmModule.ModuleLines(elmModuleText)
            .ToImmutableArray();

        var beforeCursorLines =
            Core.Elm.ElmSyntax.ElmModule.ModuleLines(beforeCursor)
            .ToImmutableArray();

        var afterCursorLines =
            Core.Elm.ElmSyntax.ElmModule.ModuleLines(afterCursor)
            .ToImmutableArray();

        var lineText =
            beforeCursorLines.Last() + afterCursorLines.First();

        var mergedWorkspace =
            workspace
            .SetNodeAtPathSorted(
                filePathOpenedInEditor,
                new FileTree.FileNode(Encoding.UTF8.GetBytes(elmModuleText)));

        var languageService =
            LanguageServiceState.InitLanguageServiceState()
            .Extract(err => throw new Exception(err));

        MutateServiceAddingFiles(mergedWorkspace, languageService);

        var hoverRequest =
            new LanguageServiceInterface.ProvideHoverRequestStruct(
                FileLocation:
                new LanguageServiceInterface.FileLocation.WorkspaceFileLocation(
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
        FileTree fileTree,
        LanguageServiceState languageServiceState)
    {

        foreach (var file in fileTree.EnumerateFilesTransitive())
        {
            var asText = Encoding.UTF8.GetString(file.fileContent.Span);

            var asBase64 = Convert.ToBase64String(file.fileContent.Span);

            var addFileResult =
                languageServiceState.HandleRequest(
                new LanguageServiceInterface.Request.AddWorkspaceFileRequest(
                    FilePath: string.Join("/", file.path),
                    Blob: new LanguageServiceInterface.FileTreeBlobNode(
                        AsBase64: asBase64,
                        AsText: asText)));

            if (addFileResult.IsErrOrNull() is { } err)
                throw new Exception(err);
        }
    }

    private static void AssertCompletionItems(
        FileTree workspace,
        IReadOnlyList<string> filePathOpenedInEditor,
        string elmModuleTextBefore,
        string elmModuleTextWithSplitSymbol,
        IReadOnlyList<MonacoCompletionItem> expectedCompletionItems)
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
                new FileTree.FileNode(Encoding.UTF8.GetBytes(elmModuleTextBefore)));

        MutateServiceAddingFiles(mergedWorkspaceBefore, languageService);

        var elmModuleText =
            beforeCursor + afterCursor;

        var elmModuleTextLines =
            Core.Elm.ElmSyntax.ElmModule.ModuleLines(elmModuleText)
            .ToImmutableArray();

        var beforeCursorLines =
            Core.Elm.ElmSyntax.ElmModule.ModuleLines(beforeCursor)
            .ToImmutableArray();

        var afterCursorLines =
            Core.Elm.ElmSyntax.ElmModule.ModuleLines(afterCursor)
            .ToImmutableArray();

        var lineText =
            beforeCursorLines.Last() + afterCursorLines.First();

        var mergedWorkspace =
            workspace
            .SetNodeAtPathSorted(
                filePathOpenedInEditor,
                new FileTree.FileNode(Encoding.UTF8.GetBytes(elmModuleText)));

        var completionItemsRequest =
            new LanguageServiceInterface.ProvideCompletionItemsRequestStruct(
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
