using AwesomeAssertions;
using Pine.Core.Elm.LanguageServer;
using Pine.Core.IO;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text;
using Xunit;

namespace Pine.Core.Tests.Elm.LanguageServer;

public class WorkspaceFromFileStoreMountsTests
{
    [Fact]
    public void EnumerateFiles_is_recursive_deduplicated_sorted_and_utf8_decoded()
    {
        var files =
            new Dictionary<IImmutableList<string>, ReadOnlyMemory<byte>>(
                EnumerableExtensions.EqualityComparer<IImmutableList<string>>())
            {
                [["src", "Main.elm"]] = Encoding.UTF8.GetBytes("module Main exposing (..)\n"),
                [["elm.json"]] = Encoding.UTF8.GetBytes("""{"type":"application"}"""),
                [["src", "With space.elm"]] = Encoding.UTF8.GetBytes("🌲"),
            };

        var reader =
            new DelegatingFileStoreReader(
                path => files.GetValueOrDefault(path),
                _ =>
                [
                    ["src", "With space.elm"],
                    ["src", "Main.elm"],
                    ["elm.json"],
                    ["src", "Main.elm"],
                ]);

        var workspace =
            new WorkspaceFromFileStoreMounts(
                [new FileStoreMount(new Uri("file:///workspace/"), reader)]);

        var filesResult = workspace.EnumerateFiles("file:///workspace/");

        var workspaceFiles =
            filesResult.Should().BeOfType<Result<WorkspaceAccessError, IReadOnlyList<WorkspaceFile>>.Ok>()
            .Which.Value;

        workspaceFiles.Select(file => file.DocumentUri)
            .Should().Equal(
            "file:///workspace/elm.json",
            "file:///workspace/src/Main.elm",
            "file:///workspace/src/With%20space.elm");

        workspaceFiles[^1].Text.Should().Be("🌲");
    }

    [Fact]
    public void ReadFile_uses_longest_matching_uri_root_and_segment_boundaries()
    {
        var outerStore = new FileStoreFromConcurrentDictionary();
        var nestedStore = new FileStoreFromConcurrentDictionary();

        outerStore.SetFileContent(["project", "Main.elm"], Encoding.UTF8.GetBytes("outer"));
        nestedStore.SetFileContent(["Main.elm"], Encoding.UTF8.GetBytes("nested"));

        var workspace =
            new WorkspaceFromFileStoreMounts(
                [
                    new FileStoreMount(new Uri("file:///workspace/"), outerStore),
                    new FileStoreMount(new Uri("file:///workspace/project/"), nestedStore),
                ]);

        ReadText(workspace, "file:///workspace/project/Main.elm").Should().Be("nested");

        var enumerated =
            workspace.EnumerateFiles("file:///workspace/")
            .Should().BeOfType<Result<WorkspaceAccessError, IReadOnlyList<WorkspaceFile>>.Ok>()
            .Which.Value;

        enumerated.Should().ContainSingle()
            .Which.Should().Be(
            new WorkspaceFile(
                "file:///workspace/project/Main.elm",
                "nested"));

        var outsideResult = workspace.ReadFile("file:///workspace-other/Main.elm");

        outsideResult.Should().BeOfType<Result<WorkspaceAccessError, WorkspaceFile?>.Err>()
            .Which.Value.Kind.Should().Be(WorkspaceAccessErrorKind.OutsideMount);
    }

    [Fact]
    public void ReadFile_distinguishes_missing_invalid_text_invalid_uri_and_backend_failure()
    {
        var store = new FileStoreFromConcurrentDictionary();
        store.SetFileContent(["invalid.elm"], new byte[] { 0xFF });

        var workspace =
            new WorkspaceFromFileStoreMounts(
                [new FileStoreMount(new Uri("memory://workspace/"), store)]);

        workspace.ReadFile("memory://workspace/missing.elm")
            .Should().BeOfType<Result<WorkspaceAccessError, WorkspaceFile?>.Ok>()
            .Which.Value.Should().BeNull();

        workspace.ReadFile("memory://workspace/invalid.elm")
            .Should().BeOfType<Result<WorkspaceAccessError, WorkspaceFile?>.Err>()
            .Which.Value.Kind.Should().Be(WorkspaceAccessErrorKind.InvalidText);

        workspace.ReadFile("not a uri")
            .Should().BeOfType<Result<WorkspaceAccessError, WorkspaceFile?>.Err>()
            .Which.Value.Kind.Should().Be(WorkspaceAccessErrorKind.InvalidUri);

        var throwingWorkspace =
            new WorkspaceFromFileStoreMounts(
                [
                    new FileStoreMount(
                        new Uri("memory://workspace/"),
                        new DelegatingFileStoreReader(
                            _ => throw new InvalidOperationException("backend unavailable"),
                            _ => []))
                ]);

        throwingWorkspace.ReadFile("memory://workspace/Main.elm")
            .Should().BeOfType<Result<WorkspaceAccessError, WorkspaceFile?>.Err>()
            .Which.Value.Kind.Should().Be(WorkspaceAccessErrorKind.BackendFailure);
    }

    [Fact]
    public void ReadFile_rejects_percent_encoded_path_delimiters()
    {
        var workspace =
            new WorkspaceFromFileStoreMounts(
                [
                    new FileStoreMount(
                        new Uri("memory://workspace/"),
                        new FileStoreFromConcurrentDictionary())
                ]);

        workspace.ReadFile("memory://workspace/src%2FMain.elm")
            .Should().BeOfType<Result<WorkspaceAccessError, WorkspaceFile?>.Err>()
            .Which.Value.Kind.Should().BeOneOf(
            WorkspaceAccessErrorKind.InvalidPath,
            WorkspaceAccessErrorKind.InvalidUri);
    }

    [Fact]
    public void FindNearestElmProject_walks_parents_within_the_selected_mount()
    {
        var store = new FileStoreFromConcurrentDictionary();
        store.SetFileContent(["elm.json"], Encoding.UTF8.GetBytes("{}"));
        store.SetFileContent(["nested", "elm.json"], Encoding.UTF8.GetBytes("{}"));
        store.SetFileContent(["nested", "src", "Main.elm"], Encoding.UTF8.GetBytes(""));

        var workspace =
            new WorkspaceFromFileStoreMounts(
                [new FileStoreMount(new Uri("file:///workspace/"), store)]);

        var result = workspace.FindNearestElmProject("file:///workspace/nested/src/Main.elm");

        result.Should().BeOfType<Result<WorkspaceAccessError, ElmProjectLocation?>.Ok>()
            .Which.Value.Should().Be(
            new ElmProjectLocation("file:///workspace/nested/elm.json"));
    }

    private static string? ReadText(WorkspaceFromFileStoreMounts workspace, string documentUri) =>
        workspace.ReadFile(documentUri)
        .Should().BeOfType<Result<WorkspaceAccessError, WorkspaceFile?>.Ok>()
        .Which.Value?.Text;
}
