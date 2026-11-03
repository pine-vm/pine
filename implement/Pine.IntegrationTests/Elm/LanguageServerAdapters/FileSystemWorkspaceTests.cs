using AwesomeAssertions;
using Pine.Core.Elm.LanguageServer;
using Pine.Core.Functional;
using Pine.Elm.LanguageServerAdapters;
using System;
using System.IO;
using System.Linq;
using System.Threading.Tasks;
using Xunit;

namespace Pine.IntegrationTests.Elm.LanguageServerAdapters;

public class FileSystemWorkspaceTests
{
    [Fact]
    public async Task Workspace_file_URIs_preserve_escaped_names_and_support_VS_Code_drive_encoding()
    {
        var directory =
            Path.GetFullPath(Path.Combine("artifacts", "workspace-uri", Guid.NewGuid().ToString("N") + " # %20"));

        Directory.CreateDirectory(Path.Combine(directory, "tests"));
        var manifest = Path.Combine(directory, "elm.json");
        var source = Path.Combine(directory, "tests", "Tests.elm");

        try
        {
            await File.WriteAllTextAsync(manifest, "{}");
            await File.WriteAllTextAsync(source, "module Tests exposing (..)\n");
            var rootUri = ClientUri(new Uri(directory + Path.DirectorySeparatorChar));
            var sourceUri = ClientUri(new Uri(source));
            var workspace = new FileSystemWorkspace();

            var files =
                workspace.EnumerateFiles(rootUri)
                .Should().BeOfType<Result<WorkspaceAccessError, System.Collections.Generic.IReadOnlyList<WorkspaceFile>>.Ok>()
                .Which.Value;

            files.Select(file => file.DocumentUri).Should().Contain(new Uri(manifest).AbsoluteUri);

            workspace.ReadFile(sourceUri)
                .Should().BeOfType<Result<WorkspaceAccessError, WorkspaceFile?>.Ok>()
                .Which.Value!.Text.Should().StartWith("module Tests");

            workspace.FindNearestElmProject(sourceUri)
                .Should().BeOfType<Result<WorkspaceAccessError, ElmProjectLocation?>.Ok>()
                .Which.Value!.ElmJsonDocumentUri.Should().Be(new Uri(manifest).AbsoluteUri);
        }
        finally
        {
            Directory.Delete(directory, recursive: true);
        }

        static string ClientUri(Uri uri)
        {
            var value = uri.AbsoluteUri;

            if (value.Length > 10 && value.StartsWith("file:///") && char.IsAsciiLetter(value[8]) && value[9] == ':')
                return value[..8] + char.ToLowerInvariant(value[8]) + "%3A" + value[10..];

            return value;
        }
    }
}
