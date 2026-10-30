using AwesomeAssertions;
using Pine.Core.Elm.Syntax;
using Pine.Core.Elm.Syntax.Abstract;
using Pine.Core.Functional;
using Pine.Core.IO;
using System;
using System.Collections.Generic;
using System.IO;
using System.IO.Compression;
using System.Linq;
using System.Security.Cryptography;
using System.Text;
using Xunit;

namespace Pine.Core.Tests.Elm.Syntax.Abstract;

public class RepositoryRoundtripTests
{
    [Fact]
    public void All_parsable_elm_files_roundtrip_through_concrete_syntax()
    {
        var repositoryRoot = FindRepositoryRoot();
        var failures = new List<string>();
        var files = LoadRepositoryElmFiles(repositoryRoot);

        files.Should().NotBeEmpty("the latest commit should contain Elm source files");

        foreach (var (relativePath, contents) in files)
        {
            var parseResult = ElmSyntaxParser.ParseModuleText(Encoding.UTF8.GetString(contents.Span));

            if (parseResult.IsOkOrNull() is not { } concreteFile)
            {
                continue;
            }

            if (concreteFile.IncompleteDeclarations.Count is not 0)
            {
                continue;
            }

            var roundtripStage = "convert parsed concrete syntax to the first abstract representation";

            try
            {
                var firstAbstract = ConvertFromConcrete.FromFile(concreteFile);

                roundtripStage = "convert and render the first abstract representation";

                var rendered =
                    ElmSyntaxAbstractTestHelper.RenderModuleForSyntaxRoundtrip(firstAbstract);

                roundtripStage = "parse the rendered concrete syntax";

                var reparsedConcrete =
                    ElmSyntaxParser.ParseModuleText(rendered)
                    .Extract(error => throw new Exception(error + "\n\n" + rendered));

                if (reparsedConcrete.IncompleteDeclarations.Count is not 0)
                {
                    throw new Exception("Rendered syntax contains incomplete declarations:\n\n" + rendered);
                }

                roundtripStage = "convert the reparsed concrete syntax to the second abstract representation";
                var secondAbstract = ConvertFromConcrete.FromFile(reparsedConcrete);

                var normalizedFirst =
                    ElmSyntaxAbstractTestHelper.NormalizeForSemanticComparison(firstAbstract);

                var normalizedSecond =
                    ElmSyntaxAbstractTestHelper.NormalizeForSemanticComparison(secondAbstract);

                if (!normalizedFirst.Equals(normalizedSecond))
                {
                    throw new Exception(DescribeFirstDifference(normalizedFirst, normalizedSecond));
                }
            }
            catch (Exception exception)
            {
                failures.Add(
                    relativePath + ": Failed to " + roundtripStage + " (" +
                    exception.GetType().Name + "): " + exception.Message);
            }
        }

        failures.Should().BeEmpty(
            "every parsable Elm file in the latest commit should roundtrip through the abstract and concrete syntax models:\n" +
            string.Join("\n", failures));
    }

    [Fact]
    public void Repository_elm_corpus_reads_committed_contents_without_working_tree_files()
    {
        var repositoryRoot =
            Path.Combine(Path.GetTempPath(), "pine-roundtrip-corpus-" + Guid.NewGuid().ToString("N"));

        Directory.CreateDirectory(repositoryRoot);

        try
        {
            var committedPaths = new[] { "Caf\u00e9.elm", "Tracked file.elm", "bin/Tracked.elm" };
            var blob = WriteGitObject(repositoryRoot, "blob", Encoding.UTF8.GetBytes("committed source"));
            var subtree =
                WriteGitObject(repositoryRoot, "tree", TreeEntry("100644", "Tracked.elm", blob));

            var tree =
                WriteGitObject(
                    repositoryRoot,
                    "tree",
                    [
                        .. TreeEntry("100644", "Caf\u00e9.elm", blob),
                        .. TreeEntry("100644", "Tracked file.elm", blob),
                        .. TreeEntry("40000", "bin", subtree),
                        .. TreeEntry("100644", "notes.txt", blob),
                    ]);

            var commit =
                WriteGitObject(
                    repositoryRoot,
                    "commit",
                    Encoding.UTF8.GetBytes(
                        "tree " + tree + "\nauthor Fixture <fixture@example.com> 1760000000 +0000\n" +
                        "committer Fixture <fixture@example.com> 1760000000 +0000\n\nCorpus fixture\n"));

            System.IO.File.WriteAllText(Path.Combine(repositoryRoot, ".git", "HEAD"), commit + "\n");

            foreach (var relativePath in committedPaths)
            {
                var path = Path.GetFullPath(relativePath, repositoryRoot);
                Directory.CreateDirectory(Path.GetDirectoryName(path)!);
                System.IO.File.WriteAllText(path, "modified working-tree source");
            }

            foreach (var relativePath in new[]
            {
                "bin/Debug/net10.0/artifacts/elm-fuzz-tests/project/tests/Tests.elm",
                "obj/Generated.elm",
                "artifacts/elm-fuzz-tests/project/tests/Tests.elm",
                "Untracked.elm",
            })
            {
                var path = Path.GetFullPath(relativePath, repositoryRoot);
                Directory.CreateDirectory(Path.GetDirectoryName(path)!);
                System.IO.File.WriteAllText(path, "temporary source");
            }

            var files = LoadRepositoryElmFiles(repositoryRoot);

            foreach (var relativePath in committedPaths)
            {
                System.IO.File.Delete(Path.GetFullPath(relativePath, repositoryRoot));
            }

            Directory.Delete(Path.Combine(repositoryRoot, "bin"), recursive: true);
            Directory.Delete(Path.Combine(repositoryRoot, "obj"), recursive: true);
            Directory.Delete(Path.Combine(repositoryRoot, "artifacts"), recursive: true);

            foreach (var corpus in new[] { files, LoadRepositoryElmFiles(repositoryRoot) })
            {
                corpus.Select(file => file.path).Should().Equal(committedPaths);
                corpus.Select(file => Encoding.UTF8.GetString(file.contents.Span))
                    .Should().Equal("committed source", "committed source", "committed source");
            }
        }
        finally
        {
            Filesystem.DeleteLocalDirectoryRecursive(repositoryRoot);
        }
    }

    // Read committed blobs, not temporary projects that parallel tests create and delete.
    private static (string path, ReadOnlyMemory<byte> contents)[] LoadRepositoryElmFiles(string repositoryRoot) =>
        GitCore.LoadFromLocalFiles.LoadSubdirectoryContentsFromHead(repositoryRoot, [])
        .Select(file => (path: string.Join("/", file.Key), contents: file.Value))
        .Where(file => file.path.EndsWith(".elm", StringComparison.Ordinal))
        .OrderBy(file => file.path, StringComparer.Ordinal)
        .ToArray();

    private static byte[] TreeEntry(string mode, string name, string objectId) =>
        [.. Encoding.UTF8.GetBytes(mode + " " + name + "\0"), .. Convert.FromHexString(objectId)];

    private static string WriteGitObject(string repositoryRoot, string type, byte[] contents)
    {
        byte[] bytes = [.. Encoding.ASCII.GetBytes(type + " " + contents.Length + "\0"), .. contents];
        var objectId = Convert.ToHexStringLower(SHA1.HashData(bytes));
        var path = Path.Combine(repositoryRoot, ".git", "objects", objectId[..2], objectId[2..]);
        Directory.CreateDirectory(Path.GetDirectoryName(path)!);

        using var file = System.IO.File.Create(path);
        using var compressed = new ZLibStream(file, CompressionLevel.SmallestSize);
        compressed.Write(bytes);

        return objectId;
    }

    private static string DescribeFirstDifference(
        Core.Elm.Syntax.Abstract.File first,
        Core.Elm.Syntax.Abstract.File second)
    {
        var firstJson = ElmSyntaxAbstractJson.FileToJsonString(first);
        var secondJson = ElmSyntaxAbstractJson.FileToJsonString(second);
        var commonLength = 0;

        while (commonLength < firstJson.Length &&
            commonLength < secondJson.Length &&
            firstJson[commonLength] == secondJson[commonLength])
        {
            commonLength++;
        }

        const int ContextLength = 120;
        var contextStart = Math.Max(0, commonLength - ContextLength);

        return
            "Abstract syntax differs at JSON offset " + commonLength +
            ". First: " +
            firstJson.Substring(contextStart, Math.Min(ContextLength * 2, firstJson.Length - contextStart)) +
            ". Second: " +
            secondJson.Substring(contextStart, Math.Min(ContextLength * 2, secondJson.Length - contextStart));
    }

    private static string FindRepositoryRoot()
    {
        foreach (var startingPath in
            new[] { Directory.GetCurrentDirectory(), AppContext.BaseDirectory }.Distinct())
        {
            for (var directory = new DirectoryInfo(startingPath);
                directory is not null;
                directory = directory.Parent)
            {
                if (System.IO.File.Exists(Path.Combine(directory.FullName, "pine.slnx")))
                {
                    return directory.FullName;
                }
            }
        }

        throw new DirectoryNotFoundException("Could not find the repository root containing pine.slnx.");
    }
}
