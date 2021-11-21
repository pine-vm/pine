using System.Collections.Immutable;
using System.Linq;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace test_elm_fullstack;

[TestClass]
public class TestLoadFromGithub
{
    [TestMethod]
    public void Test_LoadFromGithub_Tree()
    {
        var expectedFilesNamesAndHashes = new[]
        {
            ("elm-fullstack.json", "64c2c48a13c28a92366e6db67a6204084919d906ff109644f4237b22b87e952e"),

            ("elm-app/elm.json", "f6d1d18ccceb520cf43f27e5bc30060553c580e44151dbb0a32e3ded0763b209"),

            ("elm-app/src/Backend/Main.elm", "61ff36d96ea01dd1572c2f35c1c085dd23f1225fbebfbd4b3c71a69f3daa204a"),
            ("elm-app/src/Backend/InterfaceToHost.elm", "7c263cc27f29148a0ca2db1cdef5f7a17a5c0357839dec02f04c45cf8a491116"),

            ("elm-app/src/FrontendWeb/Main.elm", "6e82dcde8a9dc45ef65b27724903770d1bed74da458571811840687b4c790705"),
        };

        var loadFromGithubResult =
            Pine.LoadFromGitHubOrGitLab.LoadFromUrl(
                "https://github.com/elm-fullstack/elm-fullstack/tree/30c482748f531899aac2b2d4895e5f0e52258be7/implement/PersistentProcess/example-elm-apps/default-full-stack-app");

        Assert.IsNull(loadFromGithubResult.Err, "No error: " + loadFromGithubResult.Err);

        var loadedFilesNamesAndContents =
            loadFromGithubResult.Ok.tree.EnumerateBlobsTransitive()
            .Select(blobPathAndContent => (
                fileName: string.Join("/", blobPathAndContent.path),
                fileContent: blobPathAndContent.blobContent))
            .ToImmutableList();

        var loadedFilesNamesAndHashes =
            loadedFilesNamesAndContents
            .Select(fileNameAndContent =>
                (fileNameAndContent.fileName,
                    Pine.CommonConversion.StringBase16FromByteArray(
                        Pine.CommonConversion.HashSHA256(fileNameAndContent.fileContent.ToArray())).ToLowerInvariant()))
            .ToImmutableList();

        CollectionAssert.AreEquivalent(
            expectedFilesNamesAndHashes,
            loadedFilesNamesAndHashes,
            "Loaded files equal expected files.");
    }

    [TestMethod]
    public void Test_LoadFromGithub_Tree_at_root()
    {
        var expectedFilesNamesAndHashes = new[]
        {
            (fileName: "README.md", fileHash: "e80817b2aa00350dff8f00207083b3b21b0726166dd695475be512ce86507238"),
        };

        var loadFromGithubResult =
            Pine.LoadFromGitHubOrGitLab.LoadFromUrl(
                "https://github.com/elm-fullstack/elm-fullstack/blob/30c482748f531899aac2b2d4895e5f0e52258be7/");

        Assert.IsNull(loadFromGithubResult.Err, "No error: " + loadFromGithubResult.Err);

        var loadedFilesNamesAndContents =
            loadFromGithubResult.Ok.tree.EnumerateBlobsTransitive()
            .Select(blobPathAndContent => (
                fileName: string.Join("/", blobPathAndContent.path),
                fileContent: blobPathAndContent.blobContent))
            .ToImmutableList();

        var loadedFilesNamesAndHashes =
            loadedFilesNamesAndContents
            .Select(fileNameAndContent =>
                (fileNameAndContent.fileName,
                    fileHash: Pine.CommonConversion.StringBase16FromByteArray(
                        Pine.CommonConversion.HashSHA256(fileNameAndContent.fileContent.ToArray())).ToLowerInvariant()))
            .ToImmutableList();

        foreach (var expectedFileNameAndHash in expectedFilesNamesAndHashes)
        {
            Assert.IsTrue(
                loadedFilesNamesAndHashes.Contains(expectedFileNameAndHash),
                "Collection of loaded files contains a file named '" + expectedFileNameAndHash.fileName +
                "' with hash " + expectedFileNameAndHash.fileHash + ".");
        }
    }

    [TestMethod]
    public void Test_LoadFromGithub_Object()
    {
        var expectedFileHash = "e80817b2aa00350dff8f00207083b3b21b0726166dd695475be512ce86507238";

        var loadFromGithubResult =
            Pine.LoadFromGitHubOrGitLab.LoadFromUrl(
                "https://github.com/elm-fullstack/elm-fullstack/blob/30c482748f531899aac2b2d4895e5f0e52258be7/README.md");

        Assert.IsNull(loadFromGithubResult.Err, "No error: " + loadFromGithubResult.Err);

        var blobContent = loadFromGithubResult.Ok.tree.BlobContent;

        Assert.IsNotNull(blobContent, "Found blobContent.");

        Assert.AreEqual(expectedFileHash,
            Pine.CommonConversion.StringBase16FromByteArray(
                Pine.CommonConversion.HashSHA256(blobContent.ToArray()))
            .ToLowerInvariant(),
            "Loaded blob content hash equals expected hash.");
    }

    [TestMethod]
    public void LoadFromGithub_Commits_Contents_And_Lineage()
    {
        var loadFromGithubResult =
            Pine.LoadFromGitHubOrGitLab.LoadFromUrl(
                "https://github.com/Viir/bots/tree/6c5442434768625a4df9d0dfd2f54d61d9d1f61e/implement/applications");

        Assert.IsNull(loadFromGithubResult.Err, "No error: " + loadFromGithubResult.Err);

        Assert.AreEqual(
            "https://github.com/Viir/bots/tree/6c5442434768625a4df9d0dfd2f54d61d9d1f61e/implement/applications",
            loadFromGithubResult.Ok.urlInCommit);

        Assert.AreEqual(
            "https://github.com/Viir/bots/tree/1f915f4583cde98e0491e66bc73d7df0e92d1aac/implement/applications",
            loadFromGithubResult.Ok.urlInFirstParentCommitWithSameValueAtThisPath);

        Assert.AreEqual("6c5442434768625a4df9d0dfd2f54d61d9d1f61e", loadFromGithubResult.Ok.rootCommit.hash);
        Assert.AreEqual("Support finding development guides\n", loadFromGithubResult.Ok.rootCommit.content.message);
        Assert.AreEqual("Michael Rätzel", loadFromGithubResult.Ok.rootCommit.content.author.name);
        Assert.AreEqual("viir@viir.de", loadFromGithubResult.Ok.rootCommit.content.author.email);

        Assert.AreEqual("1f915f4583cde98e0491e66bc73d7df0e92d1aac", loadFromGithubResult.Ok.firstParentCommitWithSameTree.hash);
        Assert.AreEqual("Guide users\n\nClarify the bot uses drones if available.\n", loadFromGithubResult.Ok.firstParentCommitWithSameTree.content.message);
        Assert.AreEqual("John", loadFromGithubResult.Ok.firstParentCommitWithSameTree.content.author.name);
        Assert.AreEqual("john-dev@botengine.email", loadFromGithubResult.Ok.firstParentCommitWithSameTree.content.author.email);
    }


    [TestMethod]
    public void LoadFromGithub_URL_points_only_to_repository()
    {
        var loadFromGithubResult =
            Pine.LoadFromGitHubOrGitLab.LoadFromUrl(
                "https://github.com/elm-fullstack/elm-fullstack");

        Assert.IsNull(loadFromGithubResult.Err, "No error: " + loadFromGithubResult.Err);

        var loadedFilesPathsAndContents =
            loadFromGithubResult.Ok.tree.EnumerateBlobsTransitive()
            .Select(blobPathAndContent => (
                filePath: string.Join("/", blobPathAndContent.path),
                fileContent: blobPathAndContent.blobContent))
            .ToImmutableList();

        var readmeFile =
            loadedFilesPathsAndContents
            .FirstOrDefault(c => c.filePath.Equals("readme.md", System.StringComparison.InvariantCultureIgnoreCase));

        Assert.IsNotNull(readmeFile.fileContent, "Loaded files contain readme.md");
    }
}
