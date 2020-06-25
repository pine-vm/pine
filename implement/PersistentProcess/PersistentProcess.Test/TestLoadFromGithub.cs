using Microsoft.VisualStudio.TestTools.UnitTesting;
using System.Linq;
using System.Collections.Immutable;
using System.Text;

namespace Kalmit.PersistentProcess.Test
{
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
                Kalmit.LoadFromGithub.LoadFromUrl(
                    "https://github.com/elm-fullstack/elm-fullstack/tree/30c482748f531899aac2b2d4895e5f0e52258be7/implement/PersistentProcess/example-elm-apps/default-full-stack-app");

            Assert.IsNull(loadFromGithubResult.Error, "No error: " + loadFromGithubResult.Error);

            var loadedFilesNamesAndContents =
                loadFromGithubResult.Success.tree.EnumerateBlobsTransitive()
                .Select(blobPathAndContent => (
                    fileName: string.Join("/", blobPathAndContent.path.Select(name => Encoding.UTF8.GetString(name.ToArray()))),
                    fileContent: blobPathAndContent.blobContent))
                .ToImmutableList();

            var loadedFilesNamesAndHashes =
                loadedFilesNamesAndContents
                .Select(fileNameAndContent =>
                    (fileNameAndContent.fileName,
                        Kalmit.CommonConversion.StringBase16FromByteArray(
                            Kalmit.CommonConversion.HashSHA256(fileNameAndContent.fileContent.ToArray())).ToLowerInvariant()))
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
                Kalmit.LoadFromGithub.LoadFromUrl(
                    "https://github.com/elm-fullstack/elm-fullstack/blob/30c482748f531899aac2b2d4895e5f0e52258be7/");

            Assert.IsNull(loadFromGithubResult.Error, "No error: " + loadFromGithubResult.Error);

            var loadedFilesNamesAndContents =
                loadFromGithubResult.Success.tree.EnumerateBlobsTransitive()
                .Select(blobPathAndContent => (
                    fileName: string.Join("/", blobPathAndContent.path.Select(name => Encoding.UTF8.GetString(name.ToArray()))),
                    fileContent: blobPathAndContent.blobContent))
                .ToImmutableList();

            var loadedFilesNamesAndHashes =
                loadedFilesNamesAndContents
                .Select(fileNameAndContent =>
                    (fileName: fileNameAndContent.fileName,
                        fileHash: Kalmit.CommonConversion.StringBase16FromByteArray(
                            Kalmit.CommonConversion.HashSHA256(fileNameAndContent.fileContent.ToArray())).ToLowerInvariant()))
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
                Kalmit.LoadFromGithub.LoadFromUrl(
                    "https://github.com/elm-fullstack/elm-fullstack/blob/30c482748f531899aac2b2d4895e5f0e52258be7/README.md");

            Assert.IsNull(loadFromGithubResult.Error, "No error: " + loadFromGithubResult.Error);

            var blobContent = loadFromGithubResult.Success.tree.BlobContent;

            Assert.IsNotNull(blobContent, "Found blobContent.");

            Assert.AreEqual(expectedFileHash,
                Kalmit.CommonConversion.StringBase16FromByteArray(
                    Kalmit.CommonConversion.HashSHA256(blobContent.ToArray()))
                .ToLowerInvariant(),
                "Loaded blob content hash equals expected hash.");
        }

        [TestMethod]
        public void LoadFromGithub_Commits_Contents()
        {
            var loadFromGithubResult =
                Kalmit.LoadFromGithub.LoadFromUrl(
                    "https://github.com/Viir/bots/tree/6c5442434768625a4df9d0dfd2f54d61d9d1f61e/implement/applications");

            Assert.IsNull(loadFromGithubResult.Error, "No error: " + loadFromGithubResult.Error);

            Assert.AreEqual("https://github.com/Viir/bots/tree/6c5442434768625a4df9d0dfd2f54d61d9d1f61e/implement/applications", loadFromGithubResult.Success.stableUrl);

            Assert.AreEqual("6c5442434768625a4df9d0dfd2f54d61d9d1f61e", loadFromGithubResult.Success.rootCommit.hash);
            Assert.AreEqual("Support finding development guides\n", loadFromGithubResult.Success.rootCommit.content.message);
            Assert.AreEqual("Michael Rätzel", loadFromGithubResult.Success.rootCommit.content.author.name);
            Assert.AreEqual("viir@viir.de", loadFromGithubResult.Success.rootCommit.content.author.email);

            Assert.AreEqual("1f915f4583cde98e0491e66bc73d7df0e92d1aac", loadFromGithubResult.Success.firstParentCommitWithSameTree.hash);
            Assert.AreEqual("Guide users\n\nClarify the bot uses drones if available.\n", loadFromGithubResult.Success.firstParentCommitWithSameTree.content.message);
            Assert.AreEqual("John", loadFromGithubResult.Success.firstParentCommitWithSameTree.content.author.name);
            Assert.AreEqual("john-dev@botengine.email", loadFromGithubResult.Success.firstParentCommitWithSameTree.content.author.email);
        }
    }
}
