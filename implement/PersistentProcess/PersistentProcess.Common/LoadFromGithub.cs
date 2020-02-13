using LibGit2Sharp;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Linq;
using System.Security.Cryptography;
using System.Text;
using System.Text.RegularExpressions;

namespace Kalmit
{
    static public class LoadFromGithub
    {
        /// <summary>
        /// Sample address to a tree:
        /// https://github.com/elm-fullstack/elm-fullstack/tree/30c482748f531899aac2b2d4895e5f0e52258be7/guide
        /// 
        /// Sample address to a blob:
        /// https://github.com/elm-fullstack/elm-fullstack/blob/30c482748f531899aac2b2d4895e5f0e52258be7/guide/persistence-in-elm-fullstack.md
        /// 
        /// Support loading from the root tree:
        /// https://github.com/elm-fullstack/elm-fullstack/tree/30c482748f531899aac2b2d4895e5f0e52258be7/
        /// https://github.com/elm-fullstack/elm-fullstack/tree/30c482748f531899aac2b2d4895e5f0e52258be7
        /// </summary>
        static string GithubRegexPattern(
            string repositoryGroupName,
            string typeGroupName,
            string refGroupName,
            string pathGroupName) =>
            "(?<" + repositoryGroupName + ">" + Regex.Escape("https://github.com/") + "[^/]+/[^/]+)/(?<" + typeGroupName + ">blob|tree)/" +
            "(?<" + refGroupName + ">[^/]+)($|/(?<" + pathGroupName + ">.*))";

        public class ParseObjectUrlResult
        {
            public string repository;

            public GitObjectType objectType;

            public string @ref;

            public string path;

            public enum GitObjectType
            {
                blob = 1,
                tree = 2,
            }
        }

        static public ParseObjectUrlResult ParseGitHubObjectUrl(string objectUrl)
        {
            const string repositoryGroupName = "repo";
            const string typeGroupName = "type";
            const string refGroupName = "ref";
            const string pathGroupName = "path";

            var regexMatch = Regex.Match(
                objectUrl,
                GithubRegexPattern(
                    repositoryGroupName: repositoryGroupName,
                    typeGroupName: typeGroupName,
                    refGroupName: refGroupName,
                    pathGroupName: pathGroupName));

            if (!regexMatch.Success)
                return null;

            return new ParseObjectUrlResult
            {
                repository = regexMatch.Groups[repositoryGroupName].Value,
                objectType = Enum.Parse<ParseObjectUrlResult.GitObjectType>(regexMatch.Groups[typeGroupName].Value),
                @ref = regexMatch.Groups[refGroupName].Value,
                path = regexMatch.Groups[pathGroupName].Value,
            };
        }

        static public LoadFromUrlResult LoadFromUrl(string sourceUrl)
        {
            var parsedUrl = ParseGitHubObjectUrl(sourceUrl);

            if (parsedUrl == null)
                return new LoadFromUrlResult
                {
                    Error = "Failed to parse string '" + sourceUrl + "' as GitHub object URL.",
                };

            var refLooksLikeCommit = Regex.IsMatch(parsedUrl.@ref, "[A-Fa-f0-9]{40}");

            var tempWorkingDirectory = Kalmit.Filesystem.CreateRandomDirectoryInTempDirectory();

            var gitRepositoryLocalDirectory = Path.Combine(tempWorkingDirectory, "git-repository");

            //  https://github.com/libgit2/libgit2sharp/wiki/git-clone
            Repository.Clone(parsedUrl.repository, gitRepositoryLocalDirectory, new CloneOptions { Checkout = false });

            Composition.TreeComponent literalNodeObject = null;

            using (var gitRepository = new Repository(gitRepositoryLocalDirectory))
            {
                var commit = gitRepository.Lookup(parsedUrl.@ref) as Commit;

                if (commit == null)
                    return new LoadFromUrlResult
                    {
                        Error = "I did not find the commit for ref '" + parsedUrl.@ref + "'.",
                    };

                var pathNodesNames = parsedUrl.path.Split('/', StringSplitOptions.RemoveEmptyEntries);

                var findGitObjectResult =
                    FindGitObjectAtPath(commit.Tree, pathNodesNames);

                var linkedObject = findGitObjectResult?.Success;

                if (linkedObject == null)
                    return new LoadFromUrlResult
                    {
                        Error = "I did not find an object at path '" + parsedUrl.path + "'.",
                    };

                static Composition.TreeComponent convertToLiteralNodeObjectRecursive(GitObject gitObject)
                {
                    if (gitObject is Tree gitTree)
                    {
                        return new Composition.TreeComponent
                        {
                            TreeContent =
                                gitTree.Select(treeEntry =>
                                    ((IImmutableList<byte>)Encoding.BigEndianUnicode.GetBytes(treeEntry.Name).ToImmutableList(),
                                    convertToLiteralNodeObjectRecursive(treeEntry.Target)))
                                .ToImmutableList(),
                        };
                    }

                    if (gitObject is Blob gitBlob)
                    {
                        var memoryStream = new MemoryStream();

                        gitBlob.GetContentStream().CopyTo(memoryStream);

                        var blobContent = memoryStream.ToArray();

                        var expectedSHA = gitBlob.Sha.ToLowerInvariant();

                        //  This will change with the introduction of the new hash in git.
                        //  We could branch on the length of 'gitBlob.Sha' to choose between old and new hash.
                        //  (https://github.com/git/git/blob/74583d89127e21255c12dd3c8a3bf60b497d7d03/Documentation/technical/hash-function-transition.txt)
                        //  (https://www.youtube.com/watch?v=qHERDFUSa14)
                        var loadedBlobSHA1Base16Lower =
                            BitConverter.ToString(GitBlobSHAFromBlobContent(blobContent)).Replace("-", "")
                            .ToLowerInvariant();

                        if (loadedBlobSHA1Base16Lower != expectedSHA)
                            throw new Exception("Unexpected content for git object : SHA is " + loadedBlobSHA1Base16Lower + " instead of " + expectedSHA);

                        return new Composition.TreeComponent
                        {
                            BlobContent = memoryStream.ToArray().ToImmutableList(),
                        };
                    }

                    throw new Exception("Unexpected kind of git object: " + gitObject.GetType() + ", " + gitObject.Id);
                }

                try
                {
                    literalNodeObject = convertToLiteralNodeObjectRecursive(linkedObject);
                }
                catch (Exception e)
                {
                    return new LoadFromUrlResult
                    {
                        Error = "Failed to convert from git object:\n" + e.ToString(),
                    };
                }
            }

            DeleteLocalDirectoryRecursive(tempWorkingDirectory);

            return new LoadFromUrlResult { Success = literalNodeObject };
        }

        class FollowGitPathResult
        {
            public GitObject Success;

            public object Error;
        }

        static FollowGitPathResult FindGitObjectAtPath(Tree root, IEnumerable<string> nodesNames)
        {
            if (root == null)
                return null;

            GitObject currentObject = root;

            foreach (var nodeName in nodesNames)
            {
                var currentTree = currentObject as Tree;

                if (currentTree == null)
                    return new FollowGitPathResult
                    {
                        Error = new object(),
                    };

                var treeEntry = currentTree?[nodeName];

                currentObject =
                    treeEntry?.Mode == Mode.Nonexistent || !(treeEntry?.TargetType == TreeEntryTargetType.Blob || treeEntry?.TargetType == TreeEntryTargetType.Tree)
                    ?
                    null
                    :
                    treeEntry.Target;
            }

            return new FollowGitPathResult
            {
                Success = currentObject,
            };
        }

        static byte[] GitBlobSHAFromBlobContent(byte[] blobContent)
        {
            var prefixAsText = "blob " + blobContent.Length.ToString() + "\0";

            var hashedValue = Encoding.ASCII.GetBytes(prefixAsText).Concat(blobContent).ToArray();

            using var hasher = new SHA1Managed();
            return hasher.ComputeHash(hashedValue);
        }

        public class LoadFromUrlResult
        {
            public object Error;

            public Composition.TreeComponent Success;

            public IImmutableList<byte> AsBlob => Success?.BlobContent;
        }

        /// <summary>
        /// https://github.com/libgit2/libgit2sharp/issues/769#issuecomment-198833179
        /// </summary>
        static void DeleteLocalDirectoryRecursive(string directoryPath)
        {
            if (!Directory.Exists(directoryPath))
            {
                return;
            }

            var files = Directory.GetFiles(directoryPath);
            var directories = Directory.GetDirectories(directoryPath);

            foreach (var file in files)
            {
                File.SetAttributes(file, FileAttributes.Normal);
                File.Delete(file);
            }

            foreach (var dir in directories)
            {
                DeleteLocalDirectoryRecursive(dir);
            }

            File.SetAttributes(directoryPath, FileAttributes.Normal);

            Directory.Delete(directoryPath, false);
        }
    }
}
