using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Linq;
using System.Security.Cryptography;
using System.Text;
using System.Text.RegularExpressions;
using LibGit2Sharp;

namespace Pine
{
    static public class LoadFromGitHubOrGitLab
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
        ///
        /// Blob on GitLab:
        /// https://gitlab.com/gilmi/strema/-/blob/de9f6a401f89215cb6cebbbbf2eed0252aeef1d1/overview.org
        ///
        /// Tree on GitLab:
        /// https://gitlab.com/gilmi/strema/-/tree/de9f6a401f89215cb6cebbbbf2eed0252aeef1d1/src/Strema
        /// </summary>
        static string GitHubOrGitLabRegexPattern(
            string repositoryGroupName,
            string typeGroupName,
            string refGroupName,
            string pathGroupName) =>
            "(?<" + repositoryGroupName + ">" +
            "https://(github\\.com|gitlab\\.com)/" +
            "[^/]+/[^/]+)/(-/|)(?<" + typeGroupName + ">blob|tree)/" +
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

            public ParseObjectUrlResult WithRef(string @ref) => new ParseObjectUrlResult
            {
                repository = repository,
                objectType = objectType,
                @ref = @ref,
                path = path
            };
        }

        static public ParseObjectUrlResult ParsePathFromUrl(string objectUrl)
        {
            const string repositoryGroupName = "repo";
            const string typeGroupName = "type";
            const string refGroupName = "ref";
            const string pathGroupName = "path";

            var regexMatch = Regex.Match(
                objectUrl,
                GitHubOrGitLabRegexPattern(
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

        static public string BackToUrl(ParseObjectUrlResult parseObjectUrlResult) =>
            parseObjectUrlResult.repository + "/" +
            parseObjectUrlResult.objectType + "/" +
            parseObjectUrlResult.@ref + "/" +
            parseObjectUrlResult.path;

        static public LoadFromUrlResult LoadFromUrl(string sourceUrl)
        {
            var parsedUrl = ParsePathFromUrl(sourceUrl);

            if (parsedUrl == null)
                return new LoadFromUrlResult
                {
                    Error = "Failed to parse string '" + sourceUrl + "' as GitHub or GitLab object URL.",
                };

            var refLooksLikeCommit = Regex.IsMatch(parsedUrl.@ref, "[A-Fa-f0-9]{40}");

            var tempWorkingDirectory = Filesystem.CreateRandomDirectoryInTempDirectory();

            var gitRepositoryLocalDirectory = Path.Combine(tempWorkingDirectory, "git-repository");

            var branchName = refLooksLikeCommit ? null : parsedUrl.@ref;

            var cloneUrl = parsedUrl.repository.TrimEnd('/') + ".git";

            try
            {
                //  https://github.com/libgit2/libgit2sharp/wiki/git-clone
                Repository.Clone(
                    cloneUrl, gitRepositoryLocalDirectory,
                    new CloneOptions { Checkout = false, BranchName = branchName });
            }
            catch (Exception e)
            {
                throw new Exception("Failed to clone from '" + cloneUrl + "'", e);
            }

            Composition.TreeWithStringPath literalNodeObject = null;
            string urlInCommit = null;
            string urlInFirstParentCommitWithSameValueAtThisPath = null;
            (string hash, CommitContent content)? rootCommit = null;
            (string hash, CommitContent content)? firstParentCommitWithSameTree = null;

            using (var gitRepository = new Repository(gitRepositoryLocalDirectory))
            {
                var commit = gitRepository.Lookup(parsedUrl.@ref) as Commit;

                if (commit == null)
                    return new LoadFromUrlResult
                    {
                        Error = "I did not find the commit for ref '" + parsedUrl.@ref + "'.",
                    };

                urlInCommit = BackToUrl(parsedUrl.WithRef(commit.Sha));

                rootCommit = GetCommitHashAndContent(commit);

                var pathNodesNames = parsedUrl.path.Split('/', StringSplitOptions.RemoveEmptyEntries);

                var findGitObjectResult =
                    FindGitObjectAtPath(commit.Tree, pathNodesNames);

                var linkedObject = findGitObjectResult?.Success;

                if (linkedObject == null)
                    return new LoadFromUrlResult
                    {
                        Error = "I did not find an object at path '" + parsedUrl.path + "' in " + commit.Sha,
                    };

                IEnumerable<Commit> traceBackTreeParents()
                {
                    var queue = new Queue<Commit>();

                    queue.Enqueue(commit);

                    while (queue.TryDequeue(out var currentCommit))
                    {
                        yield return currentCommit;

                        foreach (var parent in currentCommit.Parents)
                        {
                            if (FindGitObjectAtPath(parent.Tree, pathNodesNames)?.Success?.Sha != linkedObject?.Sha)
                                continue;

                            queue.Enqueue(parent);
                        }
                    }
                }

                firstParentCommitWithSameTree =
                    GetCommitHashAndContent(traceBackTreeParents().OrderBy(commit => commit.Author.When).First());

                urlInFirstParentCommitWithSameValueAtThisPath =
                    BackToUrl(parsedUrl.WithRef(firstParentCommitWithSameTree.Value.hash));

                static Composition.TreeWithStringPath convertToLiteralNodeObjectRecursive(GitObject gitObject)
                {
                    if (gitObject is Tree gitTree)
                    {
                        return Composition.TreeWithStringPath.Tree(
                            treeContent:
                                gitTree.Select(treeEntry =>
                                    (treeEntry.Name,
                                    convertToLiteralNodeObjectRecursive(treeEntry.Target)))
                                .ToImmutableList());
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

                        return Composition.TreeWithStringPath.Blob(blobContent: memoryStream.ToArray());
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

            try
            {
                DeleteLocalDirectoryRecursive(tempWorkingDirectory);
            }
            catch
            {
                /*
                Adapt to observations 2020-02-15:
                A user got a `System.IO.DirectoryNotFoundException` out of `DeleteLocalDirectoryRecursive`.
                Also, it seems common other software interferring with contents of `Path.GetTempPath()` (https://github.com/dotnet/runtime/issues/3778).
                */
            }

            return new LoadFromUrlResult
            {
                Success = new LoadFromUrlSuccess
                {
                    tree = literalNodeObject,
                    urlInCommit = urlInCommit,
                    urlInFirstParentCommitWithSameValueAtThisPath = urlInFirstParentCommitWithSameValueAtThisPath,
                    rootCommit = rootCommit.Value,
                    firstParentCommitWithSameTree = firstParentCommitWithSameTree.Value,
                }
            };
        }

        static public (string hash, CommitContent content) GetCommitHashAndContent(Commit commit)
        {
            return (commit.Sha, new CommitContent
            {
                message = commit.Message,
                author = GetParticipantSignature(commit.Author),
                committer = GetParticipantSignature(commit.Committer),
            });
        }

        static public GitParticipantSignature GetParticipantSignature(Signature signature)
        {
            return new GitParticipantSignature
            {
                name = signature.Name,
                email = signature.Email,
            };
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

            public LoadFromUrlSuccess Success;
        }

        public class LoadFromUrlSuccess
        {
            public Composition.TreeWithStringPath tree;

            public string urlInCommit;

            public string urlInFirstParentCommitWithSameValueAtThisPath;

            public (string hash, CommitContent content) rootCommit;

            public (string hash, CommitContent content) firstParentCommitWithSameTree;

            public IReadOnlyList<byte> AsBlob => tree?.BlobContent;
        }

        public class CommitContent
        {
            public string message;

            public GitParticipantSignature author;

            public GitParticipantSignature committer;
        }

        public class GitParticipantSignature
        {
            public string name;

            public string email;
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
