using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Linq;
using System.Security.Cryptography;
using System.Text;
using System.Text.RegularExpressions;
using LibGit2Sharp;

namespace Pine;

static public class LoadFromGitHubOrGitLab
{
    static public CacheByFileName? RepositoryFilesPartialForCommitCacheDefault = null;

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
        "[^/]+/[^/]+)(/(?<" + typeGroupName + ">blob|tree)/" +
        "(?<" + refGroupName + ">[^/]+)($|/(?<" + pathGroupName + ">.*))|)";

    public record ParsedUrl(
        string repository,
        ParsedUrlInRepository? inRepository);

    public record ParsedUrlInRepository(
        GitObjectType objectType,
        string @ref,
        string path);

    public enum GitObjectType
    {
        blob = 1,
        tree = 2,
    }

    public record GetRepositoryFilesPartialForCommitRequest(
        string commit,
        IReadOnlyList<string> cloneUrlCandidates);

    static public ParsedUrl? ParseUrl(string objectUrl)
    {
        const string repositoryGroupName = "repo";
        const string typeGroupName = "type";
        const string refGroupName = "ref";
        const string pathGroupName = "path";

        var regexMatch = Regex.Match(
            objectUrl,
            "^" +
            GitHubOrGitLabRegexPattern(
                repositoryGroupName: repositoryGroupName,
                typeGroupName: typeGroupName,
                refGroupName: refGroupName,
                pathGroupName: pathGroupName) +
                "$");

        if (!regexMatch.Success)
            return null;

        var inRepository =
            regexMatch.Groups[typeGroupName].Success ?
            new ParsedUrlInRepository(
                objectType: Enum.Parse<GitObjectType>(regexMatch.Groups[typeGroupName].Value),
                @ref: regexMatch.Groups[refGroupName].Value,
                path: regexMatch.Groups[pathGroupName].Value)
            :
            null;

        return new ParsedUrl
        (
            repository: regexMatch.Groups[repositoryGroupName].Value,
            inRepository: inRepository
        );
    }

    static public string BackToUrl(ParsedUrl parsedUrl) =>
        parsedUrl.repository +
        (parsedUrl.inRepository == null
        ? "" :
        "/" + parsedUrl.inRepository.objectType + "/" + parsedUrl.inRepository.@ref + "/" + parsedUrl.inRepository.path);

    static public Result<string, LoadFromUrlSuccess> LoadFromUrl(string sourceUrl) =>
        LoadFromUrl(
            sourceUrl: sourceUrl,
            getRepositoryFilesPartialForCommit: GetRepositoryFilesPartialForCommitDefault);

    static public Result<string, LoadFromUrlSuccess> LoadFromUrl(
        string sourceUrl,
        Func<GetRepositoryFilesPartialForCommitRequest, IImmutableDictionary<IImmutableList<string>, ReadOnlyMemory<byte>>> getRepositoryFilesPartialForCommit)
    {
        var parsedUrl = ParseUrl(sourceUrl);

        if (parsedUrl == null)
            return Result<string, LoadFromUrlSuccess>.err(
                "Failed to parse string '" + sourceUrl + "' as GitHub or GitLab URL.");

        string? branchName = null;
        bool refLooksLikeCommit = false;

        if (parsedUrl.inRepository != null)
        {
            refLooksLikeCommit = Regex.IsMatch(parsedUrl.inRepository.@ref, "[A-Fa-f0-9]{40}");

            branchName = refLooksLikeCommit ? null : parsedUrl.inRepository.@ref;
        }

        var cloneUrl = parsedUrl.repository.TrimEnd('/') + ".git";

        if (getRepositoryFilesPartialForCommit == null)
            getRepositoryFilesPartialForCommit =
                req => GetRepositoryFilesPartialForCommitViaLibGitSharpCheckout(
                    cloneUrl: req.cloneUrlCandidates[0],
                    commit: req.commit);

        var repositoryFilesPartial =
            refLooksLikeCommit ?
            getRepositoryFilesPartialForCommit(
                new GetRepositoryFilesPartialForCommitRequest(
                    commit: parsedUrl.inRepository!.@ref,
                    cloneUrlCandidates: ImmutableList.Create(cloneUrl)))
            :
            GetRepositoryFilesPartialForBranchViaLibGitSharpCheckout(cloneUrl, branchName);

        var tempWorkingDirectory = Filesystem.CreateRandomDirectoryInTempDirectory();
        var gitRepositoryLocalDirectory = Path.Combine(tempWorkingDirectory, "git-repository");

        try
        {
            foreach (var fileWithPath in repositoryFilesPartial)
            {
                var absoluteFilePath = Path.Combine(new[] { gitRepositoryLocalDirectory }.Concat(fileWithPath.Key).ToArray());
                var absoluteDirectoryPath = Path.GetDirectoryName(absoluteFilePath)!;

                Directory.CreateDirectory(absoluteDirectoryPath);
                File.WriteAllBytes(absoluteFilePath, fileWithPath.Value.ToArray());
            }

            (string hash, CommitContent content)? rootCommit = null;

            using var gitRepository = new Repository(gitRepositoryLocalDirectory);

            Commit? startCommit = null;

            if (parsedUrl.inRepository == null)
            {
                startCommit = gitRepository.Head.Commits.FirstOrDefault();

                if (startCommit == null)
                    return Result<string, LoadFromUrlSuccess>.err(
                        "Failed to get the first commit from HEAD");
            }
            else
            {
                startCommit = gitRepository.Lookup(parsedUrl.inRepository.@ref) as Commit;

                if (startCommit == null)
                    return Result<string, LoadFromUrlSuccess>.err(
                        "I did not find the commit for ref '" + parsedUrl.inRepository.@ref + "'.");
            }

            ParsedUrlInRepository partInRepositoryWithCommit(Commit replacementCommit) =>
                parsedUrl.inRepository == null ?
                    new ParsedUrlInRepository(GitObjectType.tree, @ref: replacementCommit.Sha, path: "") :
                    parsedUrl.inRepository with { @ref = replacementCommit.Sha };

            var urlInCommit = BackToUrl(parsedUrl with { inRepository = partInRepositoryWithCommit(startCommit) });

            rootCommit = GetCommitHashAndContent(startCommit);

            var parsedUrlPath =
                parsedUrl.inRepository == null ? "" : parsedUrl.inRepository.path;

            var pathNodesNames = parsedUrlPath.Split('/', StringSplitOptions.RemoveEmptyEntries);

            var findGitObjectResult =
                FindGitObjectAtPath(startCommit.Tree, pathNodesNames);

            return
                findGitObjectResult
                .mapError(_ => "I did not find an object at path '" + parsedUrlPath + "' in " + startCommit.Sha)
                .andThen(linkedObject =>
                {
                    IEnumerable<Commit> traceBackTreeParents()
                    {
                        var queue = new Queue<Commit>();

                        queue.Enqueue(startCommit);

                        while (queue.TryDequeue(out var currentCommit))
                        {
                            yield return currentCommit;

                            foreach (var parent in currentCommit.Parents)
                            {
                                if (FindGitObjectAtPath(parent.Tree, pathNodesNames).map(find => find.Sha).withDefault(() => null) != linkedObject?.Sha)
                                    continue;

                                queue.Enqueue(parent);
                            }
                        }
                    }

                    var firstParentCommitWithSameTreeRef =
                        traceBackTreeParents().OrderBy(commit => commit.Author.When).First();

                    var firstParentCommitWithSameTree =
                        GetCommitHashAndContent(firstParentCommitWithSameTreeRef);

                    var urlInFirstParentCommitWithSameValueAtThisPath =
                        BackToUrl(parsedUrl with { inRepository = partInRepositoryWithCommit(firstParentCommitWithSameTreeRef) });

                    static TreeNodeWithStringPath convertToLiteralNodeObjectRecursive(GitObject gitObject)
                    {
                        if (gitObject is Tree gitTree)
                        {
                            return TreeNodeWithStringPath.SortedTree(
                                treeContent:
                                    gitTree.Select(treeEntry =>
                                        (treeEntry.Name,
                                        convertToLiteralNodeObjectRecursive(treeEntry.Target)))
                                    .ToImmutableList());
                        }

                        if (gitObject is Blob gitBlob)
                        {
                            var memoryStream = new MemoryStream();

                            var gitBlobContentStream = gitBlob.GetContentStream();

                            if (gitBlobContentStream == null)
                                throw new Exception("Failed to get content of git blob");

                            gitBlobContentStream.CopyTo(memoryStream);

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

                            return TreeNodeWithStringPath.Blob(memoryStream.ToArray());
                        }

                        throw new Exception("Unexpected kind of git object: " + gitObject.GetType() + ", " + gitObject.Id);
                    }

                    try
                    {
                        var literalNodeObject = convertToLiteralNodeObjectRecursive(linkedObject);

                        return Result<string, LoadFromUrlSuccess>.ok(
                            new LoadFromUrlSuccess
                            (
                                tree: literalNodeObject,
                                urlInCommit: urlInCommit,
                                urlInFirstParentCommitWithSameValueAtThisPath: urlInFirstParentCommitWithSameValueAtThisPath,
                                rootCommit: rootCommit.Value,
                                firstParentCommitWithSameTree: firstParentCommitWithSameTree
                            )
                        );
                    }
                    catch (Exception e)
                    {
                        return Result<string, LoadFromUrlSuccess>.err("Failed to convert from git object:\n" + e.ToString());
                    }
                });
        }
        finally
        {
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
        }
    }

    static IImmutableDictionary<IImmutableList<string>, ReadOnlyMemory<byte>> GetRepositoryFilesPartialForCommitDefault(GetRepositoryFilesPartialForCommitRequest request)
    {
        var getNew = () =>
        {
            foreach (var cloneUrlCandidate in request.cloneUrlCandidates)
            {
                try
                {
                    return GetRepositoryFilesPartialForCommitViaLibGitSharpCheckout(
                        cloneUrl: cloneUrlCandidate,
                        commit: request.commit);
                }
                catch { }
            }

            return null;
        };

        var cache = RepositoryFilesPartialForCommitCacheDefault;

        if (cache != null)
        {
            return
                Composition.ToFlatDictionaryWithPathComparer(
                    Composition.SortedTreeFromSetOfBlobsWithCommonFilePath(
                        ZipArchive.EntriesFromZipArchive(
                            cache.GetOrUpdate(request.commit, () => ZipArchive.ZipArchiveFromEntries(getNew()))))
                    .EnumerateBlobsTransitive());
        }

        return getNew();
    }

    static public IImmutableDictionary<IImmutableList<string>, ReadOnlyMemory<byte>> GetRepositoryFilesPartialForCommitViaLibGitSharpCheckout(
        string cloneUrl,
        string commit)
    {
        var tempWorkingDirectory = Filesystem.CreateRandomDirectoryInTempDirectory();

        try
        {
            //  https://github.com/libgit2/libgit2sharp/wiki/git-clone
            Repository.Clone(
                cloneUrl, tempWorkingDirectory,
                new CloneOptions { Checkout = false, BranchName = null });

            // TODO: Test reduce size of returned files using partial clone?

            using var gitRepository = new Repository(tempWorkingDirectory);

            gitRepository.CheckoutPaths(commit, ImmutableList.Create(tempWorkingDirectory.TrimEnd('/') + "/"));

            return
                Composition.ToFlatDictionaryWithPathComparer(
                    Filesystem.GetAllFilesFromDirectory(tempWorkingDirectory)
                    .Where(predicate: c => c.path?[0] == ".git"));
        }
        catch (Exception e)
        {
            throw new Exception("Failed to clone from '" + cloneUrl + "'", e);
        }
        finally
        {
            DeleteLocalDirectoryRecursive(tempWorkingDirectory);
        }
    }

    static public IImmutableDictionary<IImmutableList<string>, ReadOnlyMemory<byte>> GetRepositoryFilesPartialForCommitViaEnvironmentGitCheckout(
        string cloneUrl,
        string commit)
    {
        /*
         * We could further reduce the size of return values by supporting checkout of only a subdirectory.
         * But, by default, the pack files we get from servers might then contain excess blob contents.
         * */

        var tempWorkingDirectory = Filesystem.CreateRandomDirectoryInTempDirectory();

        try
        {
            {
                var startInfo = new System.Diagnostics.ProcessStartInfo("git", "clone  --filter=blob:none  " + cloneUrl + "  .")
                {
                    WorkingDirectory = tempWorkingDirectory,
                };

                var process = System.Diagnostics.Process.Start(startInfo: startInfo)!;

                process.WaitForExit();

                if (process.ExitCode != 0)
                    throw new Exception("Failed git clone with exit code " + process.ExitCode);
            }

            /*
             * 2021-12-05
             * Avoid runtime exception in LibGit2Sharp:
             * LibGit2Sharp.NotFoundException: 'object not found - no match for id (4a4187d9b86dde380542195187c91f391a518890)'
             *

            using var gitRepository = new Repository(tempWorkingDirectory);

            gitRepository.CheckoutPaths(commit, ImmutableList.Create(tempWorkingDirectory.TrimEnd('/') + "/"));
            */

            {
                var startInfo = new System.Diagnostics.ProcessStartInfo("git", "checkout " + commit)
                {
                    WorkingDirectory = tempWorkingDirectory,
                };

                var process = System.Diagnostics.Process.Start(startInfo: startInfo)!;

                process.WaitForExit();

                if (process.ExitCode != 0)
                    throw new Exception("Failed git checkout with exit code " + process.ExitCode);
            }

            return
                Composition.ToFlatDictionaryWithPathComparer(
                    Filesystem.GetAllFilesFromDirectory(tempWorkingDirectory)
                    .Where(predicate: c => c.path?[0] == ".git"));
        }
        catch (Exception e)
        {
            throw new Exception("Failed to clone from '" + cloneUrl + "'", e);
        }
        finally
        {
            DeleteLocalDirectoryRecursive(tempWorkingDirectory);
        }
    }

    static public IImmutableDictionary<IImmutableList<string>, ReadOnlyMemory<byte>> GetRepositoryFilesPartialForBranchViaLibGitSharpCheckout(
        string cloneUrl,
        string? branchName)
    {
        var tempWorkingDirectory = Filesystem.CreateRandomDirectoryInTempDirectory();

        try
        {
            //  https://github.com/libgit2/libgit2sharp/wiki/git-clone
            Repository.Clone(
                cloneUrl, tempWorkingDirectory,
                new CloneOptions { Checkout = false, BranchName = branchName });

            return
                Composition.ToFlatDictionaryWithPathComparer(
                    Filesystem.GetAllFilesFromDirectory(tempWorkingDirectory)
                    .Where(predicate: c => c.path?[0] == ".git"));
        }
        catch (Exception e)
        {
            throw new Exception("Failed to clone from '" + cloneUrl + "'", e);
        }
        finally
        {
            DeleteLocalDirectoryRecursive(tempWorkingDirectory);
        }
    }

    static public (string hash, CommitContent content) GetCommitHashAndContent(Commit commit)
    {
        return (commit.Sha, new CommitContent
        (
            message: commit.Message,
            author: GetParticipantSignature(commit.Author),
            committer: GetParticipantSignature(commit.Committer)
        ));
    }

    static public GitParticipantSignature GetParticipantSignature(Signature signature)
    {
        return new GitParticipantSignature
        (
            name: signature.Name,
            email: signature.Email
        );
    }

    static Result<object, GitObject> FindGitObjectAtPath(Tree root, IEnumerable<string> nodesNames)
    {
        if (root == null)
            return Result<object, GitObject>.err(new object());

        GitObject? currentObject = root;

        foreach (var nodeName in nodesNames)
        {
            var currentTree = currentObject as Tree;

            if (currentTree == null)
                return Result<object, GitObject>.err(new object());

            var treeEntry = currentTree?[nodeName];

            currentObject =
                treeEntry?.Mode == Mode.Nonexistent || !(treeEntry?.TargetType == TreeEntryTargetType.Blob || treeEntry?.TargetType == TreeEntryTargetType.Tree)
                ?
                null
                :
                treeEntry.Target;
        }

        if (currentObject == null)
            return Result<object, GitObject>.err(new object());

        return Result<object, GitObject>.ok(currentObject);
    }

    static byte[] GitBlobSHAFromBlobContent(byte[] blobContent)
    {
        var prefixAsText = "blob " + blobContent.Length.ToString() + "\0";

        var hashedValue = Encoding.ASCII.GetBytes(prefixAsText).Concat(blobContent).ToArray();

        return SHA1.HashData(hashedValue);
    }

    public record LoadFromUrlSuccess(
        TreeNodeWithStringPath tree,
        string urlInCommit,
        string urlInFirstParentCommitWithSameValueAtThisPath,
        (string hash, CommitContent content) rootCommit,
        (string hash, CommitContent content) firstParentCommitWithSameTree)
    {
        public ReadOnlyMemory<byte>? AsBlob => tree.BlobContent;
    }

    public record CommitContent(
        string message,
        GitParticipantSignature author,
        GitParticipantSignature committer);

    public record GitParticipantSignature(
        string name,
        string email);

    static void DeleteLocalDirectoryRecursive(string directoryPath) =>
        Filesystem.DeleteLocalDirectoryRecursive(directoryPath);
}
