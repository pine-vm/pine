using LibGit2Sharp;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Linq;
using System.Security.Cryptography;
using System.Text;
using System.Text.RegularExpressions;

namespace Pine;

public static class LoadFromGitHubOrGitLab
{
    public static CacheByFileName? RepositoryFilesPartialForCommitCacheDefault = null;

    /// <summary>
    /// Sample address to a tree:
    /// https://github.com/elm-time/elm-time/tree/2fe621f492eced8a6f0da6e89e0aa90d01d6a04c/guide
    /// 
    /// Sample address to a blob:
    /// https://github.com/elm-time/elm-time/blob/2fe621f492eced8a6f0da6e89e0aa90d01d6a04c/guide/persistence-in-elm-time.md
    /// 
    /// Support loading from the root tree:
    /// https://github.com/elm-time/elm-time/tree/02dd3ba746a020d011b3e7da69b245fe63933350/
    /// https://github.com/elm-time/elm-time/tree/02dd3ba746a020d011b3e7da69b245fe63933350
    ///
    /// Blob on GitLab:
    /// https://gitlab.com/gilmi/strema/-/blob/de9f6a401f89215cb6cebbbbf2eed0252aeef1d1/overview.org
    ///
    /// Tree on GitLab:
    /// https://gitlab.com/gilmi/strema/-/tree/de9f6a401f89215cb6cebbbbf2eed0252aeef1d1/src/Strema
    /// </summary>
    private static string GitHubOrGitLabRegexPattern(
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

    public static ParsedUrl? ParseUrl(string objectUrl)
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

    public static string BackToUrl(ParsedUrl parsedUrl) =>
        parsedUrl.repository +
        (parsedUrl.inRepository == null
        ? "" :
        "/" + parsedUrl.inRepository.objectType + "/" + parsedUrl.inRepository.@ref + "/" + parsedUrl.inRepository.path);

    public static Result<string, LoadFromUrlSuccess> LoadFromUrl(string sourceUrl) =>
        LoadFromUrl(
            sourceUrl: sourceUrl,
            getRepositoryFilesPartialForCommit: GetRepositoryFilesPartialForCommitDefault);

    public static Result<string, LoadFromUrlSuccess> LoadFromUrl(
        string sourceUrl,
        Func<GetRepositoryFilesPartialForCommitRequest, Result<string, IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>>>> getRepositoryFilesPartialForCommit)
    {
        ElmTime.Git.LibGit2Sharp.SetupTask.Value.Wait();

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

        getRepositoryFilesPartialForCommit ??=
            req => GetRepositoryFilesPartialForCommitViaLibGitSharpCheckout(
                cloneUrl: req.cloneUrlCandidates[0],
                commit: req.commit);

        var repositoryFilesResult =
            refLooksLikeCommit ?
            getRepositoryFilesPartialForCommit(
                new GetRepositoryFilesPartialForCommitRequest(
                    commit: parsedUrl.inRepository!.@ref,
                    cloneUrlCandidates: [cloneUrl]))
            :
            GetRepositoryFilesPartialForBranchViaLibGitSharpCheckout(cloneUrl, branchName);

        var tempWorkingDirectory = Filesystem.CreateRandomDirectoryInTempDirectory();
        var gitRepositoryLocalDirectory = Path.Combine(tempWorkingDirectory, "git-repository");

        return
            repositoryFilesResult
            .AndThen(repositoryFilesPartial =>
            {
                try
                {
                    foreach (var fileWithPath in repositoryFilesPartial)
                    {
                        var absoluteFilePath = Path.Combine([gitRepositoryLocalDirectory, .. fileWithPath.Key]);
                        var absoluteDirectoryPath = Path.GetDirectoryName(absoluteFilePath)!;

                        Directory.CreateDirectory(absoluteDirectoryPath);
                        File.WriteAllBytes(absoluteFilePath, fileWithPath.Value.ToArray());
                    }

                    (string hash, CommitContent content)? rootCommit = null;

                    using var gitRepository = new Repository(gitRepositoryLocalDirectory);

                    var loadStartCommitResult =
                        GetCommitFromReference(
                            cloneUrl: cloneUrl,
                            RefCanonicalNameFromPathComponentInGitHubRepository(parsedUrl.inRepository?.@ref))
                        .MapError(err => "I did not find the commit for ref '" + err + "'.")
                        .AndThen(commitId =>
                        {
                            if (gitRepository.Lookup(commitId) is Commit commit)
                                return Result<string, Commit>.ok(commit);

                            return Result<string, Commit>.err("Did not find commit " + commitId);
                        });

                    return
                        loadStartCommitResult
                        .AndThen(startCommit =>
                        {
                            ParsedUrlInRepository partInRepositoryWithCommit(Commit replacementCommit) =>
                            parsedUrl.inRepository == null ?
                            new ParsedUrlInRepository(GitObjectType.tree, @ref: replacementCommit.Sha, path: "") :
                            parsedUrl.inRepository with { @ref = replacementCommit.Sha };

                            var urlInCommit = BackToUrl(parsedUrl with { inRepository = partInRepositoryWithCommit(startCommit) });

                            rootCommit = GetCommitHashAndContent(startCommit);

                            var parsedUrlPath = parsedUrl.inRepository == null ? "" : parsedUrl.inRepository.path;

                            var pathNodesNames = parsedUrlPath.Split('/', StringSplitOptions.RemoveEmptyEntries);

                            return
                            FindGitObjectAtPath(startCommit.Tree, pathNodesNames)
                            .MapError(_ => "I did not find an object at path '" + parsedUrlPath + "' in " + startCommit.Sha)
                            .AndThen(linkedObject =>
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
                                            if (FindGitObjectAtPath(parent.Tree, pathNodesNames).Map(find => (string?)find.Sha).WithDefault(null) != linkedObject?.Sha)
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
                                    return Result<string, LoadFromUrlSuccess>.err("Failed to convert from git object:\n" + e);
                                }
                            });
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
            });
    }

    private static Result<string, IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>>> GetRepositoryFilesPartialForCommitDefault(
        GetRepositoryFilesPartialForCommitRequest request)
    {
        var loadNew =
            () =>
            {
                var loadCandidates =
                    request.cloneUrlCandidates.Select(
                        cloneUrlCandidate =>
                        new Func<Result<string, IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>>>>(
                        () => GetRepositoryFilesPartialForCommitViaLibGitSharpCheckout(
                        cloneUrl: cloneUrlCandidate,
                        commit: request.commit)));

                return
                loadCandidates.FirstOkOrAllErrors().MapError(
                    candidatesErrors => "Failed for " + candidatesErrors.Count + " clone urls:\n" + string.Join("\n", candidatesErrors));
            };

        var localCache = new Lazy<Result<string, IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>>>>(loadNew);

        var externalCache = RepositoryFilesPartialForCommitCacheDefault;

        var fromExternalCache =
            externalCache?.GetOrTryAdd(
            fileName: request.commit,
            tryBuild: () =>
            {
                return localCache.Value.Unpack(
                    fromErr: _ => null,
                    fromOk: files => (byte[]?)ZipArchive.ZipArchiveFromEntries(files));
            });

        return fromExternalCache switch
        {
            not null => Result<string, IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>>>.ok(
                PineValueComposition.ToFlatDictionaryWithPathComparer(
                    PineValueComposition.SortedTreeFromSetOfBlobsWithCommonFilePath(
                        ZipArchive.EntriesFromZipArchive(fromExternalCache.Value))
                    .EnumerateBlobsTransitive())),

            _ => localCache.Value
        };
    }

    public static Result<string, IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>>> GetRepositoryFilesPartialForBranchViaLibGitSharpCheckout(
        string cloneUrl,
        string? branchName)
    {
        ElmTime.Git.LibGit2Sharp.SetupTask.Value.Wait();

        var refName = RefCanonicalNameFromPathComponentInGitHubRepository(branchName);

        return
            GetCommitFromReference(cloneUrl, refName)
            .MapError(err => "Failed to get commit from ref '" + refName + "': " + err)
            .AndThen(commitId => GetRepositoryFilesPartialForCommitViaLibGitSharpCheckout(cloneUrl, commitId));
    }

    public static Result<string, IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>>> GetRepositoryFilesPartialForCommitViaLibGitSharpCheckout(
        string cloneUrl,
        string commit)
    {
        var tempWorkingDirectory =
            Path.GetFullPath(Filesystem.CreateRandomDirectoryInTempDirectory().TrimEnd('/') + "/");

        /*
         * 2023-05-20
         * Adapt to https://github.com/libgit2/libgit2sharp/issues/1945:
         * On MacOS, `Repository.Init` yields different WorkingDirectory than provided in constructor
         * 
         * Before we started to map the directory path to resolve symbolic links here, `Repository.CheckoutPaths` failed on OSX with an error message like this:
         * Unable to process file '/var/folders/24/8k48jl6d249_n_qfxwsl6xvm0000gn/T/zqdzg3qp.4ni/'. This file is not located under the working directory of the repository ('/private/var/folders/24/8k48jl6d249_n_qfxwsl6xvm0000gn/T/zqdzg3qp.4ni/').
         * 
         * */
        tempWorkingDirectory =
            ResolveLinkTargetFullNameIncludingParents(tempWorkingDirectory);

        try
        {
            //  https://github.com/libgit2/libgit2sharp/wiki/git-clone
            Repository.Clone(
                cloneUrl, tempWorkingDirectory,
                new CloneOptions { Checkout = false, BranchName = null });

            // TODO: Test reduce size of returned files using partial clone?

            using var gitRepository = new Repository(tempWorkingDirectory);

            gitRepository.CheckoutPaths(commit, [tempWorkingDirectory.TrimEnd('/') + "/"]);

            return
                Result<string, IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>>>.ok(
                    PineValueComposition.ToFlatDictionaryWithPathComparer(
                        Filesystem.GetAllFilesFromDirectory(tempWorkingDirectory)
                        .Where(predicate: c => c.path?[0] == ".git")));
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

    private static string ResolveLinkTargetFullNameIncludingParents(string directory)
    {
        if (directory == Path.GetPathRoot(directory))
            return directory;

        if (Directory.ResolveLinkTarget(directory, returnFinalTarget: true) is { } resolved)
            return resolved.FullName;

        if (Path.GetDirectoryName(directory) is { } parent)
        {
            return ResolveLinkTargetFullNameIncludingParents(parent) + Path.DirectorySeparatorChar + Path.GetFileName(directory);
        }

        return directory;
    }

    public static string RefCanonicalNameFromPathComponentInGitHubRepository(string? refPathComponent)
    {
        if (refPathComponent is null)
            return "HEAD";

        if (RefLooksLikeCommit(refPathComponent))
            return refPathComponent;

        return "refs/heads/" + refPathComponent;
    }

    public static Result<string, string> GetCommitFromReference(
        string cloneUrl,
        string referenceCanonicalName)
    {
        if (RefLooksLikeCommit(referenceCanonicalName))
            return Result<string, string>.ok(referenceCanonicalName);

        var remoteReferences = Repository.ListRemoteReferences(cloneUrl).ToImmutableList();

        return GetCommitFromReference(
            ImmutableHashSet<string>.Empty,
            remoteReferences,
            referenceCanonicalName);
    }

    public static Result<string, string> GetCommitFromReference(
        IImmutableSet<string> stack,
        IReadOnlyList<Reference> remoteReferences,
        string referenceCanonicalName)
    {
        if (RefLooksLikeCommit(referenceCanonicalName))
            return Result<string, string>.ok(referenceCanonicalName);

        if (stack.Contains(referenceCanonicalName))
            return Result<string, string>.err("Cyclic reference: '" + referenceCanonicalName + "'");

        var matchingReference =
            remoteReferences.FirstOrDefault(c => c.CanonicalName == referenceCanonicalName);

        if (matchingReference == null)
        {
            return Result<string, string>.err("Found no reference matching '" + referenceCanonicalName + "' (" + remoteReferences.Count + " remote references)");
        }

        return GetCommitFromReference(
            stack.Add(referenceCanonicalName),
            remoteReferences,
            matchingReference.TargetIdentifier);
    }

    public static bool RefLooksLikeCommit(string reference) => Regex.IsMatch(reference, "[A-Fa-f0-9]{40}");

    public static IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>> GetRepositoryFilesPartialForCommitViaEnvironmentGitCheckout(
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
                PineValueComposition.ToFlatDictionaryWithPathComparer(
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

    public static (string hash, CommitContent content) GetCommitHashAndContent(Commit commit)
    {
        return (commit.Sha, new CommitContent
        (
            message: commit.Message,
            author: GetParticipantSignature(commit.Author),
            committer: GetParticipantSignature(commit.Committer)
        ));
    }

    public static GitParticipantSignature GetParticipantSignature(Signature signature)
    {
        return new GitParticipantSignature
        (
            name: signature.Name,
            email: signature.Email
        );
    }

    private static Result<object, GitObject> FindGitObjectAtPath(Tree root, IEnumerable<string> nodesNames)
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

    private static byte[] GitBlobSHAFromBlobContent(byte[] blobContent)
    {
        var prefixAsText = "blob " + blobContent.Length + "\0";

        return SHA1.HashData([.. Encoding.ASCII.GetBytes(prefixAsText), .. blobContent]);
    }

    public record LoadFromUrlSuccess(
        TreeNodeWithStringPath tree,
        string urlInCommit,
        string urlInFirstParentCommitWithSameValueAtThisPath,
        (string hash, CommitContent content) rootCommit,
        (string hash, CommitContent content) firstParentCommitWithSameTree)
    {
        public ReadOnlyMemory<byte>? AsBlob =>
            tree switch
            {
                TreeNodeWithStringPath.BlobNode blob => blob.Bytes,
                _ => null
            };
    }

    public record CommitContent(
        string message,
        GitParticipantSignature author,
        GitParticipantSignature committer);

    public record GitParticipantSignature(
        string name,
        string email);

    private static void DeleteLocalDirectoryRecursive(string directoryPath) =>
        Filesystem.DeleteLocalDirectoryRecursive(directoryPath);
}
