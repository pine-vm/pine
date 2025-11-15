using Pine.Core;
using Pine.Core.Files;
using Pine.Core.IO;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
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
    /// https://github.com/pine-vm/pine/tree/2fe621f492eced8a6f0da6e89e0aa90d01d6a04c/guide
    /// 
    /// Sample address to a blob:
    /// https://github.com/pine-vm/pine/blob/2fe621f492eced8a6f0da6e89e0aa90d01d6a04c/guide/persistence-in-elm-time.md
    /// 
    /// Support loading from the root tree:
    /// https://github.com/pine-vm/pine/tree/02dd3ba746a020d011b3e7da69b245fe63933350/
    /// https://github.com/pine-vm/pine/tree/02dd3ba746a020d011b3e7da69b245fe63933350
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
        var parsedUrl = ParseUrl(sourceUrl);

        if (parsedUrl is null)
            return "Failed to parse string '" + sourceUrl + "' as GitHub or GitLab URL.";

        string? branchName = null;
        var refLooksLikeCommit = false;

        if (parsedUrl.inRepository != null)
        {
            refLooksLikeCommit = Regex.IsMatch(parsedUrl.inRepository.@ref, "[A-Fa-f0-9]{40}");

            branchName = refLooksLikeCommit ? null : parsedUrl.inRepository.@ref;
        }

        var cloneUrl = parsedUrl.repository.TrimEnd('/') + ".git";

        getRepositoryFilesPartialForCommit ??=
            req => GetRepositoryFilesPartialForCommitViaGitCore(
                cloneUrl: req.cloneUrlCandidates[0],
                commit: req.commit);

        var repositoryFilesResult =
            refLooksLikeCommit ?
            getRepositoryFilesPartialForCommit(
                new GetRepositoryFilesPartialForCommitRequest(
                    commit: parsedUrl.inRepository!.@ref,
                    cloneUrlCandidates: [cloneUrl]))
            :
            GetRepositoryFilesPartialForBranchViaGitCore(cloneUrl, branchName);

        return
            repositoryFilesResult
            .AndThen(repositoryFilesPartial =>
            {
                try
                {
                    var refName = RefCanonicalNameFromPathComponentInGitHubRepository(parsedUrl.inRepository?.@ref);

                    var loadStartCommitResult =
                        GetCommitFromReference(cloneUrl, refName)
                        .MapError(err => "I did not find the commit for ref '" + err + "'.");

                    return
                        loadStartCommitResult
                        .AndThen(commitSha =>
                        {
                            // Fetch the repository with commit data using GitCore
                            var repository = GitCore.LoadFromUrl.FetchBloblessClone(
                                gitUrl: cloneUrl,
                                commitSha: commitSha,
                                depth: null);

                            var commitObject =
                                repository.GetObject(commitSha)
                                ?? throw new Exception("Commit " + commitSha + " not found in repository");

                            if (commitObject.Type is not GitCore.PackFile.ObjectType.Commit)
                                throw new Exception("Object " + commitSha + " is not a commit");

                            var commit = GitCore.GitObjects.ParseCommit(commitObject.Data);

                            ParsedUrlInRepository partInRepositoryWithCommit(string replacementCommitSha) =>
                                parsedUrl.inRepository is null ?
                                new ParsedUrlInRepository(GitObjectType.tree, @ref: replacementCommitSha, path: "") :
                                parsedUrl.inRepository with { @ref = replacementCommitSha };

                            var urlInCommit = BackToUrl(parsedUrl with { inRepository = partInRepositoryWithCommit(commitSha) });

                            var rootCommit = GetCommitHashAndContentFromGitCore(commitSha, commit);

                            var parsedUrlPath = parsedUrl.inRepository == null ? "" : parsedUrl.inRepository.path;

                            var pathNodesNames = parsedUrlPath.Split('/', StringSplitOptions.RemoveEmptyEntries);

                            // Get the tree or blob at the specified path
                            var treeHash = commit.TreeHash;
                            var currentPath = new List<string>();

                            // Navigate to the specified path
                            foreach (var pathComponent in pathNodesNames)
                            {
                                var treeObj = repository.GetObject(treeHash);
                                if (treeObj is null || treeObj.Type is not GitCore.PackFile.ObjectType.Tree)
                                    return "Path not found: " + string.Join("/", currentPath);

                                var tree = GitCore.GitObjects.ParseTree(treeObj.Data);
                                var entry = tree.Entries.FirstOrDefault(e => e.Name == pathComponent);

                                if (entry is null)
                                    return "Path not found: " + string.Join("/", currentPath.Append(pathComponent));

                                treeHash = entry.HashBase16;
                                currentPath.Add(pathComponent);
                            }

                            // Collect all blob SHAs we need
                            var blobShas = new List<string>();
                            void CollectBlobShas(string objHash)
                            {
                                var obj = repository.GetObject(objHash);

                                // If object is not in repository, assume it's a blob that needs to be fetched
                                if (obj is null)
                                {
                                    blobShas.Add(objHash);
                                    return;
                                }

                                if (obj.Type is GitCore.PackFile.ObjectType.Blob)
                                {
                                    blobShas.Add(objHash);
                                }
                                else if (obj.Type is GitCore.PackFile.ObjectType.Tree)
                                {
                                    var tree = GitCore.GitObjects.ParseTree(obj.Data);
                                    foreach (var entry in tree.Entries)
                                    {
                                        CollectBlobShas(entry.HashBase16);
                                    }
                                }
                            }

                            CollectBlobShas(treeHash);

                            // Fetch all blobs if we have any
                            if (blobShas.Count > 0)
                            {
                                var blobsPackFileData = GitCore.GitSmartHttp.FetchSpecificObjectsAsync(
                                    cloneUrl, blobShas).GetAwaiter().GetResult();

                                // Parse the blobs pack file
                                var blobsIndexResult = GitCore.PackIndex.GeneratePackIndexV2(blobsPackFileData);
                                var blobsIndexEntries = GitCore.PackIndex.ParsePackIndexV2(blobsIndexResult.IndexData);
                                var blobObjects = GitCore.PackFile.ParseAllObjects(blobsPackFileData, blobsIndexEntries);

                                // Add blobs to repository
                                var blobPackObjects = blobObjects
                                    .Where(obj => obj.Type is GitCore.PackFile.ObjectType.Blob)
                                    .ToDictionary(obj => obj.SHA1base16, obj => obj);

                                repository = repository.WithObjects(blobPackObjects.ToImmutableDictionary());
                            }

                            // Trace back to find first parent with same tree at this path
                            (string hash, CommitContent content) TraceBackToFirstParentWithSameTree()
                            {
                                var visited = new HashSet<string>();
                                var queue = new Queue<string>();
                                queue.Enqueue(commitSha);

                                (string hash, CommitContent content)? earliestCommit = null;
                                DateTimeOffset earliestTime = DateTimeOffset.MaxValue;

                                while (queue.Count > 0)
                                {
                                    var currentCommitSha = queue.Dequeue();

                                    if (!visited.Add(currentCommitSha))
                                        continue;

                                    var currentCommitObj = repository.GetObject(currentCommitSha);
                                    if (currentCommitObj is null || currentCommitObj.Type is not GitCore.PackFile.ObjectType.Commit)
                                        continue;

                                    var currentCommit = GitCore.GitObjects.ParseCommit(currentCommitObj.Data);

                                    // Check if this commit has the same tree at the specified path
                                    var currentTreeHash = currentCommit.TreeHash;
                                    foreach (var pathComponent in pathNodesNames)
                                    {
                                        var treeObj = repository.GetObject(currentTreeHash);
                                        if (treeObj is null || treeObj.Type is not GitCore.PackFile.ObjectType.Tree)
                                        {
                                            currentTreeHash = string.Empty;
                                            break;
                                        }

                                        var tree = GitCore.GitObjects.ParseTree(treeObj.Data);
                                        var entry = tree.Entries.FirstOrDefault(e => e.Name == pathComponent);

                                        if (entry is null)
                                        {
                                            currentTreeHash = string.Empty;
                                            break;
                                        }

                                        currentTreeHash = entry.HashBase16;
                                    }

                                    if (currentTreeHash == treeHash)
                                    {
                                        var commitTime = currentCommit.Author.Timestamp;
                                        if (commitTime < earliestTime)
                                        {
                                            earliestTime = commitTime;
                                            earliestCommit = GetCommitHashAndContentFromGitCore(currentCommitSha, currentCommit);
                                        }

                                        // Continue tracing through parents
                                        foreach (var parentHash in currentCommit.ParentHashes)
                                        {
                                            queue.Enqueue(parentHash);
                                        }
                                    }
                                }

                                return earliestCommit ?? rootCommit;
                            }

                            var firstParentCommitWithSameTree = TraceBackToFirstParentWithSameTree();

                            var urlInFirstParentCommitWithSameValueAtThisPath =
                                BackToUrl(parsedUrl with { inRepository = partInRepositoryWithCommit(firstParentCommitWithSameTree.hash) });

                            // Convert the tree or blob to the expected format
                            FileTree ConvertGitObjectToBlobTree(string objectHash)
                            {
                                var obj = repository.GetObject(objectHash);
                                if (obj is null)
                                    throw new Exception("Object not found: " + objectHash);

                                if (obj.Type is GitCore.PackFile.ObjectType.Blob)
                                {
                                    // Verify blob hash
                                    var blobContent = obj.Data.ToArray();
                                    var expectedSHA = objectHash.ToLowerInvariant();
                                    var loadedBlobSHA1Base16Lower =
                                        Convert.ToHexStringLower(GitBlobSHAFromBlobContent(blobContent));

                                    if (loadedBlobSHA1Base16Lower != expectedSHA)
                                        throw new Exception("Unexpected content for git object : SHA is " + loadedBlobSHA1Base16Lower + " instead of " + expectedSHA);

                                    return FileTree.File(blobContent);
                                }

                                if (obj.Type is GitCore.PackFile.ObjectType.Tree)
                                {
                                    var tree = GitCore.GitObjects.ParseTree(obj.Data);
                                    return FileTree.SortedDirectory(
                                        directoryContent:
                                            tree.Entries.Select(entry =>
                                                (entry.Name, ConvertGitObjectToBlobTree(entry.HashBase16)))
                                            .ToImmutableList());
                                }

                                throw new Exception("Unexpected git object type: " + obj.Type);
                            }

                            var literalNodeObject = ConvertGitObjectToBlobTree(treeHash);

                            return Result<string, LoadFromUrlSuccess>.ok(
                                new LoadFromUrlSuccess
                                (
                                    tree: literalNodeObject,
                                    urlInCommit: urlInCommit,
                                    urlInFirstParentCommitWithSameValueAtThisPath: urlInFirstParentCommitWithSameValueAtThisPath,
                                    rootCommit: rootCommit,
                                    firstParentCommitWithSameTree: firstParentCommitWithSameTree
                                )
                            );
                        });
                }
                catch (Exception e)
                {
                    return "Failed to load from git repository:\n" + e;
                }
            });
    }

    private static (string hash, CommitContent content) GetCommitHashAndContentFromGitCore(
        string commitSha,
        GitCore.GitObjects.CommitObject commit)
    {
        // GitCore trims trailing newlines, but the original implementation kept them
        // Add back a single trailing newline to match expected behavior
        var message = commit.Message;
        if (!string.IsNullOrEmpty(message) && !message.EndsWith('\n'))
            message += "\n";

        return (commitSha, new CommitContent
        (
            message: message,
            author: new GitParticipantSignature(name: commit.Author.Name, email: commit.Author.Email),
            committer: new GitParticipantSignature(name: commit.Committer.Name, email: commit.Committer.Email)
        ));
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
                        () => GetRepositoryFilesPartialForCommitViaGitCore(
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
                    fromOk: files => (byte[]?)ZipArchive.ZipArchiveFromFiles(files));
            });

        return fromExternalCache switch
        {
            not null => Result<string, IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>>>.ok(
                PineValueComposition.ToFlatDictionaryWithPathComparer(
                    PineValueComposition.SortedTreeFromSetOfBlobsWithCommonFilePath(
                        ZipArchive.EntriesFromZipArchive(fromExternalCache.Value))
                    .EnumerateFilesTransitive())),

            _ => localCache.Value
        };
    }

    public static Result<string, IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>>> GetRepositoryFilesPartialForBranchViaGitCore(
        string cloneUrl,
        string? branchName)
    {
        var refName = RefCanonicalNameFromPathComponentInGitHubRepository(branchName);

        return
            GetCommitFromReference(cloneUrl, refName)
            .MapError(err => "Failed to get commit from ref '" + refName + "': " + err)
            .AndThen(commitId => GetRepositoryFilesPartialForCommitViaGitCore(cloneUrl, commitId));
    }

    public static Result<string, IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>>> GetRepositoryFilesPartialForCommitViaGitCore(
        string cloneUrl,
        string commit)
    {
        try
        {
            // Use GitCore to load the tree contents
            var treeContents = GitCore.LoadFromUrl.LoadTreeContentsFromGitUrl(
                gitUrl: cloneUrl,
                commitSha: commit);

            // Convert from GitCore's IReadOnlyList<string> path format to the expected format
            return Result<string, IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>>>.ok(
                PineValueComposition.ToFlatDictionaryWithPathComparer(
                    treeContents.Select(kvp => (path: kvp.Key, blobContent: kvp.Value))));
        }
        catch (Exception e)
        {
            return Result<string, IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>>>.err(
                "Failed to load from '" + cloneUrl + "' at commit " + commit + ": " + e.Message);
        }
    }

    // This method name is kept for backwards compatibility but now uses GitCore
    public static Result<string, IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>>> GetRepositoryFilesPartialForBranchViaLibGitSharpCheckout(
        string cloneUrl,
        string? branchName)
    {
        var refName = RefCanonicalNameFromPathComponentInGitHubRepository(branchName);

        return
            GetCommitFromReference(cloneUrl, refName)
            .MapError(err => "Failed to get commit from ref '" + refName + "': " + err)
            .AndThen(commitId => GetRepositoryFilesPartialForCommitViaGitCore(cloneUrl, commitId));
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

        try
        {
            // Parse the clone URL to extract base URL, owner, and repo
            // Expected format: https://github.com/owner/repo.git or https://gitlab.com/owner/repo.git
            var uri = new Uri(cloneUrl);
            var baseUrl = $"{uri.Scheme}://{uri.Host}";
            var pathParts = uri.AbsolutePath.TrimStart('/').TrimEnd('/').Replace(".git", "").Split('/');

            if (pathParts.Length < 2)
                return Result<string, string>.err("Invalid clone URL format: " + cloneUrl);

            var owner = pathParts[0];
            var repo = pathParts[1];

            // Extract branch name from canonical name (e.g., "refs/heads/main" -> "main")
            var branchName = referenceCanonicalName.StartsWith("refs/heads/")
                ? referenceCanonicalName.Substring("refs/heads/".Length)
                : referenceCanonicalName;

            if (branchName == "HEAD")
            {
                // Try common default branch names
                foreach (var defaultBranch in new[] { "main", "master" })
                {
                    try
                    {
                        var commitSha = GitCore.GitSmartHttp.FetchBranchCommitShaAsync(
                            baseUrl, owner, repo, defaultBranch).GetAwaiter().GetResult();
                        return Result<string, string>.ok(commitSha);
                    }
                    catch
                    {
                        // Try next default branch name
                    }
                }

                return Result<string, string>.err("Failed to resolve HEAD to common default branches (main, master)");
            }

            var commitShaResult = GitCore.GitSmartHttp.FetchBranchCommitShaAsync(
                baseUrl, owner, repo, branchName).GetAwaiter().GetResult();

            return Result<string, string>.ok(commitShaResult);
        }
        catch (Exception e)
        {
            return Result<string, string>.err("Failed to resolve reference '" + referenceCanonicalName + "': " + e.Message);
        }
    }

    public static Result<string, string> GetCommitFromReference(
        IImmutableSet<string> stack,
        IReadOnlyList<(string CanonicalName, string TargetIdentifier)> remoteReferences,
        string referenceCanonicalName)
    {
        if (RefLooksLikeCommit(referenceCanonicalName))
            return Result<string, string>.ok(referenceCanonicalName);

        if (stack.Contains(referenceCanonicalName))
            return Result<string, string>.err("Cyclic reference: '" + referenceCanonicalName + "'");

        var matchingReference =
            remoteReferences.FirstOrDefault(c => c.CanonicalName == referenceCanonicalName);

        if (matchingReference == default)
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

    private static byte[] GitBlobSHAFromBlobContent(byte[] blobContent)
    {
        var prefixAsText = "blob " + blobContent.Length + "\0";

        return SHA1.HashData([.. Encoding.ASCII.GetBytes(prefixAsText), .. blobContent]);
    }

    public record LoadFromUrlSuccess(
        FileTree tree,
        string urlInCommit,
        string urlInFirstParentCommitWithSameValueAtThisPath,
        (string hash, CommitContent content) rootCommit,
        (string hash, CommitContent content) firstParentCommitWithSameTree)
    {
        public ReadOnlyMemory<byte>? AsBlob =>
            tree switch
            {
                FileTree.FileNode blob => blob.Bytes,
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
