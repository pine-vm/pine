using System;
using System.Collections.Generic;

namespace Pine;

public record LoadCompositionOrigin(
    LoadFromGitHubOrGitLab.LoadFromUrlSuccess? FromGit = null,
    object? FromLocalFileSystem = null,
    LoadFromElmEditor.ParseUrlResult? FromEditor = null,
    object? FromHttp = null);

public static class LoadComposition
{
    public static ProcessWithLog<string, Result<string, (TreeNodeWithStringPath tree, LoadCompositionOrigin origin)>> LoadFromPathResolvingNetworkDependencies(string sourcePath)
    {
        var asProcess = AsProcessWithStringLog(sourcePath);

        if (LoadFromGitHubOrGitLab.ParseUrl(sourcePath) != null)
        {
            return
                asProcess
                .WithLogEntryAdded("This path looks like a URL into a remote git repository. Trying to load from there...")
                .MapResult(LoadFromGitHubOrGitLab.LoadFromUrl)
                .ResultAddLogEntriesIfOk(LogEntriesForLoadFromGitSuccess)
                .ResultMap(loadFromGitOk => (loadFromGitOk.tree, new LoadCompositionOrigin(FromGit: loadFromGitOk)));
        }

        if (LoadFromElmEditor.ParseUrl(sourcePath) != null)
        {
            return
                asProcess
                .WithLogEntryAdded("This path looks like a URL into a code editor. Trying to load from there...")
                .MapResult(LoadFromElmEditor.LoadFromUrl)
                .ResultMap(loadFromEditorOk => (loadFromEditorOk.tree, new LoadCompositionOrigin(FromEditor: loadFromEditorOk.parsedUrl)));
        }

        if (System.Text.RegularExpressions.Regex.Match(sourcePath, "^http(s|)\\:", System.Text.RegularExpressions.RegexOptions.IgnoreCase).Success)
        {
            return
                asProcess
                .WithLogEntryAdded("Loading via HTTP...")
                .MapResult(BlobLibrary.DownloadBlobViaHttpGetResponseBody)
                .ResultMap(loadFromHttpGet =>
                (TreeNodeWithStringPath.Blob(loadFromHttpGet), new LoadCompositionOrigin(FromHttp: new object())));
        }

        return
            asProcess
            .WithLogEntryAdded("Trying to load from local file system...")
            .MapResult(sourcePath =>
            {
                try
                {
                    var treeComponentFromSource = LoadFromLocalFilesystem.LoadSortedTreeFromPath(sourcePath);

                    if (treeComponentFromSource == null)
                        return Result<string, TreeNodeWithStringPath>.err("I did not find a file or directory at '" + sourcePath + "'.");

                    return Result<string, TreeNodeWithStringPath>.ok(treeComponentFromSource);
                }
                catch (Exception e)
                {
                    return Result<string, TreeNodeWithStringPath>.err("Failed to load from local file system: " + e);
                }
            })
            .ResultMap(tree => (tree, new LoadCompositionOrigin(FromLocalFileSystem: new object())));
    }

    public static IEnumerable<string> LogEntriesForLoadFromGitSuccess(LoadFromGitHubOrGitLab.LoadFromUrlSuccess loadFromGitSuccess)
    {
        yield return "This path points to commit " + loadFromGitSuccess?.rootCommit.hash;

        if (loadFromGitSuccess?.rootCommit.hash == null)
            yield break;

        if (loadFromGitSuccess.firstParentCommitWithSameTree.hash != null &&
            loadFromGitSuccess.firstParentCommitWithSameTree.hash != loadFromGitSuccess.rootCommit.hash)
        {
            yield return "The first parent commit with same tree is " + loadFromGitSuccess.urlInFirstParentCommitWithSameValueAtThisPath;
        }

        var commitToDisplayParticipants =
            loadFromGitSuccess.firstParentCommitWithSameTree;

        static string describeGitParticipant(LoadFromGitHubOrGitLab.GitParticipantSignature participant) =>
            participant?.name + " <" + participant?.email + ">";

        yield return "Participants from commit " + commitToDisplayParticipants.hash + ":\n" +
            "Author: " + describeGitParticipant(commitToDisplayParticipants.content.author) + "\n" +
            "Committer: " + describeGitParticipant(commitToDisplayParticipants.content.committer);
    }

    public static ProcessWithLog<string, T> AsProcessWithStringLog<T>(T result) => new ProcessWithLog<string, T>.Result(result);
}
