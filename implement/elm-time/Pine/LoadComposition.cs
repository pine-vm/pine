using System;
using System.Collections.Generic;

namespace Pine;

public abstract record LoadCompositionOrigin
{
    public sealed record FromGit(
        LoadFromGitHubOrGitLab.LoadFromUrlSuccess Success)
        : LoadCompositionOrigin;

    public sealed record FromLocalFileSystem
        : LoadCompositionOrigin;

    public sealed record FromEditor(
        LoadFromElmEditor.ParseUrlResult ParsedUrl)
        : LoadCompositionOrigin;

    public sealed record FromHttp
        : LoadCompositionOrigin;
}

public static class LoadComposition
{
    public static ProcessWithLog<string, Result<string, (TreeNodeWithStringPath tree, LoadCompositionOrigin origin)>> LoadFromPathResolvingNetworkDependencies(string sourcePath)
    {
        var asProcess = AsProcessWithStringLog(sourcePath);

        if (LoadFromGitHubOrGitLab.ParseUrl(sourcePath) is not null)
        {
            return
                asProcess
                .WithLogEntryAdded("This path looks like a URL into a remote git repository. Trying to load from there...")
                .MapResult(LoadFromGitHubOrGitLab.LoadFromUrl)
                .ResultAddLogEntriesIfOk(LogEntriesForLoadFromGitSuccess)
                .ResultMap(loadFromGitOk =>
                (loadFromGitOk.tree, (LoadCompositionOrigin)new LoadCompositionOrigin.FromGit(loadFromGitOk)));
        }

        if (LoadFromElmEditor.ParseUrl(sourcePath) is not null)
        {
            return
                asProcess
                .WithLogEntryAdded("This path looks like a URL into a code editor. Trying to load from there...")
                .MapResult(LoadFromElmEditor.LoadFromUrl)
                .ResultMap(loadFromEditorOk =>
                (loadFromEditorOk.tree, (LoadCompositionOrigin)new LoadCompositionOrigin.FromEditor(loadFromEditorOk.parsedUrl)));
        }

        if (System.Text.RegularExpressions.Regex.Match(sourcePath, "^http(s|)\\:", System.Text.RegularExpressions.RegexOptions.IgnoreCase).Success)
        {
            return
                asProcess
                .WithLogEntryAdded("Loading via HTTP...")
                .MapResult(BlobLibrary.DownloadBlobViaHttpGetResponseBody)
                .ResultMap(loadFromHttpGet =>
                (TreeNodeWithStringPath.Blob(loadFromHttpGet), (LoadCompositionOrigin)new LoadCompositionOrigin.FromHttp()));
        }

        return
            asProcess
            .WithLogEntryAdded("Trying to load from local file system...")
            .MapResult(sourcePath =>
            {
                try
                {
                    var treeComponentFromSource = LoadFromLocalFilesystem.LoadSortedTreeFromPath(sourcePath);

                    if (treeComponentFromSource is null)
                        return Result<string, TreeNodeWithStringPath>.err("I did not find a file or directory at '" + sourcePath + "'.");

                    return Result<string, TreeNodeWithStringPath>.ok(treeComponentFromSource);
                }
                catch (Exception e)
                {
                    return Result<string, TreeNodeWithStringPath>.err("Failed to load from local file system: " + e);
                }
            })
            .ResultMap(tree => (tree, (LoadCompositionOrigin)new LoadCompositionOrigin.FromLocalFileSystem()));
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
