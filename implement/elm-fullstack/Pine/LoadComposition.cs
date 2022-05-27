using System;
using System.Collections.Generic;
using static Pine.Composition;

namespace Pine;

public record LoadCompositionOrigin(
    LoadFromGitHubOrGitLab.LoadFromUrlSuccess? FromGit = null,
    object? FromLocalFileSystem = null,
    LoadFromElmEditor.ParseUrlResult? FromEditor = null);

static public class LoadComposition
{
    static public ProcessWithLog<string, Result<string, (TreeWithStringPath tree, LoadCompositionOrigin origin)?>> LoadFromPathResolvingNetworkDependencies(string sourcePath)
    {
        var asProcess = AsProcessWithStringLog(sourcePath);

        if (LoadFromGitHubOrGitLab.ParseUrl(sourcePath) != null)
        {
            return
                asProcess
                .WithLogEntryAdded("This path looks like a URL into a remote git repository. Trying to load from there...")
                .MapResult(LoadFromGitHubOrGitLab.LoadFromUrl)
                .ResultAddLogEntriesIfOk(LogEntriesForLoadFromGitSuccess)
                .ResultMap(loadFromGitOk =>
                ((TreeWithStringPath tree, LoadCompositionOrigin origin)?)
                (loadFromGitOk.tree, new LoadCompositionOrigin(FromGit: loadFromGitOk)));
        }

        if (LoadFromElmEditor.ParseUrl(sourcePath) != null)
        {
            return
                asProcess
                .WithLogEntryAdded("This path looks like a URL into a code editor. Trying to load from there...")
                .MapResult(LoadFromElmEditor.LoadFromUrl)
                .ResultMap(loadFromEditorOk =>
                ((TreeWithStringPath tree, LoadCompositionOrigin origin)?)
                (loadFromEditorOk.tree, new LoadCompositionOrigin(FromEditor: loadFromEditorOk.parsedUrl)));
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
                        return Result<string, TreeWithStringPath>.err("I did not find a file or directory at '" + sourcePath + "'.");

                    return Result<string, TreeWithStringPath>.ok(treeComponentFromSource);
                }
                catch (Exception e)
                {
                    return Result<string, TreeWithStringPath>.err("Failed to load from local file system: " + e?.ToString());
                }
            })
            .ResultMap(tree =>
            ((TreeWithStringPath tree, LoadCompositionOrigin origin)?)(tree, new LoadCompositionOrigin(FromLocalFileSystem: new object())));
    }

    static public IEnumerable<string> LogEntriesForLoadFromGitSuccess(LoadFromGitHubOrGitLab.LoadFromUrlSuccess loadFromGitSuccess)
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

    static public ProcessWithLog<string, T> AsProcessWithStringLog<T>(T Result) => new(Result: Result);

}
