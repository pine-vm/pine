using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Pine
{
    /// <summary>
    /// Project state models based on https://github.com/elm-time/elm-time/blob/742650b6a6f1e3dc723d76fbb8c189ca16a0bee6/implement/example-apps/elm-editor/src/ProjectState_2021_01.elm
    /// </summary>
    namespace ProjectState_2021_01
    {
        /*
         * Example JSON:
         * 
         * {"base":"https://github.com/elm-time/elm-time/tree/742650b6a6f1e3dc723d76fbb8c189ca16a0bee6/implement/example-apps/elm-editor/default-app","differenceFromBase":{"removeNodes":[],"changeBlobs":[[["src","Main.elm"],[{"ReuseBytes":[537]},{"RemoveBytes":[6]},{"AddBytes":[{"AsBase64":"ICAgIDQK"}]},{"ReuseBytes":[553]}]]]}}
         * 
         * */
        public record ProjectState(
            string @base,
            ProjectStateDifference differenceFromBase);

        /*
         * Example JSON:
         * 
         * {"removeNodes":[],"changeBlobs":[[["src","Main.elm"],[{"ReuseBytes":[537]},{"RemoveBytes":[6]},{"AddBytes":[{"AsBase64":"ICAgIDQK"}]},{"ReuseBytes":[553]}]]]}
         * 
         * */
        public record ProjectStateDifference(
            IReadOnlyList<IReadOnlyList<string>> removeNodes,
            IReadOnlyList<(IReadOnlyList<string>, IReadOnlyList<BlobChangeSequenceElement>)> changeBlobs)
        {
            /// <summary>
            /// https://github.com/elm-time/elm-time/blob/742650b6a6f1e3dc723d76fbb8c189ca16a0bee6/implement/example-apps/elm-editor/src/ProjectState_2021_01.elm#L69-L96
            /// 
            /// applyBlobChanges : List BlobChangeSequenceElement -> Bytes.Bytes -> Bytes.Bytes
            /// </summary>
            public static ReadOnlyMemory<byte> ApplyBlobChanges(
                IReadOnlyList<BlobChangeSequenceElement> changes,
                ReadOnlyMemory<byte>? blobBefore)
            {
                static (ReadOnlyMemory<byte> originalBlobRemainingBytes, ReadOnlyMemory<byte> changedBlobBytes) applyChange(
                    BlobChangeSequenceElement change, ReadOnlyMemory<byte> originalBlobRemainingBytes, ReadOnlyMemory<byte> changedBlobBytes)
                {
                    var reuseBytes = change.ReuseBytes?[0];
                    var removeBytes = change.RemoveBytes?[0];
                    var addBytes = change.AddBytes?[0];

                    if (reuseBytes != null)
                    {
                        return (
                            originalBlobRemainingBytes[reuseBytes.Value..],
                            CommonConversion.Concat(changedBlobBytes.Span, originalBlobRemainingBytes[..reuseBytes.Value].Span));
                    }

                    if (removeBytes != null)
                    {
                        return (
                            originalBlobRemainingBytes.Slice(removeBytes.Value),
                            changedBlobBytes);
                    }

                    if (addBytes != null)
                    {
                        var bytes = Convert.FromBase64String(addBytes.AsBase64);

                        return (
                            originalBlobRemainingBytes,
                            CommonConversion.Concat(changedBlobBytes.Span, [.. bytes]));
                    }

                    throw new Exception("Unexpected shape of BlobChangeSequenceElement");
                }

                return
                    changes.Aggregate(
                        seed:
                        (originalBlobRemainingBytes: blobBefore ?? ReadOnlyMemory<byte>.Empty,
                        changedBlobBytes: ReadOnlyMemory<byte>.Empty),
                        (prev, change) => applyChange(change, originalBlobRemainingBytes: prev.originalBlobRemainingBytes, changedBlobBytes: prev.changedBlobBytes))
                    .changedBlobBytes;
            }
        }

        /*
         * type BlobChangeSequenceElement
         *      = ReuseBytes Int
         *      | RemoveBytes Int
         *      | AddBytes Bytes.Bytes
         * */
        public record BlobChangeSequenceElement(
            IReadOnlyList<int>? ReuseBytes = default,
            IReadOnlyList<int>? RemoveBytes = default,
            IReadOnlyList<Bytes>? AddBytes = default);

        public record Bytes(string AsBase64);
    }

    public static class LoadFromElmEditor
    {
        public record ParseUrlResult(
            string projectStateString,
            string? projectStateDeflateBase64String,
            string? projectStateHashString,
            string? filePathToOpenString);

        public record ProjectState(ProjectState_2021_01.ProjectState version_2021_01);

        public record LoadFromUrlSuccess(
            ParseUrlResult parsedUrl,
            TreeNodeWithStringPath tree);

        /// <summary>
        /// Sample addresses:
        /// https://elm-editor.com/?project-state=https%3A%2F%2Fgithub.com%2Felm-time%2Felm-time%2Ftree%2F742650b6a6f1e3dc723d76fbb8c189ca16a0bee6%2Fimplement%2Fexample-apps%2Felm-editor%2Fdefault-app&project-state-hash=ba36b62d7a0e2ffd8ed107782138be0e2b25257a67dc9273508b00daa003b6f3&file-path-to-open=src%2FMain.elm
        /// https://elm-editor.com/?project-state-deflate-base64=XZDLasMwEEX%2FZdZO5Ecip97FLYVQWmi3RgQ9xg9qW0aSQ4vRv9cyZNHsZg5zzwyzwA2N7fR4TeM0ucYJFAsIbhEKaJ2bbEFI07l2FnupB4L9sKvnvreOy%2B%2BHzhlEkh9SeowF5bROMFMyTzOV01qIk0xOT5InlMcCkZJumHoccHQEf3iod3ya7KZE1TltiMKaz70LHCJQXV2jwVHiq9FDuV24gMFB3%2FBDK7RQVCwC2fKxwbLXIoCqAmvkmn7n3bhf3cCiaoEvnC2Wv24LHbOc%2BSjAoLpTurGzUnewNjZspYf1M5fnc3N5%2BXwDv4399x0z5hlj3vs%2F&project-state-hash=c34a6a5e4ee0ea6308c9965dfbfbe68d28ecc07dca1cba8f9a2dac50700324e9&file-path-to-open=src%2FMain.elm
        /// 
        /// </summary>
        public static ParseUrlResult? ParseUrl(string url)
        {
            try
            {
                var uri = new Uri(url);

                var parsedQuery = System.Web.HttpUtility.ParseQueryString(uri.Query);

                var projectStateString = parsedQuery["project-state"];
                var projectStateDeflateBase64String = parsedQuery["project-state-deflate-base64"];
                var projectStateHashString = parsedQuery["project-state-hash"];
                var filePathToOpenString = parsedQuery["file-path-to-open"];

                try
                {
                    projectStateString ??=
                        projectStateDeflateBase64String == null ? null :
                        Encoding.UTF8.GetString(CommonConversion.Inflate(Convert.FromBase64String(projectStateDeflateBase64String)).Span);
                }
                catch { }

                if (projectStateString == null)
                    return null;

                return new ParseUrlResult(
                    projectStateString: projectStateString,
                    projectStateDeflateBase64String: projectStateDeflateBase64String,
                    projectStateHashString: projectStateHashString,
                    filePathToOpenString: filePathToOpenString);
            }
            catch
            {
                return null;
            }
        }

        public static Result<string, LoadFromUrlSuccess> LoadFromUrl(string sourceUrl)
        {
            var parsedUrl = ParseUrl(sourceUrl);

            if (parsedUrl == null)
                return Result<string, LoadFromUrlSuccess>.err("Failed to parse string '" + sourceUrl + "' as Elm Editor URL.");

            LoadFromUrlSuccess returnValueFromTree(TreeNodeWithStringPath tree) =>
                new(parsedUrl: parsedUrl, tree: tree);

            if (LoadFromGitHubOrGitLab.ParseUrl(parsedUrl.projectStateString) != null)
            {
                return
                    LoadFromGitHubOrGitLab.LoadFromUrl(parsedUrl.projectStateString)
                    .MapError(error => "Failed to load from Git host: " + error)
                    .Map(loadFromGitHostSuccess => returnValueFromTree(loadFromGitHostSuccess.tree));
            }

            // Support parsing tuples: https://github.com/arogozine/TupleAsJsonArray/tree/e59f8c4edee070b096220b6cab77eba997b19d3a

            var jsonSerializerOptions = new System.Text.Json.JsonSerializerOptions
            {
                Converters =
                {
                    new TupleAsJsonArray.TupleConverterFactory(),
                }
            };

            var projectState = System.Text.Json.JsonSerializer.Deserialize<ProjectState>(
                parsedUrl.projectStateString,
                options: jsonSerializerOptions)!;

            if (projectState.version_2021_01 != null)
            {
                return
                    LoadProjectState(projectState.version_2021_01)
                    .Map(returnValueFromTree);
            }

            return Result<string, LoadFromUrlSuccess>.err("Project state has an unexpected shape: " + parsedUrl.projectStateString);
        }

        public static Result<string, TreeNodeWithStringPath> LoadProjectState(ProjectState_2021_01.ProjectState projectState)
        {
            TreeNodeWithStringPath? baseComposition = null;

            if (projectState.@base != null)
            {
                var loadFromGitHost = LoadFromGitHubOrGitLab.LoadFromUrl(projectState.@base);

                if (loadFromGitHost is Result<string, LoadFromGitHubOrGitLab.LoadFromUrlSuccess>.Err loadFromGitHostError)
                {
                    return Result<string, TreeNodeWithStringPath>.err(
                        "Failed to load from Git host: " + loadFromGitHostError);
                }

                baseComposition = loadFromGitHost.Map(loaded => loaded.tree).Extract(error => throw new Exception(error));
            }

            return
                ApplyProjectStateDifference_2021_01(projectState.differenceFromBase, baseComposition)
                .MapError(error => "Failed to apply difference: " + error);
        }

        /// <summary>
        /// https://github.com/elm-time/elm-time/blob/742650b6a6f1e3dc723d76fbb8c189ca16a0bee6/implement/example-apps/elm-editor/src/FileTreeInWorkspace.elm#L106-L132
        /// 
        /// applyProjectStateDifference_2021_01 : ProjectState_2021_01.ProjectStateDifference -> FileTree.FileTreeNode Bytes.Bytes -> Result String (FileTree.FileTreeNode Bytes.Bytes)
        /// </summary>
        public static Result<string, TreeNodeWithStringPath> ApplyProjectStateDifference_2021_01(
            ProjectState_2021_01.ProjectStateDifference differenceFromBase,
            TreeNodeWithStringPath? baseComposition)
        {
            var compositionAfterRemovals =
                differenceFromBase.removeNodes.Aggregate(
                    seed: baseComposition,
                    (previousComposition, nodePath) => previousComposition?.RemoveNodeAtPath(nodePath))
                ?? TreeNodeWithStringPath.EmptyTree;

            var projectStateAfterChangeBlobs =
                differenceFromBase.changeBlobs.Aggregate(
                    seed: compositionAfterRemovals,
                    (previousComposition, blobChange) =>
                    {
                        var blobValueBefore =
                        compositionAfterRemovals.GetNodeAtPath(blobChange.Item1) switch
                        {
                            TreeNodeWithStringPath.BlobNode blob => blob.Bytes,
                            _ => null
                        };

                        var changedBlobValue = ProjectState_2021_01.ProjectStateDifference.ApplyBlobChanges(
                            blobChange.Item2, blobValueBefore);

                        return previousComposition.SetNodeAtPathSorted(blobChange.Item1, TreeNodeWithStringPath.Blob(changedBlobValue));
                    });

            return Result<string, TreeNodeWithStringPath>.ok(projectStateAfterChangeBlobs);
        }
    }
}
