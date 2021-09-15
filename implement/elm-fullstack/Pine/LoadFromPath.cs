using static Pine.Composition;

namespace Pine
{
    static public class LoadFromPath
    {
        static public Result<string, (TreeWithStringPath tree, bool comesFromLocalFilesystem)> LoadTreeFromPath(string path)
        {
            if (LoadFromGitHubOrGitLab.ParsePathFromUrl(path) != null)
            {
                var loadFromGitHost =
                    LoadFromGitHubOrGitLab.LoadFromUrl(path);

                if (loadFromGitHost?.Ok == null)
                {
                    return Result<string, (TreeWithStringPath tree, bool comesFromLocalFilesystem)>.err(
                        "Failed to load from Git host: " + loadFromGitHost?.Err?.ToString());
                }

                return loadFromGitHost.map(loadFromGitHostOk => (loadFromGitHostOk.tree, comesFromLocalFilesystem: false));
            }

            if (LoadFromElmEditor.ParseUrl(path) != null)
            {
                var loadFromElmEditor = LoadFromElmEditor.LoadFromUrl(path);

                if (loadFromElmEditor?.Ok == null)
                {
                    return Result<string, (TreeWithStringPath tree, bool comesFromLocalFilesystem)>.err(
                        "Failed to load from Elm Editor: " + loadFromElmEditor?.Err?.ToString());
                }

                return Result<string, (TreeWithStringPath tree, bool comesFromLocalFilesystem)>.ok(
                    (loadFromElmEditor.Ok.tree, comesFromLocalFilesystem: false));
            }

            return Result<string, (TreeWithStringPath tree, bool comesFromLocalFilesystem)>.ok(
                (tree: LoadFromLocalFilesystem.LoadSortedTreeFromPath(path), comesFromLocalFilesystem: true));
        }
    }
}
