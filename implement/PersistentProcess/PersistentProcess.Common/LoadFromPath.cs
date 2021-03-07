using static Kalmit.Composition;

namespace Kalmit
{
    static public class LoadFromPath
    {
        static public Result<string, (TreeWithStringPath tree, bool comesFromLocalFilesystem)> LoadTreeFromPath(string path)
        {
            if (LoadFromGitHubOrGitLab.ParsePathFromUrl(path) != null)
            {
                var loadFromGitHost =
                    LoadFromGitHubOrGitLab.LoadFromUrl(path);

                if (loadFromGitHost?.Success == null)
                {
                    return Result<string, (TreeWithStringPath tree, bool comesFromLocalFilesystem)>.err(
                        "Failed to load from Git host: " + loadFromGitHost?.Error?.ToString());
                }

                return Result<string, (TreeWithStringPath tree, bool comesFromLocalFilesystem)>.ok(
                    (tree: loadFromGitHost.Success.tree, comesFromLocalFilesystem: false));
            }
            else
            {
                return Result<string, (TreeWithStringPath tree, bool comesFromLocalFilesystem)>.ok(
                    (tree: LoadFromLocalFilesystem.LoadSortedTreeFromPath(path), comesFromLocalFilesystem: true));
            }
        }
    }
}
