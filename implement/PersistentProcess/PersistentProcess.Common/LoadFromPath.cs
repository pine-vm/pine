using static Kalmit.Composition;

namespace Kalmit
{
    static public class LoadFromPath
    {
        static public Result<string, (TreeComponent tree, bool comesFromLocalFilesystem)> LoadTreeFromPath(string path)
        {
            if (path.ToLowerInvariant().StartsWith("https://github.com"))
            {
                var loadFromGithubResult =
                    LoadFromGithub.LoadFromUrl(path);

                if (loadFromGithubResult?.Success == null)
                {
                    return Result<string, (TreeComponent tree, bool comesFromLocalFilesystem)>.err(
                        "Failed to load from Github: " + loadFromGithubResult?.Error?.ToString());
                }

                return Result<string, (TreeComponent tree, bool comesFromLocalFilesystem)>.ok(
                    (tree: loadFromGithubResult.Success, comesFromLocalFilesystem: false));
            }
            else
            {
                return Result<string, (TreeComponent tree, bool comesFromLocalFilesystem)>.ok(
                    (tree: LoadFromLocalFilesystem.LoadTreeFromPath(path), comesFromLocalFilesystem: true));
            }
        }
    }
}
