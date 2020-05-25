using static Kalmit.Composition;

namespace Kalmit
{
    static public class LoadFromPath
    {
        static public Result<string, TreeComponent> LoadTreeFromPath(string path)
        {
            if (path.ToLowerInvariant().StartsWith("https://github.com"))
            {
                var loadFromGithubResult =
                    LoadFromGithub.LoadFromUrl(path);

                if (loadFromGithubResult?.Success == null)
                {
                    return Result<string, TreeComponent>.err("Failed to load from Github: " + loadFromGithubResult?.Error?.ToString());
                }

                return Result<string, TreeComponent>.ok(loadFromGithubResult.Success);
            }
            else
            {
                return Result<string, TreeComponent>.ok(LoadFromLocalFilesystem.LoadTreeFromPath(path));
            }
        }
    }
}
