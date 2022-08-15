using static Pine.Composition;

namespace Pine;

static public class LoadFromPath
{
    static public Result<string, (TreeWithStringPath tree, bool comesFromLocalFilesystem)> LoadTreeFromPath(string path) =>
        LoadComposition.LoadFromPathResolvingNetworkDependencies(path)
        .ResultMap(loaded => (loaded!.Value.tree, loaded.Value.origin?.FromLocalFileSystem != null))
        .LogToActions(System.Console.WriteLine);
}
