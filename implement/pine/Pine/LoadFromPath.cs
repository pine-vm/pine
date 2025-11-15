using Pine.Core;
using Pine.Core.Files;

namespace Pine;

public static class LoadFromPath
{
    public static Result<string, (FileTree tree, bool comesFromLocalFilesystem)> LoadTreeFromPath(string path) =>
        LoadComposition.LoadFromPathResolvingNetworkDependencies(path)
        .ResultMap(loaded => (loaded.tree, loaded.origin is LoadCompositionOrigin.FromLocalFileSystem))
        .LogToActions(System.Console.WriteLine);
}
