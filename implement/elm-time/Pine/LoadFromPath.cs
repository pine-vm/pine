namespace Pine;

public static class LoadFromPath
{
    public static Result<string, (TreeNodeWithStringPath tree, bool comesFromLocalFilesystem)> LoadTreeFromPath(string path) =>
        LoadComposition.LoadFromPathResolvingNetworkDependencies(path)
        .ResultMap(loaded => (loaded.tree, loaded.origin?.FromLocalFileSystem != null))
        .LogToActions(System.Console.WriteLine);
}
