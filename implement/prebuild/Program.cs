using Pine.Core;

namespace prebuild;

public class Program
{
    public const string DestinationFilePath = "./Pine.Core/" + ReusedInstances.EmbeddedResourceFilePath;

    public static void Main()
    {
        System.Console.WriteLine(
            "Current working directory: " + System.Environment.CurrentDirectory);

        var fromFreshBuild =
            ReusedInstances.BuildPineListValueReusedInstances(
                ReusedInstances.ExpressionsSource());

        var file =
            ReusedInstances.BuildPrecompiledDictFile(fromFreshBuild);

        var absolutePath = System.IO.Path.GetFullPath(DestinationFilePath);

        System.Console.WriteLine(
            "Resolved the destination path of " + DestinationFilePath +
            " to " + absolutePath);

        System.IO.Directory.CreateDirectory(
            System.IO.Path.GetDirectoryName(absolutePath));

        System.IO.File.WriteAllBytes(
            absolutePath,
            file.ToArray());

        System.Console.WriteLine(
            "Saved the prebuilt dictionary with " +
            fromFreshBuild.PineValueLists.Count + " list values to " + absolutePath);
    }
}