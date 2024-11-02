using Pine.Core;
using Pine.Core.Elm;
using Pine.Elm;
using System.Collections.Immutable;
using System.Linq;

namespace prebuild;

public class Program
{
    public const string DestinationFilePath = "./Pine.Core/" + ReusedInstances.EmbeddedResourceFilePath;

    public const string PreviousCompilerFilePath =
        "./history/2024-11-01-compiler-bundle/elm-syntax-parser-and-compiler.json.gzip";

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

        BundleElmCompiler();
    }

    public static ElmCompiler LoadPreviousCompiler()
    {
        using var sourceFile =
            new System.IO.FileStream(
                path: PreviousCompilerFilePath,
                System.IO.FileMode.Open,
                System.IO.FileAccess.Read);

        var envDict =
            BundledElmEnvironments.LoadBundledCompiledEnvironments(sourceFile, gzipDecompress: true)
            .Extract(err => throw new System.Exception(err));

        var compiledEnv =
            envDict.Values
            .OfType<PineValue.ListValue>()
            .OrderByDescending(list => list.NodesCount)
            .First();

        return
            ElmCompiler.ElmCompilerFromEnvValue(compiledEnv)
            .Extract(err => throw new System.Exception(err));
    }

    public static void BundleElmCompiler()
    {
        var previousCompiler = LoadPreviousCompiler();

        var elmCompilerSource =
            ElmCompiler.CompilerSourceFilesDefault.Value;

        if (true)
        {
            var elmCompiler =
                ElmCompiler.BuildCompilerFromSourceFiles(
                    elmCompilerSource,
                    overrideElmCompiler: previousCompiler)
                .Extract(err => throw new System.Exception(err));

            BundledElmEnvironments.CompressAndWriteBundleFile(
                ImmutableDictionary<TreeNodeWithStringPath, PineValue>.Empty
                .SetItem(elmCompilerSource, elmCompiler.CompilerEnvironment));
        }
        else
        {
            var elmCompilerSourceZipArchivePath =
                "./elm-compiler-source.zip";

            var zipArchive = Pine.ZipArchive.ZipArchiveFromEntries(
                elmCompilerSource.EnumerateBlobsTransitive()
                .Select(entry => (string.Join("/", entry.path), entry.blobContent)));

            System.IO.File.WriteAllBytes(
                elmCompilerSourceZipArchivePath,
                zipArchive);

            var cliFilePath = "TODO: Download from GitHub Releases";

            var process =
                System.Diagnostics.Process.Start(
                    cliFilePath,
                    arguments:
                    string.Join(
                        "  ",
                        ["compile-interactive-env"
                        ,"--env-source=" + elmCompilerSourceZipArchivePath
                        ,"--output-compact-build=compact-build.json"
                        ]));

            process.WaitForExit();

            System.Console.WriteLine("Process exited with code " + process.ExitCode);

            var fileContent = System.IO.File.ReadAllBytes("compact-build.json");

            System.Console.WriteLine("Read " + fileContent.Length + " bytes from compact-build.json");

            BundledElmEnvironments.CompressAndWriteBundleFile(fileContent);
        }
    }
}