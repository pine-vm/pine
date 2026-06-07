using Pine.Core.CodeAnalysis;
using Pine.Core.DotNet;
using System;
using System.Collections.Frozen;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;

namespace Pine.Core.Bundle;

using CompiledDictionary =
    IReadOnlyDictionary<PineValue, Func<PineValue, PineValue?>>;

using FileTree =
    IReadOnlyDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>>;

/// <summary>
/// Pine functions and programs compiled to .NET which are bundled with the assembly.
/// </summary>
public record BundledPineToDotnet(
    Func<CompiledDictionary> BuildDictionary)
{
    static public readonly System.Threading.Tasks.Task<BundledPineToDotnet> LoadBundledTask =
        System.Threading.Tasks.Task.Run(() => LoadFromEmbedded(Assembly.GetExecutingAssembly()));

    /// <summary>
    /// Default relative path of the embedded resource file that contains the .NET assembly.
    /// </summary>
    public const string ResourceFilePath = "prebuilt-artifact/pine-default-leaves.dll";

    public const string CompiledNamespacePrefix = "PrecompiledPineToDotNet";

    /// <summary>
    /// Loads <see cref="BundledPineToDotnet"/> from an assembly's embedded resource using <see cref="ResourceFilePath"/>.
    /// Returns <c>null</c> on failure and logs a message to <see cref="Console"/>.
    /// </summary>
    /// <param name="assembly">The assembly containing the embedded resource.</param>
    /// <returns>The loaded <see cref="BundledPineToDotnet"/>, or <c>null</c> if loading failed.</returns>
    public static BundledPineToDotnet? LoadFromEmbedded(
        Assembly assembly) =>
        LoadEmbedded(assembly)
        .Extract(
            err =>
            {
                Console.WriteLine("Failed loading from embedded resource: " + err);

                return null;
            });

    public static Result<string, BundledPineToDotnet> LoadEmbedded(
        Assembly assembly,
        string embeddedResourceFilePath = ResourceFilePath)
    {
        var manifestEmbeddedProvider =
            new Microsoft.Extensions.FileProviders.ManifestEmbeddedFileProvider(assembly);

        var embeddedFileInfo = manifestEmbeddedProvider.GetFileInfo(embeddedResourceFilePath);

        if (!embeddedFileInfo.Exists)
        {
            return "Did not find file " + embeddedResourceFilePath + " in assembly " + assembly.FullName;
        }

        if (embeddedFileInfo.Length is 0)
        {
            return "File " + embeddedResourceFilePath + " in assembly " + assembly.FullName + " is empty";
        }

        using var readStream = embeddedFileInfo.CreateReadStream();

        using var memoryStream = new System.IO.MemoryStream();

        readStream.CopyTo(memoryStream);

        var asMemory = memoryStream.ToArray();

        return LoadFromAssembly(asMemory);
    }

    public static Result<string, BundledPineToDotnet> LoadFromAssembly(
        byte[] assemblyBytes)
    {
        var searchResult =
            CompileToAssembly.SearchDictionaryBuilderInAssembly(assemblyBytes);

        {
            if (searchResult.IsErrOrNull() is { } err)
            {
                return
                    "Failed to find dictionary builder in compiled assembly: " + err;
            }
        }

        if (searchResult.IsOkOrNull() is not { } buildDictionary)
        {
            throw new NotImplementedException(
                "Unexpected return type: " + searchResult.GetType().FullName);
        }

        return new BundledPineToDotnet(buildDictionary);
    }

    public static void BuildAndWriteBundleFile(
        ElmInteractiveEnvironment.ParsedInteractiveEnvironment parsedEnvironment,
        string destinationDirectory,
        Action<string> logger,
        bool writeCSharpFilesArchive)
    {
        var parseCache = new PineVMParseCache();

        var (staticProgram, functionMetadata, declsFailed) =
            CodeAnalysis.CodeAnalysis.ParseAsStaticMonomorphicProgram(
                parsedEnvironment,
                includeDeclaration: IncludeDeclarationDefault,
                parseCache: parseCache)
            .Extract(err => throw new Exception("Failed parsing as static program: " + err));

        foreach (var decl in declsFailed)
        {
            logger(
                "Warning: Declaration failed to parse and was not included in the bundle: " +
                decl.Key.FullName + ": " + decl.Value);
        }

        BuildAndWriteBundleFile(
            staticProgram,
            functionMetadata,
            destinationDirectory: destinationDirectory,
            logger: logger,
            writeCSharpFilesArchive);
    }

    private static bool IncludeDeclarationDefault(DeclQualifiedName name)
    {
        return s_includedDeclarationsDefault.Contains(name);
    }

    private static readonly FrozenSet<DeclQualifiedName> s_includedDeclarationsDefault =
        new[]
        {
            DeclQualifiedName.Create(["Basics"], "remainderBy"),
            DeclQualifiedName.Create(["Basics"], "modBy"),

            DeclQualifiedName.Create(["String"], "trim"),
            DeclQualifiedName.Create(["String"], "trimLeft"),
            DeclQualifiedName.Create(["String"], "trimRight"),

            DeclQualifiedName.Create(["String"], "split"),
            DeclQualifiedName.Create(["String"], "padLeft"),
            DeclQualifiedName.Create(["String"], "padRight"),

            DeclQualifiedName.Create(["String"], "contains"),
            DeclQualifiedName.Create(["String"], "indexes"),

            DeclQualifiedName.Create(["String"], "toInt"),
            DeclQualifiedName.Create(["String"], "fromInt"),

            DeclQualifiedName.Create(["Dict"], "size"),
            DeclQualifiedName.Create(["Dict"], "insert"),
            DeclQualifiedName.Create(["Dict"], "remove"),

            DeclQualifiedName.Create(["Json","Decode"], "parseJsonStringToValue"),
            DeclQualifiedName.Create(["Json","Encode"], "encode"),

            DeclQualifiedName.Create(["Bitwise"], "and"),
            DeclQualifiedName.Create(["Bitwise"], "complement"),
            DeclQualifiedName.Create(["Bitwise"], "or"),
            DeclQualifiedName.Create(["Bitwise"], "xor"),
            DeclQualifiedName.Create(["Bitwise"], "shiftLeftBy"),
            DeclQualifiedName.Create(["Bitwise"], "shiftRightBy"),
            DeclQualifiedName.Create(["Bitwise"], "shiftRightZfBy"),

            DeclQualifiedName.Create(["Pine"], "stringFromValue"),
            DeclQualifiedName.Create(["Pine"], "valueFromString"),
            DeclQualifiedName.Create(["Pine"], "applyKernelFunction"),

            /*
             * Would benefit more from memoization, not clear which variant is more efficient at the moment.
             * 
            DeclQualifiedName.Create(["Pine"], "evaluateExpression"),
            DeclQualifiedName.Create(["Pine"], "encodeExpressionAsValue"),
            DeclQualifiedName.Create(["Pine"], "parseExpressionFromValue"),
            */
        }
        .ToFrozenSet();

    private static void BuildAndWriteBundleFile(
        StaticProgram<DeclQualifiedName> staticProgram,
        IReadOnlyDictionary<DeclQualifiedName, StaticProgramFunctionMetadata> functionMetadata,
        string destinationDirectory,
        Action<string> logger,
        bool writeCSharpFilesArchive)
    {
        var csharpFiles =
            BuildBundleCSharpFiles(staticProgram, functionMetadata);

        var csharpFilesAggregateSize =
            csharpFiles.Values
            .Sum(fileContent => fileContent.Length);

        logger(
            "Compiled static program to " +
            CommandLineInterface.FormatIntegerForDisplay(csharpFiles.Count) +
            " C# files, totaling " +
            CommandLineInterface.FormatIntegerForDisplay(csharpFilesAggregateSize) +
            " bytes of C# code:");

        foreach (var fileDescription in DescribeFileTree(csharpFiles))
        {
            logger(fileDescription);
        }

        BuildAndWriteBundleFileAssembly(
            csharpFiles,
            destinationDirectory,
            logger);

        if (writeCSharpFilesArchive)
        {
            WriteBundleArchiveFile(csharpFiles, destinationDirectory, logger);
        }
    }

    private static IEnumerable<string> DescribeFileTree(FileTree files)
    {
        var filesSorted =
            files
            .OrderBy(kv => kv.Key, EnumerableExtensions.Comparer<IReadOnlyList<string>>());

        foreach (var file in filesSorted)
        {
            yield return
                " - " + string.Join('/', file.Key) + ": " +
                CommandLineInterface.FormatIntegerForDisplay(file.Value.Length) +
                " bytes";
        }
    }

    public static ReadOnlyMemory<byte> LoadCSharpFilesFromFileAndBuildBundleFileAssembly(
        string csharpFilesDirectory,
        string destinationDirectory,
        Action<string> logger)
    {
        var csharpFiles =
            FileTreeFromPath(csharpFilesDirectory);

        logger(
            "Loaded " +
            CommandLineInterface.FormatIntegerForDisplay(csharpFiles.Count) +
            " C# files from " + csharpFilesDirectory + ", totaling " +
            CommandLineInterface.FormatIntegerForDisplay(csharpFiles.Values.Sum(v => v.Length)) +
            " bytes of C# code:");

        foreach (var fileDescription in DescribeFileTree(csharpFiles))
        {
            logger(fileDescription);
        }

        return
            BuildAndWriteBundleFileAssembly(
                csharpFiles,
                destinationDirectory,
                logger);
    }

    private static FileTree FileTreeFromPath(string fileSystemPath)
    {
        var fileContent = System.IO.File.ReadAllBytes(fileSystemPath);

        if (fileSystemPath.EndsWith(".tar.gz", StringComparison.InvariantCultureIgnoreCase) ||
            fileSystemPath.EndsWith(".tgz", StringComparison.InvariantCultureIgnoreCase))
        {
            return TarGZipArchive.ExtractArchive(fileContent);
        }
        else
        {
            throw new NotImplementedException("Unsupported file type: " + fileSystemPath);
        }
    }

    public static ReadOnlyMemory<byte> BuildAndWriteBundleFileAssembly(
        FileTree csharpFiles,
        string destinationDirectory,
        Action<string> logger)
    {
        var assemblyBytes =
            BuildBundleFileAssemblyFromCSharpFiles(csharpFiles);

        logger(
            "Compiled C# files to " +
            CommandLineInterface.FormatIntegerForDisplay(assemblyBytes.Length) + " bytes of .NET assembly.");

        WriteBundleFile(assemblyBytes, destinationDirectory);

        return assemblyBytes;
    }

    public static FileTree BuildBundleCSharpFiles(
        StaticProgram<DeclQualifiedName> staticProgram,
        IReadOnlyDictionary<DeclQualifiedName, StaticProgramFunctionMetadata> functionMetadata)
    {
        var asCSharp =
            StaticProgramCSharp.FromStaticProgram(staticProgram, functionMetadata);

        var csharpFiles =
            asCSharp.BuildCSharpProjectFiles(
                namespacePrefix: CompiledNamespacePrefix.Split('.'));

        return csharpFiles;
    }

    public static ReadOnlyMemory<byte> BuildBundleFileAssemblyFromCSharpFiles(
        FileTree csharpFiles)
    {
        var compileToAssemblyResult =
            CompileToAssembly.Compile(
                csharpFiles,
                optimizationLevel: Microsoft.CodeAnalysis.OptimizationLevel.Debug)
            .Extract(
                err =>
                throw new Exception("Compilation to assembly failed: " + err));

        return compileToAssemblyResult.Assembly;
    }

    private static void WriteBundleFile(
        ReadOnlyMemory<byte> fileContent,
        string destinationDirectory)
    {
        Console.WriteLine(
            "Current working directory: " + Environment.CurrentDirectory);

        Console.WriteLine(
            "Using destination directory: " + destinationDirectory);

        var resourceFilePathRel =
            System.IO.Path.Combine(destinationDirectory, ResourceFilePath);

        var absolutePath = System.IO.Path.GetFullPath(resourceFilePathRel);

        Console.WriteLine(
            "Resolved the destination path of " + resourceFilePathRel +
            " to " + absolutePath);

        System.IO.Directory.CreateDirectory(
            System.IO.Path.GetDirectoryName(absolutePath));

        System.IO.File.WriteAllBytes(
            absolutePath,
            fileContent.ToArray());

        Console.WriteLine(
            "Saved the prebuilt dictionary to " + absolutePath);
    }

    private static void WriteBundleArchiveFile(
        FileTree csharpFiles,
        string destinationDirectory,
        Action<string> logger)
    {
        var archiveBytes = TarGZipArchive.CreateArchive(csharpFiles);

        // Write archive file in the same directory as the assembly file
        var assemblyDirectory =
            System.IO.Path.GetDirectoryName(
                System.IO.Path.Combine(destinationDirectory, ResourceFilePath));

        var archiveFileName = "distilled-csharp-files.tar.gz";
        var archiveFilePath = System.IO.Path.Combine(assemblyDirectory!, archiveFileName);
        var absoluteArchivePath = System.IO.Path.GetFullPath(archiveFilePath);

        System.IO.File.WriteAllBytes(
            absoluteArchivePath,
            archiveBytes.ToArray());

        logger(
            "Created C# files archive with " +
            CommandLineInterface.FormatIntegerForDisplay(archiveBytes.Length) +
            " bytes (compressed from " +
            CommandLineInterface.FormatIntegerForDisplay(csharpFiles.Values.Sum(v => v.Length)) +
            " bytes) at " + absoluteArchivePath);
    }
}


