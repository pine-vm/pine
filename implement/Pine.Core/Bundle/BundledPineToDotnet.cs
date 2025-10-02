using Pine.Core.CodeAnalysis;
using Pine.Core.DotNet;
using System;
using System.Collections.Frozen;
using System.Collections.Generic;
using System.Reflection;

namespace Pine.Core.Bundle;

using CompiledDictionary =
    IReadOnlyDictionary<PineValue, Func<PineValue, PineValue?>>;

/// <summary>
/// Pine functions and programs compiled to .NET which are bundled with the assembly.
/// </summary>
public record BundledPineToDotnet(
    Func<Result<string, CompiledDictionary>> BuildDictionary)
{
    static public readonly System.Threading.Tasks.Task<BundledPineToDotnet> LoadBundledTask =
        System.Threading.Tasks.Task.Run(() => LoadFromEmbedded(Assembly.GetExecutingAssembly()));

    /// <summary>
    /// Default relative path of the embedded resource file that contains the .NET assembly.
    /// </summary>
    public const string ResourceFilePath = "prebuilt-artifact/pine-default-leaves.dll";

    public const string CompiledNamespacePrefix = "PrecompiledPineToDotNet";

    private static readonly DeclQualifiedName
        s_dispatcherDictionaryContainerTypeName =
        DeclQualifiedName.FromString(
            CompiledNamespacePrefix + ".Dispatcher");

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
        return
            new BundledPineToDotnet(
                () => CompileToAssembly.BuildDictionaryFromAssembly(
                    assemblyBytes,
                    s_dispatcherDictionaryContainerTypeName.FullName));
    }

    public static void BuildAndWriteBundleFile(
        ElmInteractiveEnvironment.ParsedInteractiveEnvironment parsedEnvironment,
        string? destinationDirectory = null)
    {
        var parseCache = new PineVMParseCache();

        var (staticProgram, declsFailed) =
            CodeAnalysis.CodeAnalysis.ParseAsStaticMonomorphicProgram(
                parsedEnvironment,
                includeDeclaration: IncludeDeclarationDefault,
                parseCache: parseCache)
            .Extract(err => throw new Exception("Failed parsing as static program: " + err));

        BuildAndWriteBundleFile(staticProgram, destinationDirectory: destinationDirectory);
    }

    private static bool IncludeDeclarationDefault(DeclQualifiedName name)
    {
        return s_includedDeclarationsDefault.Contains(name);
    }

    private static readonly FrozenSet<DeclQualifiedName> s_includedDeclarationsDefault =
        new[]
        {
            new DeclQualifiedName(["Basics"], "remainderBy"),
            new DeclQualifiedName(["Basics"], "modBy"),

            /*
            new DeclQualifiedName(["String"], "trim"),
            new DeclQualifiedName(["String"], "trimLeft"),
            new DeclQualifiedName(["String"], "trimRight"),

            new DeclQualifiedName(["String"], "toInt"),
            new DeclQualifiedName(["String"], "fromInt"),

            new DeclQualifiedName(["Dict"], "insert"),
            new DeclQualifiedName(["Dict"], "remove"),
            */
        }
        .ToFrozenSet();

    private static void BuildAndWriteBundleFile(
        StaticProgram staticProgram,
        string? destinationDirectory)
    {
        var fileContent = BuildBundleFile(staticProgram);

        WriteBundleFile(fileContent, destinationDirectory);
    }

    public static ReadOnlyMemory<byte> BuildBundleFile(
        StaticProgram staticProgram)
    {
        var asCSharp =
            StaticProgramCSharp.FromStaticProgram(
                staticProgram,
                DeclarationSyntaxContext.None);

        var compileToAssemblyResult =
            CompileToAssembly.Compile(
                asCSharp,
                namespacePrefix: CompiledNamespacePrefix.Split('.'),
                optimizationLevel: Microsoft.CodeAnalysis.OptimizationLevel.Release)
            .Extract(err =>
            throw new Exception("Compilation to assembly failed: " + err));

        return compileToAssemblyResult.Assembly;
    }

    private static void WriteBundleFile(
        ReadOnlyMemory<byte> fileContent,
        string? destinationDirectory = null)
    {
        Console.WriteLine(
            "Current working directory: " + Environment.CurrentDirectory);

        if (string.IsNullOrEmpty(destinationDirectory))
        {
            destinationDirectory = Environment.CurrentDirectory;
        }

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
}

