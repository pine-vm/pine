using Newtonsoft.Json;
using System;
using System.Collections.Immutable;
using System.IO;
using System.Linq;
using System.Text;

namespace Kalmit.PersistentProcess.WebHost
{
    static public class BuildConfigurationFromArguments
    {
        public const string FrontendWebStaticFileName = "FrontendWeb.html";

        public const string ElmAppSubdirectoryName = "elm-app";

        static string StaticFilesSubdirectoryName => WebAppConfiguration.staticFilesDirectoryName;

        static public (
            string sourceCompositionId,
            Func<byte[]> compileConfigZipArchive,
            IImmutableDictionary<IImmutableList<string>, IImmutableList<byte>> loweredElmAppFiles)
            BuildConfigurationZipArchiveFromPath(
                string sourcePath,
                Action<string> verboseLogWriteLine)
        {
            var loadFromPathResult = LoadFromPath.LoadTreeFromPath(sourcePath);

            if (loadFromPathResult?.Ok == null)
            {
                throw new Exception("Failed to load from path '" + sourcePath + "': " + loadFromPathResult?.Err);
            }

            var sourceComposition = Composition.FromTree(loadFromPathResult.Ok);

            var sourceCompositionId = CommonConversion.StringBase16FromByteArray(Composition.GetHash(sourceComposition));

            Console.WriteLine("Loaded source composition " + sourceCompositionId + " from '" + sourcePath + "'.");

            var compileActionAndLoweredElmAppFiles =
                BuildConfigurationZipArchive(
                    sourceComposition: sourceComposition,
                    verboseLogWriteLine: verboseLogWriteLine);

            return
                (sourceCompositionId: sourceCompositionId,
                compileActionAndLoweredElmAppFiles.compileConfigZipArchive,
                compileActionAndLoweredElmAppFiles.loweredElmAppFiles);
        }

        static public (Func<byte[]> compileConfigZipArchive, IImmutableDictionary<IImmutableList<string>, IImmutableList<byte>> loweredElmAppFiles)
            BuildConfigurationZipArchive(
                Composition.Component sourceComposition,
                Action<string> verboseLogWriteLine)
        {
            Console.WriteLine(
                "Starting to build app from '" +
                CommonConversion.StringBase16FromByteArray(Composition.GetHash(sourceComposition)) +
                "'.");

            var parseSourceAsTree = Composition.ParseAsTree(sourceComposition);

            if (parseSourceAsTree.Ok == null)
                throw new Exception("Failed to map source to tree.");

            var sourceFiles =
                ElmApp.ToFlatDictionaryWithPathComparer(
                    parseSourceAsTree.Ok.EnumerateBlobsTransitive()
                    .Select(sourceFilePathAndContent =>
                        (path: (IImmutableList<string>)sourceFilePathAndContent.path.Select(pathComponent => Encoding.UTF8.GetString(pathComponent.ToArray())).ToImmutableList(),
                        sourceFilePathAndContent.blobContent))
                        .ToImmutableList());

            var elmAppSourceFiles =
                sourceFiles
                .Where(sourceFile => sourceFile.Key.FirstOrDefault() == "elm-app")
                .Select(sourceFile => (path: String.Join("/", sourceFile.Key.Skip(1)), content: sourceFile.Value))
                .ToImmutableList();

            var elmAppFilesBeforeLowering =
                ElmApp.ToFlatDictionaryWithPathComparer(
                    ElmApp.FilesFilteredForElmApp(elmAppSourceFiles)
                    .Select(filePathAndContent =>
                        ((IImmutableList<string>)filePathAndContent.filePath.Split(new[] { '/', '\\' }).ToImmutableList(),
                        filePathAndContent.fileContent)));

            Console.WriteLine("I found " + elmAppFilesBeforeLowering.Count + " files to build the Elm app.");

            var elmAppContainsFrontend = elmAppFilesBeforeLowering.ContainsKey(ElmAppInterfaceConvention.FrontendElmAppRootFilePath);

            Console.WriteLine("This Elm app contains " + (elmAppContainsFrontend ? "a" : "no") + " frontend at '" + string.Join("/", ElmAppInterfaceConvention.FrontendElmAppRootFilePath) + "'.");

            var loweredElmAppFiles = ElmApp.AsCompletelyLoweredElmApp(
                originalAppFiles: elmAppFilesBeforeLowering,
                originalSourceFiles: sourceFiles,
                ElmAppInterfaceConfig.Default,
                verboseLogWriteLine);

            var compileConfigFile = new Func<byte[]>(() =>
            {
                WebAppConfigurationJsonStructure jsonStructure = null;

                if (sourceFiles.TryGetValue(ImmutableList.Create(WebAppConfiguration.jsonFileName), out var jsonFile))
                {
                    Console.WriteLine("I found a file at '" + WebAppConfiguration.jsonFileName + "'. I use this to build the configuration.");

                    jsonStructure = JsonConvert.DeserializeObject<WebAppConfigurationJsonStructure>(
                        Encoding.UTF8.GetString(jsonFile.ToArray()));
                }
                else
                {
                    Console.WriteLine("I did not find a file at '" + WebAppConfiguration.jsonFileName + "'. I build the configuration without the 'elm-fullstack.json'.");
                }

                byte[] frontendWebFile = null;

                if (elmAppContainsFrontend)
                {
                    var frontendWebHtml = ProcessFromElm019Code.CompileElmToHtml(
                        loweredElmAppFiles,
                        ElmAppInterfaceConvention.FrontendElmAppRootFilePath,
                        elmMakeCommandAppendix: jsonStructure?.frontendWebElmMakeCommandAppendix);

                    frontendWebFile = Encoding.UTF8.GetBytes(frontendWebHtml);
                }

                // TODO: Simplify: Remove static files when apps are migrated to the new interface.
                var staticFilesFromFrontendWeb =
                    frontendWebFile == null ?
                    Array.Empty<(IImmutableList<string> name, IImmutableList<byte> content)>() :
                    new[] { (name: (IImmutableList<string>)ImmutableList.Create(FrontendWebStaticFileName), (IImmutableList<byte>)frontendWebFile.ToImmutableList()) };

                var staticFilesFromDirectory =
                    sourceFiles
                    .Where(sourceFile => sourceFile.Key.FirstOrDefault() == StaticFilesSubdirectoryName)
                    .Select(sourceFile => (path: (IImmutableList<string>)sourceFile.Key.Skip(1).ToImmutableList(), content: sourceFile.Value))
                    .ToImmutableList();

                Console.WriteLine("I found " + staticFilesFromDirectory.Count + " static files to include.");

                var staticFiles =
                    staticFilesFromDirectory.AddRange(staticFilesFromFrontendWeb);

                var webAppConfig =
                    new WebAppConfiguration()
                    .WithElmApp(elmAppFilesBeforeLowering)
                    .WithStaticFiles(staticFiles)
                    .WithJsonStructure(jsonStructure);

                var webAppConfigFiles =
                    sourceFiles.SetItems(webAppConfig.AsFiles());

                var webAppConfigFile = ZipArchive.ZipArchiveFromEntries(webAppConfigFiles);

                return webAppConfigFile;
            });

            return (compileConfigFile, loweredElmAppFiles);
        }

        static string FindDirectoryUpwardContainingElmJson(string searchBeginDirectory)
        {
            var currentDirectory = searchBeginDirectory;

            while (true)
            {
                if (!(0 < currentDirectory?.Length))
                    return null;

                if (File.Exists(Path.Combine(currentDirectory, "elm.json")))
                    return currentDirectory;

                currentDirectory = Path.GetDirectoryName(currentDirectory);
            }
        }
    }
}
