using Newtonsoft.Json;
using System;
using System.Collections.Immutable;
using System.IO;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;

namespace Kalmit.PersistentProcess.WebHost
{
    static public class BuildConfigurationFromArguments
    {
        public const string FrontendWebStaticFileName = "FrontendWeb.html";

        public const string ElmAppSubdirectoryName = "elm-app";

        public static IImmutableList<string> FrontendElmAppRootFilePath =>
            ImmutableList.Create("src", "FrontendWeb", "Main.elm");

        static public (Func<byte[]> compileConfigZipArchive, IImmutableDictionary<IImmutableList<string>, IImmutableList<byte>> loweredElmAppFiles)
            BuildConfigurationZipArchive(
            string frontendWebElmMakeCommandAppendix,
            Action<string> verboseLogWriteLine)
        {
            var currentDirectory = Environment.CurrentDirectory;

            Console.WriteLine("The currentDirectory is '" + currentDirectory + "'.");

            var elmAppFilesBeforeLowering =
                ElmApp.ToFlatDictionaryWithPathComparer(
                    ElmApp.FilesFilteredForElmApp(
                        Filesystem.GetAllFilesFromDirectory(Path.Combine(currentDirectory, ElmAppSubdirectoryName)))
                    .Select(filePathAndContent =>
                        ((IImmutableList<string>)filePathAndContent.filePath.Split(new[] { '/', '\\' }).ToImmutableList(),
                        filePathAndContent.fileContent)));

            Console.WriteLine("I found " + elmAppFilesBeforeLowering.Count + " files to build the Elm app.");

            var elmAppContainsFrontend = elmAppFilesBeforeLowering.ContainsKey(FrontendElmAppRootFilePath);

            Console.WriteLine("This Elm app contains " + (elmAppContainsFrontend ? "a" : "no") + " frontend at '" + string.Join("/", FrontendElmAppRootFilePath) + "'.");

            var loweredElmAppFiles = ElmApp.AsCompletelyLoweredElmApp(
                elmAppFilesBeforeLowering,
                ElmAppInterfaceConfig.Default,
                verboseLogWriteLine);

            var compileConfigFile = new Func<byte[]>(() =>
            {
                byte[] frontendWebFile = null;

                if (elmAppContainsFrontend)
                {
                    var frontendWebHtml = ProcessFromElm019Code.CompileElmToHtml(
                        loweredElmAppFiles,
                        FrontendElmAppRootFilePath,
                        frontendWebElmMakeCommandAppendix);

                    frontendWebFile = Encoding.UTF8.GetBytes(frontendWebHtml);
                }

                WebAppConfigurationJsonStructure jsonStructure = null;

                var jsonFileSearchPath = Path.Combine(currentDirectory, "elm-fullstack.json");

                if (File.Exists(jsonFileSearchPath))
                {
                    Console.WriteLine("I found a file at '" + jsonFileSearchPath + "'. I use this to build the configuration.");

                    var jsonFile = File.ReadAllBytes(jsonFileSearchPath);

                    jsonStructure = JsonConvert.DeserializeObject<WebAppConfigurationJsonStructure>(Encoding.UTF8.GetString(jsonFile));
                }
                else
                {
                    Console.WriteLine("I did not find a file at '" + jsonFileSearchPath + "'. I build the configuration without the 'elm-fullstack.json'.");
                }

                var staticFiles =
                    frontendWebFile == null ?
                    Array.Empty<(IImmutableList<string> name, IImmutableList<byte> content)>() :
                    new[] { (name: (IImmutableList<string>)ImmutableList.Create(FrontendWebStaticFileName), (IImmutableList<byte>)frontendWebFile.ToImmutableList()) };

                var webAppConfig =
                    new WebAppConfiguration()
                    .WithElmApp(loweredElmAppFiles)
                    .WithStaticFiles(staticFiles)
                    .WithJsonStructure(jsonStructure);

                var webAppConfigFiles =
                    webAppConfig.AsFiles();

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
