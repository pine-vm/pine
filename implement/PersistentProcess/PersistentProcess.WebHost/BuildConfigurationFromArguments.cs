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

        static public void BuildConfiguration(string[] args)
        {
            string argumentValueFromParameterName(string parameterName) =>
                args
                .Select(arg => Regex.Match(arg, parameterName + "=(.*)", RegexOptions.IgnoreCase))
                .FirstOrDefault(match => match.Success)
                ?.Groups[1].Value;

            var outputArgument = argumentValueFromParameterName("--output");

            var loweredElmOutputArgument = argumentValueFromParameterName("--lowered-elm-output");

            //  TODO: Remove redundancy: Probably move these arguments towards integrating app into the CLI project.

            var frontendWebElmMakeCommandAppendix = argumentValueFromParameterName("--frontend-web-elm-make-appendix");

            var (configZipArchive, loweredElmAppFiles) = BuildConfigurationZipArchive(frontendWebElmMakeCommandAppendix);

            if (0 < loweredElmOutputArgument?.Length)
            {
                Console.WriteLine("I write the lowered Elm app to '" + loweredElmOutputArgument + "'.");

                foreach (var file in loweredElmAppFiles)
                {
                    var outputPath = Path.Combine(new[] { loweredElmOutputArgument }.Concat(file.Key).ToArray());
                    Directory.CreateDirectory(Path.GetDirectoryName(outputPath));
                    File.WriteAllBytes(outputPath, file.Value.ToArray());
                }
            }

            var configZipArchiveFileId =
                CommonConversion.StringBase16FromByteArray(CommonConversion.HashSHA256(configZipArchive));

            var webAppConfigFileId =
                Composition.GetHash(Composition.FromTree(Composition.TreeFromSetOfBlobsWithCommonFilePath(
                    ZipArchive.EntriesFromZipArchive(configZipArchive))));

            Console.WriteLine(
                "I built zip archive " + configZipArchiveFileId + " containing web app config " + webAppConfigFileId + ".");

            if (outputArgument == null)
            {
                Console.WriteLine("I did not see a path for output, so I don't attempt to save the configuration to a file.");
            }
            else
            {
                Directory.CreateDirectory(Path.GetDirectoryName(outputArgument));
                File.WriteAllBytes(outputArgument, configZipArchive);

                Console.WriteLine("I saved zip arcchive " + configZipArchiveFileId + " to '" + outputArgument + "'");
            }
        }

        static public (byte[] configZipArchive, IImmutableDictionary<IImmutableList<string>, IImmutableList<byte>> loweredElmAppFiles)
            BuildConfigurationZipArchive(
            string frontendWebElmMakeCommandAppendix)
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
                elmAppFilesBeforeLowering, ElmAppInterfaceConfig.Default);

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

            return (webAppConfigFile, loweredElmAppFiles);
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
