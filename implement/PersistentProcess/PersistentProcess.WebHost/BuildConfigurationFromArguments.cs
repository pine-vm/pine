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

        static public void BuildConfiguration(string[] args)
        {
            string argumentValueFromParameterName(string parameterName) =>
                args
                .Select(arg => Regex.Match(arg, parameterName + "=(.*)", RegexOptions.IgnoreCase))
                .FirstOrDefault(match => match.Success)
                ?.Groups[1].Value;

            var outputArgument = argumentValueFromParameterName("--output");

            var frontendWebElmSource = argumentValueFromParameterName("--frontend-web-elm-source");

            var currentDirectory = Environment.CurrentDirectory;

            Console.WriteLine(
                "The currentDirectory is '" + currentDirectory + "', frontendWebElmSource is '" + frontendWebElmSource + "'.");

            var elmAppFiles =
                ElmApp
                .FilesFilteredForElmApp(Filesystem.GetAllFilesFromDirectory(Path.Combine(currentDirectory, ElmAppSubdirectoryName)))
                .ToImmutableList();

            Console.WriteLine("I found " + elmAppFiles.Count + " files to build the Elm app.");

            byte[] frontendWebFile = null;

            if (0 < frontendWebElmSource?.Length)
            {
                var frontendWebElmSearchBegin =
                    Path.IsPathRooted(frontendWebElmSource) ? frontendWebElmSource :
                    Path.Combine(currentDirectory, frontendWebElmSource);

                Console.WriteLine("I begin to search for an Elm app at '" + frontendWebElmSearchBegin + "'.");

                var frontendWebElmAppRootDirectory = FindDirectoryUpwardContainingElmJson(frontendWebElmSearchBegin);

                if (frontendWebElmAppRootDirectory == null)
                {
                    var errorMessage = "I did not find a directory containing the frontend Elm app.";
                    Console.WriteLine(errorMessage);
                    throw new ArgumentException(errorMessage);
                }

                Console.WriteLine("I found an Elm app in directory '" + frontendWebElmAppRootDirectory + "'.");

                var frontendWebElmSourceFileName = Path.GetFileName(frontendWebElmSource);

                var frontendWebElmCodeFiles =
                    ElmApp
                    .FilesFilteredForElmApp(Filesystem.GetAllFilesFromDirectory(frontendWebElmAppRootDirectory))
                    .ToImmutableList();

                Console.WriteLine("I found " + frontendWebElmCodeFiles.Count + " files to build the frontend Elm app.");

                var pathToEntryPointFile =
                    Path.GetRelativePath(frontendWebElmAppRootDirectory, frontendWebElmSearchBegin);

                var frontendWebHtml = ProcessFromElm019Code.CompileElmToHtml(frontendWebElmCodeFiles, pathToEntryPointFile);

                frontendWebFile = Encoding.UTF8.GetBytes(frontendWebHtml);
            }

            WebAppConfigurationMap map = null;

            var mapFileSearchPath = Path.Combine(currentDirectory, "map.json");

            if (File.Exists(mapFileSearchPath))
            {
                Console.WriteLine("I found a file at '" + mapFileSearchPath + "'. I use this to build the configuration.");

                var mapFile = File.ReadAllBytes(mapFileSearchPath);

                map = JsonConvert.DeserializeObject<WebAppConfigurationMap>(Encoding.UTF8.GetString(mapFile));
            }
            else
            {
                Console.WriteLine("I did not find a file at '" + mapFileSearchPath + "'. I build the configuration without the 'map.json'.");
            }

            var staticFiles =
                frontendWebFile == null ?
                Array.Empty<(string name, byte[] content)>() :
                new[] { (name: FrontendWebStaticFileName, frontendWebFile) };

            var webAppConfig =
                new WebAppConfiguration()
                .WithElmApp(ZipArchive.ZipArchiveFromEntries(elmAppFiles))
                .WithStaticFiles(staticFiles)
                .WithMap(map);

            var webAppConfigFile = ZipArchive.ZipArchiveFromEntries(webAppConfig.AsFiles());

            var webAppConfigFileId = CommonConversion.StringBase16FromByteArray(CommonConversion.HashSHA256(webAppConfigFile));

            Console.WriteLine("I built web app config " + webAppConfigFileId + ".");

            if (outputArgument == null)
            {
                Console.WriteLine("I did not see a path for output, so I don't attempt to save the configuration to a file.");
            }
            else
            {
                Directory.CreateDirectory(Path.GetDirectoryName(outputArgument));
                File.WriteAllBytes(outputArgument, webAppConfigFile);

                Console.WriteLine("I saved web app config " + webAppConfigFileId + " to '" + outputArgument + "'");
            }
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
