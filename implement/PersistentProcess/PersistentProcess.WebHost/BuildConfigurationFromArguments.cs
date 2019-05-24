using System;
using System.Collections.Immutable;
using System.IO;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using Microsoft.AspNetCore.Hosting;
using Newtonsoft.Json;

namespace Kalmit.PersistentProcess.WebHost
{
    static public class BuildConfigurationFromArguments
    {
        public const string ClientHtmlStaticFileName = "client.html";

        static public IWebHostBuilder BuildConfiguration(
            this IWebHostBuilder webHostBuilder,
            string[] args)
        {
            string argumentValueFromParameterName(string parameterName) =>
                args
                .Select(arg => Regex.Match(arg, parameterName + "=(.*)", RegexOptions.IgnoreCase))
                .FirstOrDefault(match => match.Success)
                ?.Groups[1].Value;

            var clientAppSource = argumentValueFromParameterName("--client-app-source");

            var currentDirectory = Environment.CurrentDirectory;

            Console.WriteLine(
                "I build the configuration before starting the server. The currentDirectory is '" +
                currentDirectory + "', clientAppSource is '" + clientAppSource + "'.");

            var tempConfigDirectory = Path.Combine(currentDirectory, ".kalmit", "temp-config");
            var webAppConfigFilePath = Path.Combine(tempConfigDirectory, "web-app-config.zip");
            var processStoreDirectoryPathDefault = Path.Combine(tempConfigDirectory, "process-store");

            Directory.CreateDirectory(tempConfigDirectory);

            webHostBuilder.WithSettingWebAppConfigurationFilePath(webAppConfigFilePath);

            //  Provide a default location for the process store, which can be overridden.
            webHostBuilder.WithSettingProcessStoreDirectoryPathDefault(processStoreDirectoryPathDefault);

            var serverElmAppFiles =
                ElmAppWithEntryConfig
                .FromFilesFilteredForElmApp(Filesystem.GetAllFilesFromDirectory(currentDirectory))
                .AsFiles();

            Console.WriteLine("I found " + serverElmAppFiles.Count + " Elm app files to build the server app.");

            byte[] clientElmApp = null;

            if (0 < clientAppSource?.Length)
            {
                var clientElmAppSearchBegin =
                    Path.IsPathRooted(clientAppSource) ? clientAppSource : Path.Combine(currentDirectory, clientAppSource);

                Console.WriteLine("I begin to search for an Elm app at '" + clientElmAppSearchBegin + "'.");

                var clientElmAppRootDirectory = FindDirectoryUpwardContainingElmJson(clientElmAppSearchBegin);

                if (clientElmAppRootDirectory == null)
                {
                    var errorMessage = "I did not find a directory containing the client Elm app.";
                    Console.WriteLine(errorMessage);
                    throw new ArgumentException(errorMessage);
                }

                Console.WriteLine("I found an Elm app in directory '" + clientElmAppRootDirectory + "'.");

                var clientAppSourceFileName = Path.GetFileName(clientAppSource);

                var clientElmCodeFiles =
                    ElmAppWithEntryConfig
                    .FromFilesFilteredForElmApp(
                        Filesystem.GetAllFilesFromDirectory(clientElmAppRootDirectory)
                        .Select(file => ("elm-app/" + file.name, file.content))
                        .ToImmutableList())
                    .AsFiles()
                    .Select(file => (name: file.name.Substring(8), file.content))
                    .ToImmutableList();

                Console.WriteLine("I found " + clientElmCodeFiles.Count + " Elm app files to build the client app.");

                var pathToEntryPointFile =
                    clientElmCodeFiles
                    .Select(file => file.name)
                    .FirstOrDefault(candidate => Path.GetFileName(candidate) == clientAppSourceFileName);

                var clientHtml = ProcessFromElm019Code.CompileElmToHtml(clientElmCodeFiles, pathToEntryPointFile);

                clientElmApp = Encoding.UTF8.GetBytes(clientHtml);
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
                clientElmApp == null ?
                Array.Empty<(string name, byte[] content)>() :
                new[] { (name: ClientHtmlStaticFileName, clientElmApp) };

            var webAppConfig =
                new WebAppConfiguration()
                .WithElmApp(ZipArchive.ZipArchiveFromEntries(serverElmAppFiles))
                .WithStaticFiles(staticFiles)
                .WithMap(map);

            var webAppConfigFile = ZipArchive.ZipArchiveFromEntries(webAppConfig.AsFiles());

            Console.WriteLine("I built web app config " +
                CommonConversion.StringBase16FromByteArray(CommonConversion.HashSHA256(webAppConfigFile)) +
                " to use for the server.");

            File.WriteAllBytes(webAppConfigFilePath, webAppConfigFile);

            return webHostBuilder;
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
