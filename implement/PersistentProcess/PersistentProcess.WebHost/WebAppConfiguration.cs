using Newtonsoft.Json;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text;

namespace Kalmit.PersistentProcess
{
    public class WebAppConfigurationJsonStructure
    {
        public ConditionalMapFromStringToString[] mapsFromRequestUrlToStaticFileName;

        public RateLimitWindow singleRateLimitWindowPerClientIPv4Address;

        public FluffySpoon.AspNet.LetsEncrypt.LetsEncryptOptions letsEncryptOptions;

        public string frontendWebElmMakeCommandAppendix;

        public class ConditionalMapFromStringToString
        {
            public string matchingRegexPattern;

            public string resultString;
        }

        public class RateLimitWindow
        {
            public int windowSizeInMs;

            public int limit;
        }
    }

    public class WebAppConfiguration
    {
        public WebAppConfigurationJsonStructure JsonStructure;

        public IReadOnlyCollection<(IImmutableList<string> staticFilePath, IImmutableList<byte> staticFileContent)> StaticFiles;

        public IReadOnlyCollection<(IImmutableList<string> filePath, IImmutableList<byte> fileContent)> ElmAppFiles;

        static string jsonFileName => "elm-fullstack.json";

        static public string staticFilesDirectoryName => "elm-fullstack-static-files";

        static string elmAppDirectoryName => "elm-app";

        static public WebAppConfiguration FromFiles(IReadOnlyCollection<(IImmutableList<string> path, IImmutableList<byte> content)> files)
        {
            var jsonStructureFile =
                files.FirstOrDefault(file => file.path.Count == 1 && String.Equals(file.path[0], jsonFileName, StringComparison.InvariantCultureIgnoreCase));

            IEnumerable<(IImmutableList<string> relativePath, IImmutableList<byte> fileContent)> GetFilesFromDirectory(
                IImmutableList<string> directoryPath) =>
                files
                .Where(file => file.path.Take(directoryPath.Count).SequenceEqual(directoryPath))
                .Select(file => (relativePath: (IImmutableList<string>)file.path.Skip(directoryPath.Count).ToImmutableList(), fileContent: file.content));

            var elmAppFiles =
                GetFilesFromDirectory(ImmutableList.Create(elmAppDirectoryName))
                .ToImmutableList();

            var jsonStructure =
                jsonStructureFile.content == null ? (WebAppConfigurationJsonStructure)null
                : JsonConvert.DeserializeObject<WebAppConfigurationJsonStructure>(Encoding.UTF8.GetString(jsonStructureFile.content.ToArray()));

            var staticFiles =
                GetFilesFromDirectory(ImmutableList.Create(staticFilesDirectoryName))
                .ToImmutableList();

            return new WebAppConfiguration
            {
                JsonStructure = jsonStructure,
                StaticFiles = staticFiles,
                ElmAppFiles = elmAppFiles,
            };
        }

        public IImmutableDictionary<IImmutableList<string>, IImmutableList<byte>> AsFiles()
        {
            var jsonStructureFileEntries =
                new[] { JsonStructure }
                .WhereNotNull()
                .Select(map => ((IImmutableList<string>)ImmutableList.Create(jsonFileName),
                    (IImmutableList<byte>)Encoding.UTF8.GetBytes(JsonConvert.SerializeObject(map)).ToImmutableList()))
                .ToList();

            var staticFilesEntries =
                StaticFiles
                ?.Select(fileNameAndContent =>
                    ((IImmutableList<string>)ImmutableList.Create(staticFilesDirectoryName).AddRange(fileNameAndContent.staticFilePath),
                    (IImmutableList<byte>)fileNameAndContent.staticFileContent.ToImmutableList()))
                .ToList();

            var elmAppFilesEntries =
                ElmAppFiles.EmptyIfNull()
                .Select(elmAppFilePathAndContent =>
                    ((IImmutableList<string>)ImmutableList.Create(elmAppDirectoryName).AddRange(elmAppFilePathAndContent.filePath),
                    elmAppFilePathAndContent.fileContent))
                .ToList();

            return
                jsonStructureFileEntries.EmptyIfNull()
                .Concat(staticFilesEntries.EmptyIfNull())
                .Concat(elmAppFilesEntries.EmptyIfNull())
                .ToImmutableDictionary(
                    filePathAndContent => (IImmutableList<string>)filePathAndContent.Item1,
                    filePathAndContent => filePathAndContent.Item2)
                .WithComparers(EnumerableExtension.EqualityComparer<string>());
        }

        public WebAppConfiguration WithJsonStructure(WebAppConfigurationJsonStructure jsonStructure) =>
            new WebAppConfiguration
            {
                JsonStructure = jsonStructure,
                StaticFiles = StaticFiles,
                ElmAppFiles = ElmAppFiles,
            };

        public WebAppConfiguration WithStaticFiles(IReadOnlyCollection<(IImmutableList<string> staticFilePath, IImmutableList<byte> staticFileContent)> staticFiles) =>
            new WebAppConfiguration
            {
                JsonStructure = JsonStructure,
                StaticFiles = staticFiles,
                ElmAppFiles = ElmAppFiles,
            };

        public WebAppConfiguration WithElmApp(IReadOnlyCollection<(IImmutableList<string> filePath, IImmutableList<byte> fileContent)> elmAppFiles) =>
            new WebAppConfiguration
            {
                JsonStructure = JsonStructure,
                StaticFiles = StaticFiles,
                ElmAppFiles = elmAppFiles,
            };

        public WebAppConfiguration WithElmApp(IImmutableDictionary<IImmutableList<string>, IImmutableList<byte>> elmAppFiles) =>
            WithElmApp(elmAppFiles.Select(entry => (entry.Key, entry.Value)).ToImmutableList());
    }
}
