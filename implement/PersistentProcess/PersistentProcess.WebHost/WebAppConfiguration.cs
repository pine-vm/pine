using Newtonsoft.Json;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;

namespace Kalmit.PersistentProcess
{
    public class WebAppConfigurationJsonStructure
    {
        public ConditionalMapFromStringToString[] mapsFromRequestUrlToStaticFileName;

        public RateLimitWindow singleRateLimitWindowPerClientIPv4Address;

        public FluffySpoon.AspNet.LetsEncrypt.LetsEncryptOptions letsEncryptOptions;

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

        public IReadOnlyCollection<(string staticFileName, byte[] staticFileContent)> StaticFiles;

        public byte[] ElmAppFile;

        static string staticFilesDirectoryName => "static-files";

        static string jsonFileName => "elm-fullstack.json";

        static string elmAppFileName => "elm-app.zip";

        static public WebAppConfiguration FromFiles(IReadOnlyCollection<(string name, byte[] content)> files)
        {
            var jsonStructureFile =
                files.FirstOrDefault(file => String.Equals(file.name, jsonFileName, StringComparison.InvariantCultureIgnoreCase));

            var elmAppFile =
                files.FirstOrDefault(file => String.Equals(file.name, elmAppFileName, StringComparison.InvariantCultureIgnoreCase));

            var jsonStructure =
                jsonStructureFile.content == null ? (WebAppConfigurationJsonStructure)null
                : JsonConvert.DeserializeObject<WebAppConfigurationJsonStructure>(Encoding.UTF8.GetString(jsonStructureFile.content));

            var staticFiles =
                files.Select(file =>
                {
                    var staticFilePathMatch = Regex.Match(file.name, staticFilesDirectoryName + @"(\\|\/)(.+)", RegexOptions.IgnoreCase);

                    if (!staticFilePathMatch.Success)
                        return (null, null);

                    return (name: staticFilePathMatch.Groups[2].Value, file.content);
                })
                .Where(file => 0 < file.name?.Length)
                .ToList();

            return new WebAppConfiguration
            {
                JsonStructure = jsonStructure,
                StaticFiles = staticFiles,
                ElmAppFile = elmAppFile.content,
            };
        }

        public IReadOnlyCollection<(string name, byte[] content)> AsFiles()
        {
            var jsonStructureFileEntries =
                new[] { JsonStructure }
                .WhereNotNull()
                .Select(map => (jsonFileName, Encoding.UTF8.GetBytes(JsonConvert.SerializeObject(map)))).ToList();

            var staticFilesEntries =
                StaticFiles
                ?.Select(fileNameAndContent =>
                    (staticFilesDirectoryName + @"\" + fileNameAndContent.staticFileName, fileNameAndContent.staticFileContent))
                .ToList();

            var elmAppFilesEntries =
                new[] { ElmAppFile }
                .WhereNotNull()
                .Select(elmAppFile => (elmAppFileName, elmAppFile)).ToList();

            return
                (jsonStructureFileEntries.EmptyIfNull())
                .Concat(staticFilesEntries.EmptyIfNull())
                .Concat(elmAppFilesEntries).ToList();
        }

        public WebAppConfiguration WithJsonStructure(WebAppConfigurationJsonStructure jsonStructure) =>
            new WebAppConfiguration
            {
                JsonStructure = jsonStructure,
                StaticFiles = StaticFiles,
                ElmAppFile = ElmAppFile,
            };

        public WebAppConfiguration WithStaticFiles(IReadOnlyCollection<(string staticFileName, byte[] staticFileContent)> staticFiles) =>
            new WebAppConfiguration
            {
                JsonStructure = JsonStructure,
                StaticFiles = staticFiles,
                ElmAppFile = ElmAppFile,
            };

        public WebAppConfiguration WithElmApp(byte[] elmAppFile) =>
            new WebAppConfiguration
            {
                JsonStructure = JsonStructure,
                StaticFiles = StaticFiles,
                ElmAppFile = elmAppFile,
            };
    }
}
