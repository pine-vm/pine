using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using Newtonsoft.Json;

namespace Kalmit.PersistentProcess
{
    public class WebAppConfigurationMap
    {
        public ConditionalMapFromStringToString[] mapsFromRequestUrlToStaticFileName;

        public RateLimitWindow singleRateLimitWindowPerClientIPv4Address;

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
        public WebAppConfigurationMap Map;

        public IReadOnlyCollection<(string staticFileName, byte[] staticFileContent)> StaticFiles;

        public byte[] ElmAppFile;

        static string staticFilesDirectoryName => "static-files";

        static string mapFileName => "map.json";

        static string elmAppFileName => "elm-app.zip";

        static public WebAppConfiguration FromFiles(IReadOnlyCollection<(string name, byte[] content)> files)
        {
            var mapFile =
                files.FirstOrDefault(file => String.Equals(file.name, mapFileName, StringComparison.InvariantCultureIgnoreCase));

            var elmAppFile =
                files.FirstOrDefault(file => String.Equals(file.name, elmAppFileName, StringComparison.InvariantCultureIgnoreCase));

            var map =
                mapFile.content == null ? (WebAppConfigurationMap)null
                : JsonConvert.DeserializeObject<WebAppConfigurationMap>(Encoding.UTF8.GetString(mapFile.content));

            var staticFiles =
                files.Select(file =>
                {
                    var staticFilePathMatch = Regex.Match(file.name, staticFilesDirectoryName + @"\\(.+)", RegexOptions.IgnoreCase);

                    if (!staticFilePathMatch.Success)
                        return (null, null);

                    return (name: staticFilePathMatch.Groups[1].Value, file.content);
                })
                .Where(file => 0 < file.name?.Length)
                .ToList();

            return new WebAppConfiguration
            {
                Map = map,
                StaticFiles = staticFiles,
                ElmAppFile = elmAppFile.content,
            };
        }

        public IReadOnlyCollection<(string name, byte[] content)> AsFiles()
        {
            var mapFileEntries =
                new[] { Map }
                .WhereNotNull()
                .Select(map => (mapFileName, Encoding.UTF8.GetBytes(JsonConvert.SerializeObject(map)))).ToList();

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
                (mapFileEntries.EmptyIfNull())
                .Concat(staticFilesEntries.EmptyIfNull())
                .Concat(elmAppFilesEntries).ToList();
        }

        public WebAppConfiguration WithMap(WebAppConfigurationMap map) =>
            new WebAppConfiguration
            {
                Map = map,
                StaticFiles = StaticFiles,
                ElmAppFile = ElmAppFile,
            };

        public WebAppConfiguration WithStaticFiles(IReadOnlyCollection<(string staticFileName, byte[] staticFileContent)> staticFiles) =>
            new WebAppConfiguration
            {
                Map = Map,
                StaticFiles = staticFiles,
                ElmAppFile = ElmAppFile,
            };

        public WebAppConfiguration WithElmApp(byte[] elmAppFile) =>
            new WebAppConfiguration
            {
                Map = Map,
                StaticFiles = StaticFiles,
                ElmAppFile = elmAppFile,
            };
    }
}
