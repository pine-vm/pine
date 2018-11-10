using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using Newtonsoft.Json;

namespace Kalmit
{
    public class ElmAppWithEntryConfig
    {
        public ElmAppEntryConfig? EntryConfig;

        public IReadOnlyCollection<(string filePath, byte[] fileContent)> ElmAppFiles;

        const string entryConfigFileName = "elm-app.entry-config.json";

        const string elmAppFilesDirectory = "elm-app";

        static public ElmAppWithEntryConfig FromFiles(IReadOnlyCollection<(string name, byte[] content)> files)
        {
            var entryConfigFile =
                files.FirstOrDefault(file => String.Equals(file.name, entryConfigFileName, StringComparison.InvariantCultureIgnoreCase));

            var entryConfig =
                entryConfigFile.content == null ? (ElmAppEntryConfig?)null
                : JsonConvert.DeserializeObject<ElmAppEntryConfig>(Encoding.UTF8.GetString(entryConfigFile.content));

            var codeFiles =
                files.Select(file =>
                {
                    var codeFilePath = Regex.Match(file.name, elmAppFilesDirectory + @"\\(.+)", RegexOptions.IgnoreCase);

                    if (!codeFilePath.Success)
                        return (null, null);

                    return (name: codeFilePath.Groups[1].Value, file.content);
                })
                .Where(file => 0 < file.name?.Length && ProcessFromElm019Code.FilePathMatchesPatternOfFilesInElmApp(file.name))
                .ToList();

            return new ElmAppWithEntryConfig
            {
                EntryConfig = entryConfig,
                ElmAppFiles = codeFiles,
            };
        }

        public IReadOnlyCollection<(string name, byte[] content)> AsFiles()
        {
            var entryConfigFiles =
                new[] { EntryConfig }
                .WhereHasValue()
                .Select(entryConfig =>
                    (entryConfigFileName, Encoding.UTF8.GetBytes(JsonConvert.SerializeObject(entryConfig))));

            var elmAppFiles =
                ElmAppFiles
                ?.Select(appFile => (elmAppFilesDirectory + @"\" + appFile.filePath, appFile.fileContent));

            return (elmAppFiles.EmptyIfNull()).Concat(entryConfigFiles).ToList();
        }
    }
}