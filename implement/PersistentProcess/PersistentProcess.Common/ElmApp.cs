using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text.RegularExpressions;

namespace Kalmit
{
    public struct ElmAppInterfaceConfig
    {
        public const string PathToFileWithElmEntryPoint = "src/Main.elm";

        public const string PathToInitialStateFunction = "Main.interfaceToHost_initState";

        public const string PathToSerializedEventFunction = "Main.interfaceToHost_processEvent";

        public const string PathToSerializeStateFunction = "Main.interfaceToHost_serializeState";

        public const string PathToDeserializeStateFunction = "Main.interfaceToHost_deserializeState";
    }

    public class ElmApp
    {
        static public bool FilePathMatchesPatternOfFilesInElmApp(string filePath) =>
            Regex.IsMatch(
                Path.GetFileName(filePath),
                "(^" + Regex.Escape("elm.json") + "|" + Regex.Escape(".elm") + ")$",
                RegexOptions.IgnoreCase);

        static public IEnumerable<(string filePath, byte[] fileContent)> FilesFilteredForElmApp(
            IEnumerable<(string filePath, byte[] fileContent)> files) =>
            files
            .Where(file => 0 < file.filePath?.Length && FilePathMatchesPatternOfFilesInElmApp(file.filePath));
    }
}