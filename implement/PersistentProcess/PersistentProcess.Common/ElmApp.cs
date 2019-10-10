using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text.RegularExpressions;

namespace Kalmit
{
    public struct ElmAppInterfaceConfig
    {
        public string RootModuleFilePath;

        public string RootModuleName;

        static public ElmAppInterfaceConfig Default => new ElmAppInterfaceConfig
        {
            RootModuleFilePath = "src/Backend/Main.elm",
            RootModuleName = "Backend.Main"
        };
    }

    public struct ElmAppInterfaceConvention
    {
        public const string PathToFileWithElmEntryPoint = "src/Backend/Main.elm";

        public const string PathToInitialStateFunction = ".interfaceToHost_initState";

        public const string PathToSerializedEventFunction = ".interfaceToHost_processEvent";

        public const string PathToSerializeStateFunction = ".interfaceToHost_serializeState";

        public const string PathToDeserializeStateFunction = ".interfaceToHost_deserializeState";
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