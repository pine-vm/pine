using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;

namespace Kalmit
{
    public struct ElmAppInterfaceConfig
    {
        public string RootModuleName;

        static public ElmAppInterfaceConfig Default => new ElmAppInterfaceConfig
        {
            RootModuleName = "Backend.Main"
        };
    }

    public struct ElmAppInterfaceConvention
    {
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

        static public IImmutableDictionary<IEnumerable<string>, byte[]> ToFlatDictionaryWithPathComparer(
          IEnumerable<(string filePath, byte[] fileContent)> fileList) =>
          fileList.ToImmutableDictionary(
              entry => (IEnumerable<string>)entry.filePath.Split(new[] { '/', '\\' }), entry => entry.fileContent)
          .WithComparers(EnumerableExtension.EqualityComparer<string>());

        static public IImmutableDictionary<IEnumerable<string>, byte[]> AsCompletelyLoweredElmApp(
            IEnumerable<(string filePath, byte[] fileContent)> originalAppFilesList,
            ElmAppInterfaceConfig interfaceConfig)
        {
            var originalAppFiles = ToFlatDictionaryWithPathComparer(originalAppFilesList);

            if (originalAppFiles.ContainsKey(InterfaceToHostRootModuleFilePath))
            {
                //  Support integrating applications supplying their own lowered version.
                return originalAppFiles;
            }

            var backendMainFilePath = FilePathFromModuleName(interfaceConfig.RootModuleName);

            var backendMainOriginalFile = originalAppFiles[backendMainFilePath];

            return
                originalAppFiles.SetItem(
                    InterfaceToHostRootModuleFilePath,
                    Encoding.UTF8.GetBytes(LoweredRootElmModuleCode(interfaceConfig.RootModuleName)));
        }

        static IEnumerable<string> FilePathFromModuleName(string moduleName)
        {
            var pathComponents = moduleName.Split(new[] { '.' });

            var fileName = pathComponents.Last() + ".elm";
            var directoryNames = pathComponents.Reverse().Skip(1).Reverse();

            return new[] { "src" }.Concat(directoryNames).Concat(new[] { fileName });
        }

        static public string InterfaceToHostRootModuleName => "Backend.InterfaceToHost_Root";

        static public IEnumerable<string> InterfaceToHostRootModuleFilePath => FilePathFromModuleName(InterfaceToHostRootModuleName);

        static public string LoweredRootElmModuleCode(string rootModuleNameBeforeLowering) => $@"
module " + InterfaceToHostRootModuleName + $@" exposing
    (State
    , interfaceToHost_deserializeState
    , interfaceToHost_initState
    , interfaceToHost_processEvent
    , interfaceToHost_serializeState
    , main
    )

import " + rootModuleNameBeforeLowering + $@" as RootModuleBeforeLowering
import Platform


type alias State = RootModuleBeforeLowering.State


interfaceToHost_initState = RootModuleBeforeLowering.interfaceToHost_initState


interfaceToHost_processEvent = RootModuleBeforeLowering.interfaceToHost_processEvent


interfaceToHost_serializeState = RootModuleBeforeLowering.interfaceToHost_serializeState


interfaceToHost_deserializeState = RootModuleBeforeLowering.interfaceToHost_deserializeState


-- Support function-level dead code elimination (https://elm-lang.org/blog/small-assets-without-the-headache) Elm code needed to inform the Elm compiler about our entry points.


main : Program Int State String
main =
    Platform.worker
        {{ init = \_ -> ( interfaceToHost_initState, Cmd.none )
        , update =
            \event stateBefore ->
                interfaceToHost_processEvent event (stateBefore |> interfaceToHost_serializeState |> interfaceToHost_deserializeState) |> Tuple.mapSecond (always Cmd.none)
        , subscriptions = \_ -> Sub.none
        }}
";

    }
}