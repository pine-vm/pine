using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text.RegularExpressions;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace PersistentAppFromElmCode.Test
{
    [TestClass]
    public class TestContainer
    {
        static string PathToExampleElmApps => "./../../../../example-elm-apps";

        [TestMethod]
        public void Get_echo_from_elm_app()
        {
            var elmCodeFiles =
                Filesystem.GetAllFilesFromDirectory(Path.Combine(PathToExampleElmApps, "echo"))
                .Where(file => PersistentAppFromElmCode.Common.PersistentAppFromElm019Code.FilePathMatchesPatternOfFilesInElmApp(file.name))
                .ToList();

            using (var app = Common.PersistentAppFromElm019Code.WithCustomSerialization(
                elmCodeFiles: elmCodeFiles,
                pathToFileWithElmEntryPoint: "Echo.elm",
                pathToInitialStateFunction: "Echo.initState",
                pathToSerializedRequestFunction: "Echo.serializedRequest",
                pathToSerializeStateFunction: "Echo.serializeState",
                pathToDeserializeStateFunction: "Echo.deserializeState"))
            {
                var response = app.Request("Hello!");

                Assert.AreEqual("Echo from Elm:Hello!", response);
            }
        }
    }
}
