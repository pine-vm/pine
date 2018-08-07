using System.Collections.Generic;
using System.IO;
using System.Linq;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace PersistentAppFromElmCode.Test
{
    [TestClass]
    public class TestContainer
    {
        static string PathToExampleElmApps => "./../../../../example-elm-apps";

        static IReadOnlyCollection<(string, byte[])> GetFilesFromDirectory(string directoryPath)
        {
            var directory = new DirectoryInfo(directoryPath);

            return
                directory.GetFiles("*", SearchOption.AllDirectories)
                .Select(file => (Path.GetRelativePath(directoryPath, file.FullName), File.ReadAllBytes(file.FullName)))
                .ToList();
        }

        [TestMethod]
        public void Get_echo_from_elm_app()
        {
            var initialApp = Common.PersistentAppFromElmCode.WithCustomSerialization(
                elmCodeFiles: GetFilesFromDirectory(Path.Combine(PathToExampleElmApps, "echo")),
                pathToSerializedRequestFunction: "Echo.serializedRequest",
                pathToSerializeStateFunction: "Echo.serializeState",
                pathToDeserializeStateFunction: "Echo.deserializeState");

            var response = initialApp.Request("Hello!");

            Assert.AreEqual("Echo from Elm:Hello!", response);
        }
    }
}
