using Microsoft.VisualStudio.TestTools.UnitTesting;
using Pine.Elm019;
using System.Text.Json;

namespace TestElmTime;

[TestClass]
public class Elm019JsonFileParsingTests
{
    [TestMethod]
    public void Parse_direct_dependencies()
    {
        var elmJsonFile =
            """
            {
                "type": "application",
                "source-directories": [
                    "src",
                    "elm-syntax/src",
                    "elm-syntax-encode-json/src"
                ],
                "elm-version": "0.19.1",
                "dependencies": {
                    "direct": {
                        "cmditch/elm-bigint": "2.0.1",
                        "danfishgold/base64-bytes": "1.1.0",
                        "elm/bytes": "1.0.8",
                        "elm/core": "1.0.5",
                        "elm/json": "1.1.3",
                        "elm/parser": "1.1.0",
                        "elm-community/result-extra": "2.4.0",
                        "folkertdev/elm-sha2": "1.0.0"
                    },
                    "indirect": {
                        "elm/regex": "1.0.0",
                        "elm-community/list-extra": "8.7.0",
                        "elm-community/maybe-extra": "5.3.0",
                        "rtfeldman/elm-hex": "1.0.0"
                    }
                },
                "test-dependencies": {
                    "direct": {
                        "elm-explorations/test": "2.2.0"
                    },
                    "indirect": {
                        "elm/html": "1.0.0",
                        "elm/random": "1.0.0",
                        "elm/time": "1.0.0",
                        "elm/virtual-dom": "1.0.3"
                    }
                }
            }
            """;

        var elmJsonParsed =
            JsonSerializer.Deserialize<ElmJsonStructure>(elmJsonFile);

        Assert.IsNotNull(elmJsonParsed);

        Assert.AreEqual("application", elmJsonParsed.Type);

        Assert.AreEqual(3, elmJsonParsed.SourceDirectories.Count);

        Assert.AreEqual("src", elmJsonParsed.SourceDirectories[0]);

        Assert.AreEqual("elm-syntax/src", elmJsonParsed.SourceDirectories[1]);

        Assert.AreEqual("elm-syntax-encode-json/src", elmJsonParsed.SourceDirectories[2]);

        Assert.AreEqual("0.19.1", elmJsonParsed.ElmVersion);

        var directDependencies = elmJsonParsed.Dependencies.Direct;

        Assert.AreEqual(8, directDependencies.Count);

        Assert.AreEqual("2.0.1", directDependencies["cmditch/elm-bigint"]);

        Assert.AreEqual("1.1.0", directDependencies["danfishgold/base64-bytes"]);

        Assert.AreEqual("1.0.8", directDependencies["elm/bytes"]);

        Assert.AreEqual("1.0.5", directDependencies["elm/core"]);

        Assert.AreEqual("1.1.3", directDependencies["elm/json"]);

        Assert.AreEqual("1.1.0", directDependencies["elm/parser"]);

        Assert.AreEqual("2.4.0", directDependencies["elm-community/result-extra"]);

        Assert.AreEqual("1.0.0", directDependencies["folkertdev/elm-sha2"]);

        var indirectDependencies = elmJsonParsed.Dependencies.Indirect;

        Assert.AreEqual(4, indirectDependencies.Count);

        Assert.AreEqual("1.0.0", indirectDependencies["elm/regex"]);

        Assert.AreEqual("8.7.0", indirectDependencies["elm-community/list-extra"]);

        Assert.AreEqual("5.3.0", indirectDependencies["elm-community/maybe-extra"]);

        Assert.AreEqual("1.0.0", indirectDependencies["rtfeldman/elm-hex"]);
    }
}
