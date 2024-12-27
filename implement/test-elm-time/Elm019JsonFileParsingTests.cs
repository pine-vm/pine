using Microsoft.VisualStudio.TestTools.UnitTesting;
using Pine.Elm019;
using System.Text.Json;

namespace TestElmTime;

[TestClass]
public class Elm019JsonFileParsingTests
{
    [TestMethod]
    public void Parse_elm_json_of_application_type()
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

        var directDependencies =
            elmJsonParsed.Dependencies.Direct
            ??
            throw new System.Exception("directDependencies is null");

        Assert.AreEqual(8, directDependencies.Count);

        Assert.AreEqual("2.0.1", directDependencies["cmditch/elm-bigint"]);

        Assert.AreEqual("1.1.0", directDependencies["danfishgold/base64-bytes"]);

        Assert.AreEqual("1.0.8", directDependencies["elm/bytes"]);

        Assert.AreEqual("1.0.5", directDependencies["elm/core"]);

        Assert.AreEqual("1.1.3", directDependencies["elm/json"]);

        Assert.AreEqual("1.1.0", directDependencies["elm/parser"]);

        Assert.AreEqual("2.4.0", directDependencies["elm-community/result-extra"]);

        Assert.AreEqual("1.0.0", directDependencies["folkertdev/elm-sha2"]);

        var indirectDependencies =
            elmJsonParsed.Dependencies.Indirect
            ??
            throw new System.Exception("indirectDependencies is null");

        Assert.AreEqual(4, indirectDependencies.Count);

        Assert.AreEqual("1.0.0", indirectDependencies["elm/regex"]);

        Assert.AreEqual("8.7.0", indirectDependencies["elm-community/list-extra"]);

        Assert.AreEqual("5.3.0", indirectDependencies["elm-community/maybe-extra"]);

        Assert.AreEqual("1.0.0", indirectDependencies["rtfeldman/elm-hex"]);
    }

    [TestMethod]
    public void Parse_elm_json_of_package_type()
    {
        var elmJsonFile =
            """
            {
                "type": "package",
                "name": "stil4m/elm-syntax",
                "summary": "Elm Syntax in Elm: for parsing and writing Elm in Elm",
                "license": "MIT",
                "version": "7.3.8",
                "exposed-modules": [
                    "Elm.Dependency",
                    "Elm.Interface",
                    "Elm.Parser",
                    "Elm.Processing",
                    "Elm.RawFile",
                    "Elm.Writer",
                    "Elm.Syntax.Comments",
                    "Elm.Syntax.Declaration",
                    "Elm.Syntax.Documentation",
                    "Elm.Syntax.Exposing",
                    "Elm.Syntax.Expression",
                    "Elm.Syntax.File",
                    "Elm.Syntax.Import",
                    "Elm.Syntax.Infix",
                    "Elm.Syntax.Module",
                    "Elm.Syntax.ModuleName",
                    "Elm.Syntax.Node",
                    "Elm.Syntax.Pattern",
                    "Elm.Syntax.Range",
                    "Elm.Syntax.Signature",
                    "Elm.Syntax.TypeAlias",
                    "Elm.Syntax.TypeAnnotation",
                    "Elm.Syntax.Type"
                ],
                "elm-version": "0.19.0 <= v < 0.20.0",
                "dependencies": {
                    "elm/core": "1.0.0 <= v < 2.0.0",
                    "elm/json": "1.0.0 <= v < 2.0.0",
                    "elm/parser": "1.0.0 <= v < 2.0.0",
                    "rtfeldman/elm-hex": "1.0.0 <= v < 2.0.0",
                    "stil4m/structured-writer": "1.0.1 <= v < 2.0.0"
                },
                "test-dependencies": {
                    "elm-explorations/test": "2.0.0 <= v < 3.0.0"
                }
            }
            """;

        var elmJsonParsed =
            JsonSerializer.Deserialize<ElmJsonStructure>(elmJsonFile);

        Assert.IsNotNull(elmJsonParsed);

        Assert.AreEqual("package", elmJsonParsed.Type);

        Assert.AreEqual("stil4m/elm-syntax", elmJsonParsed.Name);

        Assert.AreEqual("Elm Syntax in Elm: for parsing and writing Elm in Elm", elmJsonParsed.Summary);

        Assert.AreEqual("MIT", elmJsonParsed.License);

        Assert.AreEqual("7.3.8", elmJsonParsed.Version);

        Assert.AreEqual(23, elmJsonParsed.ExposedModules.Count);

        Assert.AreEqual("Elm.Dependency", elmJsonParsed.ExposedModules[0]);
        Assert.AreEqual("Elm.Interface", elmJsonParsed.ExposedModules[1]);
        Assert.AreEqual("Elm.Parser", elmJsonParsed.ExposedModules[2]);
        Assert.AreEqual("Elm.Processing", elmJsonParsed.ExposedModules[3]);
        Assert.AreEqual("Elm.RawFile", elmJsonParsed.ExposedModules[4]);
        Assert.AreEqual("Elm.Writer", elmJsonParsed.ExposedModules[5]);
        Assert.AreEqual("Elm.Syntax.Comments", elmJsonParsed.ExposedModules[6]);
        Assert.AreEqual("Elm.Syntax.Declaration", elmJsonParsed.ExposedModules[7]);
        Assert.AreEqual("Elm.Syntax.Documentation", elmJsonParsed.ExposedModules[8]);
        Assert.AreEqual("Elm.Syntax.Exposing", elmJsonParsed.ExposedModules[9]);
        Assert.AreEqual("Elm.Syntax.Expression", elmJsonParsed.ExposedModules[10]);
        Assert.AreEqual("Elm.Syntax.File", elmJsonParsed.ExposedModules[11]);
        Assert.AreEqual("Elm.Syntax.Import", elmJsonParsed.ExposedModules[12]);
        Assert.AreEqual("Elm.Syntax.Infix", elmJsonParsed.ExposedModules[13]);
        Assert.AreEqual("Elm.Syntax.Module", elmJsonParsed.ExposedModules[14]);
        Assert.AreEqual("Elm.Syntax.ModuleName", elmJsonParsed.ExposedModules[15]);
        Assert.AreEqual("Elm.Syntax.Node", elmJsonParsed.ExposedModules[16]);
        Assert.AreEqual("Elm.Syntax.Pattern", elmJsonParsed.ExposedModules[17]);
        Assert.AreEqual("Elm.Syntax.Range", elmJsonParsed.ExposedModules[18]);
        Assert.AreEqual("Elm.Syntax.Signature", elmJsonParsed.ExposedModules[19]);
        Assert.AreEqual("Elm.Syntax.TypeAlias", elmJsonParsed.ExposedModules[20]);
        Assert.AreEqual("Elm.Syntax.TypeAnnotation", elmJsonParsed.ExposedModules[21]);
        Assert.AreEqual("Elm.Syntax.Type", elmJsonParsed.ExposedModules[22]);

        Assert.AreEqual("0.19.0 <= v < 0.20.0", elmJsonParsed.ElmVersion);

        var dependencies =
            elmJsonParsed.Dependencies.Flat
            ??
            throw new System.Exception("dependencies is null");

        Assert.AreEqual(5, dependencies.Count);

        Assert.AreEqual("1.0.0 <= v < 2.0.0", dependencies["elm/core"]);

        Assert.AreEqual("1.0.0 <= v < 2.0.0", dependencies["elm/json"]);

        Assert.AreEqual("1.0.0 <= v < 2.0.0", dependencies["elm/parser"]);

        Assert.AreEqual("1.0.0 <= v < 2.0.0", dependencies["rtfeldman/elm-hex"]);

        Assert.AreEqual("1.0.1 <= v < 2.0.0", dependencies["stil4m/structured-writer"]);

    }

    [TestMethod]
    public void Parse_elm_json_of_package_type_with_exposed_modules_as_object()
    {
        var elmJsonFile =
            """
            {
                "type": "package",
                "name": "elm/core",
                "summary": "Elm's standard libraries",
                "license": "BSD-3-Clause",
                "version": "1.0.5",
                "exposed-modules": {
                    "Primitives": [
                        "Basics",
                        "String",
                        "Char",
                        "Bitwise",
                        "Tuple"
                    ],
                    "Collections": [
                        "List",
                        "Dict",
                        "Set",
                        "Array"
                    ],
                    "Error Handling": [
                        "Maybe",
                        "Result"
                    ],
                    "Debug": [
                        "Debug"
                    ],
                    "Effects": [
                        "Platform.Cmd",
                        "Platform.Sub",
                        "Platform",
                        "Process",
                        "Task"
                    ]
                },
                "elm-version": "0.19.0 <= v < 0.20.0",
                "dependencies": {},
                "test-dependencies": {}
            }
            """;

        var elmJsonParsed =
            JsonSerializer.Deserialize<ElmJsonStructure>(elmJsonFile);

        Assert.IsNotNull(elmJsonParsed);

        Assert.AreEqual("package", elmJsonParsed.Type);

        Assert.AreEqual("elm/core", elmJsonParsed.Name);

        Assert.AreEqual(17, elmJsonParsed.ExposedModules.Count);

        Assert.AreEqual("Basics", elmJsonParsed.ExposedModules[0]);
        Assert.AreEqual("String", elmJsonParsed.ExposedModules[1]);
        Assert.AreEqual("Char", elmJsonParsed.ExposedModules[2]);
        Assert.AreEqual("Bitwise", elmJsonParsed.ExposedModules[3]);
        Assert.AreEqual("Tuple", elmJsonParsed.ExposedModules[4]);

        Assert.AreEqual("List", elmJsonParsed.ExposedModules[5]);
        Assert.AreEqual("Dict", elmJsonParsed.ExposedModules[6]);
        Assert.AreEqual("Set", elmJsonParsed.ExposedModules[7]);
        Assert.AreEqual("Array", elmJsonParsed.ExposedModules[8]);

        Assert.AreEqual("Maybe", elmJsonParsed.ExposedModules[9]);
        Assert.AreEqual("Result", elmJsonParsed.ExposedModules[10]);

        Assert.AreEqual("Debug", elmJsonParsed.ExposedModules[11]);

        Assert.AreEqual("Platform.Cmd", elmJsonParsed.ExposedModules[12]);
        Assert.AreEqual("Platform.Sub", elmJsonParsed.ExposedModules[13]);
        Assert.AreEqual("Platform", elmJsonParsed.ExposedModules[14]);
        Assert.AreEqual("Process", elmJsonParsed.ExposedModules[15]);
        Assert.AreEqual("Task", elmJsonParsed.ExposedModules[16]);

    }
}
