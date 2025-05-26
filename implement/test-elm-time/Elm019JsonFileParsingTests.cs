using FluentAssertions;
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

        elmJsonParsed.Should().NotBeNull();

        elmJsonParsed!.Type.Should().Be("application");

        elmJsonParsed.SourceDirectories.Count.Should().Be(3);

        elmJsonParsed.SourceDirectories[0].Should().Be("src");

        elmJsonParsed.SourceDirectories[1].Should().Be("elm-syntax/src");

        elmJsonParsed.SourceDirectories[2].Should().Be("elm-syntax-encode-json/src");

        elmJsonParsed.ElmVersion.Should().Be("0.19.1");

        var directDependencies =
            elmJsonParsed.Dependencies.Direct
            ??
            throw new System.Exception("directDependencies is null");

        directDependencies.Count.Should().Be(8);

        directDependencies["cmditch/elm-bigint"].Should().Be("2.0.1");

        directDependencies["danfishgold/base64-bytes"].Should().Be("1.1.0");

        directDependencies["elm/bytes"].Should().Be("1.0.8");

        directDependencies["elm/core"].Should().Be("1.0.5");

        directDependencies["elm/json"].Should().Be("1.1.3");

        directDependencies["elm/parser"].Should().Be("1.1.0");

        directDependencies["elm-community/result-extra"].Should().Be("2.4.0");

        directDependencies["folkertdev/elm-sha2"].Should().Be("1.0.0");

        var indirectDependencies =
            elmJsonParsed.Dependencies.Indirect
            ??
            throw new System.Exception("indirectDependencies is null");

        indirectDependencies.Count.Should().Be(4);

        indirectDependencies["elm/regex"].Should().Be("1.0.0");

        indirectDependencies["elm-community/list-extra"].Should().Be("8.7.0");

        indirectDependencies["elm-community/maybe-extra"].Should().Be("5.3.0");

        indirectDependencies["rtfeldman/elm-hex"].Should().Be("1.0.0");
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

        elmJsonParsed.Should().NotBeNull();

        elmJsonParsed!.Type.Should().Be("package");

        elmJsonParsed.Name.Should().Be("stil4m/elm-syntax");

        elmJsonParsed.Summary.Should().Be("Elm Syntax in Elm: for parsing and writing Elm in Elm");

        elmJsonParsed.License.Should().Be("MIT");

        elmJsonParsed.Version.Should().Be("7.3.8");

        elmJsonParsed.ExposedModules.Count.Should().Be(23);

        elmJsonParsed.ExposedModules[0].Should().Be("Elm.Dependency");
        elmJsonParsed.ExposedModules[1].Should().Be("Elm.Interface");
        elmJsonParsed.ExposedModules[2].Should().Be("Elm.Parser");
        elmJsonParsed.ExposedModules[3].Should().Be("Elm.Processing");
        elmJsonParsed.ExposedModules[4].Should().Be("Elm.RawFile");
        elmJsonParsed.ExposedModules[5].Should().Be("Elm.Writer");
        elmJsonParsed.ExposedModules[6].Should().Be("Elm.Syntax.Comments");
        elmJsonParsed.ExposedModules[7].Should().Be("Elm.Syntax.Declaration");
        elmJsonParsed.ExposedModules[8].Should().Be("Elm.Syntax.Documentation");
        elmJsonParsed.ExposedModules[9].Should().Be("Elm.Syntax.Exposing");
        elmJsonParsed.ExposedModules[10].Should().Be("Elm.Syntax.Expression");
        elmJsonParsed.ExposedModules[11].Should().Be("Elm.Syntax.File");
        elmJsonParsed.ExposedModules[12].Should().Be("Elm.Syntax.Import");
        elmJsonParsed.ExposedModules[13].Should().Be("Elm.Syntax.Infix");
        elmJsonParsed.ExposedModules[14].Should().Be("Elm.Syntax.Module");
        elmJsonParsed.ExposedModules[15].Should().Be("Elm.Syntax.ModuleName");
        elmJsonParsed.ExposedModules[16].Should().Be("Elm.Syntax.Node");
        elmJsonParsed.ExposedModules[17].Should().Be("Elm.Syntax.Pattern");
        elmJsonParsed.ExposedModules[18].Should().Be("Elm.Syntax.Range");
        elmJsonParsed.ExposedModules[19].Should().Be("Elm.Syntax.Signature");
        elmJsonParsed.ExposedModules[20].Should().Be("Elm.Syntax.TypeAlias");
        elmJsonParsed.ExposedModules[21].Should().Be("Elm.Syntax.TypeAnnotation");
        elmJsonParsed.ExposedModules[22].Should().Be("Elm.Syntax.Type");

        elmJsonParsed.ElmVersion.Should().Be("0.19.0 <= v < 0.20.0");

        var dependencies =
            elmJsonParsed.Dependencies.Flat
            ??
            throw new System.Exception("dependencies is null");

        dependencies.Count.Should().Be(5);

        dependencies["elm/core"].Should().Be("1.0.0 <= v < 2.0.0");

        dependencies["elm/json"].Should().Be("1.0.0 <= v < 2.0.0");

        dependencies["elm/parser"].Should().Be("1.0.0 <= v < 2.0.0");

        dependencies["rtfeldman/elm-hex"].Should().Be("1.0.0 <= v < 2.0.0");

        dependencies["stil4m/structured-writer"].Should().Be("1.0.1 <= v < 2.0.0");

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

        elmJsonParsed.Should().NotBeNull();

        elmJsonParsed!.Type.Should().Be("package");

        elmJsonParsed.Name.Should().Be("elm/core");

        elmJsonParsed.ExposedModules.Count.Should().Be(17);

        elmJsonParsed.ExposedModules[0].Should().Be("Basics");
        elmJsonParsed.ExposedModules[1].Should().Be("String");
        elmJsonParsed.ExposedModules[2].Should().Be("Char");
        elmJsonParsed.ExposedModules[3].Should().Be("Bitwise");
        elmJsonParsed.ExposedModules[4].Should().Be("Tuple");

        elmJsonParsed.ExposedModules[5].Should().Be("List");
        elmJsonParsed.ExposedModules[6].Should().Be("Dict");
        elmJsonParsed.ExposedModules[7].Should().Be("Set");
        elmJsonParsed.ExposedModules[8].Should().Be("Array");

        elmJsonParsed.ExposedModules[9].Should().Be("Maybe");
        elmJsonParsed.ExposedModules[10].Should().Be("Result");

        elmJsonParsed.ExposedModules[11].Should().Be("Debug");

        elmJsonParsed.ExposedModules[12].Should().Be("Platform.Cmd");
        elmJsonParsed.ExposedModules[13].Should().Be("Platform.Sub");
        elmJsonParsed.ExposedModules[14].Should().Be("Platform");
        elmJsonParsed.ExposedModules[15].Should().Be("Process");
        elmJsonParsed.ExposedModules[16].Should().Be("Task");

    }
}
