using AwesomeAssertions;
using Pine.Core.Elm.ElmInElm;
using Pine.Core.Files;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmInElm;

public class BundledFilesTests
{
    [Fact]
    public void Compiler_sources_preserve_embedded_relative_paths()
    {
        var files = BundledFiles.CompilerSourceContainerFilesDefault.Value;

        string[][] expectedPaths =
            [
                ["elm.json"],
                ["src", "ElmCompiler.elm"],
                ["elm-syntax", "src", "Elm", "Syntax", "Expression.elm"],
                ["pine-elm-syntax", "elm.json"],
                ["pine-elm-syntax", "src", "ElmSyntax", "Concrete", "Parser", "FromString.elm"]
            ];

        foreach (var path in expectedPaths)
        {
            var file = files.GetNodeAtPath(path).Should().BeOfType<FileTree.FileNode>().Which;

            file.Bytes.Length.Should().BeGreaterThan(0);
        }

        files.GetNodeAtPath(["elm-kernel-modules"]).Should().BeNull();
    }

    [Fact]
    public void Kernel_sources_preserve_embedded_relative_paths()
    {
        var files = BundledFiles.ElmKernelModulesDefault.Value;

        string[][] expectedPaths =
            [
                ["Basics.elm"],
                ["Bytes", "Decode.elm"],
                ["Elm", "Kernel", "Parser.elm"]
            ];

        foreach (var path in expectedPaths)
        {
            var file = files.GetNodeAtPath(path).Should().BeOfType<FileTree.FileNode>().Which;

            file.Bytes.Length.Should().BeGreaterThan(0);
        }
    }
}
