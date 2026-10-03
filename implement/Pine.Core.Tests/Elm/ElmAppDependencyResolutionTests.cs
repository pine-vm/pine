using AwesomeAssertions;
using Pine.Core.Elm;
using Pine.Core.Files;
using System;
using System.Collections.Generic;
using System.Text;
using Xunit;

namespace Pine.Core.Tests.Elm;

public class ElmAppDependencyResolutionTests
{
    [Fact]
    public void Conflicting_package_versions_report_existing_and_new_values()
    {
        var appFiles = AppFilesWithPackageVersions("1.0.5", "1.0.4");

        Action loadPackages =
            () => ElmAppDependencyResolution.LoadPackagesForElmApp(appFiles);

        var exception = loadPackages.Should().Throw<ArgumentException>().Which;

        exception.Message.Should().Contain("elm/core");
        exception.Message.Should().Contain("existing value");
        exception.Message.Should().Contain("new value");
        exception.Message.Should().Contain("'1.0.5'");
        exception.Message.Should().Contain("'1.0.4'");
    }

    [Fact]
    public void Identical_package_versions_across_elm_json_files_are_loaded_once()
    {
        var appFiles = AppFilesWithPackageVersions("1.0.5", "1.0.5");
        var loadedPackages = new List<(string name, string version)>();

        var packages =
            ElmAppDependencyResolution.LoadPackagesForElmApp(
                appFiles,
                loadPackage: (name, version) =>
                {
                    loadedPackages.Add((name, version));

                    return
                        FileTreeExtensions.ToFlatDictionaryWithPathComparer(
                            FileTree.FromSetOfFilesWithStringPath(
                                [
                                (new[] { "elm.json" },
                                (ReadOnlyMemory<byte>)
                                """
                                {"type":"package","dependencies":{}}
                                """u8.ToArray())
                                ]));
                });

        loadedPackages.Should().BeEquivalentTo([("elm/core", "1.0.5")]);
        packages.Keys.Should().BeEquivalentTo(["elm/core"]);
    }

    private static IReadOnlyDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>>
        AppFilesWithPackageVersions(string firstVersion, string secondVersion)
    {
        return
            FileTreeExtensions.ToFlatDictionaryWithPathComparer(
                FileTree.FromSetOfFilesWithStringPath(
                    [
                    (new[] { "first", "elm.json" },
                    (ReadOnlyMemory<byte>)Encoding.UTF8.GetBytes(
                        $$"""
                        {
                            "type": "application",
                            "dependencies": {
                                "direct": { "elm/core": "{{firstVersion}}" },
                                "indirect": {}
                            }
                        }
                        """)),
                    (new[] { "second", "elm.json" },
                    (ReadOnlyMemory<byte>)Encoding.UTF8.GetBytes(
                        $$"""
                        {
                            "type": "application",
                            "dependencies": {
                                "direct": { "elm/core": "{{secondVersion}}" },
                                "indirect": {}
                            }
                        }
                        """))
                    ]));
    }
}
