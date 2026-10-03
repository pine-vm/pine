using AwesomeAssertions;
using Pine.Core.Addressing;
using Pine.Core.CommonEncodings;
using System;
using System.Collections.Immutable;
using System.Linq;
using Xunit;

namespace Pine.IntegrationTests;

public class LoadCompositionTests
{
    [Fact]
    public void Composition_from_link_in_elm_editor()
    {
        var testCases =
            new[]
            {
                new
                {
                    input =
                    "https://elm-editor.com/?project-state=https%3A%2F%2Fgithub.com%2Felm-time%2Felm-time%2Ftree%2F742650b6a6f1e3dc723d76fbb8c189ca16a0bee6%2Fimplement%2Fexample-apps%2Felm-editor%2Fdefault-app&file-path-to-open=src%2FMain.elm",
                    expectedCompositionId = "2210e99738cf2925f2f3adb5f907c062dce9d245065773c724b8d385db6e81a5"
                },
                new
                {
                    input =
                    "https://elm-editor.com/?project-state-deflate-base64=XZDLasMwEEX%2FZdZO5Ecip97FLYVQWmi3RgQ9xg9qW0aSQ4vRv9cyZNHsZg5zzwyzwA2N7fR4TeM0ucYJFAsIbhEKaJ2bbEFI07l2FnupB4L9sKvnvreOy%2B%2BHzhlEkh9SeowF5bROMFMyTzOV01qIk0xOT5InlMcCkZJumHoccHQEf3iod3ya7KZE1TltiMKaz70LHCJQXV2jwVHiq9FDuV24gMFB3%2FBDK7RQVCwC2fKxwbLXIoCqAmvkmn7n3bhf3cCiaoEvnC2Wv24LHbOc%2BSjAoLpTurGzUnewNjZspYf1M5fnc3N5%2BXwDv4399x0z5hlj3vs%2F&file-path-to-open=src%2FMain.elm",
                    expectedCompositionId = "f8886ab710d70f1e14131415eef8943da882878cd3709aea8aeaece00ccc322f"
                },
                new
                {
                    input =
                    "https://elm-editor.com/?project-state-deflate-base64=dZDJasMwFEX%2FRWsnnmJ5gC6SDhBKQ9NFQ2pM0PDsmNqWkeTQYPzvtUwDbUl2ege9ew%2BvRyeQqhTNwXM89%2BC4KOkRJQpQgo5atyqx7aLUx47OmahtqOpZ3lWV0oR9%2Fpu0BLDDhYcDh2KCcxd8zkLP5yHOKY2YG8WMuJg4FADbZd1WUEOjbfgi5j0jbaumSOClFtLmkJOu0oYjC%2FEyz0FCw%2BBJino1GfZIQi1OsBEcFErSzELsSJoCVpWgBqQpUpKN2y%2BkbOZjNsqstEdv0ClYnfW0FPhhNlgGmqgLxRNbcn4B46BMK16Ml1nfL4v1w%2FYZDdO3v3mBnw3Z2HPpfq3IuZCia%2FgNgzCOfmJ%2BGywmdssAzm60ftyI%2FS5oPnbbYu%2FFmu7eO74Ud9es%2FDgOAuOVDcPwDQ%3D%3D&file-path-to-open=src%2FMain.elm",
                    expectedCompositionId = "4e5b769b79dee2ffe39e34a0b51f4eab1142ddfbffdf515047045c52047cc56d"
                },
                new
                {
                    input =
                    "https://elm-editor.com/?project-state-deflate-base64=jY%2FNasMwEITfZc9OJMuO3RhyiAs9lUIbSH%2BMCJa9tgWWFSQ5UIzevUqh0F7a3nZnZ7%2BdXeCCxko9nRhl8YnGUCwgaotQwODc2RaE9NINs1g3WpGjlIYI7SxxBpFkaZZ3adZkLBFxnG5YxyiNRUO7JKdZl28pbWOW3xCpziMqnBxxGMraoSUmCEqgWQXeyqJzcuotRNDKrkODU4N3RqvyM8sCwa0v%2BKBbtFBUPIJmqKcey1GLq1BVUGq3xlEBj6oFnnC2WL6HO2G4yRPuowX2bfslhcZe0VkaHr3tdzvwv1ruD7R%2FPJTz20vTv7KtE8%2FHud3rv9b%2BQf5m%2BRGaJTTlnnPuvf8A&file-path-to-open=Bot.elm",
                    expectedCompositionId = "0725bf170667586c890c2245e59b44179333c91339ad514f17b123d9bd9718c9",
                }
            };

        foreach (var testCase in testCases)
        {
            try
            {
                var loadCompositionResult =
                    LoadComposition.LoadFromPathResolvingNetworkDependencies(testCase.input)
                    .LogToList();

                var loaded =
                    loadCompositionResult
                    .result.Extract(error => throw new Exception("Failed to load from path: " + error));

                var inspectComposition =
                    loaded.tree.EnumerateFilesTransitive()
                    .Select(
                        blobAtPath =>
                        {
                            string? utf8 = null;

                            try
                            {
                                utf8 = System.Text.Encoding.UTF8.GetString(blobAtPath.fileContent.Span);
                            }
                            catch
                            { }

                            return
                                new
                                {
                                    blobAtPath.path,
                                    blobAtPath.fileContent,
                                    utf8
                                };
                        })
                    .ToImmutableList();

                var composition = FileTreeEncoding.Encode(loaded.tree);
                var compositionId = Convert.ToHexStringLower(PineValueHashTree.ComputeHash(composition).Span);

                compositionId.Should().Be(testCase.expectedCompositionId);
            }
            catch (Exception e)
            {
                throw new Exception("Failed in test case " + testCase.input, e);
            }
        }
    }
}
