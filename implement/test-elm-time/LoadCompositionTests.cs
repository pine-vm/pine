using FluentAssertions;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Pine;
using Pine.Core;
using System;
using System.Collections.Immutable;
using System.Linq;

namespace TestElmTime;

[TestClass]
public class LoadCompositionTests
{
    [TestMethod]
    public void Composition_from_link_in_elm_editor()
    {
        var testCases = new[]
        {
            new
            {
                input = "https://elm-editor.com/?project-state=https%3A%2F%2Fgithub.com%2Felm-time%2Felm-time%2Ftree%2F742650b6a6f1e3dc723d76fbb8c189ca16a0bee6%2Fimplement%2Fexample-apps%2Felm-editor%2Fdefault-app&file-path-to-open=src%2FMain.elm",
                expectedCompositionId = "87631bb4691d71bbae850baddc5e58d41938000e1036a2c6d57e81c08ec2887b"
            },
            new
            {
                input = "https://elm-editor.com/?project-state-deflate-base64=XZDLasMwEEX%2FZdZO5Ecip97FLYVQWmi3RgQ9xg9qW0aSQ4vRv9cyZNHsZg5zzwyzwA2N7fR4TeM0ucYJFAsIbhEKaJ2bbEFI07l2FnupB4L9sKvnvreOy%2B%2BHzhlEkh9SeowF5bROMFMyTzOV01qIk0xOT5InlMcCkZJumHoccHQEf3iod3ya7KZE1TltiMKaz70LHCJQXV2jwVHiq9FDuV24gMFB3%2FBDK7RQVCwC2fKxwbLXIoCqAmvkmn7n3bhf3cCiaoEvnC2Wv24LHbOc%2BSjAoLpTurGzUnewNjZspYf1M5fnc3N5%2BXwDv4399x0z5hlj3vs%2F&file-path-to-open=src%2FMain.elm",
                expectedCompositionId = "657b4d44c22c9a0624738ac64d215ffdb030cc6e4a9f4beb27e72f9c54a65d67"
            },
            new
            {
                input = "https://elm-editor.com/?project-state-deflate-base64=dZDJasMwFEX%2FRWsnnmJ5gC6SDhBKQ9NFQ2pM0PDsmNqWkeTQYPzvtUwDbUl2ege9ew%2BvRyeQqhTNwXM89%2BC4KOkRJQpQgo5atyqx7aLUx47OmahtqOpZ3lWV0oR9%2Fpu0BLDDhYcDh2KCcxd8zkLP5yHOKY2YG8WMuJg4FADbZd1WUEOjbfgi5j0jbaumSOClFtLmkJOu0oYjC%2FEyz0FCw%2BBJino1GfZIQi1OsBEcFErSzELsSJoCVpWgBqQpUpKN2y%2BkbOZjNsqstEdv0ClYnfW0FPhhNlgGmqgLxRNbcn4B46BMK16Ml1nfL4v1w%2FYZDdO3v3mBnw3Z2HPpfq3IuZCia%2FgNgzCOfmJ%2BGywmdssAzm60ftyI%2FS5oPnbbYu%2FFmu7eO74Ud9es%2FDgOAuOVDcPwDQ%3D%3D&file-path-to-open=src%2FMain.elm",
                expectedCompositionId = "dc36a13696bb67efe1c6376cf8b980c0d0ebbfb7946bd2df7a0be9450cf5e099"
            },
            new
            {
                input = "https://elm-editor.com/?project-state-deflate-base64=jY%2FNasMwEITfZc9OJMuO3RhyiAs9lUIbSH%2BMCJa9tgWWFSQ5UIzevUqh0F7a3nZnZ7%2BdXeCCxko9nRhl8YnGUCwgaotQwODc2RaE9NINs1g3WpGjlIYI7SxxBpFkaZZ3adZkLBFxnG5YxyiNRUO7JKdZl28pbWOW3xCpziMqnBxxGMraoSUmCEqgWQXeyqJzcuotRNDKrkODU4N3RqvyM8sCwa0v%2BKBbtFBUPIJmqKcey1GLq1BVUGq3xlEBj6oFnnC2WL6HO2G4yRPuowX2bfslhcZe0VkaHr3tdzvwv1ruD7R%2FPJTz20vTv7KtE8%2FHud3rv9b%2BQf5m%2BRGaJTTlnnPuvf8A&file-path-to-open=Bot.elm",
                expectedCompositionId = "ff93dc32233db40c9c58bf0d69ce6e4a361eed0eb044abd9a73aab1db631cbaf",
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
                    loaded.tree.EnumerateBlobsTransitive()
                    .Select(blobAtPath =>
                    {
                        string? utf8 = null;

                        try
                        {
                            utf8 = System.Text.Encoding.UTF8.GetString(blobAtPath.blobContent.Span);
                        }
                        catch { }

                        return
                            new
                            {
                                blobAtPath.path,
                                blobAtPath.blobContent,
                                utf8
                            };
                    })
                    .ToImmutableList();

                var composition = PineValueComposition.FromTreeWithStringPath(loaded.tree);
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
