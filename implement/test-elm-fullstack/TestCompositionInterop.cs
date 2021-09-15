using System;
using System.Collections.Immutable;
using System.Linq;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Pine;

namespace test_elm_fullstack
{
    [TestClass]
    public class TestCompositionInterop
    {
        [TestMethod]
        public void Composition_from_link_in_elm_editor()
        {
            var testCases = new[]
            {
                new
                {
                    input = "https://elm-editor.com/?project-state=https%3A%2F%2Fgithub.com%2Felm-fullstack%2Felm-fullstack%2Ftree%2F742650b6a6f1e3dc723d76fbb8c189ca16a0bee6%2Fimplement%2Fexample-apps%2Felm-editor%2Fdefault-app&file-path-to-open=src%2FMain.elm",
                    expectedCompositionId = "ba36b62d7a0e2ffd8ed107782138be0e2b25257a67dc9273508b00daa003b6f3"
                },
                new
                {
                    input = "https://elm-editor.com/?project-state-deflate-base64=XZDLasMwEEX%2FZdZO5Ecip97FLYVQWmi3RgQ9xg9qW0aSQ4vRv9cyZNHsZg5zzwyzwA2N7fR4TeM0ucYJFAsIbhEKaJ2bbEFI07l2FnupB4L9sKvnvreOy%2B%2BHzhlEkh9SeowF5bROMFMyTzOV01qIk0xOT5InlMcCkZJumHoccHQEf3iod3ya7KZE1TltiMKaz70LHCJQXV2jwVHiq9FDuV24gMFB3%2FBDK7RQVCwC2fKxwbLXIoCqAmvkmn7n3bhf3cCiaoEvnC2Wv24LHbOc%2BSjAoLpTurGzUnewNjZspYf1M5fnc3N5%2BXwDv4399x0z5hlj3vs%2F&file-path-to-open=src%2FMain.elm",
                    expectedCompositionId = "c34a6a5e4ee0ea6308c9965dfbfbe68d28ecc07dca1cba8f9a2dac50700324e9"
                },
                new
                {
                    input = "https://elm-editor.com/?project-state-deflate-base64=dZDJasMwFEX%2FRWsnnmJ5gC6SDhBKQ9NFQ2pM0PDsmNqWkeTQYPzvtUwDbUl2ege9ew%2BvRyeQqhTNwXM89%2BC4KOkRJQpQgo5atyqx7aLUx47OmahtqOpZ3lWV0oR9%2Fpu0BLDDhYcDh2KCcxd8zkLP5yHOKY2YG8WMuJg4FADbZd1WUEOjbfgi5j0jbaumSOClFtLmkJOu0oYjC%2FEyz0FCw%2BBJino1GfZIQi1OsBEcFErSzELsSJoCVpWgBqQpUpKN2y%2BkbOZjNsqstEdv0ClYnfW0FPhhNlgGmqgLxRNbcn4B46BMK16Ml1nfL4v1w%2FYZDdO3v3mBnw3Z2HPpfq3IuZCia%2FgNgzCOfmJ%2BGywmdssAzm60ftyI%2FS5oPnbbYu%2FFmu7eO74Ud9es%2FDgOAuOVDcPwDQ%3D%3D&file-path-to-open=src%2FMain.elm",
                    expectedCompositionId = "037e66cbbb5c06cecb6760efcbb0ad4c7b8e4ed036f331e1a28c897839dd55b1"
                }
            };

            foreach (var testCase in testCases)
            {
                try
                {
                    var loadResult = LoadFromPath.LoadTreeFromPath(testCase.input);

                    if (loadResult.Ok.tree == null)
                        throw new Exception("Failed to load from path: " + loadResult.Err);

                    var inspectComposition =
                        loadResult.Ok.tree.EnumerateBlobsTransitive()
                        .Select(blobAtPath =>
                        {
                            string utf8 = null;

                            try
                            {
                                utf8 = System.Text.Encoding.UTF8.GetString(blobAtPath.blobContent.ToArray());
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

                    var composition = Composition.FromTreeWithStringPath(loadResult.Ok.tree);
                    var compositionId = CommonConversion.StringBase16FromByteArray(Composition.GetHash(composition));

                    Assert.AreEqual(testCase.expectedCompositionId, compositionId);
                }
                catch (Exception e)
                {
                    throw new Exception("Failed in test case " + testCase.input, e);
                }
            }
        }
    }
}
