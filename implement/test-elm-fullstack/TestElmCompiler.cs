using System;
using System.Collections.Immutable;
using System.Linq;
using Kalmit;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace test_elm_fullstack
{
    [TestClass]
    public class TestElmCompiler
    {
        [TestMethod]
        public void Expose_name_in_Elm_module()
        {
            var testCases = new[]
            {
                new
                {
                    originalElmModule = @"module Module exposing (somethingElse)",
                    expectedElmModule = @"module Module exposing (somethingElse, nameToExpose)"
                },
                new
                {
                    originalElmModule = @"module Module exposing (somethingElse, nameToExpose)",
                    expectedElmModule = @"module Module exposing (somethingElse, nameToExpose)"
                },
                new
                {
                    originalElmModule = @"module Module exposing (nameToExpose, somethingElse)",
                    expectedElmModule = @"module Module exposing (nameToExpose, somethingElse)"
                },
                new
                {
                    originalElmModule = @"module Namespace.Module exposing (UnrelatedType(TagA,TagB), somethingElse)",
                    expectedElmModule = @"module Namespace.Module exposing (UnrelatedType(TagA,TagB), somethingElse, nameToExpose)"
                },
                new
                {
                    originalElmModule = @"module Module exposing (..)",
                    expectedElmModule = @"module Module exposing (..)"
                },
                new
                {
                    originalElmModule = @"module Module exposing
                        ( somethingElse)",
                    expectedElmModule = @"module Module exposing
                        ( somethingElse, nameToExpose)",
                },
                new
                {
                    originalElmModule = @"
{- Comment before module declaration
-}

module Module exposing ( somethingElse)",
                    expectedElmModule = @"
{- Comment before module declaration
-}

module Module exposing ( somethingElse, nameToExpose)",
                },
            };

            foreach (var testCase in testCases)
            {
                var compiledModule = Kalmit.CompileElm.ExposeValueInElmModule(testCase.originalElmModule, "nameToExpose");

                Assert.AreEqual(testCase.expectedElmModule, compiledModule);
            }
        }

        [TestMethod]
        public void Expose_custom_type_all_tags_in_Elm_module()
        {
            var testCases = new[]
            {
                new
                {
                    originalElmModule = @"module Module exposing (somethingElse)",
                    expectedElmModule = @"module Module exposing (somethingElse, TypeToExpose(..))"
                },
                new
                {
                    originalElmModule = @"module Module exposing (somethingElse, TypeToExpose)",
                    expectedElmModule = @"module Module exposing (somethingElse, TypeToExpose(..))"
                },
                new
                {
                    originalElmModule = @"module Module exposing (somethingElse, TypeToExpose (..))",
                    expectedElmModule = @"module Module exposing (somethingElse, TypeToExpose (..))"
                },
                new
                {
                    originalElmModule = @"module Module exposing (somethingElse, TypeToExpose(SomeTag))",
                    expectedElmModule = @"module Module exposing (somethingElse, TypeToExpose(..))"
                },
                new
                {
                    originalElmModule = @"module Module exposing (..)",
                    expectedElmModule = @"module Module exposing (..)"
                },
            };

            foreach (var testCase in testCases)
            {
                var compiledModule = Kalmit.CompileElm.ExposeCustomTypeAllTagsInElmModule(testCase.originalElmModule, "TypeToExpose");

                Assert.AreEqual(testCase.expectedElmModule, compiledModule);
            }
        }

        [TestMethod]
        public void Canonicalize_Elm_type_text_and_compile_lowered_Elm_app()
        {
            var testCases = new[]
            {
                new
                {
                    elmModules = new[]
                    {
                        (moduleName: "RootModule",
                        moduleText: @"
type alias Record =
    {}
")
                    },
                    typeToResolve = @"Record",
                    typeToResolveModule = @"RootModule",
                    expectedCanonicalText = "{}"
                },
                new
                {
                    elmModules = new[]
                    {
                        (moduleName: "RootModule",
                        moduleText: @"
type alias Record =
    { a : Int
    }
")
                    },
                    typeToResolve = @"Record",
                    typeToResolveModule = @"RootModule",
                    expectedCanonicalText = "{a:Int}"
                },
                new
                {
                    elmModules = new[]
                    {
                        (moduleName: "RootModule",
                        moduleText: @"
type alias Record =
    { a : Int, b : String }
")
                    },
                    typeToResolve = @"Record",
                    typeToResolveModule = @"RootModule",
                    expectedCanonicalText = "{a:Int,b:String}"
                },
                new
                {
                    elmModules = new[]
                    {
                        (moduleName: "RootModule",
                        moduleText: @"
type alias Record =
    { a : Int, b : (String, Int) }
")
                    },
                    typeToResolve = @"Record",
                    typeToResolveModule = @"RootModule",
                    expectedCanonicalText = "{a:Int,b:(String,Int)}"
                },
                new
                {
                    elmModules = new[]
                    {
                        (moduleName: "RootModule",
                        moduleText: @"
type alias Record =
    { a : Int, b : Result String Int, c : String }
")
                    },
                    typeToResolve = @"Record",
                    typeToResolveModule = @"RootModule",
                    expectedCanonicalText = "{a:Int,b:Result String Int,c:String}"
                },
                new
                {
                    elmModules = new[]
                    {
                        (moduleName: "RootModule",
                        moduleText: @"
type alias Record =
    { a : Int
    , b : Result String Int
    , c : (String, Int)
    , d : List { daa : String, dab : List String }
    }
")
                    },
                    typeToResolve = @"Record",
                    typeToResolveModule = @"RootModule",
                    expectedCanonicalText = "{a:Int,b:Result String Int,c:(String,Int),d:List {daa:String,dab:List String}}"
                },
                new
                {
                    elmModules = new[]
                    {
                        (moduleName: "RootModule",
                        moduleText: @"
type alias Record =
    { a : Int
    , b : Result (Maybe String) (Result String Int)
    }
")
                    },
                    typeToResolve = @"Record",
                    typeToResolveModule = @"RootModule",
                    expectedCanonicalText = "{a:Int,b:Result (Maybe String) (Result String Int)}"
                },
                new
                {
                    elmModules = new[]
                    {
                        (moduleName: "RootModule",
                        moduleText: @"
import OtherModule exposing (..)

type alias RootRecord =
    { a : Int
    , custom : OtherModule.CustomType
    , record : OtherModule.Record
    }
"),
                        (moduleName: "OtherModule",
                        moduleText: @"
type alias Record =
    { fieldFromOtherModule : Int
    }

type CustomType
    = TagA
    | TagB
")
                    },
                    typeToResolve = @"RootRecord",
                    typeToResolveModule = @"RootModule",
                    expectedCanonicalText = "{a:Int,custom:OtherModule.CustomType,record:{fieldFromOtherModule:Int}}"
                },
                new
                {
                    elmModules = new[]
                    {
                        (moduleName: "RootModule",
                        moduleText: @"
type alias Record =
    { customTypeInstance : CustomTypeWithTypeParameter Int }

type CustomTypeWithTypeParameter a
    = CustomTypeWithTypeParameter a

")
                    },
                    typeToResolve = @"Record",
                    typeToResolveModule = @"RootModule",
                    expectedCanonicalText = "{customTypeInstance:RootModule.CustomTypeWithTypeParameter Int}"
                },
                new
                {
                    elmModules = new[]
                    {
                        (moduleName: "RootModule",
                        moduleText: @"
import ListDict exposing (..)

type alias DictionaryInstance =
    ListDict.Dict LinkDirection LinkProperties

type alias LinkDirection =
    { a : Int
    , b : Int
    }

type alias LinkProperties =
    { linkProperty : Int }
"),
                        (moduleName: "ListDict",
                        moduleText: @"
type Dict key value
    = Dict (List ( key, value ))
")
                    },
                    typeToResolve = @"DictionaryInstance",
                    typeToResolveModule = @"RootModule",
                    expectedCanonicalText = "ListDict.Dict {a:Int,b:Int} {linkProperty:Int}"
                },
            };

            foreach (var testCase in testCases)
            {
                var elmModules =
                    testCase.elmModules
                    .ToImmutableDictionary(module => module.moduleName,
                    module =>
                    "module " + module.moduleName + " exposing (..)\n\n" +
                    "interfaceToHost_processEvent : String -> " + testCase.typeToResolve + " -> ( " + testCase.typeToResolve + ", String )\n\n" +
                    module.moduleText);

                var resolvedType = Kalmit.CompileElmValueSerializer.ResolveType(
                    testCase.typeToResolve,
                    testCase.typeToResolveModule,
                    elmModules,
                    Console.WriteLine);

                Assert.AreEqual(testCase.expectedCanonicalText, resolvedType.canonicalTypeText);

                try
                {
                    var originalAppFilesList =
                        elmModules
                        .Select(module => ((IImmutableList<string>)ImmutableList.Create("src", module.Key + ".elm"),
                            (IImmutableList<byte>)System.Text.Encoding.UTF8.GetBytes(module.Value).ToImmutableList()))
                        .ToImmutableList();

                    var originalAppFiles =
                        ElmApp.ToFlatDictionaryWithPathComparer(originalAppFilesList);

                    var loweredElmApp =
                        ElmApp.AsCompletelyLoweredElmApp(
                            sourceFiles: originalAppFiles,
                            new ElmAppInterfaceConfig { RootModuleName = "RootModule" },
                            Console.WriteLine);
                }
                catch (Exception e)
                {
                    throw new Exception("Building lowered Elm app failed for test case with expected canonical text of '" + testCase.expectedCanonicalText + "'.", e);
                }
            }
        }
    }
}
