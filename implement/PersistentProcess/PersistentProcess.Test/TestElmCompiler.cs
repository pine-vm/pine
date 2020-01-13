using System.Linq;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace Kalmit.PersistentProcess.Test
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
        public void Canonicalize_Elm_type_text()
        {
            var testCases = new[]
            {
                new
                {
                    elmModules = new[]
                    {
                        (moduleName: "Module",
                        moduleText: @"
type alias Record =
    {}
")
                    },
                    typeToResolve = @"Record",
                    typeToResolveModule = @"Module",
                    expectedCanonicalText = "{}"
                },
                new
                {
                    elmModules = new[]
                    {
                        (moduleName: "Module",
                        moduleText: @"
type alias Record =
    { a : Int
    }
")
                    },
                    typeToResolve = @"Record",
                    typeToResolveModule = @"Module",
                    expectedCanonicalText = "{a:Int}"
                },
                new
                {
                    elmModules = new[]
                    {
                        (moduleName: "Module",
                        moduleText: @"
type alias Record =
    { a : Int, b : String }
")
                    },
                    typeToResolve = @"Record",
                    typeToResolveModule = @"Module",
                    expectedCanonicalText = "{a:Int,b:String}"
                },
                new
                {
                    elmModules = new[]
                    {
                        (moduleName: "Module",
                        moduleText: @"
type alias Record =
    { a : Int, b : (String, Int) }
")
                    },
                    typeToResolve = @"Record",
                    typeToResolveModule = @"Module",
                    expectedCanonicalText = "{a:Int,b:(String,Int)}"
                },
                new
                {
                    elmModules = new[]
                    {
                        (moduleName: "Module",
                        moduleText: @"
type alias Record =
    { a : Int, b : Result String Int, c : String }
")
                    },
                    typeToResolve = @"Record",
                    typeToResolveModule = @"Module",
                    expectedCanonicalText = "{a:Int,b:Result String Int,c:String}"
                },
                new
                {
                    elmModules = new[]
                    {
                        (moduleName: "Module",
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
                    typeToResolveModule = @"Module",
                    expectedCanonicalText = "{a:Int,b:Result String Int,c:(String,Int),d:List {daa:String,dab:List String}}"
                },
                new
                {
                    elmModules = new[]
                    {
                        (moduleName: "Module",
                        moduleText: @"
type alias Record =
    { a : Int
    , b : Result (Maybe String) (Result String Int)
    }
")
                    },
                    typeToResolve = @"Record",
                    typeToResolveModule = @"Module",
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
            };

            foreach (var testCase in testCases)
            {
                var resolvedType = Kalmit.CompileElmValueSerializer.ResolveType(
                    testCase.typeToResolve,
                    testCase.typeToResolveModule,
                    moduleName =>
                    {
                        var matchingModuleEntry = testCase.elmModules.FirstOrDefault(c => c.moduleName == moduleName);

                        if (matchingModuleEntry.moduleName == null)
                            return null;

                        return "module " + moduleName + " exposing (..)\n\n" + matchingModuleEntry.moduleText;
                    });

                Assert.AreEqual(testCase.expectedCanonicalText, resolvedType.canonicalTypeText);
            }
        }
    }
}
