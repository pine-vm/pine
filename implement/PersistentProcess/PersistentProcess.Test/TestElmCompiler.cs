using FluentAssertions;
using Kalmit.ProcessStore;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using MoreLinq;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

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
    }
}
