using AwesomeAssertions;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Interpreter;
using Pine.Core.Tests.Elm.ElmCompilerTests;
using System;
using System.Collections.Generic;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet;

public class DirectInterpreterCompilationCacheTests
{
    [Fact]
    public void Caller_can_reuse_eval_cache_across_compilations()
    {
        var appCodeTree =
            TestCase.FileTreeFromElmModulesWithoutPackages(
                [
                """
                module Main exposing (value)

                value =
                    42
                """
                ]);

        var evalCache = new Dictionary<DirectInterpreter.EvalCacheEntryKey, PineValue>();

        var firstCompilation =
            ElmCompiler.CompileInteractiveEnvironment(
                appCodeTree,
                rootFilePaths: [["src", "Main.elm"]],
                directInterpreterEvalCache: evalCache);

        var firstCompiled =
            firstCompilation.Extract(error => throw new Exception(error)).compiledEnvValue;

        evalCache.Should().NotBeEmpty();

        var entryCountAfterFirstCompilation = evalCache.Count;

        var secondCompilation =
            ElmCompiler.CompileInteractiveEnvironment(
                appCodeTree,
                rootFilePaths: [["src", "Main.elm"]],
                directInterpreterEvalCache: evalCache);

        var secondCompiled =
            secondCompilation.Extract(error => throw new Exception(error)).compiledEnvValue;

        secondCompiled.Should().Be(firstCompiled);
        evalCache.Should().HaveCount(entryCountAfterFirstCompilation);
    }
}
