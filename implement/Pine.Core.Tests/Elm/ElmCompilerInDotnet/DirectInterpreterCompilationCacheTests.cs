using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm;
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
    public void Caller_can_reuse_eval_cache_across_root_evaluations()
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
                rootDeclarations: [DeclQualifiedName.Create(["Main"], "value")]);

        var firstCompiled =
            firstCompilation.Extract(error => throw new Exception(error)).compiledEnvValue;

        var firstEvaluated =
            ElmSourceCompilation.EvaluateZeroParameterRoots(
                firstCompiled,
                DirectInterpreter.WithSharedEvalCache(new PineVMParseCache(), evalCache))
            .Extract(error => throw new Exception(error));

        evalCache.Should().NotBeEmpty();
        var entryCountAfterFirstCompilation = evalCache.Count;

        var secondCompilation =
            ElmCompiler.CompileInteractiveEnvironment(
                appCodeTree,
                rootDeclarations: [DeclQualifiedName.Create(["Main"], "value")]);

        var secondCompiled =
            secondCompilation.Extract(error => throw new Exception(error)).compiledEnvValue;

        secondCompiled.Should().Be(firstCompiled);

        var secondEvaluated =
            ElmSourceCompilation.EvaluateZeroParameterRoots(
                secondCompiled,
                DirectInterpreter.WithSharedEvalCache(new PineVMParseCache(), evalCache))
            .Extract(error => throw new Exception(error));

        secondEvaluated.Should().Be(firstEvaluated);
        evalCache.Should().HaveCount(entryCountAfterFirstCompilation);
    }
}
