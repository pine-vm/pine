using AwesomeAssertions;
using Pine.Core.Addressing;
using Pine.Core.CodeAnalysis;
using Pine.Core.DotNet;
using Pine.Core.Elm;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Elm.ElmInElm;
using Pine.Core.Encodings;
using Pine.Core.Files;
using Pine.Core.Internal;
using Pine.Core.PineLanguage;
using Pine.Core.Runtime.IntermediateVM;
using Pine.Core.Testing;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text.Json;
using System.Threading;
using Xunit;

namespace Pine.Core.Tests.DotNet;

public class ElmToCSharpCompilationTests
{
    [Fact]
    public void Elm_parseExpression_compiles_to_evaluation_closed_CSharp_and_executes_as_DotNet()
    {
        var (function, compilation) = CompileParser("parseExpression");
        using var assembly = Load(compilation);
        AssertCases(function, assembly, "parse-expression.json");
    }

    [Fact]
    public void Elm_parseFile_compiles_to_evaluation_closed_CSharp_and_executes_as_DotNet()
    {
        var (function, compilation) = CompileParser("parseFile");
        var directory = Path.Combine(TestResultSummary.FindTestDataDirectory("ElmToCSharp"), "parse-file", "expect");

        var mismatches =
            PineCSharpScenarios.CompareSnapshots(
                directory,
                new Dictionary<string, string>
                {
                    ["methods.csharp-snapshot"] = compilation.MethodsSource,
                    ["literals.csharp-snapshot"] = compilation.LiteralsSource
                });

        using var assembly = Load(compilation);
        AssertCases(function, assembly, "parse-file.json");
        mismatches.Should().BeEmpty(string.Join("\n", mismatches));
    }

    /// <summary>
    /// Prepares a complete native parser graph without specializing input text.
    /// Profiling harnesses can load it once, warm the entry, and reuse it for repository modules.
    /// </summary>
    public static (FunctionRecord Function, PineCSharpCompilation Compilation) CompileParser(
        string declaration, Action<PineValue, PineVMParseCache>? inspectEnvironment = null)
    {
        var parseCache = new PineVMParseCache();

        var source =
            BundledFiles.CompilerSourceContainerFilesDefault.Value
            .GetNodeAtPath(["pine-elm-syntax", "src"])
            ?? throw new InvalidOperationException("Missing pine-elm-syntax source tree.");

        var modules = BundledFiles.ElmKernelModulesDefault.Value;

        foreach (var (path, file) in source.EnumerateFilesTransitive())
            modules = modules.SetNodeAtPathSorted(path, FileTree.File(file));

        var compiledElm =
            ElmCompiler.CompileInteractiveEnvironment(
                modules,
                rootDeclarations: [DeclQualifiedName.FromString("ElmSyntax.Concrete.Parser.FromString." + declaration)])
            .Extract(error => throw new Exception("Elm compilation failed: " + error));

        inspectEnvironment?.Invoke(compiledElm.compiledEnvValue, parseCache);

        var function =
            ElmInteractiveEnvironment.ParseFunctionFromElmModule(
                compiledElm.compiledEnvValue,
                "ElmSyntax.Concrete.Parser.FromString",
                declaration,
                parseCache)
            .Extract(error => throw new Exception(error)).functionRecord;

        function.ParameterCount.Should().Be(1);
        function.ArgumentsAlreadyCollected.Length.Should().Be(0);

        var functions = PineValue.List(function.EnvFunctions);

        var constraint =
            PineValueClass.Create(
                [new KeyValuePair<IReadOnlyList<int>, PineValue>([0], functions)]);

        var compilation =
            PineCSharpCompiler.Compile(
                function.InnerFunction,
                [constraint],
                new(IncludeGenericEntry: false, ContainerNamespace: "Pine.Generated.ElmParserRegression"),
                parseCache,
                new ConcurrentPineValueHashCache())
            .Extract(
                error =>
                {
                    var names = NamesFromCompiledEnv.FromCompiledEnvironment(compiledElm.compiledEnvValue, parseCache);
                    var path = new Stack<string>();

                    for (var current = error.Path; current is not null; current = current.Parent)
                    {
                        var encoded = ExpressionEncoding.EncodeExpressionAsValue(current.Function.Expression);

                        var declarations =
                            names.NamesForDecl(encoded, current.Function.EnvironmentConstraint ?? PineValueClass.Empty);

                        path.Push(
                            declarations is { Count: > 0 }
                            ?
                            string.Join(", ", declarations)
                            :
                            "function " + current.Function.Index);
                    }

                    var plan = error.Instruction?.TemplatePlan;

                    var ops =
                        error.Graph!.Blocks[error.Block.Value].Operations.Select(
                            op => StackInstruction.RenderInstructionDisplay(
                                op.Instruction,
                                value => value is PineValue.ListValue { NodesCount: > 30 }
                                ?
                                "<large literal>"
                                :
                                StackInstruction.LiteralDisplayStringDefault(value)));

                    throw new Exception(
                        error.Describe(includeExpressionHashes: true) +
                        "\nSource path: " + string.Join(" -> ", path) +
                        "\nTemplate: " +
                        plan?.EnvironmentCount +
                        ", producer: " +
                        error.Instruction?.TemplateProducer +
                        "\nOffending block:\n" + string.Join("\n", ops));
                });

        return (function, compilation);
    }

    private static PineCSharpAssembly Load(PineCSharpCompilation compilation) =>
        PineCSharpAssembly.Load(compilation)
        .Extract(
            errors =>
            throw new Exception(
                string.Join(
                    "\n",
                    errors.Where(error => error.Severity == Microsoft.CodeAnalysis.DiagnosticSeverity.Error))));

    private static void AssertCases(FunctionRecord function, PineCSharpAssembly assembly, string fixtureName)
    {
        var fixture = Path.Combine(TestResultSummary.FindTestDataDirectory("ElmToCSharp"), fixtureName);
        using var cases = JsonDocument.Parse(File.ReadAllText(fixture));
        cases.RootElement.GetArrayLength().Should().BeGreaterThan(0);

        foreach (var pair in cases.RootElement.EnumerateArray())
        {
            pair.GetArrayLength().Should().Be(2);
            var input = pair[0].GetString()!;
            var expected = pair[1].GetString()!;
            var argument = ElmValueEncoding.ElmValueAsPineValue(ElmValue.StringInstance(input));

            var environment =
                ElmInteractiveEnvironment.ApplyFunctionArgumentsForEvalExpr(function, [argument])
                .Extract(error => throw new Exception(error)).environment;

            var result = assembly.Evaluate(PineValueInProcess.Create(environment), CancellationToken.None);
            result.Should().NotBeNull("the parser's fixed code environment must select the native specialization");

            var value =
                ElmValueEncoding.PineValueAsElmValue(result!.Evaluate(), null, null)
                .Extract(error => throw new Exception(error));

            ElmValue.RenderAsElmExpression(value).expressionString.Should().Be(expected, "input {0}", input);
        }
    }
}
