using ElmTime.ElmInteractive;
using ElmTime.ElmSyntax;
using FluentAssertions;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Pine.Core;
using Pine.Core.Elm;
using Pine.Core.PopularEncodings;
using Pine.Elm;
using Pine.PineVM;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text;

namespace TestElmTime;

[TestClass]
public class PGOTests
{
    static readonly PineVMParseCache ParseCache = new();

    [TestMethod]
    public void PGO_reduces_Elm_record_access()
    {
        var elmModule =
            """
            module Test exposing (..)

            usingRecordAccess record fieldId =
                case fieldId of
                    0 ->
                        record.alfa

                    1 ->
                        record.delta

                    _ ->
                        record.other

            """;

        var appCodeTree = AppCodeTreeForElmModules([elmModule]);

        using var interactiveSession =
            new InteractiveSessionPine(
                ElmCompiler.CompilerSourceFilesDefault.Value,
                appCodeTree: appCodeTree,
                overrideSkipLowering: true,
                entryPointsFilePaths: null,
                caching: true,
                autoPGO: null);

        // Force integration of the 'Test' module.
        var testSubmissionResult = interactiveSession.Submit(" Test.usingRecordAccess { alfa = 4, delta = 71 }  0 ");

        var testSubmissionResponse =
            testSubmissionResult.Extract(err => throw new Exception(err));

        testSubmissionResponse.InteractiveResponse.DisplayText.Should().Be("4");

        var interactiveEnvironmentValue = interactiveSession.CurrentEnvironmentValue();

        var (_, usingRecordAccessFunction) =
            ElmInteractiveEnvironment.ParseFunctionFromElmModule(
                interactiveEnvironment: interactiveEnvironmentValue,
                moduleName: "Test",
                declarationName: "usingRecordAccess",
                ParseCache)
            .Extract(err => throw new Exception(err));

        var usingRecordAccessScenarios = new[]
        {
            new
            {
                record =
                new ElmValue.ElmRecord(
                    [
                    ("alfa", ElmValue.Integer(13)),
                    ("delta", ElmValue.Integer(17))
                    ]),

                fieldId = 0,

                expected = IntegerEncoding.EncodeSignedInteger(13)
            },

            new
            {
                record =
                new ElmValue.ElmRecord(
                    [
                    ("alfa", ElmValue.Integer(41)),
                    ("delta", ElmValue.Integer(47))
                    ]),

                fieldId = 1,

                expected = IntegerEncoding.EncodeSignedInteger(47)
            },

            new
            {
                record =
                new ElmValue.ElmRecord(
                    [
                    ("alfa", ElmValue.StringInstance("Arancino")),
                    ("delta", ElmValue.StringInstance("Bruschetta"))
                    ]),

                fieldId = 0,

                expected = ElmValueEncoding.ElmValueAsPineValue(ElmValue.StringInstance("Arancino"))
            },

            new
            {
                record =
                new ElmValue.ElmRecord(
                    [
                    ("alfa", ElmValue.StringInstance("hello")),
                    ("delta", ElmValue.StringInstance("world"))
                    ]),

                fieldId = 1,

                expected = ElmValueEncoding.ElmValueAsPineValue(ElmValue.StringInstance("world"))
            },

            new
            {
                record =
                new ElmValue.ElmRecord(
                    [
                    ("alfa", ElmValue.Integer(89)),
                    ("other", ElmValue.Integer(97))
                    ]),

                fieldId = 3,

                expected = ElmValueEncoding.ElmValueAsPineValue(ElmValue.Integer(97))
            },

            new
            {
                record =
                new ElmValue.ElmRecord(
                    [
                    ("alfa", ElmValue.Integer(41)),
                    ("beta", ElmValue.Integer(43)),
                    ("gamma", ElmValue.Integer(47)),
                    ("delta", ElmValue.Integer(49))
                    ]),

                fieldId = 1,

                expected = IntegerEncoding.EncodeSignedInteger(49)
            },

            new
            {
                record =
                new ElmValue.ElmRecord(
                    [
                    ("alfa", ElmValue.Integer(71)),
                    ("beta", ElmValue.Integer(73)),
                    ("gamma", ElmValue.Integer(79)),
                    ("delta", ElmValue.Integer(83))
                    ]),

                fieldId = 0,

                expected = IntegerEncoding.EncodeSignedInteger(71)
            },

            new
            {
                record =
                new ElmValue.ElmRecord(
                    [
                    ("alfa", ElmValue.StringInstance("Arancino")),
                    ("beta", ElmValue.Integer(43)),
                    ("gamma", ElmValue.Integer(47)),
                    ("delta", ElmValue.Integer(49)),
                    ]),

                fieldId = 0,

                expected = ElmValueEncoding.ElmValueAsPineValue( ElmValue.StringInstance("Arancino"))
            },

            new
            {
                record =
                new ElmValue.ElmRecord(
                    [
                    ("alfa", ElmValue.StringInstance("Arancini")),
                    ("beta", ElmValue.StringInstance("Bruschette")),
                    ("gamma", ElmValue.Integer(123)),
                    ("delta", ElmValue.StringInstance("Dolmades")),
                    ]),

                fieldId = 1,

                expected = ElmValueEncoding.ElmValueAsPineValue(ElmValue.StringInstance("Dolmades"))
            },

            new
            {
                record =
                new ElmValue.ElmRecord(
                    [
                    ("alfa", ElmValue.Integer(11)),
                    ("beta", ElmValue.Integer(13)),
                    ("gamma", ElmValue.Integer(17)),
                    ("delta", ElmValue.Integer(31)),
                    ]),

                fieldId = 1,

                expected = ElmValueEncoding.ElmValueAsPineValue(ElmValue.Integer(31))
            },

            new
            {
                record =
                new ElmValue.ElmRecord(
                    [
                    ("alfa", ElmValue.Integer(21)),
                    ("beta", ElmValue.Integer(23)),
                    ("gamma", ElmValue.Integer(27)),
                    ("delta", ElmValue.Integer(41)),
                    ]),

                fieldId = 1,

                expected = ElmValueEncoding.ElmValueAsPineValue(ElmValue.Integer(41))
            },

            new
            {
                record =
                new ElmValue.ElmRecord(
                    [
                    ("alfa", ElmValue.StringInstance("Arancino")),
                    ("beta", ElmValue.StringInstance("Bruschetta")),
                    ("other", ElmValue.Integer(101)),
                    ("delta", ElmValue.StringInstance("Dolmades")),
                    ]),

                fieldId = 3,

                expected = ElmValueEncoding.ElmValueAsPineValue(ElmValue.Integer(101))
            },

        };

        static long ReportsAverageInstructionCount(IReadOnlyList<PineVM.EvaluationReport> reports) =>
            reports.Sum(report => report.InstructionCount) / reports.Count;

        IReadOnlyList<PineVM.EvaluationReport> RunScenariosWithGivenVM(PineVM pineVM) =>
            [.. usingRecordAccessScenarios
            .Select(scenario =>
            {
                return
                ElmInteractiveEnvironment.ApplyFunctionArgumentsForEvalExpr(
                        usingRecordAccessFunction,
                        appendArguments:
                        [
                            ElmValueEncoding.ElmValueAsPineValue(scenario.record),
                            IntegerEncoding.EncodeSignedInteger(scenario.fieldId)
                        ])
                .AndThen(composedArgs =>
                pineVM.EvaluateExpressionOnCustomStack(
                    composedArgs.expression,
                    composedArgs.environment,
                    new PineVM.EvaluationConfig(ParseAndEvalCountLimit: 1234))
                .Map(evalReport =>
                {
                    evalReport.ReturnValue.Should().Be(scenario.expected);

                    Console.WriteLine(
                        "Completed scenario using " + evalReport.InstructionCount +
                        " instructions and " + evalReport.InvocationCount + " invocations");

                    return evalReport;
                }))
                .Extract(fromErr: err => throw new Exception("Failed for scenario: " + err));
            })];

        var nonOptimizingPineVM =
            new PineVM(
                disablePrecompiled: true);

        var nonOptimizedScenariosStats =
            RunScenariosWithGivenVM(nonOptimizingPineVM);

        var nonOptimizedAverageInstructionCount =
            ReportsAverageInstructionCount(nonOptimizedScenariosStats);

        Console.WriteLine("\nAverage instruction count not optimized: " + nonOptimizedAverageInstructionCount + "\n");

        nonOptimizedAverageInstructionCount.Should().BeGreaterThan(40);

        var invocationReports = new List<PineVM.EvaluationReport>();

        var profilingVM =
            new PineVM(
                evalCache: null,
                reportFunctionApplication: invocationReports.Add,
                disableReductionInCompilation: true,
                disablePrecompiled: true);

        RunScenariosWithGivenVM(profilingVM);

        Console.WriteLine(
            "Collected " + invocationReports.Count + " invocation reports from " +
            usingRecordAccessScenarios.Length + " scenarios.");

        var codeAnalysisStopwatch = System.Diagnostics.Stopwatch.StartNew();

        var compilationEnvClasses =
            CodeAnalysis.EnvironmentClassesFromInvocationReports(
                invocationReports,
                limitInvocationSampleCount: 1000,
                limitSampleCountPerSample: 40,
                classUsageCountMin: 4,
                limitClassesPerExpression: 30);

        codeAnalysisStopwatch.Stop();

        Console.WriteLine(
            "Analyzed " + invocationReports.Count + " invocation reports in " +
            codeAnalysisStopwatch.ElapsedMilliseconds + " ms and selected " +
            compilationEnvClasses.Sum(exprGroup => exprGroup.Value.Count) + " total environment classes for " +
            compilationEnvClasses.Count(exprGroup => 0 < exprGroup.Value.Count) + " expressions.");

        {
            foreach (var exprEnvClasses in compilationEnvClasses)
            {
                var exprValueHash =
                    PineValueHashTree.ComputeHash(
                        ExpressionEncoding.EncodeExpressionAsValue(exprEnvClasses.Key));

                for (var i = 0; i < exprEnvClasses.Value.Count; i++)
                {
                    var envClass = exprEnvClasses.Value[i];

                    Console.WriteLine(
                        "\nEnvironment class [" + i + "] " + envClass.HashBase16[..8] +
                        " for expr " + Convert.ToHexStringLower(exprValueHash.Span)[..8] +
                        " has " + envClass.ParsedEnvItems.Count + " env items:");

                    var envItems = envClass.ParsedEnvItems.ToArray();

                    for (var j = 0; j < envClass.ParsedEnvItems.Count; j++)
                    {
                        var envItem = envItems[j];

                        var envItemValueHash = PineValueHashTree.ComputeHash(envItem.Value);

                        var envItemDisplayText =
                            Convert.ToHexStringLower(envItemValueHash.Span)[..8] +
                            " - " +
                            ElmValueEncoding.PineValueAsElmValue(envItem.Value, null, null)
                            .Unpack(
                                fromErr: _ =>
                                "???",
                                fromOk:
                                elmValue =>
                                {
                                    var asExprString = ElmValue.RenderAsElmExpression(elmValue).expressionString;

                                    if (asExprString.Length < 100)
                                        return asExprString;

                                    return asExprString[..100] + "...";
                                });

                        Console.WriteLine(
                            "Item [" + j + "]: " + string.Join("-", envItem.Key) + ": " + envItemDisplayText);
                    }
                }
            }
        }

        var optimizedPineVM =
            new PineVM(
                evalCache: null,
                reportFunctionApplication: null,
                compilationEnvClasses: compilationEnvClasses);


        var optimizedScenariosStats =
            RunScenariosWithGivenVM(optimizedPineVM);

        var optimizedAverageInstructionCount =
            ReportsAverageInstructionCount(optimizedScenariosStats);

        Console.WriteLine("\nAverage instruction count optimized: " + optimizedAverageInstructionCount + "\n");

        var speedupFactor = nonOptimizedAverageInstructionCount / optimizedAverageInstructionCount;

        speedupFactor.Should().BeGreaterThanOrEqualTo(2);

        optimizedAverageInstructionCount.Should().BeLessThanOrEqualTo(40);
    }

    [TestMethod]
    public void PGO_reduces_Elm_record_update()
    {
        var elmModule =
            """
            module Test exposing (..)

            usingRecordUpdate record fieldId fieldValue =
                case fieldId of
                    0 ->
                        { record | alfa = fieldValue }

                    1 ->
                        { record | delta = fieldValue }

                    _ ->
                        { record | zz_other = fieldValue }

            """;

        var appCodeTree = AppCodeTreeForElmModules([elmModule]);

        using var interactiveSession =
            new InteractiveSessionPine(
                ElmCompiler.CompilerSourceFilesDefault.Value,
                appCodeTree: appCodeTree,
                overrideSkipLowering: true,
                entryPointsFilePaths: null,
                caching: true,
                autoPGO: null);

        // Force integration of the 'Test' module.
        var testSubmissionResult = interactiveSession.Submit(" Test.usingRecordUpdate { alfa = 4, delta = 71 }  0  41 ");

        var testSubmissionResponse =
            testSubmissionResult.Extract(err => throw new Exception(err));

        testSubmissionResponse.InteractiveResponse.DisplayText.Should().Be("{ alfa = 41, delta = 71 }");

        var interactiveEnvironmentValue = interactiveSession.CurrentEnvironmentValue();

        var (_, usingRecordUpdateFunction) =
            ElmInteractiveEnvironment.ParseFunctionFromElmModule(
                interactiveEnvironment: interactiveEnvironmentValue,
                moduleName: "Test",
                declarationName: "usingRecordUpdate",
                ParseCache)
            .Extract(err => throw new Exception(err));

        var recordUpdateScenarios = new[]
        {
            new
            {
                record =
                new ElmValue.ElmRecord(
                    [
                    ("alfa", ElmValue.Integer(13)),
                    ("beta", ElmValue.Integer(43)),
                    ("delta", ElmValue.Integer(17))
                    ]),

                fieldId = 0,

                fieldValue = ElmValue.Integer(73),

                expected =
                new ElmValue.ElmRecord(
                    [
                    ("alfa", ElmValue.Integer(73)),
                    ("beta", ElmValue.Integer(43)),
                    ("delta", ElmValue.Integer(17))
                    ]),
            },

            new
            {
                record =
                new ElmValue.ElmRecord(
                    [
                    ("alfa", ElmValue.Integer(41)),
                    ("beta", ElmValue.Integer(43)),
                    ("delta", ElmValue.Integer(47))
                    ]),

                fieldId = 1,

                fieldValue = ElmValue.Integer(79),

                expected =
                new ElmValue.ElmRecord(
                    [
                    ("alfa", ElmValue.Integer(41)),
                    ("beta", ElmValue.Integer(43)),
                    ("delta", ElmValue.Integer(79))
                    ]),
            },

            new
            {
                record =
                new ElmValue.ElmRecord(
                    [
                    ("alfa", ElmValue.StringInstance("Arancino")),
                    ("beta", ElmValue.Integer(49)),
                    ("delta", ElmValue.StringInstance("Bruschetta"))
                    ]),

                fieldId = 0,

                fieldValue = ElmValue.StringInstance("Pane"),

                expected =
                new ElmValue.ElmRecord(
                    [
                    ("alfa", ElmValue.StringInstance("Pane")),
                    ("beta", ElmValue.Integer(49)),
                    ("delta", ElmValue.StringInstance("Bruschetta"))
                    ])
            },

            new
            {
                record =
                new ElmValue.ElmRecord(
                    [
                    ("alfa", ElmValue.StringInstance("hello")),
                    ("beta", ElmValue.Integer(119)),
                    ("delta", ElmValue.StringInstance("world"))
                    ]),

                fieldId = 1,

                fieldValue = ElmValue.StringInstance("mondo"),

                expected =
                new ElmValue.ElmRecord(
                    [
                    ("alfa", ElmValue.StringInstance("hello")),
                    ("beta", ElmValue.Integer(119)),
                    ("delta", ElmValue.StringInstance("mondo"))
                    ])
            },

            new
            {
                record =
                new ElmValue.ElmRecord(
                    [
                    ("alfa", ElmValue.Integer(89)),
                    ("beta", ElmValue.Integer(113)),
                    ("epsilon", ElmValue.Integer(117)),
                    ("zeta", ElmValue.Integer(119)),
                    ("eta", ElmValue.Integer(121)),
                    ("theta", ElmValue.Integer(123)),
                    ("iota", ElmValue.Integer(127)),
                    ("zz_other", ElmValue.Integer(97))
                    ]),

                fieldId = 3,

                fieldValue = ElmValue.Integer(171),

                expected =
                new ElmValue.ElmRecord(
                    [
                    ("alfa", ElmValue.Integer(89)),
                    ("beta", ElmValue.Integer(113)),
                    ("epsilon", ElmValue.Integer(117)),
                    ("zeta", ElmValue.Integer(119)),
                    ("eta", ElmValue.Integer(121)),
                    ("theta", ElmValue.Integer(123)),
                    ("iota", ElmValue.Integer(127)),
                    ("zz_other", ElmValue.Integer(171))
                    ]),
            },

            new
            {
                record =
                new ElmValue.ElmRecord(
                    [
                    ("alfa", ElmValue.Integer(1_89)),
                    ("beta", ElmValue.Integer(1_113)),
                    ("epsilon", ElmValue.Integer(1_117)),
                    ("zeta", ElmValue.Integer(1_119)),
                    ("eta", ElmValue.Integer(1_121)),
                    ("theta", ElmValue.Integer(1_123)),
                    ("iota", ElmValue.Integer(1_127)),
                    ("zz_other", ElmValue.Integer(1_013))
                    ]),

                fieldId = 3,

                fieldValue = ElmValue.Integer(11_871),

                expected =
                new ElmValue.ElmRecord(
                    [
                    ("alfa", ElmValue.Integer(1_89)),
                    ("beta", ElmValue.Integer(1_113)),
                    ("epsilon", ElmValue.Integer(1_117)),
                    ("zeta", ElmValue.Integer(1_119)),
                    ("eta", ElmValue.Integer(1_121)),
                    ("theta", ElmValue.Integer(1_123)),
                    ("iota", ElmValue.Integer(1_127)),
                    ("zz_other", ElmValue.Integer(11_871))
                    ]),
            },

            new
            {
                record =
                new ElmValue.ElmRecord(
                    [
                    ("alfa", ElmValue.Integer(2_089)),
                    ("beta", ElmValue.Integer(2_113)),
                    ("epsilon", ElmValue.Integer(2_117)),
                    ("zeta", ElmValue.Integer(2_119)),
                    ("eta", ElmValue.Integer(2_121)),
                    ("theta", ElmValue.Integer(2_123)),
                    ("iota", ElmValue.Integer(2_127)),
                    ("zz_other", ElmValue.Integer(2_013))
                    ]),

                fieldId = 3,

                fieldValue = ElmValue.Integer(17_951),

                expected =
                new ElmValue.ElmRecord(
                    [
                    ("alfa", ElmValue.Integer(2_089)),
                    ("beta", ElmValue.Integer(2_113)),
                    ("epsilon", ElmValue.Integer(2_117)),
                    ("zeta", ElmValue.Integer(2_119)),
                    ("eta", ElmValue.Integer(2_121)),
                    ("theta", ElmValue.Integer(2_123)),
                    ("iota", ElmValue.Integer(2_127)),
                    ("zz_other", ElmValue.Integer(17_951))
                    ]),
            },

            new
            {
                record =
                new ElmValue.ElmRecord(
                    [
                    ("alfa", ElmValue.Integer(3_089)),
                    ("beta", ElmValue.Integer(3_113)),
                    ("epsilon", ElmValue.Integer(3_117)),
                    ("zeta", ElmValue.Integer(3_119)),
                    ("eta", ElmValue.Integer(3_121)),
                    ("theta", ElmValue.Integer(3_123)),
                    ("iota", ElmValue.Integer(3_127)),
                    ("zz_other", ElmValue.Integer(3_013))
                    ]),

                fieldId = 3,

                fieldValue = ElmValue.Integer(37_951),

                expected =
                new ElmValue.ElmRecord(
                    [
                    ("alfa", ElmValue.Integer(3_089)),
                    ("beta", ElmValue.Integer(3_113)),
                    ("epsilon", ElmValue.Integer(3_117)),
                    ("zeta", ElmValue.Integer(3_119)),
                    ("eta", ElmValue.Integer(3_121)),
                    ("theta", ElmValue.Integer(3_123)),
                    ("iota", ElmValue.Integer(3_127)),
                    ("zz_other", ElmValue.Integer(37_951))
                    ]),
            },

            new
            {
                record =
                new ElmValue.ElmRecord(
                    [
                    ("alfa", ElmValue.Integer(41)),
                    ("beta", ElmValue.Integer(43)),
                    ("gamma", ElmValue.Integer(47)),
                    ("delta", ElmValue.Integer(49))
                    ]),

                fieldId = 1,

                fieldValue = ElmValue.Integer(173),

                expected =
                new ElmValue.ElmRecord(
                    [
                    ("alfa", ElmValue.Integer(41)),
                    ("beta", ElmValue.Integer(43)),
                    ("gamma", ElmValue.Integer(47)),
                    ("delta", ElmValue.Integer(173))
                    ]),
            },

            new
            {
                record =
                new ElmValue.ElmRecord(
                    [
                    ("alfa", ElmValue.Integer(71)),
                    ("beta", ElmValue.Integer(73)),
                    ("gamma", ElmValue.Integer(79)),
                    ("delta", ElmValue.Integer(83))
                    ]),

                fieldId = 0,

                fieldValue = ElmValue.Integer(91),

                expected =
                new ElmValue.ElmRecord(
                    [
                    ("alfa", ElmValue.Integer(91)),
                    ("beta", ElmValue.Integer(73)),
                    ("gamma", ElmValue.Integer(79)),
                    ("delta", ElmValue.Integer(83))
                    ]),
            },

            new
            {
                record =
                new ElmValue.ElmRecord(
                    [
                    ("alfa", ElmValue.StringInstance("Arancino")),
                    ("beta", ElmValue.Integer(43)),
                    ("gamma", ElmValue.Integer(47)),
                    ("delta", ElmValue.Integer(49)),
                    ]),

                fieldId = 0,

                fieldValue = ElmValue.StringInstance("Acciughe"),

                expected =
                new ElmValue.ElmRecord(
                    [
                    ("alfa", ElmValue.StringInstance("Acciughe")),
                    ("beta", ElmValue.Integer(43)),
                    ("gamma", ElmValue.Integer(47)),
                    ("delta", ElmValue.Integer(49)),
                    ]),
            },

            new
            {
                record =
                new ElmValue.ElmRecord(
                    [
                    ("alfa", ElmValue.StringInstance("Arancini")),
                    ("beta", ElmValue.StringInstance("Bruschette")),
                    ("gamma", ElmValue.Integer(123)),
                    ("delta", ElmValue.StringInstance("Dolmades")),
                    ]),

                fieldId = 1,

                fieldValue = ElmValue.StringInstance("Dragoncello"),

                expected =
                new ElmValue.ElmRecord(
                    [
                    ("alfa", ElmValue.StringInstance("Arancini")),
                    ("beta", ElmValue.StringInstance("Bruschette")),
                    ("gamma", ElmValue.Integer(123)),
                    ("delta", ElmValue.StringInstance("Dragoncello")),
                    ])
            },

            new
            {
                record =
                new ElmValue.ElmRecord(
                    [
                    ("alfa", ElmValue.Integer(11)),
                    ("beta", ElmValue.Integer(13)),
                    ("gamma", ElmValue.Integer(17)),
                    ("delta", ElmValue.Integer(31)),
                    ]),

                fieldId = 1,

                fieldValue = ElmValue.Integer(97),

                expected =
                new ElmValue.ElmRecord(
                    [
                    ("alfa", ElmValue.Integer(11)),
                    ("beta", ElmValue.Integer(13)),
                    ("gamma", ElmValue.Integer(17)),
                    ("delta", ElmValue.Integer(97)),
                    ])
            },

            new
            {
                record =
                new ElmValue.ElmRecord(
                    [
                    ("alfa", ElmValue.Integer(21)),
                    ("beta", ElmValue.Integer(23)),
                    ("gamma", ElmValue.Integer(27)),
                    ("delta", ElmValue.Integer(41)),
                    ]),

                fieldId = 1,

                fieldValue = ElmValue.Integer(107),

                expected =
                new ElmValue.ElmRecord(
                    [
                    ("alfa", ElmValue.Integer(21)),
                    ("beta", ElmValue.Integer(23)),
                    ("gamma", ElmValue.Integer(27)),
                    ("delta", ElmValue.Integer(107)),
                    ])
            },

            new
            {
                record =
                new ElmValue.ElmRecord(
                    [
                    ("alfa", ElmValue.StringInstance("Arancino")),
                    ("beta", ElmValue.StringInstance("Bruschetta")),
                    ("zz_other", ElmValue.Integer(101)),
                    ("delta", ElmValue.StringInstance("Dolmades")),
                    ]),

                fieldId = 3,

                fieldValue = ElmValue.Integer(131),

                expected =
                new ElmValue.ElmRecord(
                    [
                    ("alfa", ElmValue.StringInstance("Arancino")),
                    ("beta", ElmValue.StringInstance("Bruschetta")),
                    ("zz_other", ElmValue.Integer(131)),
                    ("delta", ElmValue.StringInstance("Dolmades")),
                    ])
            },
        };

        static long ReportsAverageInstructionCount(IReadOnlyList<PineVM.EvaluationReport> reports) =>
            reports.Sum(report => report.InstructionCount) / reports.Count;

        IReadOnlyList<PineVM.EvaluationReport> RunScenariosWithGivenVM(PineVM pineVM) =>
            [.. recordUpdateScenarios
            .Select(scenario =>
            {
                return
                ElmInteractiveEnvironment.ApplyFunctionArgumentsForEvalExpr(
                    usingRecordUpdateFunction,
                    appendArguments:
                    [
                        ElmValueEncoding.ElmValueAsPineValue(scenario.record),
                        IntegerEncoding.EncodeSignedInteger(scenario.fieldId),
                        ElmValueEncoding.ElmValueAsPineValue(scenario.fieldValue),
                    ])
                .AndThen(composedArgs =>
                pineVM.EvaluateExpressionOnCustomStack(
                    composedArgs.expression,
                    composedArgs.environment,
                    new PineVM.EvaluationConfig(ParseAndEvalCountLimit: 1234))
                .Map(evalReport =>
                {
                    evalReport.ReturnValue.Should().Be(
                        ElmValueEncoding.ElmValueAsPineValue(scenario.expected),
                        "New record value matches expected");

                    Console.WriteLine(
                        "Completed scenario using " + evalReport.InstructionCount +
                        " instructions and " + evalReport.InvocationCount + " invocations");

                    return evalReport;
                }))
                .Extract(fromErr: err => throw new Exception("Failed for scenario: " + err));
            })];

        var nonOptimizingPineVM = new PineVM();

        var nonOptimizedScenariosStats =
            RunScenariosWithGivenVM(nonOptimizingPineVM);

        var nonOptimizedAverageInstructionCount =
            ReportsAverageInstructionCount(nonOptimizedScenariosStats);

        Console.WriteLine("\nAverage instruction count not optimized: " + nonOptimizedAverageInstructionCount + "\n");

        nonOptimizedAverageInstructionCount.Should().BeGreaterThan(60);

        var invocationReports = new List<PineVM.EvaluationReport>();

        var profilingVM =
            new PineVM(
                evalCache: null,
                reportFunctionApplication: invocationReports.Add,
                disableReductionInCompilation: true);

        RunScenariosWithGivenVM(profilingVM);

        Console.WriteLine(
            "Collected " + invocationReports.Count + " invocation reports from " +
            recordUpdateScenarios.Length + " scenarios.");

        var codeAnalysisStopwatch = System.Diagnostics.Stopwatch.StartNew();

        var compilationEnvClasses =
            CodeAnalysis.EnvironmentClassesFromInvocationReports(
                invocationReports,
                limitInvocationSampleCount: 1000,
                limitSampleCountPerSample: 40,
                classUsageCountMin: 4,
                limitClassesPerExpression: 30);

        codeAnalysisStopwatch.Stop();

        Console.WriteLine(
            "Analyzed " + invocationReports.Count + " invocation reports in " +
            codeAnalysisStopwatch.ElapsedMilliseconds + " ms and selected " +
            compilationEnvClasses.Sum(exprGroup => exprGroup.Value.Count) + " total environment classes for " +
            compilationEnvClasses.Count(exprGroup => 0 < exprGroup.Value.Count) + " expressions.");

        {
            foreach (var exprEnvClasses in compilationEnvClasses)
            {
                var exprValueHash =
                    PineValueHashTree.ComputeHash(
                        ExpressionEncoding.EncodeExpressionAsValue(exprEnvClasses.Key));

                for (var i = 0; i < exprEnvClasses.Value.Count; i++)
                {
                    var envClass = exprEnvClasses.Value[i];

                    Console.WriteLine(
                        "\nEnvironment class [" + i + "] " + envClass.HashBase16[..8] +
                        " for expr " + Convert.ToHexStringLower(exprValueHash.Span)[..8] +
                        " has " + envClass.ParsedEnvItems.Count + " env items:");

                    var envItems = envClass.ParsedEnvItems.ToArray();

                    for (var j = 0; j < envClass.ParsedEnvItems.Count; j++)
                    {
                        var envItem = envItems[j];

                        var envItemValueHash = PineValueHashTree.ComputeHash(envItem.Value);

                        var envItemDisplayText =
                            Convert.ToHexStringLower(envItemValueHash.Span)[..8] +
                            " - " +
                            ElmValueEncoding.PineValueAsElmValue(envItem.Value, null, null)
                            .Unpack(
                                fromErr: _ =>
                                "???",
                                fromOk:
                                elmValue =>
                                {
                                    var asExprString = ElmValue.RenderAsElmExpression(elmValue).expressionString;

                                    if (asExprString.Length < 100)
                                        return asExprString;

                                    return asExprString[..100] + "...";
                                });

                        Console.WriteLine(
                            "Item [" + j + "]: " + string.Join("-", envItem.Key) + ": " + envItemDisplayText);
                    }
                }
            }
        }

        var optimizedPineVM =
            new PineVM(
                evalCache: null,
                reportFunctionApplication: null,
                compilationEnvClasses: compilationEnvClasses);


        var optimizedScenariosStats =
            RunScenariosWithGivenVM(optimizedPineVM);

        var optimizedAverageInstructionCount =
            ReportsAverageInstructionCount(optimizedScenariosStats);

        Console.WriteLine("\nAverage instruction count optimized: " + optimizedAverageInstructionCount + "\n");

        var remainingInstructionsPercent =
            optimizedAverageInstructionCount * 100 / nonOptimizedAverageInstructionCount;

        Console.WriteLine("Remaining instructions percentage: " + remainingInstructionsPercent + "%");

        remainingInstructionsPercent.Should().BeLessThan(75);

        optimizedAverageInstructionCount.Should().BeLessThanOrEqualTo(130);
    }

    [TestMethod]
    public void PGO_reduces_Elm_list_map_tuple_first()
    {
        var elmModule =
            """
            module Test exposing (..)

            usingListMap list functionId =
                let
                    function =
                        case functionId of
                            0 ->
                                Tuple.first

                            1 ->
                                Tuple.first >> String.repeat 3
            
                            _ ->
                                Tuple.first >> String.reverse
                in
                listMap function list


            listMap : (a -> b) -> List a -> List b
            listMap f xs =
                listMapHelp f xs []


            listMapHelp : (a -> b) -> List a -> List b -> List b
            listMapHelp f remaining acc =
                case remaining of
                    [] ->
                        Pine_kernel.reverse acc

                    x :: xs ->
                        listMapHelp f xs (Pine_kernel.concat [ [ f x ], acc ])
            
            """;


        var appCodeTree = AppCodeTreeForElmModules([elmModule]);

        using var interactiveSession =
            new InteractiveSessionPine(
                ElmCompiler.CompilerSourceFilesDefault.Value,
                appCodeTree: appCodeTree,
                overrideSkipLowering: true,
                entryPointsFilePaths: null,
                caching: true,
                autoPGO: null);

        // Force integration of the 'Test' module.
        var testSubmissionResult = interactiveSession.Submit(""" Test.usingListMap [ ("alfa", 31), ("beta", 41) ] 0 """);

        var testSubmissionResponse =
            testSubmissionResult.Extract(err => throw new Exception(err));

        testSubmissionResponse.InteractiveResponse.DisplayText.Should().Be("""["alfa","beta"]""");

        var interactiveEnvironmentValue = interactiveSession.CurrentEnvironmentValue();

        var (_, usingListMapFunction) =
            ElmInteractiveEnvironment.ParseFunctionFromElmModule(
                interactiveEnvironment: interactiveEnvironmentValue,
                moduleName: "Test",
                declarationName: "usingListMap",
                ParseCache)
            .Extract(err => throw new Exception(err));

        {
            // Help identify functions.

            var functionsToInspect = new[]
            {
                new
                {
                    moduleName = "Test",
                    declarationName = "listMap",
                },
                new
                {
                    moduleName = "Test",
                    declarationName = "listMapHelp",
                },
                new
                {
                    moduleName = "Tuple",
                    declarationName = "first",
                },
            };

            foreach (var functionToInspect in functionsToInspect)
            {
                var (functionValue, functionRecord) =
                    ElmInteractiveEnvironment.ParseFunctionFromElmModule(
                        interactiveEnvironment: interactiveEnvironmentValue,
                        moduleName: functionToInspect.moduleName,
                        declarationName: functionToInspect.declarationName,
                        ParseCache)
                    .Extract(err => throw new Exception(err));

                var functionValueHash = PineValueHashTree.ComputeHash(functionValue);

                var envFunctionsHashes =
                    functionRecord.EnvFunctions
                    .ToArray()
                    .Select(envFunction => PineValueHashTree.ComputeHash(envFunction))
                    .ToArray();

                Console.WriteLine(
                    "\nFunction " + functionToInspect.moduleName + "." + functionToInspect.declarationName + " has hash " +
                    Convert.ToHexStringLower(functionValueHash.Span)[..8] + " and " +
                    functionRecord.EnvFunctions.Length + " env functions:\n" +
                    string.Join(
                        "\n",
                        envFunctionsHashes.Select(envFunctionHash => Convert.ToHexStringLower(envFunctionHash.Span)[..8])));
            }
        }


        var usageScenarios = new[]
        {
            new
            {
                list =
                new ElmValue.ElmList([]),

                functionId = 0,

                expected =
                new ElmValue.ElmList([]),
            },

            new
            {
                list =
                new ElmValue.ElmList(
                    [
                    new ElmValue.ElmList([ElmValue.StringInstance("alfa"), ElmValue.Integer(31)]),
                    new ElmValue.ElmList([ElmValue.StringInstance("beta"), ElmValue.Integer(41)]),
                    ]),

                functionId = 0,

                expected =
                new ElmValue.ElmList(
                    [
                    ElmValue.StringInstance("alfa"),
                    ElmValue.StringInstance("beta"),
                    ]),
            },

            new
            {
                list =
                new ElmValue.ElmList(
                    [
                    new ElmValue.ElmList([ElmValue.StringInstance("alfa"), ElmValue.Integer(31)]),
                    new ElmValue.ElmList([ElmValue.StringInstance("beta"), ElmValue.Integer(41)]),
                    ]),

                functionId = 1,

                expected =
                new ElmValue.ElmList(
                    [
                    ElmValue.StringInstance("alfaalfaalfa"),
                    ElmValue.StringInstance("betabetabeta"),
                    ]),
            },

            new
            {
                list =
                new ElmValue.ElmList(
                    [
                    new ElmValue.ElmList([ElmValue.StringInstance("alfa"), ElmValue.Integer(31)]),
                    new ElmValue.ElmList([ElmValue.StringInstance("beta"), ElmValue.Integer(41)]),
                    ]),

                functionId = 11,

                expected =
                new ElmValue.ElmList(
                    [
                    ElmValue.StringInstance("afla"),
                    ElmValue.StringInstance("ateb"),
                    ]),
            },

            new
            {
                list =
                new ElmValue.ElmList(
                    [
                    new ElmValue.ElmList([ElmValue.StringInstance("Focaccia"), ElmValue.Integer(31)]),
                    new ElmValue.ElmList([ElmValue.StringInstance("Pizza"), ElmValue.Integer(41)]),
                    new ElmValue.ElmList([ElmValue.StringInstance("Arancino"), ElmValue.Integer(71)]),
                    new ElmValue.ElmList([ElmValue.StringInstance("Lasagna"), ElmValue.Integer(43)]),
                    new ElmValue.ElmList([ElmValue.StringInstance("Risotto"), ElmValue.Integer(47)]),
                    new ElmValue.ElmList([ElmValue.StringInstance("Pasta"), ElmValue.Integer(49)]),
                    new ElmValue.ElmList([ElmValue.StringInstance("Gelato"), ElmValue.Integer(73)]),
                    new ElmValue.ElmList([ElmValue.StringInstance("Tiramisu"), ElmValue.Integer(79)]),
                    ]),

                functionId = 0,

                expected =
                new ElmValue.ElmList(
                    [
                    ElmValue.StringInstance("Focaccia"),
                    ElmValue.StringInstance("Pizza"),
                    ElmValue.StringInstance("Arancino"),
                    ElmValue.StringInstance("Lasagna"),
                    ElmValue.StringInstance("Risotto"),
                    ElmValue.StringInstance("Pasta"),
                    ElmValue.StringInstance("Gelato"),
                    ElmValue.StringInstance("Tiramisu"),
                    ]),
            },


            new
            {
                list =
                new ElmValue.ElmList(
                    [
                    new ElmValue.ElmList([ElmValue.Integer(71), ElmValue.Integer(31)]),
                    new ElmValue.ElmList([ElmValue.Integer(79), ElmValue.Integer(37)]),
                    new ElmValue.ElmList([ElmValue.Integer(73), ElmValue.Integer(41)]),
                    new ElmValue.ElmList([ElmValue.Integer(83), ElmValue.Integer(43)]),
                    new ElmValue.ElmList([ElmValue.Integer(97), ElmValue.Integer(47)]),
                    new ElmValue.ElmList([ElmValue.Integer(89), ElmValue.Integer(49)]),
                    ]),

                functionId = 0,

                expected =
                new ElmValue.ElmList(
                    [
                    ElmValue.Integer(71),
                    ElmValue.Integer(79),
                    ElmValue.Integer(73),
                    ElmValue.Integer(83),
                    ElmValue.Integer(97),
                    ElmValue.Integer(89),
                    ]),
            },
        };

        static long ReportsAverageInvocationCount(IReadOnlyList<PineVM.EvaluationReport> reports) =>
            reports.Sum(report => report.InvocationCount) / reports.Count;

        PineVM.EvaluationReport RunScenario(
            ElmValue.ElmList scenarioList,
            int scenarioFunctionId,
            ElmValue.ElmList scenarioExpected,
            PineVM pineVM)
        {
            return
                ElmInteractiveEnvironment.ApplyFunctionArgumentsForEvalExpr(
                    usingListMapFunction,
                    appendArguments:
                    [
                        ElmValueEncoding.ElmValueAsPineValue(scenarioList),
                        IntegerEncoding.EncodeSignedInteger(scenarioFunctionId),
                    ])
                .AndThen(composedArgs =>
                pineVM.EvaluateExpressionOnCustomStack(
                    composedArgs.expression,
                    composedArgs.environment,
                    new PineVM.EvaluationConfig(ParseAndEvalCountLimit: 12345))
                .Map(evalReport =>
                {
                    Assert.AreEqual(ElmValueEncoding.ElmValueAsPineValue(scenarioExpected), evalReport.ReturnValue);

                    Console.WriteLine(
                        "Completed scenario using " + evalReport.InstructionCount +
                        " instructions and " + evalReport.InvocationCount + " invocations");

                    return evalReport;
                }))
                .Extract(fromErr: err => throw new Exception("Failed for scenario: " + err));
        }

        IReadOnlyList<PineVM.EvaluationReport> RunScenariosWithGivenVM(PineVM pineVM) =>
            [.. usageScenarios
            .Select(scenario =>
            RunScenario(
                scenarioList: scenario.list,
                scenarioFunctionId: scenario.functionId,
                scenarioExpected: scenario.expected,
                pineVM: pineVM))];

        var nonOptimizingPineVM = new PineVM();

        var nonOptimizedScenariosStats =
            RunScenariosWithGivenVM(nonOptimizingPineVM);

        var invocationReports = new List<PineVM.EvaluationReport>();

        var profilingVM =
            new PineVM(
                evalCache: null,
                reportFunctionApplication: invocationReports.Add,
                disableReductionInCompilation: true);

        RunScenariosWithGivenVM(profilingVM);

        Console.WriteLine(
            "Collected " + invocationReports.Count + " invocation reports from " +
            usageScenarios.Length + " scenarios.");

        var pineVMCache = new PineVMCache();

        var codeAnalysisStopwatch = System.Diagnostics.Stopwatch.StartNew();

        var compilationEnvClasses =
            CodeAnalysis.EnvironmentClassesFromInvocationReports(
                invocationReports,
                limitInvocationSampleCount: 1000,
                limitSampleCountPerSample: 40,
                classUsageCountMin: 4,
                limitClassesPerExpression: 30);

        codeAnalysisStopwatch.Stop();

        Console.WriteLine(
            "Analyzed " + invocationReports.Count + " invocation reports in " +
            codeAnalysisStopwatch.ElapsedMilliseconds + " ms and selected " +
            compilationEnvClasses.Sum(exprGroup => exprGroup.Value.Count) + " total environment classes for " +
            compilationEnvClasses.Count(exprGroup => 0 < exprGroup.Value.Count) + " expressions.");

        {
            foreach (var exprEnvClasses in compilationEnvClasses)
            {
                var exprValueHash =
                    PineValueHashTree.ComputeHash(
                        ExpressionEncoding.EncodeExpressionAsValue(exprEnvClasses.Key));

                for (var i = 0; i < exprEnvClasses.Value.Count; i++)
                {
                    var envClass = exprEnvClasses.Value[i];

                    Console.WriteLine(
                        "\nEnvironment class [" + i + "] " + envClass.HashBase16[..8] +
                        " for expr " + Convert.ToHexStringLower(exprValueHash.Span)[..8] +
                        " has " + envClass.ParsedEnvItems.Count + " env items:");

                    var envItems = envClass.ParsedEnvItems.ToArray();

                    for (var j = 0; j < envClass.ParsedEnvItems.Count; j++)
                    {
                        var envItem = envItems[j];

                        var envItemValueHash = PineValueHashTree.ComputeHash(envItem.Value);

                        var envItemDisplayText =
                            Convert.ToHexStringLower(envItemValueHash.Span)[..8] +
                            " - " +
                            ElmValueEncoding.PineValueAsElmValue(envItem.Value, null, null)
                            .Unpack(
                                fromErr: _ =>
                                "???",
                                fromOk:
                                elmValue =>
                                {
                                    var asExprString = ElmValue.RenderAsElmExpression(elmValue).expressionString;

                                    if (asExprString.Length < 100)
                                        return asExprString;

                                    return asExprString[..100] + "...";
                                });

                        Console.WriteLine(
                            "Item [" + j + "]: " + string.Join("-", envItem.Key) + ": " + envItemDisplayText);
                    }
                }
            }
        }

        var optimizedPineVM =
            new PineVM(
                evalCache: null,
                reportFunctionApplication: null,
                compilationEnvClasses: compilationEnvClasses);

        var optimizedScenariosStats =
            RunScenariosWithGivenVM(optimizedPineVM);


        long nonOptimizedAverageInvocationCount =
            ReportsAverageInvocationCount(nonOptimizedScenariosStats);

        Console.WriteLine("\nAverage invocation count not optimized: " + nonOptimizedAverageInvocationCount + "\n");

        var optimizedAverageInvocationCount =
            ReportsAverageInvocationCount(optimizedScenariosStats);

        Console.WriteLine("\nAverage invocation count optimized: " + optimizedAverageInvocationCount + "\n");

        {
            var largerListOutput =
                Enumerable.Range(0, 1000)
                .Select(index => ElmValue.StringInstance("item-" + index))
                .ToImmutableArray();

            var largerListInput =
                largerListOutput
                .Select((pairFirst, index) => new ElmValue.ElmList([pairFirst, ElmValue.Integer(5678 - index)]))
                .ToImmutableArray();

            var scenarioReport =
                RunScenario(
                    scenarioList: new ElmValue.ElmList(largerListInput),
                    scenarioFunctionId: 0,
                    scenarioExpected: new ElmValue.ElmList(largerListOutput),
                    optimizedPineVM);

            /*
             * Since the item mapping function is so simple in this case (`Tuple.first`),
             * we expect the engine (with the given configuration) to create a specialized variant of
             * the `List.map` function with this item mapping function inlined.
             * Therefore, the test expects only one invocation per item of the input list.
             * */

            Assert.IsTrue(scenarioReport.InvocationCount < largerListInput.Length + 30, "Total invocation count");
        }
    }


    [TestMethod]
    public void PGO_reduces_Elm_Dict_fold()
    {
        var elmModule =
            """
            module Test exposing (..)

            import Dict


            usingDictFold dict functionId =
                case functionId of
                    0 ->
                        Dict.foldl
                            (\key value list -> ( value, key ) :: list)
                            []
                            dict
            
                    _ ->
                        Dict.foldl
                            (\key value list -> ( value, key ) :: list)
                            []
                            dict
                                    
            """;


        var appCodeTree = AppCodeTreeForElmModules([elmModule]);

        using var interactiveSession =
            new InteractiveSessionPine(
                ElmCompiler.CompilerSourceFilesDefault.Value,
                appCodeTree: appCodeTree,
                overrideSkipLowering: true,
                entryPointsFilePaths: null,
                caching: true,
                autoPGO: null);

        // Force integration of the 'Test' module.
        var testSubmissionResult =
            interactiveSession.Submit(
                """ Test.usingDictFold (Dict.fromList [ ("alfa", 31), ("beta", 41) ]) 0 """);

        var testSubmissionResponse =
            testSubmissionResult.Extract(err => throw new Exception(err));

        Assert.AreEqual("""[(41,"beta"),(31,"alfa")]""", testSubmissionResponse.InteractiveResponse.DisplayText);

        var interactiveEnvironmentValue = interactiveSession.CurrentEnvironmentValue();

        var (_, usingDictFoldFunction) =
            ElmInteractiveEnvironment.ParseFunctionFromElmModule(
                interactiveEnvironment: interactiveEnvironmentValue,
                moduleName: "Test",
                declarationName: "usingDictFold",
                ParseCache)
            .Extract(err => throw new Exception(err));

        var (_, dictFromListFunction) =
            ElmInteractiveEnvironment.ParseFunctionFromElmModule(
                interactiveEnvironment: interactiveEnvironmentValue,
                moduleName: "Dict",
                declarationName: "fromList",
                ParseCache)
            .Extract(err => throw new Exception(err));

        {
            // Help identify functions.

            var functionsToInspect = new[]
            {
                new
                {
                    moduleName = "Test",
                    declarationName = "usingDictFold",
                },
                new
                {
                    moduleName = "Dict",
                    declarationName = "fromList",
                },
                new
                {
                    moduleName = "Dict",
                    declarationName = "foldl",
                },
                new
                {
                    moduleName = "Dict",
                    declarationName = "foldr",
                },
            };

            foreach (var functionToInspect in functionsToInspect)
            {
                var (functionValue, functionRecord) =
                    ElmInteractiveEnvironment.ParseFunctionFromElmModule(
                        interactiveEnvironment: interactiveEnvironmentValue,
                        moduleName: functionToInspect.moduleName,
                        declarationName: functionToInspect.declarationName,
                        ParseCache)
                    .Extract(err => throw new Exception(err));

                var functionValueHash = PineValueHashTree.ComputeHash(functionValue);

                var envFunctionsHashes =
                    functionRecord.EnvFunctions
                    .ToArray()
                    .Select(envFunction => PineValueHashTree.ComputeHash(envFunction))
                    .ToArray();

                Console.WriteLine(
                    "\nFunction " + functionToInspect.moduleName + "." + functionToInspect.declarationName + " has hash " +
                    Convert.ToHexStringLower(functionValueHash.Span)[..8] + " and " +
                    functionRecord.EnvFunctions.Length + " env functions:\n" +
                    string.Join(
                        "\n",
                        envFunctionsHashes.Select(envFunctionHash => Convert.ToHexStringLower(envFunctionHash.Span)[..8])));
            }
        }


        var usageScenarios = new[]
        {
            new
            {
                list =
                new ElmValue.ElmList([]),

                functionId = 0,

                expected =
                new ElmValue.ElmList([]),
            },

            new
            {
                list =
                new ElmValue.ElmList(
                    [
                    new ElmValue.ElmList([ElmValue.StringInstance("alfa"), ElmValue.Integer(31)]),
                    new ElmValue.ElmList([ElmValue.StringInstance("beta"), ElmValue.Integer(41)]),
                    ]),

                functionId = 0,

                expected =
                new ElmValue.ElmList(
                    [
                    new ElmValue.ElmList([ElmValue.Integer(41), ElmValue.StringInstance("beta")]),
                    new ElmValue.ElmList([ElmValue.Integer(31), ElmValue.StringInstance("alfa")]),
                    ]),
            },

            new
            {
                list =
                new ElmValue.ElmList(
                    [
                    new ElmValue.ElmList([ElmValue.StringInstance("alfa"), ElmValue.Integer(31)]),
                    new ElmValue.ElmList([ElmValue.StringInstance("gamma"), ElmValue.Integer(41)]),
                    new ElmValue.ElmList([ElmValue.StringInstance("beta"), ElmValue.Integer(47)]),
                    ]),

                functionId = 0,

                expected =
                new ElmValue.ElmList(
                    [
                    new ElmValue.ElmList([ElmValue.Integer(41), ElmValue.StringInstance("gamma")]),
                    new ElmValue.ElmList([ElmValue.Integer(47), ElmValue.StringInstance("beta")]),
                    new ElmValue.ElmList([ElmValue.Integer(31), ElmValue.StringInstance("alfa")]),
                    ]),
            },
        };

        var toDictPineVM = new PineVM();

        PineValue dictFromList(ElmValue.ElmList list) =>
            ElmInteractiveEnvironment.ApplyFunction(
                toDictPineVM,
                dictFromListFunction,
                [ElmValueEncoding.ElmValueAsPineValue(list)])
            .Extract(err => throw new Exception(err));

        static long ReportsAverageInvocationCount(IReadOnlyList<PineVM.EvaluationReport> reports) =>
            reports.Sum(report => report.InvocationCount) / reports.Count;

        PineVM.EvaluationReport RunScenario(
            PineValue scenarioDict,
            int scenarioFunctionId,
            ElmValue.ElmList? scenarioExpected,
            PineVM pineVM)
        {
            return
                ElmInteractiveEnvironment.ApplyFunctionArgumentsForEvalExpr(
                    usingDictFoldFunction,
                    appendArguments:
                    [
                        scenarioDict,
                        IntegerEncoding.EncodeSignedInteger(scenarioFunctionId),
                    ])
                .AndThen(composedArgs =>
                pineVM.EvaluateExpressionOnCustomStack(
                    composedArgs.expression,
                    composedArgs.environment,
                    new PineVM.EvaluationConfig(ParseAndEvalCountLimit: 12345))
                .Map(evalReport =>
                {
                    if (scenarioExpected is not null)
                    {
                        Assert.AreEqual(ElmValueEncoding.ElmValueAsPineValue(scenarioExpected), evalReport.ReturnValue);
                    }

                    Console.WriteLine(
                        "Completed scenario using " + evalReport.InstructionCount +
                        " instructions and " + evalReport.InvocationCount + " invocations");

                    return evalReport;
                }))
                .Extract(fromErr: err => throw new Exception("Failed for scenario: " + err));
        }

        IReadOnlyList<PineVM.EvaluationReport> RunScenariosWithGivenVM(PineVM pineVM) =>
            [.. usageScenarios
            .Select(scenario =>
            RunScenario(
                scenarioDict: dictFromList(scenario.list),
                scenarioFunctionId: scenario.functionId,
                scenarioExpected: scenario.expected,
                pineVM: pineVM))];

        var nonOptimizingPineVM = new PineVM();

        var nonOptimizedScenariosStats =
            RunScenariosWithGivenVM(nonOptimizingPineVM);

        var invocationReports = new List<PineVM.EvaluationReport>();

        var profilingVM =
            new PineVM(
                evalCache: null,
                reportFunctionApplication: invocationReports.Add,
                disableReductionInCompilation: true);

        RunScenariosWithGivenVM(profilingVM);

        {
            var largeProfilingList =
                Enumerable.Range(0, 40)
                .Select(index =>
                new ElmValue.ElmList(
                    [
                    ElmValue.StringInstance("key-" + index),
                    ElmValue.Integer(100 + index)
                    ]))
                .ToImmutableArray();

            RunScenario(
                dictFromList(new ElmValue.ElmList(largeProfilingList)),
                scenarioFunctionId: 0,
                pineVM: profilingVM,
                scenarioExpected: null);
        }

        Console.WriteLine(
            "Collected " + invocationReports.Count + " invocation reports from " +
            usageScenarios.Length + " scenarios.");

        var codeAnalysisStopwatch = System.Diagnostics.Stopwatch.StartNew();

        var compilationEnvClasses =
            CodeAnalysis.EnvironmentClassesFromInvocationReports(
                invocationReports,
                limitInvocationSampleCount: 1000,
                limitSampleCountPerSample: 40,
                classUsageCountMin: 10,
                limitClassesPerExpression: 10);

        codeAnalysisStopwatch.Stop();

        Console.WriteLine(
            "Analyzed " + invocationReports.Count + " invocation reports in " +
            codeAnalysisStopwatch.ElapsedMilliseconds + " ms and selected " +
            compilationEnvClasses.Sum(exprGroup => exprGroup.Value.Count) + " total environment classes for " +
            compilationEnvClasses.Count(exprGroup => 0 < exprGroup.Value.Count) + " expressions.");

        {
            foreach (var exprEnvClasses in compilationEnvClasses)
            {
                var exprValueHash =
                    PineValueHashTree.ComputeHash(
                        ExpressionEncoding.EncodeExpressionAsValue(exprEnvClasses.Key));

                for (var i = 0; i < exprEnvClasses.Value.Count; i++)
                {
                    var envClass = exprEnvClasses.Value[i];

                    Console.WriteLine(
                        "\nEnvironment class [" + i + "] " + envClass.HashBase16[..8] +
                        " for expr " + Convert.ToHexStringLower(exprValueHash.Span)[..8] +
                        " has " + envClass.ParsedEnvItems.Count + " env items:");

                    var envItems = envClass.ParsedEnvItems.ToArray();

                    for (var j = 0; j < envClass.ParsedEnvItems.Count; j++)
                    {
                        var envItem = envItems[j];

                        var envItemValueHash = PineValueHashTree.ComputeHash(envItem.Value);

                        var envItemDisplayText =
                            Convert.ToHexStringLower(envItemValueHash.Span)[..8] +
                            " - " +
                            ElmValueEncoding.PineValueAsElmValue(envItem.Value, null, null)
                            .Unpack(
                                fromErr: _ =>
                                "???",
                                fromOk:
                                elmValue =>
                                {
                                    var asExprString = ElmValue.RenderAsElmExpression(elmValue).expressionString;

                                    if (asExprString.Length < 100)
                                        return asExprString;

                                    return asExprString[..100] + "...";
                                });

                        Console.WriteLine(
                            "Item [" + j + "]: " + string.Join("-", envItem.Key) + ": " + envItemDisplayText);
                    }
                }
            }
        }

        var optimizedPineVM =
            new PineVM(
                evalCache: null,
                reportFunctionApplication: null,
                compilationEnvClasses: compilationEnvClasses);

        var optimizedScenariosStats =
            RunScenariosWithGivenVM(optimizedPineVM);


        long nonOptimizedAverageInvocationCount =
            ReportsAverageInvocationCount(nonOptimizedScenariosStats);

        Console.WriteLine("\nAverage invocation count not optimized: " + nonOptimizedAverageInvocationCount + "\n");

        var optimizedAverageInvocationCount =
            ReportsAverageInvocationCount(optimizedScenariosStats);

        Console.WriteLine("\nAverage invocation count optimized: " + optimizedAverageInvocationCount + "\n");

        {
            var largerList =
                Enumerable.Range(0, 100)
                .Select(index => (itemKey: "item-" + index, itemValue: index + 1000))
                .ToImmutableArray();

            var largerListOutput =
                largerList
                .OrderByDescending(item => item.itemKey)
                .Select(item => new ElmValue.ElmList([ElmValue.Integer(item.itemValue), ElmValue.StringInstance(item.itemKey)]))
                .ToImmutableArray();

            var largerListInput =
                largerList
                .Select(item => new ElmValue.ElmList([ElmValue.StringInstance(item.itemKey), ElmValue.Integer(item.itemValue)]))
                .ToImmutableArray();

            var scenarioReport =
                RunScenario(
                    scenarioDict: dictFromList(new ElmValue.ElmList(largerListInput)),
                    scenarioFunctionId: 0,
                    scenarioExpected: new ElmValue.ElmList(largerListOutput),
                    optimizedPineVM);

            /*
             * Since the item folding function is so simple in this case,
             * we expect the engine (with the given configuration) to create a specialized variant of
             * the `Dict.foldl` function with this item mapping function inlined.
             * Therefore, the test expects at most two invocations per dictionary item.
             * */

            Assert.IsTrue(scenarioReport.InvocationCount < largerListInput.Length * 2 + 30, "Total invocation count");
        }
    }

    public static TreeNodeWithStringPath AppCodeTreeForElmModules(
    IReadOnlyList<string> elmModuleTexts)
    {
        var compilerProgram = ElmCompiler.CompilerSourceFilesDefault.Value;

        var elmJson =
        """
{
    "type": "application",
    "source-directories": [
        "src"
    ],
    "elm-version": "0.19.1",
    "dependencies": {
        "direct": {
            "elm/core": "1.0.5"
        },
        "indirect": {
        }
    },
    "test-dependencies": {
        "direct": {},
        "indirect": {}
    }
}

""";

        var elmModulesFiles =
        elmModuleTexts
        .Select(moduleText =>
        ("src/" +
        string.Join('/', ElmModule.ParseModuleName(moduleText).Extract(err => throw new Exception(err))) + ".elm",
        Encoding.UTF8.GetBytes(moduleText)))
        .ToImmutableArray();

        var appCodeTree =
        PineValueComposition.SortedTreeFromSetOfBlobsWithCommonFilePath(
            [
            ("elm.json", Encoding.UTF8.GetBytes(elmJson)),
    ..elmModulesFiles
            ]);

        return appCodeTree;
    }
}