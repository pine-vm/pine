using ElmTime.ElmInteractive;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Pine;
using Pine.ElmInteractive;
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
    [TestMethod]
    public void PGO_reduces_Elm_record_access()
    {
        var compilerProgram = IInteractiveSession.CompileElmProgramCodeFilesDefault.Value;

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

        var appCodeTree =
            PineValueComposition.SortedTreeFromSetOfBlobsWithCommonFilePath(
                [
                ("elm.json", Encoding.UTF8.GetBytes(elmJson)),
                ("src/Test.elm", Encoding.UTF8.GetBytes(elmModule))
                ]);

        using var interactiveSession =
            new InteractiveSessionPine(
                compilerProgram,
                initialState: null,
                appCodeTree: appCodeTree,
                caching: true,
                autoPGO: null);

        // Force integration of the 'Test' module.
        var testSubmissionResult = interactiveSession.Submit(" Test.usingRecordAccess { alfa = 4, delta = 71 }  0 ");

        var testSubmissionResponse =
            testSubmissionResult.Extract(err => throw new Exception(err));

        Assert.AreEqual("4", testSubmissionResponse.InteractiveResponse.DisplayText);

        var interactiveEnvironmentValue = interactiveSession.CurrentEnvironmentValue();

        var usingRecordAccessFunction =
            ElmInteractiveEnvironment.ParseFunctionFromElmModule(
                interactiveEnvironment: interactiveEnvironmentValue,
                moduleName: "Test",
                declarationName: "usingRecordAccess")
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

                expected = PineValueAsInteger.ValueFromSignedInteger(13)
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

                expected = PineValueAsInteger.ValueFromSignedInteger(47)
            },

            new
            {
                record =
                new ElmValue.ElmRecord(
                    [
                    ("alfa", ElmValue.String("Arancino")),
                    ("delta", ElmValue.String("Bruschetta"))
                    ]),

                fieldId = 0,

                expected = ElmValueEncoding.ElmValueAsPineValue(ElmValue.String("Arancino"))
            },

            new
            {
                record =
                new ElmValue.ElmRecord(
                    [
                    ("alfa", ElmValue.String("hello")),
                    ("delta", ElmValue.String("world"))
                    ]),

                fieldId = 1,

                expected = ElmValueEncoding.ElmValueAsPineValue(ElmValue.String("world"))
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

                expected = PineValueAsInteger.ValueFromSignedInteger(49)
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

                expected = PineValueAsInteger.ValueFromSignedInteger(71)
            },

            new
            {
                record =
                new ElmValue.ElmRecord(
                    [
                    ("alfa", ElmValue.String("Arancino")),
                    ("beta", ElmValue.Integer(43)),
                    ("gamma", ElmValue.Integer(47)),
                    ("delta", ElmValue.Integer(49)),
                    ]),

                fieldId = 0,

                expected = ElmValueEncoding.ElmValueAsPineValue( ElmValue.String("Arancino"))
            },

            new
            {
                record =
                new ElmValue.ElmRecord(
                    [
                    ("alfa", ElmValue.String("Arancini")),
                    ("beta", ElmValue.String("Bruschette")),
                    ("gamma", ElmValue.Integer(123)),
                    ("delta", ElmValue.String("Dolmades")),
                    ]),

                fieldId = 1,

                expected = ElmValueEncoding.ElmValueAsPineValue(ElmValue.String("Dolmades"))
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
                    ("alfa", ElmValue.String("Arancino")),
                    ("beta", ElmValue.String("Bruschetta")),
                    ("other", ElmValue.Integer(101)),
                    ("delta", ElmValue.String("Dolmades")),
                    ]),

                fieldId = 3,

                expected = ElmValueEncoding.ElmValueAsPineValue(ElmValue.Integer(101))
            },

        };

        static long ReportsAverageInstructionCount(IReadOnlyList<PineVM.EvaluationReport> reports) =>
            reports.Sum(report => report.InstructionCount) / reports.Count;

        IReadOnlyList<PineVM.EvaluationReport> RunScenariosWithGivenVM(PineVM pineVM) =>
            usingRecordAccessScenarios
            .Select(scenario =>
            {
                return
                ElmInteractiveEnvironment.ApplyFunctionArgumentsForEvalExpr(
                        usingRecordAccessFunction,
                        arguments:
                        [
                            ElmValueEncoding.ElmValueAsPineValue(scenario.record),
                            PineValueAsInteger.ValueFromSignedInteger(scenario.fieldId)
                            ])
                .AndThen(composedArgs =>
                pineVM.EvaluateExpressionOnCustomStack(
                    composedArgs.expression,
                    composedArgs.environment,
                    new PineVM.EvaluationConfig(ParseAndEvalCountLimit: 1234))
                .Map(evalReport =>
                {
                    Assert.AreEqual(scenario.expected, evalReport.ReturnValue);

                    Console.WriteLine(
                        "Completed scenario using " + evalReport.InstructionCount +
                        " instructions and " + evalReport.ParseAndEvalCount + " invocations");

                    return evalReport;
                }))
                .Extract(fromErr: err => throw new Exception("Failed for scenario: " + err));
            })
            .ToImmutableArray();

        var nonOptimizingPineVM = new PineVM();

        var nonOptimizedScenariosStats =
            RunScenariosWithGivenVM(nonOptimizingPineVM);

        var nonOptimizedAverageInstructionCount =
            ReportsAverageInstructionCount(nonOptimizedScenariosStats);

        Console.WriteLine("\nAverage instruction count not optimized: " + nonOptimizedAverageInstructionCount + "\n");

        Assert.IsTrue(40 < nonOptimizedAverageInstructionCount);

        var invocationReports = new List<PineVM.EvaluationReport>();

        var profilingVM =
            new PineVM(
                overrideParseExpression: null,
                evalCache: null,
                reportFunctionApplication: invocationReports.Add,
                disableReductionInCompilation: true);

        RunScenariosWithGivenVM(profilingVM);

        var functionApplicationsToAnalyze =
            invocationReports
            .ToImmutableArray();

        Console.WriteLine(
            "Collected " + invocationReports.Count + " invocation reports from " +
            usingRecordAccessScenarios.Length + " scenarios.");

        var pineVMCache = new PineVMCache();

        var codeAnalysisStopwatch = System.Diagnostics.Stopwatch.StartNew();

        var compilationSpecializations =
            CodeAnalysis.EnvironmentClassesFromInvocationReports(
                invocationReports,
                limitInvocationSampleCount: 1000,
                limitSampleCountPerSample: 40,
                specializationUsageCountMin: 4,
                limitSpecializationsPerExpression: 30);

        codeAnalysisStopwatch.Stop();

        Console.WriteLine(
            "Analyzed " + invocationReports.Count + " invocation reports in " +
            codeAnalysisStopwatch.ElapsedMilliseconds + " ms and selected " +
            compilationSpecializations.Sum(exprGroup => exprGroup.Value.Count) + " total specializations for " +
            compilationSpecializations.Count(exprGroup => 0 < exprGroup.Value.Count) + " expressions.");

        var optimizedPineVM =
            new PineVM(
                overrideParseExpression: pineVMCache.BuildParseExprDelegate,
                evalCache: null,
                reportFunctionApplication: null,
                compilationSpecializations: compilationSpecializations);


        var optimizedScenariosStats =
            RunScenariosWithGivenVM(optimizedPineVM);

        var optimizedAverageInstructionCount =
            ReportsAverageInstructionCount(optimizedScenariosStats);

        Console.WriteLine("\nAverage instruction count optimized: " + optimizedAverageInstructionCount + "\n");

        var speedupFactor = nonOptimizedAverageInstructionCount / optimizedAverageInstructionCount;

        Assert.IsTrue(2 <= speedupFactor);

        Assert.IsTrue(optimizedAverageInstructionCount <= 30);
    }

    [TestMethod]
    [Ignore("TODO")]
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
                List.map function list

            """;
    }
}