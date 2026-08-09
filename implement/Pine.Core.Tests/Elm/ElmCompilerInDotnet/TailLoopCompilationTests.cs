using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
using Pine.Core.Interpreter.IntermediateVM;
using System;
using System.Collections.Generic;
using System.Linq;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet;

public class TailLoopCompilationTests
{
    [Fact]
    public void String_slice_accumulator_recursion_compiles_to_backward_jump()
    {
        const string ElmModuleText =
            """
            module Test exposing (..)


            hexStringToIntHelp value remaining =
                if Pine_kernel.equal [ Pine_kernel.length remaining, 0 ] then
                    value

                else
                    hexStringToIntHelp
                        (Pine_kernel.int_add [ value, 1 ])
                        (Pine_kernel.skip [ 1, remaining ])
            """;

        AssertFunctionCompilesToLoop(
            ElmModuleText,
            "hexStringToIntHelp",
            argumentsWithFewerRecursions:
            [
            IntegerEncoding.EncodeSignedInteger(0),
            PineValue.Blob([1]),
            ],
            argumentsWithMoreRecursions:
            [
            IntegerEncoding.EncodeSignedInteger(0),
            PineValue.Blob([1, 2, 3, 4]),
            ]);
    }

    [Fact]
    public void List_slice_and_concat_accumulator_recursion_compiles_to_backward_jump()
    {
        const string ElmModuleText =
            """
            module Test exposing (..)


            splitRecordSetters remainingCount remaining leftRev =
                if Pine_kernel.equal [ remainingCount, 0 ] then
                    ( leftRev, remaining )

                else
                    splitRecordSetters
                        (Pine_kernel.int_add [ remainingCount, -1 ])
                        (Pine_kernel.skip [ 1, remaining ])
                        (Pine_kernel.concat
                            [ Pine_kernel.take [ 1, remaining ]
                            , leftRev
                            ]
                        )
            """;

        AssertFunctionCompilesToLoop(
            ElmModuleText,
            "splitRecordSetters",
            argumentsWithFewerRecursions:
            [
            IntegerEncoding.EncodeSignedInteger(1),
            PineValue.List([PineValue.Blob([1])]),
            PineValue.EmptyList,
            ],
            argumentsWithMoreRecursions:
            [
            IntegerEncoding.EncodeSignedInteger(4),
            PineValue.List(
                [
                PineValue.Blob([1]),
                PineValue.Blob([2]),
                PineValue.Blob([3]),
                PineValue.Blob([4]),
                ]),
            PineValue.EmptyList,
            ]);
    }

    private static void AssertFunctionCompilesToLoop(
        string elmModuleText,
        string functionName,
        IReadOnlyList<PineValue> argumentsWithFewerRecursions,
        IReadOnlyList<PineValue> argumentsWithMoreRecursions)
    {
        var parsedEnvironment =
            ElmCompilerTestHelper.CompileElmModules(
                [elmModuleText],
                disableInlining: false).parsedEnv;

        var functionValue =
            parsedEnvironment.Modules
            .Single(module => module.moduleName is "Test")
            .moduleContent.FunctionDeclarations[functionName];

        var parseCache = new PineVMParseCache();

        var functionRecord =
            FunctionRecord.ParseFunctionRecordTagged(functionValue, parseCache)
            .Extract(error => throw new Exception(error));

        var compilation =
            ExpressionCompilation.CompileExpression(
                functionRecord.InnerFunction,
                specializations: [],
                parseCache,
                disableReduction: true,
                enableTailRecursionOptimization: true,
                skipInlining: (_, _) => false);

        compilation.Generic.Instructions
            .Any(
            instruction =>
            instruction.Kind == StackInstructionKind.Jump_Const &&
            instruction.JumpOffset < 0)
            .Should().BeTrue();

        var vm =
            ElmCompilerTestHelper.PineVMForProfiling(
                reportFunctionApplication: _ => { },
                enableTailRecursionOptimization: true);

        EvaluationReport Evaluate(IReadOnlyList<PineValue> arguments)
        {
            var composed =
                ElmInteractiveEnvironment.ApplyFunctionArgumentsForEvalExpr(
                    functionRecord,
                    appendArguments: arguments)
                .Extract(error => throw new Exception(error));

            return
                vm.EvaluateExpressionOnCustomStack(
                    composed.expression,
                    composed.environment,
                    ElmCompilerTestHelper.DefaultTestEvaluationConfig)
                .Extract(error => throw new Exception(error.ToString()));
        }

        var reportWithFewerRecursions = Evaluate(argumentsWithFewerRecursions);
        var reportWithMoreRecursions = Evaluate(argumentsWithMoreRecursions);

        reportWithMoreRecursions.Counters.InvocationCount.Should()
            .Be(reportWithFewerRecursions.Counters.InvocationCount);

        reportWithFewerRecursions.Counters.LoopIterationCount.Should().BeGreaterThan(0);

        reportWithMoreRecursions.Counters.LoopIterationCount.Should()
            .BeGreaterThan(reportWithFewerRecursions.Counters.LoopIterationCount);
    }
}
