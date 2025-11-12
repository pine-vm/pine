using Microsoft.Extensions.DependencyInjection;
using Pine.Core;
using Pine.PineVM;
using System.Collections.Generic;
using System.Linq;

namespace ElmTime.ElmInteractive;

/// <summary>
/// Functions to parse modules and declarations from the Elm interactive environment.
/// </summary>
public static class ElmInteractiveEnvironment
{
    public static Result<string, PineValue> ApplyFunctionInElmModule(
        PineVM pineVM,
        PineValue interactiveEnvironment,
        string moduleName,
        string declarationName,
        IReadOnlyList<PineValue> arguments)
    {
        return
            Pine.Core.CodeAnalysis.ElmInteractiveEnvironment.ParseFunctionFromElmModule(
                interactiveEnvironment: interactiveEnvironment,
                moduleName: moduleName,
                declarationName: declarationName,
                pineVM.ParseCache)
                .AndThen(functionValueAndRecord =>
                {
                    var combinedArguments =
                        functionValueAndRecord.functionRecord.ArgumentsAlreadyCollected
                        .ToArray()
                        .Concat(arguments)
                        .ToArray();

                    if (combinedArguments.Length != functionValueAndRecord.functionRecord.ParameterCount)
                    {
                        return
                        (Result<string, PineValue>)
                            ("Partial application not implemented yet. Got " +
                            combinedArguments.Length +
                            " arguments, expected " +
                            functionValueAndRecord.functionRecord.ParameterCount);
                    }

                    var combinedEnvironment =
                    PineValue.List([PineValue.List(functionValueAndRecord.functionRecord.EnvFunctions),
                        PineValue.List(combinedArguments)]);

                    return
                    pineVM.EvaluateExpression(
                        functionValueAndRecord.functionRecord.InnerFunction,
                        environment: combinedEnvironment);
                });
    }
}
