using Pine.PineVM;
using Pine;
using System.Collections.Generic;
using System.Collections.Immutable;
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
            ParseFunctionFromElmModule(
                interactiveEnvironment: interactiveEnvironment,
                moduleName: moduleName,
                declarationName: declarationName,
                pineVM: pineVM)
                .AndThen(functionRecord =>
                {
                    var combinedArguments =
                        functionRecord.argumentsAlreadyCollected
                        .Concat(arguments)
                        .ToImmutableList();

                    if (combinedArguments.Count != functionRecord.functionParameterCount)
                        return Result<string, PineValue>.err(
                            "Partial application not implemented yet. Got " +
                            combinedArguments.Count +
                            " arguments, expected " +
                            functionRecord.functionParameterCount);

                    var combinedEnvironment =
                    PineValue.List([PineValue.List(functionRecord.envFunctions),
                        PineValue.List(combinedArguments)]);

                    return
                    pineVM.EvaluateExpression(
                        functionRecord.innerFunction,
                        environment: combinedEnvironment);
                });
    }

    public static Result<string, FunctionRecord> ParseFunctionFromElmModule(
        PineValue interactiveEnvironment,
        string moduleName,
        string declarationName,
        PineVM? pineVM = null)
    {
        var parseModulesResult =
            interactiveEnvironment switch
            {
                PineValue.ListValue listValue =>
                listValue.Elements
                .Select(ParseNamedElmModule)
                .ListCombine(),

                _ =>
                Result<string, IReadOnlyList<(string moduleName, ElmModule moduleContent)>>.err(
                    "interactive environment not a list")
            };

        return
            parseModulesResult
            .AndThen(modules =>
            {
                var selectedModule =
                    modules
                    .FirstOrDefault(moduleNameAndContent => moduleNameAndContent.moduleName == moduleName);

                if (selectedModule.moduleContent is null)
                    return Result<string, FunctionRecord>.err("module not found");

                var functionDeclaration =
                    selectedModule.moduleContent.FunctionDeclarations
                    .FirstOrDefault(fd => fd.Key == declarationName);

                if (functionDeclaration.Value is null)
                    return Result<string, FunctionRecord>.err("declaration not found");

                return
                ParseTagged(functionDeclaration.Value)
                .AndThen(taggedFunctionDeclaration =>
                taggedFunctionDeclaration.name is "Function"
                ?
                ParseFunctionRecordFromValue(taggedFunctionDeclaration.value, pineVM)
                :
                Result<string, FunctionRecord>.err("Unexpected tag: " + taggedFunctionDeclaration.name));
            });
    }

    public static Result<string, PineValue> ApplyFunction(
        PineVM pineVM,
        FunctionRecord functionRecord,
        IReadOnlyList<PineValue> arguments)
    {
        var combinedArguments =
            functionRecord.argumentsAlreadyCollected
            .Concat(arguments)
            .ToImmutableList();

        if (combinedArguments.Count != functionRecord.functionParameterCount)
            return Result<string, PineValue>.err(
                "Partial application not implemented yet. Got " +
                combinedArguments.Count +
                " arguments, expected " +
                functionRecord.functionParameterCount);

        var combinedEnvironment =
        PineValue.List([PineValue.List(functionRecord.envFunctions),
            PineValue.List(combinedArguments)]);

        return
        pineVM.EvaluateExpression(
            functionRecord.innerFunction,
            environment: combinedEnvironment);
    }

    public record ElmModule(
        IReadOnlyDictionary<string, PineValue> FunctionDeclarations);

    public record FunctionRecord(
        Expression innerFunction,
        int functionParameterCount,
        IReadOnlyList<PineValue> envFunctions,
        IReadOnlyList<PineValue> argumentsAlreadyCollected);

    /// <summary>
    /// Analog to 'parseFunctionRecordFromValue' in the Elm compiler.
    /// </summary>
    public static Result<string, FunctionRecord> ParseFunctionRecordFromValue(
        PineValue pineValue,
        PineVM? pineVM = null)
    {
        return
            pineValue switch
            {
                PineValue.ListValue functionRecordListItems =>
                functionRecordListItems.Elements.Count is 4
                ?
                (pineVM ?? new PineVM()).ParseExpressionFromValue(functionRecordListItems.Elements[0])
                .AndThen(innerFunction =>
                PineValueAsInteger.SignedIntegerFromValueStrict(functionRecordListItems.Elements[1])
                .MapError(err => "Failed to decode function parameter count: " + err)
                .AndThen(functionParameterCount =>
                {
                    return
                    (functionRecordListItems.Elements[2] switch
                    {
                        PineValue.ListValue listValue =>
                        Result<string, IReadOnlyList<PineValue>>.ok(listValue.Elements),

                        _ =>
                        Result<string, IReadOnlyList<PineValue>>.err("envFunctionsValue is not a list")
                    })
                    .AndThen(envFunctions =>
                    {
                        return
                        (functionRecordListItems.Elements[3] switch
                        {
                            PineValue.ListValue listValue =>
                            Result<string, IReadOnlyList<PineValue>>.ok(listValue.Elements),

                            _ =>
                            Result<string, IReadOnlyList<PineValue>>.err(
                                "argumentsAlreadyCollectedValue is not a list")
                        })
                        .AndThen(argumentsAlreadyCollected =>
                        {
                            return
                            Result<string, FunctionRecord>.ok(
                                new FunctionRecord(
                                    innerFunction: innerFunction,
                                    functionParameterCount: (int)functionParameterCount,
                                    envFunctions: envFunctions,
                                    argumentsAlreadyCollected: argumentsAlreadyCollected));
                        });
                    });
                }
                ))
                :
                Result<string, FunctionRecord>.err(
                    "List does not have the expected number of items: " + functionRecordListItems.Elements.Count),

                _ =>
                Result<string, FunctionRecord>.err("Is not a list but a blob")
            };
    }

    public static Result<string, (string moduleName, ElmModule moduleContent)> ParseNamedElmModule(
        PineValue moduleValue) =>
        ParseTagged(moduleValue)
        .AndThen(tagged =>
        {
            var moduleName = tagged.name;

            return
                ParseElmModule(tagged.value)
                .Map(module => (moduleName, module));
        });

    /// <summary>
    /// Mirroring the encoding in 'emitModuleValue' in the Elm compiler.
    /// </summary>
    public static Result<string, ElmModule> ParseElmModule(PineValue moduleValue)
    {
        // We expect to find a list of pairs of strings and Pine values.
        // Names starting with uppercase letters are type declarations, so we filter these out here.
        var allDeclarations =
            moduleValue switch
            {
                PineValue.ListValue listValue =>
                listValue.Elements
                .Select(ParseTagged)
                .ListCombine(),

                _ =>
                Result<string, IReadOnlyList<(string name, PineValue value)>>.err("module not a list")
            };

        return
            allDeclarations
            .AndThen(declarations =>
            {
                var functionDeclarations =
                    declarations
                    .Where(declaration => char.IsLower(declaration.name[0]))
                    .ToImmutableDictionary(
                        keySelector: declaration => declaration.name,
                        elementSelector: declaration => declaration.value);

                var typeDeclarations =
                    declarations
                    .Where(declaration => char.IsUpper(declaration.name[0]))
                    .ToImmutableArray();

                return
                    Result<string, ElmModule>.ok(
                        new ElmModule(
                            FunctionDeclarations: functionDeclarations));
            });
    }

    public static Result<string, (string name, PineValue value)> ParseTagged(PineValue pineValue) =>
        pineValue switch
        {
            PineValue.ListValue listValue =>
            listValue.Elements.Count is 2
            ?
            PineValueAsString.StringFromValue(listValue.Elements[0])
            .Map(tag => (tag, listValue.Elements[1]))
            :
            Result<string, (string, PineValue)>.err("Unexpected list length: " + listValue.Elements.Count),

            _ =>
            Result<string, (string, PineValue)>.err("Expected list")
        };
}
