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
    public record ParsedInteractiveEnvironment(
       IReadOnlyList<(string moduleName, PineValue moduleValue, ElmModule moduleContent)> Modules);

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
                declarationName: declarationName)
                .AndThen(functionValueAndRecord =>
                {
                    var combinedArguments =
                        functionValueAndRecord.functionRecord.argumentsAlreadyCollected
                        .Concat(arguments)
                        .ToImmutableList();

                    if (combinedArguments.Count != functionValueAndRecord.functionRecord.functionParameterCount)
                    {
                        return (Result<string, PineValue>)
                            ("Partial application not implemented yet. Got " +
                            combinedArguments.Count +
                            " arguments, expected " +
                            functionValueAndRecord.functionRecord.functionParameterCount);
                    }

                    var combinedEnvironment =
                    PineValue.List([PineValue.List(functionValueAndRecord.functionRecord.envFunctions),
                        PineValue.List(combinedArguments)]);

                    return
                    pineVM.EvaluateExpression(
                        functionValueAndRecord.functionRecord.innerFunction,
                        environment: combinedEnvironment);
                });
    }

    public static Result<string, (PineValue declValue, FunctionRecord functionRecord)> ParseFunctionFromElmModule(
        PineValue interactiveEnvironment,
        string moduleName,
        string declarationName)
    {
        return
            ParseInteractiveEnvironment(interactiveEnvironment)
            .AndThen(parsedEnv =>
            {
                var selectedModule =
                    parsedEnv.Modules
                    .FirstOrDefault(moduleNameAndContent => moduleNameAndContent.moduleName == moduleName);

                if (selectedModule.moduleContent is null)
                    return "module not found";

                var functionDeclaration =
                    selectedModule.moduleContent.FunctionDeclarations
                    .FirstOrDefault(fd => fd.Key == declarationName);

                if (functionDeclaration.Value is null)
                    return "declaration " + declarationName + " not found";

                return
                ParseFunctionRecordFromValueTagged(functionDeclaration.Value)
                .Map(parsedRecord => (functionDeclaration.Value, parsedRecord));
            });
    }

    public static Result<string, PineValue> ApplyFunction(
        IPineVM pineVM,
        FunctionRecord functionRecord,
        IReadOnlyList<PineValue> arguments)
    {
        return
            ApplyFunctionArgumentsForEvalExpr(functionRecord, arguments)
            .AndThen(composedArgs =>
            pineVM.EvaluateExpression(
                composedArgs.expression,
                composedArgs.environment));
    }

    public static Result<string, (Expression expression, PineValue environment)> ApplyFunctionArgumentsForEvalExpr(
        FunctionRecord functionRecord,
        IReadOnlyList<PineValue> arguments)
    {
        var combinedArguments =
            functionRecord.argumentsAlreadyCollected
            .Concat(arguments)
            .ToImmutableList();

        if (combinedArguments.Count != functionRecord.functionParameterCount)
        {
            return
                "Partial application not implemented yet. Got " +
                combinedArguments.Count +
                " arguments, expected " +
                functionRecord.functionParameterCount;
        }

        var combinedEnvironment =
            PineValue.List(
                [
                PineValue.List(functionRecord.envFunctions),
                PineValue.List(combinedArguments)
                ]);

        return (functionRecord.innerFunction, combinedEnvironment);
    }

    public record ElmModule(
        IReadOnlyDictionary<string, PineValue> FunctionDeclarations,
        IReadOnlyDictionary<string, PineValue> TypeDeclarations);

    public record FunctionRecord(
        Expression innerFunction,
        int functionParameterCount,
        IReadOnlyList<PineValue> envFunctions,
        IReadOnlyList<PineValue> argumentsAlreadyCollected);

    /// <summary>
    /// Analog to the 'parseFunctionRecordFromValueTagged' function in FirCompiler.elm
    /// </summary>
    public static Result<string, FunctionRecord> ParseFunctionRecordFromValueTagged(
        PineValue pineValue)
    {
        return
            ParseTagged(pineValue)
            .AndThen(taggedFunctionDeclaration =>
            taggedFunctionDeclaration.name is "Function"
            ?
            ParseFunctionRecordFromValue(taggedFunctionDeclaration.value)
            :
            /*
            (Result<string, FunctionRecord>)"Unexpected tag: " + taggedFunctionDeclaration.name

            If the declaration has zero parameters, it could be encoded as plain PineValue without wrapping in a 'Function' record.
            */
            new FunctionRecord(
                innerFunction: new Expression.Literal(pineValue),
                functionParameterCount: 0,
                envFunctions: [],
                argumentsAlreadyCollected: [])
            );
    }

    /// <summary>
    /// Analog to the 'parseFunctionRecordFromValue' function in FirCompiler.elm
    /// </summary>
    public static Result<string, FunctionRecord> ParseFunctionRecordFromValue(
        PineValue pineValue)
    {
        return
            pineValue switch
            {
                PineValue.ListValue functionRecordListItems =>
                functionRecordListItems.Elements.Count is 4
                ?
                ExpressionEncoding.ParseExpressionFromValueDefault(functionRecordListItems.Elements[0])
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
                        (Result<string, IReadOnlyList<PineValue>>)
                        "envFunctionsValue is not a list"
                    })
                    .AndThen(envFunctions =>
                    {
                        return
                        (functionRecordListItems.Elements[3] switch
                        {
                            PineValue.ListValue listValue =>
                            Result<string, IReadOnlyList<PineValue>>.ok(listValue.Elements),

                            _ =>
                            (Result<string, IReadOnlyList<PineValue>>)
                            "argumentsAlreadyCollectedValue is not a list"
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
                "List does not have the expected number of items: " + functionRecordListItems.Elements.Count,

                _ =>
                "Is not a list but a blob"
            };
    }

    public static Result<string, ParsedInteractiveEnvironment> ParseInteractiveEnvironment(
        PineValue interactiveEnvironment)
    {
        return
            interactiveEnvironment switch
            {
                PineValue.ListValue listValue =>
                listValue.Elements
                .Select(ParseNamedElmModule)
                .ListCombine()
                .Map(modules => new ParsedInteractiveEnvironment(Modules: modules)),

                _ =>
                "interactive environment not a list"
            };
    }

    public static Result<string, (string moduleName, PineValue moduleValue, ElmModule moduleContent)> ParseNamedElmModule(
        PineValue moduleValue) =>
        ParseTagged(moduleValue)
        .AndThen(tagged =>
        {
            var moduleName = tagged.name;

            return
                ParseElmModule(tagged.value)
                .Map(module => (moduleName, tagged.value, module));
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
                "module not a list"
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
                    .ToImmutableDictionary(
                        keySelector: declaration => declaration.name,
                        elementSelector: declaration => declaration.value);

                return
                    (Result<string, ElmModule>)
                    new ElmModule(
                        FunctionDeclarations: functionDeclarations,
                        TypeDeclarations: typeDeclarations);
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
            "Unexpected list length: " + listValue.Elements.Count,

            _ =>
            "Expected list"
        };
}
