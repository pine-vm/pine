using Pine.Core;
using Pine.Core.PopularEncodings;
using Pine.PineVM;
using System;
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
                declarationName: declarationName,
                pineVM.parseCache)
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

    public static Result<string, (PineValue declValue, FunctionRecord functionRecord)> ParseFunctionFromElmModule(
        PineValue interactiveEnvironment,
        string moduleName,
        string declarationName,
        PineVMParseCache parseCache)
    {
        var parseEnvResult =
            ParseInteractiveEnvironment(interactiveEnvironment);

        if (parseEnvResult.IsErrOrNull() is { } err)
        {
            return "Failed parsing environment: " + err;
        }

        if (parseEnvResult.IsOkOrNull() is not { } parsedEnv)
        {
            throw new NotImplementedException(
                "Unexpected result type: " + parseEnvResult.GetType());
        }

        var selectedModule =
            parsedEnv.Modules
            .FirstOrDefault(moduleNameAndContent => moduleNameAndContent.moduleName == moduleName);

        if (selectedModule.moduleContent is not { } moduleContent)
        {
            return
                "Did not find module '" + moduleName + "' (there are " +
                parsedEnv.Modules.Count + " other modules: " +
                string.Join(", ", parsedEnv.Modules.Select(m => m.moduleName));
        }

        var functionDeclaration =
            moduleContent.FunctionDeclarations
            .FirstOrDefault(fd => fd.Key == declarationName);

        if (functionDeclaration.Value is not { } funcDeclValue)
        {
            return
                "Did not find function '" + declarationName + "' in module '" +
                moduleName + "' (there are " +
                moduleContent.FunctionDeclarations.Count + " functions declarations and " +
                moduleContent.TypeDeclarations.Count + " type declarations in this module: " +
                string.Join(", ", moduleContent.FunctionDeclarations.Select(fd => fd.Key)) + ")";
        }

        return
        ParseFunctionRecordFromValueTagged(funcDeclValue, parseCache)
        .Map(parsedRecord => (funcDeclValue, parsedRecord));
    }

    public static Result<string, PineValue> ApplyFunction(
        Pine.Core.PineVM.IPineVM pineVM,
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
        IReadOnlyList<PineValue> appendArguments)
    {
        ReadOnlySpan<PineValue> combinedArguments =
            [..functionRecord.ArgumentsAlreadyCollected.Span,
            ..appendArguments
            ];

        if (combinedArguments.Length != functionRecord.ParameterCount)
        {
            return
                "Partial application not implemented yet. Got " +
                combinedArguments.Length +
                " arguments, expected " +
                functionRecord.ParameterCount;
        }

        var combinedEnvironment =
            PineValue.List(
                [
                PineValue.List(functionRecord.EnvFunctions),
                PineValue.List([..combinedArguments])
                ]);

        return (functionRecord.InnerFunction, combinedEnvironment);
    }

    public record ElmModule(
        IReadOnlyDictionary<string, PineValue> FunctionDeclarations,
        IReadOnlyDictionary<string, PineValue> TypeDeclarations);

    public record FunctionRecord(
        Expression InnerFunction,
        int ParameterCount,
        ReadOnlyMemory<PineValue> EnvFunctions,
        ReadOnlyMemory<PineValue> ArgumentsAlreadyCollected);

    /// <summary>
    /// Analog to the 'parseFunctionRecordFromValueTagged' function in FirCompiler.elm
    /// </summary>
    public static Result<string, FunctionRecord> ParseFunctionRecordFromValueTagged(
        PineValue pineValue,
        PineVMParseCache parseCache)
    {
        return
            ParseTagged(pineValue)
            .AndThen(taggedFunctionDeclaration =>
            taggedFunctionDeclaration.name is "Function"
            ?
            ParseFunctionRecordFromValue(taggedFunctionDeclaration.value, parseCache)
            :
            /*
            (Result<string, FunctionRecord>)"Unexpected tag: " + taggedFunctionDeclaration.name

            If the declaration has zero parameters, it could be encoded as plain PineValue without wrapping in a 'Function' record.
            */
            new FunctionRecord(
                InnerFunction: Expression.LiteralInstance(pineValue),
                ParameterCount: 0,
                EnvFunctions: ReadOnlyMemory<PineValue>.Empty,
                ArgumentsAlreadyCollected: ReadOnlyMemory<PineValue>.Empty)
            );
    }

    /// <summary>
    /// Inverse of <see cref="ParseFunctionRecordFromValueTagged"/>
    /// </summary>
    public static PineValue EncodeFunctionRecordInValueTagged(
        FunctionRecord functionRecord)
    {
        return
            PineValue.List(
                [
                StringEncoding.ValueFromString("Function"),
                EncodeFunctionRecordInValue(functionRecord)
                ]);
    }

    /// <summary>
    /// Analog to the 'parseFunctionRecordFromValue' function in FirCompiler.elm
    /// </summary>
    public static Result<string, FunctionRecord> ParseFunctionRecordFromValue(
        PineValue pineValue,
        PineVMParseCache parseCache)
    {
        if (pineValue is not PineValue.ListValue functionRecordListItems)
            return "Function record is not a list";

        if (functionRecordListItems.Items.Length is not 4)
        {
            return
                "Unexpected number of elements in function record: Not 4 but " +
                functionRecordListItems.Items.Length;
        }

        var parseInnerExprResult =
            parseCache.ParseExpression(functionRecordListItems.Items.Span[0]);

        {
            if (parseInnerExprResult.IsErrOrNull() is { } err)
            {
                return "Failed to parse inner function: " + err;
            }
        }

        if (parseInnerExprResult.IsOkOrNull() is not { } innerFunction)
        {
            throw new NotImplementedException(
                "Unexpected result type: " + parseInnerExprResult.GetType());
        }

        var parseFunctionParameterCountResult =
            IntegerEncoding.ParseSignedIntegerStrict(functionRecordListItems.Items.Span[1]);

        {
            if (parseFunctionParameterCountResult.IsErrOrNull() is { } err)
            {
                return "Failed to decode function parameter count: " + err;
            }
        }

        if (parseFunctionParameterCountResult.IsOkOrNullable() is not { } functionParameterCount)
        {
            throw new NotImplementedException(
                "Unexpected result type: " + parseFunctionParameterCountResult.GetType());
        }

        var envFunctionsAggregateValue =
            functionRecordListItems.Items.Span[2];

        if (envFunctionsAggregateValue is not PineValue.ListValue envFunctionsList)
        {
            return "envFunctionsValue is not a list";
        }

        var argumentsAlreadyCollectedAggregateValue =
            functionRecordListItems.Items.Span[3];

        if (argumentsAlreadyCollectedAggregateValue is not PineValue.ListValue argumentsAlreadyCollectedList)
        {
            return "argumentsAlreadyCollectedValue is not a list";
        }

        return
            new FunctionRecord(
                InnerFunction: innerFunction,
                ParameterCount: (int)functionParameterCount,
                EnvFunctions: envFunctionsList.Items.ToArray(),
                ArgumentsAlreadyCollected: argumentsAlreadyCollectedList.Items.ToArray());
    }

    /// <summary>
    /// Inverse of <see cref="ParseFunctionRecordFromValue"/>"/>
    /// </summary>
    public static PineValue EncodeFunctionRecordInValue(
        FunctionRecord functionRecord)
    {
        var innerFunctionValue =
            ExpressionEncoding.EncodeExpressionAsValue(functionRecord.InnerFunction);

        return
            PineValue.List(
                [
                innerFunctionValue,
                IntegerEncoding.EncodeSignedInteger(functionRecord.ParameterCount),
                PineValue.List(functionRecord.EnvFunctions.ToArray()),
                PineValue.List(functionRecord.ArgumentsAlreadyCollected.ToArray())
                ]);
    }

    public static Result<string, ParsedInteractiveEnvironment> ParseInteractiveEnvironment(
        PineValue interactiveEnvironment)
    {
        return
            interactiveEnvironment switch
            {
                PineValue.ListValue listValue =>
                listValue.Items
                .ToArray()
                .Select(ParseNamedElmModule)
                .ListCombine()
                .Map(modules => new ParsedInteractiveEnvironment(Modules: modules)),

                _ =>
                "interactive environment not a list"
            };
    }

    public static PineValue StripInteractiveDeclsFromEnvironment(PineValue interactiveEnvironment)
    {
        return
            interactiveEnvironment switch
            {
                PineValue.ListValue listValue =>
                PineValue.List(
                    [..listValue.Items
                    .ToArray()
                    .Where(envItem => !EnvItemLooksLikeInteractiveDecl(envItem))]),

                _ =>
                interactiveEnvironment
            };
    }

    public static bool EnvItemLooksLikeInteractiveDecl(PineValue envItemValue)
    {
        if (envItemValue is not PineValue.ListValue listValue)
            return false;

        if (listValue.Items.Length is not 2)
            return false;

        if (StringEncoding.StringFromValue(listValue.Items.Span[0]).IsOkOrNull() is not { } name)
            return false;

        if (name.Length < 1)
            return false;

        return char.IsLower(name[0]);
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
                listValue.Items
                .ToArray()
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
            listValue.Items.Length is 2
            ?
            StringEncoding.StringFromValue(listValue.Items.Span[0])
            .Map(tag => (tag, listValue.Items.Span[1]))
            :
            "Unexpected list length: " + listValue.Items.Length,

            _ =>
            "Expected list"
        };
}
