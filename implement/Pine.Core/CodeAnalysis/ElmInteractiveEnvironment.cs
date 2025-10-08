using Pine.Core.PopularEncodings;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.Core.CodeAnalysis;

/// <summary>
/// Functions to parse modules and declarations from an Elm interactive environment.
/// The Elm interactive environment contains all modules available in the REPL (e.g., from the seed application) as well
/// as declarations added by submissions in the REPL.
/// </summary>
public static class ElmInteractiveEnvironment
{
    /// <summary>
    /// Result of parsing the interactive environment: A collection of modules with their raw value and structured content.
    /// </summary>
    /// <param name="Modules">Sequence of parsed modules: tuple of module name, original encoded <see cref="PineValue"/>, and structured <see cref="ElmModule"/>.</param>
    public record ParsedInteractiveEnvironment(
       IReadOnlyList<(string moduleName, PineValue moduleValue, ElmModule moduleContent)> Modules);

    /// <summary>
    /// Finds and parses a function declaration with the given <paramref name="declarationName"/> inside <paramref name="moduleName"/>.
    /// </summary>
    /// <param name="interactiveEnvironment">Encoded interactive environment value (list of tagged module values).</param>
    /// <param name="moduleName">Name of the Elm module containing the declaration.</param>
    /// <param name="declarationName">Name of the function declaration to parse.</param>
    /// <param name="parseCache">Cache used to avoid re-parsing expression encodings.</param>
    /// <returns>
    /// On success: tuple containing the raw declaration <see cref="PineValue"/> and the parsed <see cref="FunctionRecord"/>.
    /// On failure: string describing the error (environment parse failure, module not found, or declaration not found / malformed).
    /// </returns>
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
                string.Join(", ", parsedEnv.Modules.Select(m => m.moduleName)) + ")";
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
        FunctionRecord.ParseFunctionRecordTagged(funcDeclValue, parseCache)
        .Map(parsedRecord => (funcDeclValue, parsedRecord));
    }

    /// <summary>
    /// Applies a parsed function to the given <paramref name="arguments"/> by composing an evaluation expression
    /// and invoking the supplied Pine VM instance.
    /// </summary>
    /// <param name="pineVM">Implementation of the Pine virtual machine used for evaluation.</param>
    /// <param name="functionRecord">The parsed function (possibly with partially collected arguments).</param>
    /// <param name="arguments">Additional argument values to append.</param>
    /// <returns>The result of evaluating the function application (Ok: value, Err: error message).</returns>
    public static Result<string, PineValue> ApplyFunction(
        PineVM.IPineVM pineVM,
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

    /// <summary>
    /// Composes an expression and environment suitable for evaluation of a function application.
    /// </summary>
    /// <param name="functionRecord">The function to apply (may already have collected arguments).</param>
    /// <param name="appendArguments">Arguments to append to those already collected.</param>
    /// <returns>
    /// On success: tuple (expression, environment) ready for <see cref="PineVM.IPineVM.EvaluateExpression"/>.
    /// On failure: error if argument count does not match (partial application not implemented yet).
    /// </returns>
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

    /// <summary>
    /// Representation of a parsed Elm module separated into function and type declarations.
    /// </summary>
    /// <param name="FunctionDeclarations">Map of function declaration names (lowercase initial) to their encoded values.</param>
    /// <param name="TypeDeclarations">Map of type/constructor declaration names (uppercase initial) to their encoded values.</param>
    public record ElmModule(
        IReadOnlyDictionary<string, PineValue> FunctionDeclarations,
        IReadOnlyDictionary<string, PineValue> TypeDeclarations);


    /// <summary>
    /// Parses the top-level interactive environment value into a sequence of Elm modules.
    /// </summary>
    /// <param name="interactiveEnvironment">Encoded list of tagged module values.</param>
    /// <returns>Parsed environment or error message.</returns>
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

    /// <summary>
    /// Removes declarations that look like interactive (REPL-entered) declarations (lowercase initial name) from the environment value.
    /// </summary>
    /// <param name="interactiveEnvironment">Original interactive environment value.</param>
    /// <returns>Environment value with interactive declarations stripped (modules only).</returns>
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

    /// <summary>
    /// Heuristic to decide whether an environment list item represents an interactive declaration
    /// (expects a 2-item list with a lowercase initial string tag).
    /// </summary>
    /// <param name="envItemValue">Candidate environment item.</param>
    /// <returns><c>true</c> if it looks like an interactive declaration; otherwise <c>false</c>.</returns>
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

    /// <summary>
    /// Parses a tagged module value to obtain its name and structured content.
    /// </summary>
    /// <param name="moduleValue">Tagged module value (list of two items: name string, module body value).</param>
    /// <returns>Tuple of module name, raw value, and parsed <see cref="ElmModule"/>.</returns>
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
    /// Splits declarations into function (lowercase initial) and type (uppercase initial) groups.
    /// </summary>
    /// <param name="moduleValue">Encoded module value.</param>
    /// <returns>Parsed <see cref="ElmModule"/> or error string.</returns>
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

    /// <summary>
    /// Parses a tagged value (2-item list: name tag and payload).
    /// </summary>
    /// <param name="pineValue">Candidate tagged value.</param>
    /// <returns>Tuple (name, value) or error string.</returns>
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

    /// <summary>
    /// Parses a tagged value (2-item list: name tag and payload) from a structural view.
    /// </summary>
    /// <param name="pineValueClass">Structural view of the candidate tagged value.</param>
    /// <returns>
    /// On success: tuple (name, payload-as-class).
    /// On failure: error string describing the malformed structure.
    /// </returns>
    public static Result<string, (string name, PineValueClass value)> ParseTagged(PineValueClass pineValueClass)
    {
        if (pineValueClass.TryGetValue([0]) is not { } tagValue)
        {
            return "Tagged value missing tag at [0]";
        }

        if (StringEncoding.StringFromValue(tagValue).IsOkOrNull() is not { } tag)
        {
            return "Failed to parse tag string";
        }

        var payloadClass = pineValueClass.PartUnderPath([1]);

        if (payloadClass.ParsedItems.Count is 0)
        {
            return "Tagged value missing payload at [1]";
        }

        return (tag, payloadClass);
    }
}
