using Pine.Core.CodeAnalysis;
using Pine.Core.Tests.Elm.ElmCompilerInDotnet.CoreLibraryModule;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet;

public class BuildStaticProgramParserConfig
{
    public static StaticProgramParserConfig<DeclQualifiedName> Default(
        ElmInteractiveEnvironment.ParsedInteractiveEnvironment parsedEnv,
        PineVMParseCache parseCache)
    {
        return
            FromEnvironment(
                declName => declName,
                parsedEnv,
                parseCache)
            .AddCoreModules(
                fromCoreModuleBasics:
                declName => new DeclQualifiedName(Namespaces: ["Basics"], declName),
                parseCache);
    }

    public static StaticProgramParserConfig<DeclQualifiedName> Default(
        IReadOnlyDictionary<DeclQualifiedName, PineValue> namedValues,
        PineVMParseCache parseCache)
    {
        return
            FromDictionary(
                namedValues.ToImmutableDictionary(kvp => kvp.Value, kvp => kvp.Key),
                parseCache)
            .AddCoreModules(
                fromCoreModuleBasics:
                declName => new DeclQualifiedName(Namespaces: ["Basics"], declName),
                parseCache);
    }

    public static StaticProgramParserConfig<IdentifierT> FromEnvironment<IdentifierT>(
        System.Func<DeclQualifiedName, IdentifierT> mapName,
        ElmInteractiveEnvironment.ParsedInteractiveEnvironment parsedEnv,
        PineVMParseCache parseCache)
    {
        // Map from extracted/lookup value -> (identifier, original declaration value)
        Dictionary<PineValue, (IdentifierT ident, PineValue originalValue)> declNamesWithOriginals = [];

        foreach (var (moduleName, moduleValue, moduleContent) in parsedEnv.Modules)
        {
            foreach (var kvp in moduleContent.FunctionDeclarations)
            {
                var declName = new DeclQualifiedName(Namespaces: [moduleName], kvp.Key);

                // Use the same logic as NamesFromCompiledEnv to determine the named value.
                // For functions with env functions, the named value might be extracted from the
                // EnvFunctions array, not the raw declaration value.
                var namedValue = kvp.Value;
                var originalValue = kvp.Value;

                if (FunctionRecord.ParseFunctionRecordTagged(kvp.Value, parseCache).IsOkOrNull() is { } functionRecord)
                {
                    var (extractedValue, _, _) =
                        NamesFromCompiledEnv.BuildApplicationFromFunctionRecord(functionRecord, arguments: [], parseCache);

                    namedValue = extractedValue;
                }

                declNamesWithOriginals[namedValue] = (mapName(declName), originalValue);
            }

            foreach (var kvp in moduleContent.TypeDeclarations)
            {
                // TODO: Do more here to recognize function values which are choice tag constructors.
                // TODO: Probably do more here to recognize function values which are record constructors.

                var declName = new DeclQualifiedName(Namespaces: [moduleName], kvp.Key);

                declNamesWithOriginals[kvp.Value] = (mapName(declName), kvp.Value);
            }
        }

        return FromDictionaryWithOriginals(declNamesWithOriginals, parseCache);
    }

    public static StaticProgramParserConfig<IdentifierT> FromDictionary<IdentifierT>(
        IReadOnlyDictionary<PineValue, IdentifierT> declNames,
        PineVMParseCache parseCache)
    {
        // Convert to format with originals (original = lookup value when not separately tracked)
        var declNamesWithOriginals =
            declNames.ToDictionary(
                kvp => kvp.Key,
                kvp => (kvp.Value, kvp.Key));

        return FromDictionaryWithOriginals(declNamesWithOriginals, parseCache);
    }

    private static StaticProgramParserConfig<IdentifierT> FromDictionaryWithOriginals<IdentifierT>(
        IReadOnlyDictionary<PineValue, (IdentifierT ident, PineValue originalValue)> declNamesWithOriginals,
        PineVMParseCache parseCache)
    {
        return
            new StaticProgramParserConfig<IdentifierT>(
                IdentifyInstanceRequired:
                (_, pineValue) =>
                {
                    if (declNamesWithOriginals.TryGetValue(pineValue, out var entry))
                    {
                        return new StaticProgramParser.IdentifyResponse<IdentifierT>(
                            Ident: entry.ident,
                            ContinueParse: true,
                            OriginalFunctionValue: entry.originalValue);
                    }

                    throw new System.InvalidOperationException(
                        $"Could not identify required instance for Pine value: {pineValue}");
                },
                IdentifyInstanceOptional:
                (_, pineValue) =>
                {
                    if (declNamesWithOriginals.TryGetValue(pineValue, out var entry))
                    {
                        return new StaticProgramParser.IdentifyResponse<IdentifierT>(
                            Ident: entry.ident,
                            ContinueParse: true,
                            OriginalFunctionValue: entry.originalValue);
                    }

                    return null;
                },
                IdentifyCrash:
                (_, _) => throw new System.NotImplementedException());
    }
}
