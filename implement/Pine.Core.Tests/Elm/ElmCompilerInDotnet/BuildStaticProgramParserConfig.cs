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
                        NamesFromCompiledEnv.BuildApplicationFromFunctionRecord(
                            functionRecord,
                            arguments: [],
                            parseCache);

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
        where IdentifierT : notnull
    {
        // One descriptor per dictionary entry: the entry's `lookupValue` is the value
        // by which the callee is named at frontend-emitted call sites (the literal
        // head of a Form B chain or a bare reference), and `originalValue` is the
        // wrapper used for Form A / Form C derivation. They differ for non-recursive
        // callees whose lookup value is the extracted inner value but whose wrapper
        // is needed to compute the encoded body / consolidated-form templates.
        var descriptors = new List<CalleeDescriptor<IdentifierT>>(declNamesWithOriginals.Count);

        foreach (var (lookupValue, entry) in declNamesWithOriginals)
        {
            descriptors.Add(
                new CalleeDescriptor<IdentifierT>(
                    Identifier: entry.ident,
                    NamedValue: lookupValue,
                    ContinueParse: true,
                    WrapperValue: entry.originalValue));
        }

        // Base config: required-throws / optional-null / crash-throws. The augmenting
        // helper installs the recognition facts derived from `descriptors`.
        var baseConfig =
            new StaticProgramParserConfig<IdentifierT>(
                IdentifyInstanceRequired:
                (_, pineValue) =>
                    throw new System.InvalidOperationException(
                        $"Could not identify required instance for Pine value: {pineValue}"),
                IdentifyInstanceOptional:
                (_, _) => null,
                IdentifyCrash:
                (_, _) => throw new System.NotImplementedException(),
                IdentifyEncodedBodyOptional:
                (_, _) => null,
                ConsolidatedFormTemplates: null);

        return CalleeRecognition.Augment(baseConfig, descriptors, parseCache);
    }
}
