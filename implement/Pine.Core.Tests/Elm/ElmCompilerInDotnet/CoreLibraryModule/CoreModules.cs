using Pine.Core.CodeAnalysis;
using Pine.Core.Elm.ElmCompilerInDotnet.CoreLibraryModule;
using System;
using System.Collections.Generic;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.CoreLibraryModule;

public static class CoreModules
{
    /// <summary>
    /// Builds a static program parser configuration that recognizes default core modules.
    /// <para>
    /// Delegates the per-callee derivation of recognition facts (Form A / Form B /
    /// Form C / bare reference) to <see cref="CalleeRecognition.Augment{IdentifierT}(StaticProgramParserConfig{IdentifierT}, IReadOnlyCollection{CalleeDescriptor{IdentifierT}}, PineVMParseCache)"/>.
    /// Mirrors the names recognized by <see cref="CoreBasics.IdentifyFunctionValue(PineValue)"/>
    /// and resolvable through <see cref="CoreBasics.GetFunctionValue(string)"/> via
    /// the registry <see cref="CoreBasics.KnownDeclarationNames"/>.
    /// </para>
    /// </summary>
    public static StaticProgramParserConfig<IdentifierT> AddCoreModules<IdentifierT>(
        this StaticProgramParserConfig<IdentifierT> previousConfig,
        Func<string, IdentifierT> fromCoreModuleBasics,
        PineVMParseCache parseCache)
        where IdentifierT : notnull
    {
        var descriptors = new List<CalleeDescriptor<IdentifierT>>();

        foreach (var declName in CoreBasics.KnownDeclarationNames)
        {
            if (CoreBasics.GetFunctionValue(declName) is not { } functionValue)
            {
                continue;
            }

            descriptors.Add(
                new CalleeDescriptor<IdentifierT>(
                    Identifier: fromCoreModuleBasics(declName),
                    NamedValue: functionValue,
                    ContinueParse: false));
        }

        return CalleeRecognition.Augment(previousConfig, descriptors, parseCache);
    }
}
