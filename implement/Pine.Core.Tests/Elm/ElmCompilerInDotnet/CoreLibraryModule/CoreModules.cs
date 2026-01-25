using Pine.Core.CodeAnalysis;
using Pine.Core.Elm.ElmCompilerInDotnet.CoreLibraryModule;
using System;
using System.Collections.Generic;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.CoreLibraryModule;

public static class CoreModules
{
    /// <summary>
    /// Builds a static program parser configuration that recognizes default core modules.
    /// </summary>
    public static StaticProgramParserConfig<IdentifierT> AddCoreModules<IdentifierT>(
        this StaticProgramParserConfig<IdentifierT> previousConfig,
        Func<string, IdentifierT> fromCoreModuleBasics,
        PineVMParseCache parseCache)
    {
        StaticProgramParser.IdentifyResponse<IdentifierT>? IdentifyOverride(
            IEnumerable<IdentifierT> stack,
            PineValue pineValue)
        {
            // First try to identify as a known function value (e.g., Basics.add, Basics.sub)
            if (CoreBasics.IdentifyFunctionValue(pineValue) is { } functionName)
            {
                var identifier = fromCoreModuleBasics(functionName);

                return new StaticProgramParser.IdentifyResponse<IdentifierT>(identifier, ContinueParse: false);
            }

            // Also try parsing as expression for backwards compatibility with application patterns
            if (parseCache.ParseExpression(pineValue).IsOkOrNull() is { } expr)
            {
                if (CoreBasics.Identify(expr) is { } basicsName)
                {
                    var identifier = fromCoreModuleBasics(basicsName.declName);

                    return new StaticProgramParser.IdentifyResponse<IdentifierT>(identifier, ContinueParse: false);
                }
            }

            return null;
        }

        StaticProgramParser.IdentifyResponse<IdentifierT> IdentifyRequired(
            IEnumerable<IdentifierT> stack,
            PineValue pineValue)
        {
            if (IdentifyOverride(stack, pineValue) is { } overriden)
            {
                return overriden;
            }

            return previousConfig.IdentifyInstanceRequired(stack, pineValue);
        }

        StaticProgramParser.IdentifyResponse<IdentifierT>? IdentifyOptional(
            IEnumerable<IdentifierT> stack,
            PineValue pineValue)
        {
            if (IdentifyOverride(stack, pineValue) is { } overriden)
            {
                return overriden;
            }

            return previousConfig.IdentifyInstanceOptional(stack, pineValue);
        }

        return
            new StaticProgramParserConfig<IdentifierT>(
                IdentifyInstanceRequired: IdentifyRequired,
                IdentifyInstanceOptional: IdentifyOptional,
                IdentifyCrash: previousConfig.IdentifyCrash);
    }
}
