using System;
using System.Collections.Generic;

namespace Pine.Core.CodeAnalysis;

/// <summary>
/// Describes a callee that the static program parser should recognize.
/// <para>
/// One of the parser's responsibilities is to prove that the different
/// surface forms of expressions serving as the same function — bare
/// references (depth K=0), nested-<see cref="Expression.ParseAndEval"/>
/// chains (Form B), the saturated single-<c>ParseAndEval</c> form whose
/// <c>Encoded</c> is the encoded inner-function body (Form A), and the
/// consolidated form produced by
/// <see cref="ReducePineExpression.TryConsolidateGenericFunctionApplicationChain(Expression.ParseAndEval, PineVMParseCache)"/>
/// (Form C) — are semantically equivalent and canonicalize them to the
/// same identifier. The variance covered includes partial application
/// (every K ∈ [0, ParameterCount]).
/// </para>
/// <para>
/// A <see cref="CalleeDescriptor{IdentifierT}"/> is the minimal input
/// needed to derive every recognition fact for one such callee: the
/// identifier the parser should emit, the value the callee is named by
/// at frontend-emitted call sites (the value passed as the literal head
/// of <see cref="CodeAnalysis.BuildGenericFunctionApplication(Expression, IReadOnlyList{Expression})"/>),
/// and whether to descend into the body for further parsing.
/// </para>
/// </summary>
/// <typeparam name="IdentifierT">The parser's identifier type.</typeparam>
/// <param name="Identifier">
/// The identifier the parser assigns to call sites and references that
/// resolve to this callee.
/// </param>
/// <param name="NamedValue">
/// The value the callee is named by at frontend-emitted call sites
/// — i.e. the value that appears as the literal head of a Form B
/// chain or a bare reference. For parsed Elm declarations whose
/// declaration value parses as a <see cref="FunctionRecord"/> this
/// is the extracted inner value (the encoded body); for built-in
/// core-library callees this is the function value (e.g.
/// <c>CoreBasics.Add_FunctionValue()</c>).
/// </param>
/// <param name="WrapperValue">
/// Optional wrapper value used for Form A / Form C derivation when it
/// differs from <paramref name="NamedValue"/>. For a parsed Elm
/// declaration this is the original declaration value (the wrapper).
/// When <c>null</c>, <paramref name="NamedValue"/> is used as the
/// wrapper value. The wrapper value is the value passed as the
/// literal head of <see cref="CodeAnalysis.BuildGenericFunctionApplication(Expression, IReadOnlyList{Expression})"/>.
/// </param>
/// <param name="ContinueParse">
/// Whether the parser should also descend into the callee's body and
/// add further declarations found there to the static program model.
/// Typically <c>true</c> for parsed declarations and <c>false</c> for
/// built-in core-library callees.
/// </param>
public record CalleeDescriptor<IdentifierT>(
    IdentifierT Identifier,
    PineValue NamedValue,
    bool ContinueParse,
    PineValue? WrapperValue = null)
{
    /// <summary>
    /// The wrapper value used for Form A / Form C derivation. Defaults
    /// to <see cref="NamedValue"/> when <see cref="WrapperValue"/> is
    /// <c>null</c>.
    /// </summary>
    public PineValue EffectiveWrapperValue => WrapperValue ?? NamedValue;
}

/// <summary>
/// Derives a <see cref="StaticProgramParserConfig{IdentifierT}"/> from a
/// sequence of <see cref="CalleeDescriptor{IdentifierT}"/>s.
/// <para>
/// Performs once, uniformly, the per-callee work that maps every
/// surface form of an expression serving as that callee back to the
/// same identifier:
/// </para>
/// <list type="bullet">
/// <item>
/// <description>
/// A lookup keyed by <see cref="CalleeDescriptor{IdentifierT}.NamedValue"/>
/// drives <c>IdentifyInstance{Required,Optional}</c> for bare references
/// and Form B chains.
/// </description>
/// </item>
/// <item>
/// <description>
/// For descriptors whose <c>NamedValue</c> parses as a
/// <see cref="FunctionRecord"/>, a lookup keyed by the encoded
/// inner-function body (computed via
/// <see cref="NamesFromCompiledEnv.BuildApplicationFromFunctionRecord(FunctionRecord, IReadOnlyList{Expression}, PineVMParseCache)"/>)
/// drives <see cref="StaticProgramParserConfig{IdentifierT}.IdentifyEncodedBodyOptional"/>
/// for Form A.
/// </description>
/// </item>
/// <item>
/// <description>
/// For each such descriptor and each application depth
/// <c>K ∈ [2, ParameterCount]</c>, a
/// <see cref="StaticProgramParser.ConsolidatedFormTemplateEntry{IdentifierT}"/>
/// computed via
/// <see cref="CodeAnalysis.TryComputeConsolidatedFormTemplate(PineValue, int, PineVMParseCache)"/>
/// is appended to <see cref="StaticProgramParserConfig{IdentifierT}.ConsolidatedFormTemplates"/>
/// for Form C.
/// </description>
/// </item>
/// </list>
/// </summary>
public static class CalleeRecognition
{
    /// <summary>
    /// Augments <paramref name="previousConfig"/> with the recognition
    /// facts derived from <paramref name="callees"/>.
    /// <para>
    /// The returned config delegates to <paramref name="previousConfig"/>
    /// when no descriptor matches, so callers can compose multiple
    /// batches (for example a batch of user declarations and a batch of
    /// core-library callees).
    /// </para>
    /// </summary>
    public static StaticProgramParserConfig<IdentifierT> Augment<IdentifierT>(
        StaticProgramParserConfig<IdentifierT> previousConfig,
        IReadOnlyCollection<CalleeDescriptor<IdentifierT>> callees,
        PineVMParseCache parseCache)
        where IdentifierT : notnull
    {
        ArgumentNullException.ThrowIfNull(previousConfig);
        ArgumentNullException.ThrowIfNull(callees);
        ArgumentNullException.ThrowIfNull(parseCache);

        // Lookup driving IdentifyInstance{Required,Optional} (bare references and Form B
        // chains): the value as it appears as the literal head of a frontend-emitted
        // call site.
        Dictionary<PineValue, CalleeDescriptor<IdentifierT>> namedValueLookup = [];

        // Lookup driving IdentifyEncodedBodyOptional (Form A): the encoded inner-function
        // body, distinct from the wrapper value at Form B call sites for non-recursive
        // callees.
        Dictionary<PineValue, CalleeDescriptor<IdentifierT>> encodedBodyLookup = [];

        // Templates driving ConsolidatedFormTemplates (Form C), one per applicable
        // application depth K ∈ [2, ParameterCount].
        var consolidatedTemplates =
            new List<StaticProgramParser.ConsolidatedFormTemplateEntry<IdentifierT>>();

        if (previousConfig.ConsolidatedFormTemplates is { } existingTemplates)
        {
            consolidatedTemplates.AddRange(existingTemplates);
        }

        foreach (var descriptor in callees)
        {
            // Last-writer-wins on duplicate NamedValue. This matches the existing
            // dictionary-based call sites and keeps composition associative.
            namedValueLookup[descriptor.NamedValue] = descriptor;

            var wrapperValue = descriptor.EffectiveWrapperValue;

            if (FunctionRecord.ParseFunctionRecordTagged(wrapperValue, parseCache).IsOkOrNull()
                is not { } functionRecord)
            {
                continue;
            }

            var (encodedBody, _, _) =
                NamesFromCompiledEnv.BuildApplicationFromFunctionRecord(
                    functionRecord,
                    arguments: [],
                    parseCache);

            encodedBodyLookup[encodedBody] = descriptor;

            for (var depthK = 2; depthK <= functionRecord.ParameterCount; ++depthK)
            {
                var template =
                    CodeAnalysis.TryComputeConsolidatedFormTemplate(
                        wrapperValue,
                        depthK,
                        parseCache);

                if (template is null)
                {
                    continue;
                }

                consolidatedTemplates.Add(
                    new StaticProgramParser.ConsolidatedFormTemplateEntry<IdentifierT>(
                        Template: template,
                        Identify:
                        new StaticProgramParser.IdentifyResponse<IdentifierT>(
                            Ident: descriptor.Identifier,
                            ContinueParse: descriptor.ContinueParse,
                            OriginalFunctionValue:
                            descriptor.ContinueParse ? wrapperValue : null)));
            }
        }

        StaticProgramParser.IdentifyResponse<IdentifierT>? IdentifyOverride(PineValue pineValue)
        {
            if (namedValueLookup.TryGetValue(pineValue, out var descriptor))
            {
                return
                    new StaticProgramParser.IdentifyResponse<IdentifierT>(
                        Ident: descriptor.Identifier,
                        ContinueParse: descriptor.ContinueParse,
                        OriginalFunctionValue:
                        descriptor.ContinueParse ? descriptor.EffectiveWrapperValue : null);
            }

            return null;
        }

        StaticProgramParser.IdentifyResponse<IdentifierT> IdentifyRequired(
            IEnumerable<IdentifierT> stack,
            PineValue pineValue)
        {
            return
                IdentifyOverride(pineValue)
                ?? previousConfig.IdentifyInstanceRequired(stack, pineValue);
        }

        StaticProgramParser.IdentifyResponse<IdentifierT>? IdentifyOptional(
            IEnumerable<IdentifierT> stack,
            PineValue pineValue)
        {
            return
                IdentifyOverride(pineValue)
                ?? previousConfig.IdentifyInstanceOptional(stack, pineValue);
        }

        StaticProgramParser.IdentifyResponse<IdentifierT>? IdentifyEncodedBody(
            IEnumerable<IdentifierT> stack,
            PineValue pineValue)
        {
            if (encodedBodyLookup.TryGetValue(pineValue, out var descriptor))
            {
                return
                    new StaticProgramParser.IdentifyResponse<IdentifierT>(
                        Ident: descriptor.Identifier,
                        ContinueParse: descriptor.ContinueParse,
                        OriginalFunctionValue:
                        descriptor.ContinueParse ? descriptor.EffectiveWrapperValue : null);
            }

            return previousConfig.IdentifyEncodedBodyOptional?.Invoke(stack, pineValue);
        }

        return
            new StaticProgramParserConfig<IdentifierT>(
                IdentifyInstanceRequired: IdentifyRequired,
                IdentifyInstanceOptional: IdentifyOptional,
                IdentifyCrash: previousConfig.IdentifyCrash,
                IdentifyEncodedBodyOptional: IdentifyEncodedBody,
                ConsolidatedFormTemplates:
                consolidatedTemplates.Count is 0 ? null : consolidatedTemplates);
    }
}
