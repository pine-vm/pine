using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Files;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet;

/// <summary>
/// Combined correctness + optimization-quality test helper. For a given set of
/// Elm source modules and a list of expected (rootExpression, expectedRendered)
/// usages, this helper:
/// <list type="number">
///   <item>compiles the modules through the full Elm compiler pipeline (the
///   same way <see cref="CompareInterpreterWithIntermediateVM"/> does),</item>
///   <item>evaluates every usage on both the intermediate VM and the
///   <see cref="ElmSyntax.ElmSyntaxInterpreter"/>, requiring the two paths to
///   agree on the result and that result, when rendered via
///   <see cref="ElmValue.RenderAsElmExpression(ElmValue)"/>, to match the
///   supplied expected expression string, and</item>
///   <item>runs <see cref="OptimizationOpportunityFinder"/> over the
///   post-optimization declarations (after pruning to declarations reachable
///   from the entry points) and asserts that no remaining optimization
///   opportunities are found.</item>
/// </list>
/// </summary>
public static class OptimizedRuntimeBehaviorVerification
{
    /// <summary>
    /// Verifies that compiling <paramref name="elmModuleTexts"/> produces a
    /// program that both yields the expected results for every entry in
    /// <paramref name="usages"/> and contains no remaining
    /// <see cref="OptimizationOpportunityFinder">optimization
    /// opportunities</see> reachable from <paramref name="entryPoints"/>.
    ///
    /// <para>
    /// Each <paramref name="usages"/> key is a root expression text in the
    /// same form accepted by
    /// <see cref="CompareInterpreterWithIntermediateVM.Eval(string)"/>
    /// (e.g. <c>"foo 1 2"</c>); the corresponding value is the expected
    /// rendered Elm expression string (the same shape produced by
    /// <see cref="ElmValue.RenderAsElmExpression(ElmValue)"/>).
    /// </para>
    ///
    /// <para>
    /// Per-category whitelists (<paramref name="ignoreRecordOperation"/>,
    /// <paramref name="ignoreBasicsArithmetic"/>,
    /// <paramref name="ignoreBasicsCompare"/>,
    /// <paramref name="ignoreBasicsEq"/>,
    /// <paramref name="ignoreBasicsAppend"/>) are forwarded verbatim to
    /// <see cref="OptimizationOpportunityFinder.FindOptimizationOpportunities(
    /// IReadOnlyDictionary{DeclQualifiedName, Core.Elm.ElmSyntax.Stil4mElmSyntax7.Declaration},
    /// ImmutableHashSet{string}?, ImmutableHashSet{string}?, ImmutableHashSet{string}?,
    /// ImmutableHashSet{string}?, ImmutableHashSet{string}?)"/>.
    /// </para>
    /// </summary>
    public static void VerifyOptimizedRuntimeBehavior(
        IReadOnlyList<string> elmModuleTexts,
        IReadOnlyList<DeclQualifiedName> entryPoints,
        IReadOnlyList<KeyValuePair<string, string>> usages,
        int maxOptimizationRounds = 2)
    {
        var framework =
            CompareInterpreterWithIntermediateVM.Prepare(
                elmModuleTexts: elmModuleTexts,
                entryPoints: entryPoints,
                maxOptimizationRounds: maxOptimizationRounds);

        VerifyWithFramework(
            framework,
            entryPoints,
            usages);
    }

    /// <summary>
    /// Overload that accepts a full <see cref="FileTree"/> (e.g. the bundled
    /// Elm compiler sources merged with a hand-written test module) and an
    /// explicit list of root file paths. Use this when the expression under
    /// test depends on libraries that are not part of
    /// <see cref="ElmCompilerTests.TestCase.DefaultAppWithoutPackages(IReadOnlyList{string})"/>.
    /// </summary>
    public static void VerifyOptimizedRuntimeBehavior(
        FileTree appCodeTree,
        IReadOnlyList<IReadOnlyList<string>> rootFilePaths,
        IReadOnlyList<DeclQualifiedName> entryPoints,
        IReadOnlyList<KeyValuePair<string, string>> usages,
        int maxOptimizationRounds = 2)
    {
        var framework =
            CompareInterpreterWithIntermediateVM.Prepare(
                appCodeTree: appCodeTree,
                rootFilePaths: rootFilePaths,
                entryPoints: entryPoints,
                maxOptimizationRounds: maxOptimizationRounds);

        VerifyWithFramework(
            framework,
            entryPoints,
            usages);
    }

    private static void VerifyWithFramework(
        CompareInterpreterWithIntermediateVM framework,
        IReadOnlyList<DeclQualifiedName> entryPoints,
        IReadOnlyList<KeyValuePair<string, string>> usages)
    {
        // ------------------ Correctness ------------------

        for (var i = 0; i < usages.Count; i++)
        {
            var (rootExpressionText, expectedRendered) = (usages[i].Key, usages[i].Value);

            var report = framework.Eval(rootExpressionText);

            var actualRendered =
                ElmValue.RenderAsElmExpression(report.Value).expressionString;

            actualRendered.Should().Be(
                expectedRendered,
                because:
                "VerifyOptimizedRuntimeBehavior: usage[" + i + "] '"
                + rootExpressionText + "' must produce the expected value.");
        }

        // ------------------ Optimization quality ------------------

        var flatDeclarations =
            ElmCompiler.FlattenModulesToDeclarationDictionary(framework.PostOptimizationModules);

        var rootSet = new HashSet<DeclQualifiedName>(entryPoints);

        var prunedDeclarations =
            DeclarationDictionaryPruning.PruneToReachable(flatDeclarations, rootSet);

        var opportunities =
            OptimizationOpportunityFinder.FindOptimizationOpportunities(
                prunedDeclarations);

        var rendered = OptimizationOpportunityFinder.RenderOpportunities(opportunities);

        rendered.Should().Be(
            string.Empty,
            because:
            "VerifyOptimizedRuntimeBehavior: after compilation the program must contain no "
            + "remaining optimization opportunities reachable from the entry points "
            + string.Join(", ", entryPoints.Select(e => e.FullName)) + ".");
    }
}
