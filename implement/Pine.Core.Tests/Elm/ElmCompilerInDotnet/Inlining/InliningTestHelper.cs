using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Elm.ElmSyntax;
using Pine.Core.Elm.ElmSyntax.SyntaxModel;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.Inlining;

using Inlining = Core.Elm.ElmCompilerInDotnet.Inlining;
using Stil4mElmSyntax7 = Core.Elm.ElmSyntax.Stil4mElmSyntax7;

public class InliningTestHelper
{
    private static readonly ImmutableDictionary<QualifiedNameRef, QualifiedNameRef> s_renderingNameMap =
        ImmutableDictionary<QualifiedNameRef, QualifiedNameRef>.Empty
        .SetItem(QualifiedNameRef.FromFullName("Basics.Int"), QualifiedNameRef.FromFullName("Int"))
        .SetItem(QualifiedNameRef.FromFullName("Basics.Bool"), QualifiedNameRef.FromFullName("Bool"))
        .SetItem(QualifiedNameRef.FromFullName("String.String"), QualifiedNameRef.FromFullName("String"))
        .SetItem(QualifiedNameRef.FromFullName("Char.Char"), QualifiedNameRef.FromFullName("Char"));

    public static string RenderModuleForSnapshotTests(Stil4mElmSyntax7.File module)
    {
        var flatDict =
            SnapshotTestFormat.ModulesToFlatDeclarationDictionary([module]);

        var rendered =
            SnapshotTestFormat.RenderQualifiedDeclarations(
                flatDict,
                SnapshotTestFormat.DeclarationSortOrder.NameAsc,
                nameMap: s_renderingNameMap);

        return rendered;
    }

    public static string CanonicalizeRenderedSnapshotText(string renderedText)
    {
        var parsedFile =
            ElmSyntaxParser.ParseModuleText(renderedText)
            .Extract(err => throw new System.Exception("Failed parsing rendered snapshot text: " + err));

        var normalized =
            NormalizeForRoundTripComparison(
                Stil4mElmSyntax7.FromFullSyntaxModel.Convert(parsedFile));

        return RenderNormalizedForRoundTripComparison(normalized);
    }

    /// <summary>
    /// Verifies that the rendered Elm text has correct parenthesization by checking that
    /// parsing the rendered text produces an AST that, when re-rendered and re-parsed,
    /// gives the same AST structure. All locations/ranges are normalized to zero for
    /// value-based equality comparison.
    /// </summary>
    public static void VerifyRenderRoundTrip(string renderedText)
    {
        var parsedFile =
            ElmSyntaxParser.ParseModuleText(renderedText)
            .Extract(
                err => throw new System.Exception(
                    "Round-trip verification failed: rendered Elm text could not be parsed: " + err));

        // Convert to Stil4m model (which normalizes locations) and back for comparison
        var normalizedVia7 = Stil4mElmSyntax7.FromFullSyntaxModel.Convert(parsedFile);
        var backToFull = Stil4mElmSyntax7.ToFullSyntaxModel.Convert(normalizedVia7);
        var reformatted = SnapshotTestFormat.Format(backToFull);
        var rerendered = Rendering.ToString(reformatted);

        var reparsedFile =
            ElmSyntaxParser.ParseModuleText(rerendered)
            .Extract(
                err => throw new System.Exception(
                    "Round-trip verification failed: re-rendered Elm text could not be parsed: " + err));

        // Normalize both ASTs by converting through Stil4m model (which drops locations)
        var normalizedFirst =
            NormalizeForRoundTripComparison(
                Stil4mElmSyntax7.FromFullSyntaxModel.Convert(parsedFile));

        var normalizedSecond =
            NormalizeForRoundTripComparison(
                Stil4mElmSyntax7.FromFullSyntaxModel.Convert(reparsedFile));

        var canonicalFirst = RenderNormalizedForRoundTripComparison(normalizedFirst);
        var canonicalSecond = RenderNormalizedForRoundTripComparison(normalizedSecond);

        if (canonicalFirst != canonicalSecond)
        {
            throw new System.Exception(
                "Round-trip verification failed: rendered Elm text produces a different AST when re-parsed.\n" +
                "This typically indicates missing parentheses in the rendered output.\n" +
                $"Rendered text:\n{renderedText}\n\n" +
                $"Re-rendered text:\n{rerendered}");
        }
    }

    private static string RenderNormalizedForRoundTripComparison(Stil4mElmSyntax7.File file) =>
        Rendering.ToString(
            SnapshotTestFormat.Format(
                Stil4mElmSyntax7.ToFullSyntaxModel.Convert(file)));

    private static Stil4mElmSyntax7.File NormalizeForRoundTripComparison(Stil4mElmSyntax7.File file) =>
        file with
        {
            Declarations =
            [.. file.Declarations.Select(NormalizeDeclarationNode)]
        };

    private static Node<Stil4mElmSyntax7.Declaration> NormalizeDeclarationNode(
        Node<Stil4mElmSyntax7.Declaration> declarationNode) =>
        declarationNode with
        {
            Value =
            declarationNode.Value switch
            {
                Stil4mElmSyntax7.Declaration.FunctionDeclaration functionDeclaration =>
                new Stil4mElmSyntax7.Declaration.FunctionDeclaration(
                    NormalizeFunction(functionDeclaration.Function)),

                _ =>
                declarationNode.Value
            }
        };

    private static Stil4mElmSyntax7.FunctionStruct NormalizeFunction(Stil4mElmSyntax7.FunctionStruct function) =>
        function with
        {
            Declaration =
            function.Declaration with
            {
                Value =
                function.Declaration.Value with
                {
                    Arguments =
                    [.. function.Declaration.Value.Arguments.Select(NormalizePatternNode)],
                    Expression =
                    NormalizeExpressionNode(function.Declaration.Value.Expression)
                }
            }
        };

    private static Node<Stil4mElmSyntax7.Pattern> NormalizePatternNode(Node<Stil4mElmSyntax7.Pattern> patternNode)
    {
        var normalizedPattern = NormalizePattern(patternNode.Value);

        return normalizedPattern switch
        {
            Stil4mElmSyntax7.Pattern.ParenthesizedPattern parenthesized =>
            NormalizePatternNode(parenthesized.Pattern) with { Range = patternNode.Range },

            _ =>
            patternNode with { Value = normalizedPattern }
        };
    }

    private static Stil4mElmSyntax7.Pattern NormalizePattern(Stil4mElmSyntax7.Pattern pattern) =>
        pattern switch
        {
            Stil4mElmSyntax7.Pattern.ParenthesizedPattern parenthesized =>
            NormalizePattern(NormalizePatternNode(parenthesized.Pattern).Value),

            Stil4mElmSyntax7.Pattern.TuplePattern tuplePattern =>
            tuplePattern with { Elements = [.. tuplePattern.Elements.Select(NormalizePatternNode)] },

            Stil4mElmSyntax7.Pattern.UnConsPattern unConsPattern =>
            unConsPattern with
            {
                Head = NormalizePatternNode(unConsPattern.Head),
                Tail = NormalizePatternNode(unConsPattern.Tail)
            },

            Stil4mElmSyntax7.Pattern.ListPattern listPattern =>
            listPattern with { Elements = [.. listPattern.Elements.Select(NormalizePatternNode)] },

            Stil4mElmSyntax7.Pattern.NamedPattern namedPattern =>
            namedPattern with { Arguments = [.. namedPattern.Arguments.Select(NormalizePatternNode)] },

            Stil4mElmSyntax7.Pattern.AsPattern asPattern =>
            asPattern with { Pattern = NormalizePatternNode(asPattern.Pattern) },

            _ =>
            pattern
        };

    private static Node<Stil4mElmSyntax7.Expression> NormalizeExpressionNode(
        Node<Stil4mElmSyntax7.Expression> expressionNode)
    {
        var normalizedExpression = NormalizeExpression(expressionNode.Value);

        return normalizedExpression switch
        {
            Stil4mElmSyntax7.Expression.ParenthesizedExpression parenthesized =>
            NormalizeExpressionNode(parenthesized.Expression) with { Range = expressionNode.Range },

            _ =>
            expressionNode with { Value = normalizedExpression }
        };
    }

    private static Stil4mElmSyntax7.Expression NormalizeExpression(Stil4mElmSyntax7.Expression expression) =>
        expression switch
        {
            Stil4mElmSyntax7.Expression.ParenthesizedExpression parenthesized =>
            NormalizeExpression(NormalizeExpressionNode(parenthesized.Expression).Value),

            Stil4mElmSyntax7.Expression.Negation negation =>
            negation with { Expression = NormalizeExpressionNode(negation.Expression) },

            Stil4mElmSyntax7.Expression.ListExpr listExpr =>
            listExpr with { Elements = [.. listExpr.Elements.Select(NormalizeExpressionNode)] },

            Stil4mElmSyntax7.Expression.IfBlock ifBlock =>
            ifBlock with
            {
                Condition = NormalizeExpressionNode(ifBlock.Condition),
                ThenBlock = NormalizeExpressionNode(ifBlock.ThenBlock),
                ElseBlock = NormalizeExpressionNode(ifBlock.ElseBlock)
            },

            Stil4mElmSyntax7.Expression.Application application =>
            application with { Arguments = [.. application.Arguments.Select(NormalizeExpressionNode)] },

            Stil4mElmSyntax7.Expression.OperatorApplication operatorApplication =>
            operatorApplication with
            {
                Left = NormalizeExpressionNode(operatorApplication.Left),
                Right = NormalizeExpressionNode(operatorApplication.Right)
            },

            Stil4mElmSyntax7.Expression.TupledExpression tupledExpression =>
            tupledExpression with { Elements = [.. tupledExpression.Elements.Select(NormalizeExpressionNode)] },

            Stil4mElmSyntax7.Expression.LambdaExpression lambdaExpression =>
            lambdaExpression with
            {
                Lambda =
                lambdaExpression.Lambda with
                {
                    Arguments = [.. lambdaExpression.Lambda.Arguments.Select(NormalizePatternNode)],
                    Expression = NormalizeExpressionNode(lambdaExpression.Lambda.Expression)
                }
            },

            Stil4mElmSyntax7.Expression.CaseExpression caseExpression =>
            caseExpression with
            {
                CaseBlock =
                caseExpression.CaseBlock with
                {
                    Expression = NormalizeExpressionNode(caseExpression.CaseBlock.Expression),
                    Cases =
                    [
                    .. caseExpression.CaseBlock.Cases.Select(
                        caseItem =>
                        caseItem with
                        {
                            Pattern = NormalizePatternNode(caseItem.Pattern),
                            Expression = NormalizeExpressionNode(caseItem.Expression)
                        })
                    ]
                }
            },

            Stil4mElmSyntax7.Expression.LetExpression letExpression =>
            letExpression with
            {
                Value =
                letExpression.Value with
                {
                    Declarations =
                    [
                    .. letExpression.Value.Declarations.Select(
                        declaration =>
                        declaration with
                        {
                            Value =
                            declaration.Value switch
                            {
                                Stil4mElmSyntax7.Expression.LetDeclaration.LetFunction letFunction =>
                                new Stil4mElmSyntax7.Expression.LetDeclaration.LetFunction(
                                    NormalizeFunction(letFunction.Function)),

                                Stil4mElmSyntax7.Expression.LetDeclaration.LetDestructuring letDestructuring =>
                                new Stil4mElmSyntax7.Expression.LetDeclaration.LetDestructuring(
                                    NormalizePatternNode(letDestructuring.Pattern),
                                    NormalizeExpressionNode(letDestructuring.Expression)),

                                _ =>
                                declaration.Value
                            }
                        })
                    ],
                    Expression = NormalizeExpressionNode(letExpression.Value.Expression)
                }
            },

            Stil4mElmSyntax7.Expression.RecordExpr recordExpression =>
            recordExpression with
            {
                Fields =
                [
                .. recordExpression.Fields.Select(
                    field =>
                    field with
                    {
                        Value = (field.Value.fieldName, NormalizeExpressionNode(field.Value.valueExpr))
                    })
                ]
            },

            Stil4mElmSyntax7.Expression.RecordAccess recordAccess =>
            recordAccess with { Record = NormalizeExpressionNode(recordAccess.Record) },

            Stil4mElmSyntax7.Expression.RecordUpdateExpression recordUpdate =>
            recordUpdate with
            {
                Fields =
                [
                .. recordUpdate.Fields.Select(
                    field =>
                    field with
                    {
                        Value = (field.Value.fieldName, NormalizeExpressionNode(field.Value.valueExpr))
                    })
                ]
            },

            _ =>
            expression
        };

    public static Stil4mElmSyntax7.File CanonicalizeAndInlineAndGetSingleModule(
        IReadOnlyList<string> elmModulesTexts,
        IReadOnlyList<string> moduleName,
        Inlining.Config config)
    {
        var parsedModules =
            elmModulesTexts
            .Select(
                text =>
                ElmSyntaxParser.ParseModuleText(text)
                .Extract(err => throw new System.Exception("Failed parsing: " + err)))
            .Select(Stil4mElmSyntax7.FromFullSyntaxModel.Convert)
            .ToList();

        var canonicalizeResult =
            Canonicalization.Canonicalize(parsedModules);

        var modulesDict =
            canonicalizeResult
            .Extract(err => throw new System.Exception("Failed canonicalization: " + err));

        var allCanonicalizedModules =
            modulesDict
            .ToDictionary(
                kvp => kvp.Key,
                kvp => kvp.Value
                .Extract(err => throw new System.Exception($"Module {string.Join(".", kvp.Key)} has errors: " + err)));

        var orderedModules =
            allCanonicalizedModules.Values.ToList();

        var flatDecls = ElmCompiler.FlattenModulesToDeclarationDictionary(orderedModules);

        var inlinedDecls =
            Inlining.Inline(flatDecls, config)
            .Extract(err => throw new System.Exception("Failed inlining: " + err));

        var inlinedModules = ElmCompiler.ReconstructModulesFromFlatDict(inlinedDecls, orderedModules);

        return
            inlinedModules
            .Single(
                m => Stil4mElmSyntax7.Module.GetModuleName(m.ModuleDefinition.Value).Value.SequenceEqual(moduleName));
    }

    public static Stil4mElmSyntax7.File CanonicalizeAndOptimizeAndGetSingleModule(
        IReadOnlyList<string> elmModulesTexts,
        IReadOnlyList<string> moduleName,
        Inlining.Config config)
    {
        var parsedModules =
            elmModulesTexts
            .Select(
                text =>
                ElmSyntaxParser.ParseModuleText(text)
                .Extract(err => throw new System.Exception("Failed parsing: " + err)))
            .Select(Stil4mElmSyntax7.FromFullSyntaxModel.Convert)
            .ToList();

        var canonicalizeResult =
            Canonicalization.Canonicalize(parsedModules);

        var modulesDict =
            canonicalizeResult
            .Extract(err => throw new System.Exception("Failed canonicalization: " + err));

        var canonicalizedModules =
            modulesDict
            .ToDictionary(
                kvp => kvp.Key,
                kvp => kvp.Value
                .Extract(err => throw new System.Exception($"Module {string.Join(".", kvp.Key)} has errors: " + err)));

        var canonicalizedOrderedModules =
            parsedModules
            .Select(
                module =>
                canonicalizedModules[Stil4mElmSyntax7.Module.GetModuleName(module.ModuleDefinition.Value).Value])
            .ToList();

        var flatDecls = ElmCompiler.FlattenModulesToDeclarationDictionary(canonicalizedOrderedModules);

        var specializedDecls =
            ElmSyntaxSpecialization.Apply(flatDecls, config)
            .Extract(err => throw new System.Exception("Failed specialization: " + err));

        var inlinedDecls =
            ElmSyntaxInlining.Apply(specializedDecls, config)
            .Extract(err => throw new System.Exception("Failed inlining stage: " + err));

        var liftedDecls = LambdaLifting.LiftLambdas(inlinedDecls);

        var loweredDecls =
            BuiltinOperatorLowering.Apply(liftedDecls)
            .Extract(err => throw new System.Exception("Failed builtin operator lowering: " + err));

        var loweredModules = ElmCompiler.ReconstructModulesFromFlatDict(loweredDecls, canonicalizedOrderedModules);

        return
            loweredModules
            .Single(
                m => Stil4mElmSyntax7.Module.GetModuleName(m.ModuleDefinition.Value).Value.SequenceEqual(moduleName));
    }

    public static Stil4mElmSyntax7.File CanonicalizeAndInlineAndLowerOperatorsAndGetSingleModule(
        IReadOnlyList<string> elmModulesTexts,
        IReadOnlyList<string> moduleName,
        Inlining.Config config)
    {
        var parsedModules =
            elmModulesTexts
            .Select(
                text =>
                ElmSyntaxParser.ParseModuleText(text)
                .Extract(err => throw new System.Exception("Failed parsing: " + err)))
            .Select(Stil4mElmSyntax7.FromFullSyntaxModel.Convert)
            .ToList();

        var canonicalizeResult =
            Canonicalization.Canonicalize(parsedModules);

        var modulesDict =
            canonicalizeResult
            .Extract(err => throw new System.Exception("Failed canonicalization: " + err));

        var canonicalizedModules =
            modulesDict
            .ToDictionary(
                kvp => kvp.Key,
                kvp => kvp.Value
                .Extract(err => throw new System.Exception($"Module {string.Join(".", kvp.Key)} has errors: " + err)));

        var orderedCanonicalizedModules =
            parsedModules
            .Select(
                module =>
                canonicalizedModules[Stil4mElmSyntax7.Module.GetModuleName(module.ModuleDefinition.Value).Value])
            .ToList();

        var flatDecls = ElmCompiler.FlattenModulesToDeclarationDictionary(orderedCanonicalizedModules);

        var inlinedDecls =
            Inlining.Inline(flatDecls, config)
            .Extract(err => throw new System.Exception("Failed inlining: " + err));

        var loweredDecls =
            BuiltinOperatorLowering.Apply(inlinedDecls)
            .Extract(err => throw new System.Exception("Failed builtin operator lowering: " + err));

        var loweredModules = ElmCompiler.ReconstructModulesFromFlatDict(loweredDecls, orderedCanonicalizedModules);

        return
            loweredModules
            .Single(
                m => Stil4mElmSyntax7.Module.GetModuleName(m.ModuleDefinition.Value).Value.SequenceEqual(moduleName));
    }
}
