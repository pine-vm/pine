using Pine.Core.CodeAnalysis;
using System;
using System.Collections.Immutable;

using SyntaxTypes = Pine.Core.Elm.ElmSyntax.ElmSyntaxAbstract;

namespace Pine.Core.Elm.ElmCompilerInDotnet;

/// <summary>
/// Application-normalization glue: rewrites every <see cref="SyntaxTypes.Expression.Application"/>
/// so that no Application has another Application as its head, via
/// <see cref="ElmSyntaxAbstractTransformations.FlattenAllNestedApplicationHeads"/>.
/// Extracted as a sibling partial-class file of <see cref="ElmSyntaxOptimization"/> so the
/// driver/specialization/inlining file is not also responsible for this small,
/// independently-callable rewrite.
/// </summary>
public partial class ElmSyntaxOptimization
{
    /// <summary>
    /// <see cref="OptimizedElmSyntaxDeclarations"/>-flavoured overload of
    /// <see cref="NormalizeApplicationsInDeclarationDictionary(ImmutableDictionary{DeclQualifiedName, SyntaxTypes.Declaration})"/>.
    /// </summary>
    public static OptimizedElmSyntaxDeclarations
        NormalizeApplicationsInDeclarationDictionary(
        OptimizedElmSyntaxDeclarations declarations) =>
        OptimizedElmSyntaxDeclarations.FromFlatDictionary(
            NormalizeApplicationsInDeclarationDictionary(
                declarations.RenderAsFlatDictionary()));

    /// <summary>
    /// Returns a copy of <paramref name="declarations"/> where every
    /// expression body has been rewritten with
    /// <see cref="ElmSyntaxAbstractTransformations.FlattenAllNestedApplicationHeads"/>
    /// so that no <see cref="SyntaxTypes.Expression.Application"/> in the
    /// output has another <see cref="SyntaxTypes.Expression.Application"/>
    /// as its head. See the helper's documentation for why this matters
    /// for code-emission performance.
    /// </summary>
    public static ImmutableDictionary<DeclQualifiedName, SyntaxTypes.Declaration>
        NormalizeApplicationsInDeclarationDictionary(
        ImmutableDictionary<DeclQualifiedName, SyntaxTypes.Declaration> declarations)
    {
        var builder = ImmutableDictionary.CreateBuilder<DeclQualifiedName, SyntaxTypes.Declaration>();

        foreach (var (key, decl) in declarations)
        {
            builder[key] = NormalizeApplicationsInDeclaration(decl);
        }

        return builder.ToImmutable();
    }

    private static SyntaxTypes.Declaration NormalizeApplicationsInDeclaration(
        SyntaxTypes.Declaration decl)
    {
        return decl switch
        {
            SyntaxTypes.Declaration.FunctionDeclaration funcDecl =>
            new SyntaxTypes.Declaration.FunctionDeclaration(
                NormalizeApplicationsInFunction(funcDecl.Function)),

            // Type/alias/port/infix declarations contain no expression bodies
            // that can host nested Application form.
            SyntaxTypes.Declaration.ChoiceTypeDeclaration or
            SyntaxTypes.Declaration.AliasDeclaration or
            SyntaxTypes.Declaration.PortDeclaration or
            SyntaxTypes.Declaration.InfixDeclaration =>
            decl,

            _ =>
            throw new NotImplementedException(
                "NormalizeApplicationsInDeclaration does not handle declaration variant: " +
                decl.GetType().Name)
        };
    }

    private static SyntaxTypes.FunctionStruct NormalizeApplicationsInFunction(
        SyntaxTypes.FunctionStruct function)
    {
        var rewrittenBody =
            ElmSyntaxAbstractTransformations.FlattenAllNestedApplicationHeads(
                function.Declaration.Expression);

        return
            function with
            {
                Declaration =
                function.Declaration with
                {
                    Expression = rewrittenBody
                }
            };
    }
}
