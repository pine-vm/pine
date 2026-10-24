using Pine.Core.CodeAnalysis;
using System;
using System.Collections.Immutable;

using AbstractSyntax = Pine.Core.Elm.Syntax.Abstract;

namespace Pine.Core.Elm.ElmCompilerInDotnet;

/// <summary>
/// Application-normalization glue: rewrites every <see cref="AbstractSyntax.Expression.Application"/>
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
    /// <see cref="NormalizeApplicationsInDeclarationDictionary(ImmutableDictionary{DeclQualifiedName, AbstractSyntax.Declaration})"/>.
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
    /// so that no <see cref="AbstractSyntax.Expression.Application"/> in the
    /// output has another <see cref="AbstractSyntax.Expression.Application"/>
    /// as its head. See the helper's documentation for why this matters
    /// for code-emission performance.
    /// </summary>
    public static ImmutableDictionary<DeclQualifiedName, AbstractSyntax.Declaration>
        NormalizeApplicationsInDeclarationDictionary(
        ImmutableDictionary<DeclQualifiedName, AbstractSyntax.Declaration> declarations)
    {
        var builder = ImmutableDictionary.CreateBuilder<DeclQualifiedName, AbstractSyntax.Declaration>();

        foreach (var (key, decl) in declarations)
        {
            builder[key] = NormalizeApplicationsInDeclaration(decl);
        }

        return builder.ToImmutable();
    }

    private static AbstractSyntax.Declaration NormalizeApplicationsInDeclaration(
        AbstractSyntax.Declaration decl)
    {
        return decl switch
        {
            AbstractSyntax.Declaration.FunctionDeclaration funcDecl =>
            new AbstractSyntax.Declaration.FunctionDeclaration(
                NormalizeApplicationsInFunction(funcDecl.Function)),

            // Type/alias/port/infix declarations contain no expression bodies
            // that can host nested Application form.
            AbstractSyntax.Declaration.ChoiceTypeDeclaration or
            AbstractSyntax.Declaration.AliasDeclaration or
            AbstractSyntax.Declaration.PortDeclaration or
            AbstractSyntax.Declaration.InfixDeclaration =>
            decl,

            _ =>
            throw new NotImplementedException(
                "NormalizeApplicationsInDeclaration does not handle declaration variant: " +
                decl.GetType().Name)
        };
    }

    private static AbstractSyntax.FunctionStruct NormalizeApplicationsInFunction(
        AbstractSyntax.FunctionStruct function)
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
