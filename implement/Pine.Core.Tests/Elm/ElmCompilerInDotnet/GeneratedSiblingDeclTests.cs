using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Xunit;

using SyntaxTypes = Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet;

/// <summary>
/// Unit tests for the shared metadata model
/// <see cref="GeneratedSiblingDecl"/> + <see cref="SiblingParameterOrigin"/>
/// + <see cref="SiblingResultTransform"/>. Verifies that:
/// <list type="number">
///   <item><description>
///     <see cref="WrapperReturnStripping.WrapperStripPlan.ToGeneratedSiblingDecl"/>
///     produces the documented identity / wrap-with-constructor shape.
///   </description></item>
///   <item><description>
///     The model's structural equality treats parameter-origin lists
///     element-wise (so two plans that differ only in one origin are
///     unequal), and matching plans hash identically.
///   </description></item>
/// </list>
/// </summary>
public class GeneratedSiblingDeclTests
{
    private static readonly System.Collections.Generic.IReadOnlyList<string> s_module =
        ["Test"];

    private static readonly DeclQualifiedName s_origName =
        DeclQualifiedName.Create(s_module, "f");

    private static readonly DeclQualifiedName s_siblingName =
        DeclQualifiedName.Create(s_module, "f__stripped");

    private static readonly DeclQualifiedName s_ctorName =
        DeclQualifiedName.Create(s_module, "Wrap");

    [Fact]
    public void WrapperStripPlan_ToGeneratedSiblingDecl_arity_zero_produces_empty_origin_list()
    {
        var plan =
            new WrapperReturnStripping.WrapperStripPlan(
                OriginalDeclName: s_origName,
                StrippedDeclName: s_siblingName,
                ConstructorName: s_ctorName,
                OriginalArity: 0,
                StrippedBody: new SyntaxTypes.Expression.UnitExpr());

        var sibling = plan.ToGeneratedSiblingDecl();

        sibling.OriginalDeclName.Should().Be(s_origName);
        sibling.SiblingDeclName.Should().Be(s_siblingName);
        sibling.OriginalArity.Should().Be(0);
        sibling.SiblingArity.Should().Be(0);
        sibling.ParameterOrigins.Should().BeEmpty();

        sibling.ResultTransform
            .Should().BeOfType<SiblingResultTransform.WrapWithConstructor>()
            .Which.Constructor.Should().Be(s_ctorName);
    }

    [Fact]
    public void WrapperStripPlan_ToGeneratedSiblingDecl_arity_three_produces_identity_origins_in_order()
    {
        var plan =
            new WrapperReturnStripping.WrapperStripPlan(
                OriginalDeclName: s_origName,
                StrippedDeclName: s_siblingName,
                ConstructorName: s_ctorName,
                OriginalArity: 3,
                StrippedBody: new SyntaxTypes.Expression.UnitExpr());

        var sibling = plan.ToGeneratedSiblingDecl();

        sibling.OriginalArity.Should().Be(3);
        sibling.SiblingArity.Should().Be(3);
        sibling.ParameterOrigins.Should().HaveCount(3);

        for (var i = 0; i < 3; i++)
        {
            var origin =
                sibling.ParameterOrigins[i]
                .Should().BeOfType<SiblingParameterOrigin.Identity>()
                .Subject;

            origin.SiblingIndex.Should().Be(i);
            origin.OriginalIndex.Should().Be(i);
        }

        sibling.ResultTransform
            .Should().BeOfType<SiblingResultTransform.WrapWithConstructor>()
            .Which.Constructor.Should().Be(s_ctorName);
    }

    [Fact]
    public void GeneratedSiblingDecl_equality_is_structural_over_parameter_origins()
    {
        var identityOriginsA =
            new SiblingParameterOrigin[]
            {
                new SiblingParameterOrigin.Identity(SiblingIndex: 0, OriginalIndex: 0),
                new SiblingParameterOrigin.Identity(SiblingIndex: 1, OriginalIndex: 1),
            };

        var identityOriginsB =
            new SiblingParameterOrigin[]
            {
                new SiblingParameterOrigin.Identity(SiblingIndex: 0, OriginalIndex: 0),
                new SiblingParameterOrigin.Identity(SiblingIndex: 1, OriginalIndex: 1),
            };

        var differentOrigins =
            new SiblingParameterOrigin[]
            {
                new SiblingParameterOrigin.Identity(SiblingIndex: 0, OriginalIndex: 0),
                new SiblingParameterOrigin.InnerOfWrapper(
                    SiblingIndex: 1,
                    OriginalIndex: 1,
                    WrapperConstructor: s_ctorName),
            };

        var resultTransform = new SiblingResultTransform.WrapWithConstructor(s_ctorName);

        var a =
            new GeneratedSiblingDecl(
                s_origName,
                s_siblingName,
                OriginalArity: 2,
                identityOriginsA,
                resultTransform);

        var bSameContent =
            new GeneratedSiblingDecl(
                s_origName,
                s_siblingName,
                OriginalArity: 2,
                identityOriginsB,
                resultTransform);

        var cDifferentOrigins =
            new GeneratedSiblingDecl(
                s_origName,
                s_siblingName,
                OriginalArity: 2,
                differentOrigins,
                resultTransform);

        a.Should().Be(bSameContent);
        a.GetHashCode().Should().Be(bSameContent.GetHashCode());
        a.Should().NotBe(cDifferentOrigins);
    }

    [Fact]
    public void GeneratedSiblingDecl_equality_distinguishes_result_transforms()
    {
        var origins = System.Array.Empty<SiblingParameterOrigin>();

        var withWrap =
            new GeneratedSiblingDecl(
                s_origName,
                s_siblingName,
                OriginalArity: 0,
                origins,
                new SiblingResultTransform.WrapWithConstructor(s_ctorName));

        var withIdentity =
            new GeneratedSiblingDecl(
                s_origName,
                s_siblingName,
                OriginalArity: 0,
                origins,
                new SiblingResultTransform.Identity());

        withWrap.Should().NotBe(withIdentity);
    }

    [Fact]
    public void SiblingParameterOrigin_InnerOfWrapper_carries_constructor_name()
    {
        var origin =
            new SiblingParameterOrigin.InnerOfWrapper(
                SiblingIndex: 0,
                OriginalIndex: 0,
                WrapperConstructor: s_ctorName);

        origin.SiblingIndex.Should().Be(0);
        origin.OriginalIndex.Should().Be(0);
        origin.WrapperConstructor.Should().Be(s_ctorName);
    }
}
