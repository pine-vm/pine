using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Elm.ElmSyntax.SyntaxModel;
using System.Collections.Immutable;
using Xunit;

using SyntaxTypes = Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet;

/// <summary>
/// Unit tests for the optimization-stage model types in
/// <see cref="OptimizedElmSyntaxDeclarations"/> and
/// <see cref="OptimizedElmSyntaxFunctionDeclaration"/>.
///
/// <para>
/// The tests cover:
/// </para>
/// <list type="bullet">
///   <item><description>
///     Structural equality of <see cref="OptimizedElmSyntaxFunctionDeclaration"/> and
///     <see cref="OptimizedElmSyntaxDeclarations"/> over their immutable-dictionary fields
///     (default record equality would be reference-based for those).
///   </description></item>
///   <item><description>
///     <see cref="OptimizedElmSyntaxDeclarations.RenderAsFlatDictionary"/> behavior:
///     empty, only-other, only-functions-without-specializations,
///     functions-with-specializations (verifying the module qualifier is taken from
///     the original declaration and the simple name from the specialization's own
///     <see cref="SyntaxTypes.FunctionImplementation.Name"/>), and a mixed case.
///     Also verifies the duplicate-key contract.
///   </description></item>
/// </list>
/// </summary>
public class OptimizedElmSyntaxDeclarationsTests
{
    private static readonly Range s_emptyRange =
        new(new Location(1, 1), new Location(1, 1));

    private static Node<T> Node<T>(T value) => new(s_emptyRange, value);

    private static SyntaxTypes.Declaration.FunctionDeclaration MakeFunctionDeclaration(
        string declName,
        SyntaxTypes.Expression? body = null)
    {
        var implementation =
            new SyntaxTypes.FunctionImplementation(
                Name: Node(declName),
                Arguments: [],
                Expression: Node(body ?? new SyntaxTypes.Expression.UnitExpr()));

        var functionStruct =
            new SyntaxTypes.FunctionStruct(
                Documentation: null,
                Signature: null,
                Declaration: Node(implementation));

        return new SyntaxTypes.Declaration.FunctionDeclaration(functionStruct);
    }

    private static SyntaxTypes.Declaration.PortDeclaration MakePortDeclaration(string portName)
    {
        var signature =
            new SyntaxTypes.Signature(
                Name: Node(portName),
                TypeAnnotation: Node<SyntaxTypes.TypeAnnotation>(new SyntaxTypes.TypeAnnotation.Unit()));

        return new SyntaxTypes.Declaration.PortDeclaration(signature);
    }

    // ----------------------------------------------------------------------
    // OptimizedElmSyntaxFunctionDeclaration
    // ----------------------------------------------------------------------

    [Fact]
    public void OptimizedElmSyntaxFunctionDeclaration_equal_when_original_and_specializations_match()
    {
        var original = MakeFunctionDeclaration("f");

        var specKey = new FunctionSpecialization([]);

        var specA = MakeFunctionDeclaration("f__specialized__1");
        var specB = MakeFunctionDeclaration("f__specialized__1");

        var declA =
            new OptimizedElmSyntaxFunctionDeclaration(
                original,
                ImmutableDictionary<FunctionSpecialization, SyntaxTypes.Declaration.FunctionDeclaration>
                .Empty.Add(specKey, specA));

        var declB =
            new OptimizedElmSyntaxFunctionDeclaration(
                original,
                ImmutableDictionary<FunctionSpecialization, SyntaxTypes.Declaration.FunctionDeclaration>
                .Empty.Add(specKey, specB));

        declA.Should().Be(declB);
        declA.GetHashCode().Should().Be(declB.GetHashCode());
    }

    [Fact]
    public void OptimizedElmSyntaxFunctionDeclaration_not_equal_when_originals_differ()
    {
        var emptySpecializations =
            ImmutableDictionary<FunctionSpecialization, SyntaxTypes.Declaration.FunctionDeclaration>.Empty;

        var declA =
            new OptimizedElmSyntaxFunctionDeclaration(
                MakeFunctionDeclaration("f"),
                emptySpecializations);

        var declB =
            new OptimizedElmSyntaxFunctionDeclaration(
                MakeFunctionDeclaration("g"),
                emptySpecializations);

        declA.Should().NotBe(declB);
    }

    [Fact]
    public void OptimizedElmSyntaxFunctionDeclaration_not_equal_when_specialization_counts_differ()
    {
        var original = MakeFunctionDeclaration("f");

        var specKey = new FunctionSpecialization([]);

        var declWithout =
            new OptimizedElmSyntaxFunctionDeclaration(
                original,
                []);

        var declWith =
            new OptimizedElmSyntaxFunctionDeclaration(
                original,
                ImmutableDictionary<FunctionSpecialization, SyntaxTypes.Declaration.FunctionDeclaration>
                .Empty.Add(specKey, MakeFunctionDeclaration("f__specialized__1")));

        declWithout.Should().NotBe(declWith);
    }

    [Fact]
    public void OptimizedElmSyntaxFunctionDeclaration_not_equal_when_specialization_keys_differ()
    {
        var original = MakeFunctionDeclaration("f");
        var specBody = MakeFunctionDeclaration("f__specialized__1");

        var keyA = new FunctionSpecialization([]);

        var keyB =
            new FunctionSpecialization(
                ImmutableDictionary<int, ParameterSpecialization>.Empty.Add(0, new ParameterSpecialization.Eliminated()));

        var declA =
            new OptimizedElmSyntaxFunctionDeclaration(
                original,
                ImmutableDictionary<FunctionSpecialization, SyntaxTypes.Declaration.FunctionDeclaration>
                .Empty.Add(keyA, specBody));

        var declB =
            new OptimizedElmSyntaxFunctionDeclaration(
                original,
                ImmutableDictionary<FunctionSpecialization, SyntaxTypes.Declaration.FunctionDeclaration>
                .Empty.Add(keyB, specBody));

        declA.Should().NotBe(declB);
    }

    [Fact]
    public void OptimizedElmSyntaxFunctionDeclaration_not_equal_when_specialization_bodies_differ()
    {
        var original = MakeFunctionDeclaration("f");

        var specKey = new FunctionSpecialization([]);

        var declA =
            new OptimizedElmSyntaxFunctionDeclaration(
                original,
                ImmutableDictionary<FunctionSpecialization, SyntaxTypes.Declaration.FunctionDeclaration>
                .Empty.Add(specKey, MakeFunctionDeclaration("f__specialized__1")));

        var declB =
            new OptimizedElmSyntaxFunctionDeclaration(
                original,
                ImmutableDictionary<FunctionSpecialization, SyntaxTypes.Declaration.FunctionDeclaration>
                .Empty.Add(specKey, MakeFunctionDeclaration("f__different__name")));

        declA.Should().NotBe(declB);
    }

    [Fact]
    public void OptimizedElmSyntaxFunctionDeclaration_equality_is_independent_of_dictionary_insertion_order()
    {
        var original = MakeFunctionDeclaration("f");

        var key1 = new FunctionSpecialization([]);

        var key2 =
            new FunctionSpecialization(
                ImmutableDictionary<int, ParameterSpecialization>.Empty.Add(0, new ParameterSpecialization.Eliminated()));

        var spec1 = MakeFunctionDeclaration("f__specialized__1");
        var spec2 = MakeFunctionDeclaration("f__specialized__2");

        var declInOrderA =
            new OptimizedElmSyntaxFunctionDeclaration(
                original,
                ImmutableDictionary<FunctionSpecialization, SyntaxTypes.Declaration.FunctionDeclaration>
                .Empty.Add(key1, spec1).Add(key2, spec2));

        var declInOrderB =
            new OptimizedElmSyntaxFunctionDeclaration(
                original,
                ImmutableDictionary<FunctionSpecialization, SyntaxTypes.Declaration.FunctionDeclaration>
                .Empty.Add(key2, spec2).Add(key1, spec1));

        declInOrderA.Should().Be(declInOrderB);
    }

    // ----------------------------------------------------------------------
    // OptimizedElmSyntaxDeclarations equality
    // ----------------------------------------------------------------------

    [Fact]
    public void OptimizedElmSyntaxDeclarations_empty_instances_are_equal()
    {
        var a =
            new OptimizedElmSyntaxDeclarations(
                [],
                []);

        var b =
            new OptimizedElmSyntaxDeclarations(
                [],
                []);

        a.Should().Be(b);
        a.GetHashCode().Should().Be(b.GetHashCode());
    }

    [Fact]
    public void OptimizedElmSyntaxDeclarations_equal_when_function_and_other_dictionaries_match()
    {
        var qualifiedName = DeclQualifiedName.FromString("Mod.f");

        var optimizedFn =
            new OptimizedElmSyntaxFunctionDeclaration(
                MakeFunctionDeclaration("f"),
                []);

        var portQualifiedName = DeclQualifiedName.FromString("Mod.p");
        SyntaxTypes.Declaration portDecl = MakePortDeclaration("p");

        var a =
            new OptimizedElmSyntaxDeclarations(
                ImmutableDictionary<DeclQualifiedName, OptimizedElmSyntaxFunctionDeclaration>
                .Empty.Add(qualifiedName, optimizedFn),
                ImmutableDictionary<DeclQualifiedName, SyntaxTypes.Declaration>
                .Empty.Add(portQualifiedName, portDecl));

        var b =
            new OptimizedElmSyntaxDeclarations(
                ImmutableDictionary<DeclQualifiedName, OptimizedElmSyntaxFunctionDeclaration>
                .Empty.Add(qualifiedName, optimizedFn),
                ImmutableDictionary<DeclQualifiedName, SyntaxTypes.Declaration>
                .Empty.Add(portQualifiedName, portDecl));

        a.Should().Be(b);
        a.GetHashCode().Should().Be(b.GetHashCode());
    }

    [Fact]
    public void OptimizedElmSyntaxDeclarations_not_equal_when_function_dictionary_differs()
    {
        var optimizedF =
            new OptimizedElmSyntaxFunctionDeclaration(
                MakeFunctionDeclaration("f"),
                []);

        var optimizedG =
            new OptimizedElmSyntaxFunctionDeclaration(
                MakeFunctionDeclaration("g"),
                []);

        var emptyOther = ImmutableDictionary<DeclQualifiedName, SyntaxTypes.Declaration>.Empty;

        var a =
            new OptimizedElmSyntaxDeclarations(
                ImmutableDictionary<DeclQualifiedName, OptimizedElmSyntaxFunctionDeclaration>
                .Empty.Add(DeclQualifiedName.FromString("Mod.f"), optimizedF),
                emptyOther);

        var b =
            new OptimizedElmSyntaxDeclarations(
                ImmutableDictionary<DeclQualifiedName, OptimizedElmSyntaxFunctionDeclaration>
                .Empty.Add(DeclQualifiedName.FromString("Mod.f"), optimizedG),
                emptyOther);

        a.Should().NotBe(b);
    }

    [Fact]
    public void OptimizedElmSyntaxDeclarations_not_equal_when_other_dictionary_differs()
    {
        var emptyFunctions =
            ImmutableDictionary<DeclQualifiedName, OptimizedElmSyntaxFunctionDeclaration>.Empty;

        var a =
            new OptimizedElmSyntaxDeclarations(
                emptyFunctions,
                ImmutableDictionary<DeclQualifiedName, SyntaxTypes.Declaration>
                .Empty.Add(DeclQualifiedName.FromString("Mod.p"), MakePortDeclaration("p")));

        var b =
            new OptimizedElmSyntaxDeclarations(
                emptyFunctions,
                ImmutableDictionary<DeclQualifiedName, SyntaxTypes.Declaration>
                .Empty.Add(DeclQualifiedName.FromString("Mod.q"), MakePortDeclaration("q")));

        a.Should().NotBe(b);
    }

    [Fact]
    public void OptimizedElmSyntaxDeclarations_equality_is_independent_of_dictionary_insertion_order()
    {
        var fnX = DeclQualifiedName.FromString("Mod.x");
        var fnY = DeclQualifiedName.FromString("Mod.y");

        var optX =
            new OptimizedElmSyntaxFunctionDeclaration(
                MakeFunctionDeclaration("x"),
                []);

        var optY =
            new OptimizedElmSyntaxFunctionDeclaration(
                MakeFunctionDeclaration("y"),
                []);

        var a =
            new OptimizedElmSyntaxDeclarations(
                ImmutableDictionary<DeclQualifiedName, OptimizedElmSyntaxFunctionDeclaration>
                .Empty.Add(fnX, optX).Add(fnY, optY),
                []);

        var b =
            new OptimizedElmSyntaxDeclarations(
                ImmutableDictionary<DeclQualifiedName, OptimizedElmSyntaxFunctionDeclaration>
                .Empty.Add(fnY, optY).Add(fnX, optX),
                []);

        a.Should().Be(b);
    }

    [Fact]
    public void OptimizedElmSyntaxDeclarations_equals_null_returns_false()
    {
        var a =
            new OptimizedElmSyntaxDeclarations(
                [],
                []);

        a.Equals(null).Should().BeFalse();
    }

    // ----------------------------------------------------------------------
    // RenderAsFlatDictionary
    // ----------------------------------------------------------------------

    [Fact]
    public void RenderAsFlatDictionary_returns_empty_for_empty_input()
    {
        var model =
            new OptimizedElmSyntaxDeclarations(
                [],
                []);

        var flat = model.RenderAsFlatDictionary();

        flat.Should().BeEmpty();
    }

    [Fact]
    public void RenderAsFlatDictionary_passes_through_OtherDeclarations()
    {
        var portName = DeclQualifiedName.FromString("Mod.p");
        SyntaxTypes.Declaration portDecl = MakePortDeclaration("p");

        var model =
            new OptimizedElmSyntaxDeclarations(
                [],
                ImmutableDictionary<DeclQualifiedName, SyntaxTypes.Declaration>.Empty.Add(portName, portDecl));

        var flat = model.RenderAsFlatDictionary();

        flat.Should().HaveCount(1);
        flat[portName].Should().BeSameAs(portDecl);
    }

    [Fact]
    public void RenderAsFlatDictionary_includes_original_function_declarations()
    {
        var qualifiedName = DeclQualifiedName.FromString("Mod.Sub.f");
        var original = MakeFunctionDeclaration("f");

        var optimized =
            new OptimizedElmSyntaxFunctionDeclaration(
                original,
                []);

        var model =
            new OptimizedElmSyntaxDeclarations(
                ImmutableDictionary<DeclQualifiedName, OptimizedElmSyntaxFunctionDeclaration>
                .Empty.Add(qualifiedName, optimized),
                []);

        var flat = model.RenderAsFlatDictionary();

        flat.Should().HaveCount(1);
        flat[qualifiedName].Should().BeSameAs(original);
    }

    [Fact]
    public void RenderAsFlatDictionary_derives_module_qualifier_for_specializations_from_Original()
    {
        var moduleQualifier = DeclQualifiedName.FromString("Mod.Sub.f");

        var original = MakeFunctionDeclaration("f");

        var specKey = new FunctionSpecialization([]);

        var specialization = MakeFunctionDeclaration("f__specialized__1");

        var optimized =
            new OptimizedElmSyntaxFunctionDeclaration(
                original,
                ImmutableDictionary<FunctionSpecialization, SyntaxTypes.Declaration.FunctionDeclaration>
                .Empty.Add(specKey, specialization));

        var model =
            new OptimizedElmSyntaxDeclarations(
                ImmutableDictionary<DeclQualifiedName, OptimizedElmSyntaxFunctionDeclaration>
                .Empty.Add(moduleQualifier, optimized),
                []);

        var flat = model.RenderAsFlatDictionary();

        flat.Should().HaveCount(2);

        // Original is keyed by its existing qualified name.
        flat[moduleQualifier].Should().BeSameAs(original);

        // Specialization's qualified name uses the original's namespaces
        // ("Mod", "Sub") combined with the specialization's own decl name.
        var expectedSpecQualifiedName =
            new DeclQualifiedName(
                Namespaces: moduleQualifier.Namespaces,
                DeclName: "f__specialized__1");

        flat.ContainsKey(expectedSpecQualifiedName).Should().BeTrue();
        flat[expectedSpecQualifiedName].Should().BeSameAs(specialization);
    }

    [Fact]
    public void RenderAsFlatDictionary_emits_all_specializations_for_a_function()
    {
        var moduleQualifier = DeclQualifiedName.FromString("Mod.f");

        var original = MakeFunctionDeclaration("f");

        var key1 = new FunctionSpecialization([]);

        var key2 =
            new FunctionSpecialization(
                ImmutableDictionary<int, ParameterSpecialization>.Empty.Add(0, new ParameterSpecialization.Eliminated()));

        var spec1 = MakeFunctionDeclaration("f__specialized__1");
        var spec2 = MakeFunctionDeclaration("f__specialized__2");

        var optimized =
            new OptimizedElmSyntaxFunctionDeclaration(
                original,
                ImmutableDictionary<FunctionSpecialization, SyntaxTypes.Declaration.FunctionDeclaration>
                .Empty.Add(key1, spec1).Add(key2, spec2));

        var model =
            new OptimizedElmSyntaxDeclarations(
                ImmutableDictionary<DeclQualifiedName, OptimizedElmSyntaxFunctionDeclaration>
                .Empty.Add(moduleQualifier, optimized),
                []);

        var flat = model.RenderAsFlatDictionary();

        flat.Should().HaveCount(3);
        flat[moduleQualifier].Should().BeSameAs(original);
        flat[new DeclQualifiedName(moduleQualifier.Namespaces, "f__specialized__1")].Should().BeSameAs(spec1);
        flat[new DeclQualifiedName(moduleQualifier.Namespaces, "f__specialized__2")].Should().BeSameAs(spec2);
    }

    [Fact]
    public void RenderAsFlatDictionary_uses_specialization_FunctionImplementation_Name_for_simple_name()
    {
        // The simple name (DeclName) for each specialization MUST come from the specialization's
        // own FunctionImplementation.Name — NOT from the original or from the key.
        var moduleQualifier = DeclQualifiedName.FromString("Mod.f");

        var original = MakeFunctionDeclaration("f");

        var specKey = new FunctionSpecialization([]);

        // Intentionally pick a name unrelated to the original "f".
        var specialization = MakeFunctionDeclaration("some_other_internal_name__specialized__7");

        var optimized =
            new OptimizedElmSyntaxFunctionDeclaration(
                original,
                ImmutableDictionary<FunctionSpecialization, SyntaxTypes.Declaration.FunctionDeclaration>
                .Empty.Add(specKey, specialization));

        var model =
            new OptimizedElmSyntaxDeclarations(
                ImmutableDictionary<DeclQualifiedName, OptimizedElmSyntaxFunctionDeclaration>
                .Empty.Add(moduleQualifier, optimized),
                []);

        var flat = model.RenderAsFlatDictionary();

        var expectedSpecQualifiedName =
            new DeclQualifiedName(
                Namespaces: moduleQualifier.Namespaces,
                DeclName: "some_other_internal_name__specialized__7");

        flat.ContainsKey(expectedSpecQualifiedName).Should().BeTrue();
        flat[expectedSpecQualifiedName].Should().BeSameAs(specialization);
    }

    [Fact]
    public void RenderAsFlatDictionary_works_for_function_in_global_namespace()
    {
        // Function with no module qualifier — namespaces list is empty.
        var globalName = DeclQualifiedName.FromString("topLevel");
        globalName.Namespaces.Should().BeEmpty();

        var original = MakeFunctionDeclaration("topLevel");

        var specKey = new FunctionSpecialization([]);

        var specialization = MakeFunctionDeclaration("topLevel__specialized__1");

        var optimized =
            new OptimizedElmSyntaxFunctionDeclaration(
                original,
                ImmutableDictionary<FunctionSpecialization, SyntaxTypes.Declaration.FunctionDeclaration>
                .Empty.Add(specKey, specialization));

        var model =
            new OptimizedElmSyntaxDeclarations(
                ImmutableDictionary<DeclQualifiedName, OptimizedElmSyntaxFunctionDeclaration>
                .Empty.Add(globalName, optimized),
                []);

        var flat = model.RenderAsFlatDictionary();

        flat.Should().HaveCount(2);

        var expectedSpecQualifiedName =
            new DeclQualifiedName(
                Namespaces: [],
                DeclName: "topLevel__specialized__1");

        flat[globalName].Should().BeSameAs(original);
        flat[expectedSpecQualifiedName].Should().BeSameAs(specialization);
    }

    [Fact]
    public void RenderAsFlatDictionary_merges_function_and_other_declarations()
    {
        var fnName = DeclQualifiedName.FromString("Mod.f");
        var portName = DeclQualifiedName.FromString("Mod.p");

        var original = MakeFunctionDeclaration("f");

        var specKey = new FunctionSpecialization([]);

        var specialization = MakeFunctionDeclaration("f__specialized__1");

        var optimized =
            new OptimizedElmSyntaxFunctionDeclaration(
                original,
                ImmutableDictionary<FunctionSpecialization, SyntaxTypes.Declaration.FunctionDeclaration>
                .Empty.Add(specKey, specialization));

        SyntaxTypes.Declaration portDecl = MakePortDeclaration("p");

        var model =
            new OptimizedElmSyntaxDeclarations(
                ImmutableDictionary<DeclQualifiedName, OptimizedElmSyntaxFunctionDeclaration>
                .Empty.Add(fnName, optimized),
                ImmutableDictionary<DeclQualifiedName, SyntaxTypes.Declaration>
                .Empty.Add(portName, portDecl));

        var flat = model.RenderAsFlatDictionary();

        flat.Should().HaveCount(3);
        flat[fnName].Should().BeSameAs(original);
        flat[new DeclQualifiedName(fnName.Namespaces, "f__specialized__1")].Should().BeSameAs(specialization);
        flat[portName].Should().BeSameAs(portDecl);
    }

    [Fact]
    public void RenderAsFlatDictionary_throws_when_specialization_collides_with_other_declaration()
    {
        var fnName = DeclQualifiedName.FromString("Mod.f");

        var original = MakeFunctionDeclaration("f");

        var specKey = new FunctionSpecialization([]);

        var specialization = MakeFunctionDeclaration("collide");

        var optimized =
            new OptimizedElmSyntaxFunctionDeclaration(
                original,
                ImmutableDictionary<FunctionSpecialization, SyntaxTypes.Declaration.FunctionDeclaration>
                .Empty.Add(specKey, specialization));

        var collidingOther = new DeclQualifiedName(fnName.Namespaces, "collide");

        var model =
            new OptimizedElmSyntaxDeclarations(
                ImmutableDictionary<DeclQualifiedName, OptimizedElmSyntaxFunctionDeclaration>
                .Empty.Add(fnName, optimized),
                ImmutableDictionary<DeclQualifiedName, SyntaxTypes.Declaration>
                .Empty.Add(collidingOther, MakePortDeclaration("collide")));

        var act = () => model.RenderAsFlatDictionary();

        act.Should().Throw<System.ArgumentException>();
    }

    // ----------------------------------------------------------------------
    // FromFlatDictionary
    // ----------------------------------------------------------------------

    [Fact]
    public void FromFlatDictionary_returns_empty_for_empty_input()
    {
        var model =
            OptimizedElmSyntaxDeclarations.FromFlatDictionary(
                []);

        model.FunctionDeclarations.Should().BeEmpty();
        model.OtherDeclarations.Should().BeEmpty();
    }

    [Fact]
    public void FromFlatDictionary_buckets_function_declarations_with_empty_specializations()
    {
        var fnName = DeclQualifiedName.FromString("Mod.f");
        var fnDecl = MakeFunctionDeclaration("f");

        var flat =
            ImmutableDictionary<DeclQualifiedName, SyntaxTypes.Declaration>
            .Empty.Add(fnName, fnDecl);

        var model = OptimizedElmSyntaxDeclarations.FromFlatDictionary(flat);

        model.OtherDeclarations.Should().BeEmpty();
        model.FunctionDeclarations.Should().ContainKey(fnName);
        model.FunctionDeclarations[fnName].Original.Should().BeSameAs(fnDecl);
        model.FunctionDeclarations[fnName].Specializations.Should().BeEmpty();
    }

    [Fact]
    public void FromFlatDictionary_buckets_non_function_declarations_into_OtherDeclarations()
    {
        var portName = DeclQualifiedName.FromString("Mod.p");
        SyntaxTypes.Declaration portDecl = MakePortDeclaration("p");

        var flat =
            ImmutableDictionary<DeclQualifiedName, SyntaxTypes.Declaration>
            .Empty.Add(portName, portDecl);

        var model = OptimizedElmSyntaxDeclarations.FromFlatDictionary(flat);

        model.FunctionDeclarations.Should().BeEmpty();
        model.OtherDeclarations.Should().ContainKey(portName);
        model.OtherDeclarations[portName].Should().BeSameAs(portDecl);
    }

    [Fact]
    public void FromFlatDictionary_mixed_input_buckets_each_kind_correctly()
    {
        var fnName = DeclQualifiedName.FromString("Mod.f");
        var portName = DeclQualifiedName.FromString("Mod.p");

        var fnDecl = MakeFunctionDeclaration("f");
        SyntaxTypes.Declaration portDecl = MakePortDeclaration("p");

        var flat =
            ImmutableDictionary<DeclQualifiedName, SyntaxTypes.Declaration>
            .Empty
            .Add(fnName, fnDecl)
            .Add(portName, portDecl);

        var model = OptimizedElmSyntaxDeclarations.FromFlatDictionary(flat);

        model.FunctionDeclarations.Should().HaveCount(1);
        model.FunctionDeclarations[fnName].Original.Should().BeSameAs(fnDecl);
        model.FunctionDeclarations[fnName].Specializations.Should().BeEmpty();

        model.OtherDeclarations.Should().HaveCount(1);
        model.OtherDeclarations[portName].Should().BeSameAs(portDecl);
    }

    [Fact]
    public void FromFlatDictionary_then_RenderAsFlatDictionary_round_trips_losslessly()
    {
        var fnName = DeclQualifiedName.FromString("Mod.Sub.f");
        var globalFnName = DeclQualifiedName.FromString("topLevel");
        var portName = DeclQualifiedName.FromString("Mod.p");

        var flat =
            ImmutableDictionary<DeclQualifiedName, SyntaxTypes.Declaration>
            .Empty
            .Add(fnName, MakeFunctionDeclaration("f"))
            .Add(globalFnName, MakeFunctionDeclaration("topLevel"))
            .Add(portName, MakePortDeclaration("p"));

        var roundTripped =
            OptimizedElmSyntaxDeclarations.FromFlatDictionary(flat).RenderAsFlatDictionary();

        roundTripped.Should().HaveCount(flat.Count);

        foreach (var (key, value) in flat)
        {
            roundTripped.Should().ContainKey(key);
            roundTripped[key].Should().BeSameAs(value);
        }
    }
}
