using AwesomeAssertions;
using Pine.Core.CommonEncodings;
using Pine.Core.Elm;
using Pine.Core.Internal;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Numerics;
using Xunit;

using ElmInterpreter = Pine.Core.Elm.ElmSyntax.ElmSyntaxInterpreter;

namespace Pine.Core.Tests.Elm.ElmSyntax.ElmSyntaxInterpreter;

public class LocalBindingEnvironmentTests
{
    [Fact]
    public void Linked_environment_looks_up_parent_bindings_and_preserves_shadowing()
    {
        var parent =
            ElmInterpreter.LocalBindingEnvironment.FromBindings(
                ImmutableDictionary<string, PineValueInProcess>.Empty
                .Add("outer", IntegerInProcess(1))
                .Add("shadowed", IntegerInProcess(2)));

        var child =
            parent.CreateChild(
                ImmutableDictionary<string, PineValueInProcess>.Empty
                .Add("shadowed", IntegerInProcess(20))
                .Add("inner", IntegerInProcess(3)));

        ElmInterpreter.ToElm(child["outer"]).Should().Be(ElmValue.Integer(1));
        ElmInterpreter.ToElm(child["shadowed"]).Should().Be(ElmValue.Integer(20));
        ElmInterpreter.ToElm(child["inner"]).Should().Be(ElmValue.Integer(3));
        child.Count.Should().Be(3);
        child.Keys.Should().BeEquivalentTo(["inner", "outer", "shadowed"]);
        child.Select(binding => binding.Key).Should().OnlyHaveUniqueItems();
    }

    [Fact]
    public void Snapshot_copies_mutable_local_layer_without_copying_immutable_parent_layers()
    {
        var parent =
            ElmInterpreter.LocalBindingEnvironment.FromBindings(
                ImmutableDictionary<string, PineValueInProcess>.Empty
                .Add("outer", IntegerInProcess(1)));

        var mutableLayer =
            new Dictionary<string, PineValueInProcess>
            {
                ["shadowed"] = IntegerInProcess(2),
                ["inner"] = IntegerInProcess(3),
            };

        var environment = parent.CreateMutableChild(mutableLayer);
        var snapshot = environment.Snapshot();

        mutableLayer["shadowed"] = IntegerInProcess(20);
        mutableLayer["inner"] = IntegerInProcess(30);
        mutableLayer["later"] = IntegerInProcess(40);

        ElmInterpreter.ToElm(environment["shadowed"]).Should().Be(ElmValue.Integer(20));
        ElmInterpreter.ToElm(snapshot["shadowed"]).Should().Be(ElmValue.Integer(2));
        ElmInterpreter.ToElm(snapshot["inner"]).Should().Be(ElmValue.Integer(3));
        snapshot.ContainsKey("later").Should().BeFalse();
        ElmInterpreter.ToElm(snapshot["outer"]).Should().Be(ElmValue.Integer(1));
    }

    private static PineValueInProcess IntegerInProcess(BigInteger integer) =>
        PineValueInProcess.Create(IntegerEncoding.EncodeSignedInteger(integer));
}
