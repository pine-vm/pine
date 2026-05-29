using AwesomeAssertions;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Elm.ElmSyntax.SyntaxModel;
using System.Collections.Generic;
using System.Collections.Immutable;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet;

/// <summary>
/// Tests verifying that <see cref="ShadowingLocation"/> implements value equality semantics,
/// comparing <see cref="ShadowingLocation.DeclarationPath"/> by sequence rather than by reference.
/// </summary>
public class ShadowingLocationEqualityTests
{
    private static readonly Range s_sampleRange =
        new(new Location(1, 1), new Location(1, 10));

    private static readonly Range s_otherRange =
        new(new Location(5, 3), new Location(5, 15));

    [Fact]
    public void Equal_instances_with_empty_declaration_path()
    {
        var a = new ShadowingLocation(s_sampleRange, []);
        var b = new ShadowingLocation(s_sampleRange, []);

        a.Should().Be(b);
        (a == b).Should().BeTrue();
        a.GetHashCode().Should().Be(b.GetHashCode());
    }

    [Fact]
    public void Equal_instances_with_single_element_path()
    {
        var a = new ShadowingLocation(s_sampleRange, ["compute"]);
        var b = new ShadowingLocation(s_sampleRange, ["compute"]);

        a.Should().Be(b);
        (a == b).Should().BeTrue();
        a.GetHashCode().Should().Be(b.GetHashCode());
    }

    [Fact]
    public void Equal_instances_with_multi_element_path()
    {
        var a = new ShadowingLocation(s_sampleRange, ["outer", "inner"]);
        var b = new ShadowingLocation(s_sampleRange, ["outer", "inner"]);

        a.Should().Be(b);
        (a == b).Should().BeTrue();
        a.GetHashCode().Should().Be(b.GetHashCode());
    }

    [Fact]
    public void Not_equal_when_declaration_paths_differ()
    {
        var a = new ShadowingLocation(s_sampleRange, ["compute"]);
        var b = new ShadowingLocation(s_sampleRange, ["transform"]);

        a.Should().NotBe(b);
        (a == b).Should().BeFalse();
    }

    [Fact]
    public void Not_equal_when_path_lengths_differ()
    {
        var a = new ShadowingLocation(s_sampleRange, ["compute"]);
        var b = new ShadowingLocation(s_sampleRange, []);

        a.Should().NotBe(b);
        (a == b).Should().BeFalse();
    }

    [Fact]
    public void Not_equal_when_ranges_differ()
    {
        var a = new ShadowingLocation(s_sampleRange, ["compute"]);
        var b = new ShadowingLocation(s_otherRange, ["compute"]);

        a.Should().NotBe(b);
        (a == b).Should().BeFalse();
    }

    [Fact]
    public void Not_equal_when_both_range_and_path_differ()
    {
        var a = new ShadowingLocation(s_sampleRange, ["compute"]);
        var b = new ShadowingLocation(s_otherRange, ["transform"]);

        a.Should().NotBe(b);
        (a == b).Should().BeFalse();
    }

    [Fact]
    public void Same_reference_is_equal()
    {
        var a = new ShadowingLocation(s_sampleRange, ["compute"]);

        a.Should().Be(a);
        a.Equals(a).Should().BeTrue();
    }

    [Fact]
    public void Not_equal_to_null()
    {
        var a = new ShadowingLocation(s_sampleRange, ["compute"]);

        a.Equals(null).Should().BeFalse();
        (a == null).Should().BeFalse();
    }

    [Fact]
    public void Equal_instances_from_different_ImmutableList_references()
    {
        // Explicitly create two distinct ImmutableList instances with the same content
        // to verify that value equality (not reference equality) is used.
        var path1 = ImmutableList.Create("alpha", "beta");
        var path2 = ImmutableList.Create("alpha", "beta");

        // Confirm they are different references
        ReferenceEquals(path1, path2).Should().BeFalse();

        var a = new ShadowingLocation(s_sampleRange, path1);
        var b = new ShadowingLocation(s_sampleRange, path2);

        a.Should().Be(b);
        (a == b).Should().BeTrue();
        a.GetHashCode().Should().Be(b.GetHashCode());
    }

    [Fact]
    public void Works_correctly_as_dictionary_key()
    {
        var loc1 = new ShadowingLocation(s_sampleRange, ["compute"]);
        var loc2 = new ShadowingLocation(s_sampleRange, ["compute"]);

        var dict =
            new Dictionary<ShadowingLocation, int>
            {
                [loc1] = 42
            };

        dict.ContainsKey(loc2).Should().BeTrue();
        dict[loc2].Should().Be(42);
    }

    [Fact]
    public void Works_correctly_in_HashSet()
    {
        var loc1 = new ShadowingLocation(s_sampleRange, ["compute"]);
        var loc2 = new ShadowingLocation(s_sampleRange, ["compute"]);
        var loc3 = new ShadowingLocation(s_otherRange, ["transform"]);

        var set = new HashSet<ShadowingLocation> { loc1, loc2, loc3 };

        // loc1 and loc2 are value-equal, so the set should contain only 2 items
        set.Should().HaveCount(2);
        set.Contains(loc1).Should().BeTrue();
        set.Contains(loc3).Should().BeTrue();
    }

    [Fact]
    public void Path_element_order_matters()
    {
        var a = new ShadowingLocation(s_sampleRange, ["alpha", "beta"]);
        var b = new ShadowingLocation(s_sampleRange, ["beta", "alpha"]);

        a.Should().NotBe(b);
        (a == b).Should().BeFalse();
    }
}
