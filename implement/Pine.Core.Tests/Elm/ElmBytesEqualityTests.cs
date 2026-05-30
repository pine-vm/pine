using AwesomeAssertions;
using Pine.Core.Elm;
using Xunit;

namespace Pine.Core.Tests.Elm;

public class ElmBytesEqualityTests
{
    /// <summary>
    /// Two <see cref="ElmValue.ElmBytes"/> values that wrap distinct byte buffers with the same
    /// contents must compare as equal (value equality semantics).
    /// </summary>
    [Fact]
    public void ElmBytes_with_equal_content_from_distinct_buffers_are_equal()
    {
        var bytesA = new ElmValue.ElmBytes(new byte[] { 0x12, 0x34, 0x56, 0x78 });
        var bytesB = new ElmValue.ElmBytes(new byte[] { 0x12, 0x34, 0x56, 0x78 });

        bytesA.Should().Be(bytesB);
    }

    /// <summary>
    /// <see cref="ElmValue.ElmBytes.GetHashCode"/> must be content-based so that equal values
    /// produce equal hash codes, as required by the equality/hash-code contract.
    /// </summary>
    [Fact]
    public void ElmBytes_with_equal_content_from_distinct_buffers_have_equal_hash_codes()
    {
        var bytesA = new ElmValue.ElmBytes(new byte[] { 0x12, 0x34, 0x56, 0x78 });
        var bytesB = new ElmValue.ElmBytes(new byte[] { 0x12, 0x34, 0x56, 0x78 });

        bytesA.GetHashCode().Should().Be(bytesB.GetHashCode());
    }

    /// <summary>
    /// Equal-content <see cref="ElmValue.ElmBytes"/> values must remain interchangeable when used
    /// as keys in hash-based collections, which relies on both equality and hashing being content-based.
    /// </summary>
    [Fact]
    public void ElmBytes_with_equal_content_resolve_to_same_dictionary_entry()
    {
        var bytesA = new ElmValue.ElmBytes(new byte[] { 0x12, 0x34, 0x56, 0x78 });
        var bytesB = new ElmValue.ElmBytes(new byte[] { 0x12, 0x34, 0x56, 0x78 });

        var dictionary =
            new System.Collections.Generic.Dictionary<ElmValue, int>
            {
                [bytesA] = 1,
            };

        dictionary.ContainsKey(bytesB).Should().BeTrue();
    }

    /// <summary>
    /// Equal-content <see cref="ElmValue.ElmBytes"/> values must compare equal even when wrapped in
    /// an <see cref="ElmValue.ElmTag"/>, whose equality short-circuits on the cached hash code.
    /// </summary>
    [Fact]
    public void ElmBytes_with_equal_content_are_equal_when_wrapped_in_tag()
    {
        var taggedA =
            ElmValue.TagInstance(
                "Tag",
                [new ElmValue.ElmBytes(new byte[] { 0x12, 0x34, 0x56, 0x78 })]);

        var taggedB =
            ElmValue.TagInstance(
                "Tag",
                [new ElmValue.ElmBytes(new byte[] { 0x12, 0x34, 0x56, 0x78 })]);

        taggedA.Should().Be(taggedB);
    }

    /// <summary>
    /// <see cref="ElmValue.ElmBytes"/> values with different contents must not compare as equal.
    /// </summary>
    [Fact]
    public void ElmBytes_with_different_content_are_not_equal()
    {
        var bytesA = new ElmValue.ElmBytes(new byte[] { 0x12, 0x34, 0x56, 0x78 });
        var bytesB = new ElmValue.ElmBytes(new byte[] { 0x12, 0x34, 0x56, 0x79 });

        bytesA.Should().NotBe(bytesB);
    }
}
