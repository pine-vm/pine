using AwesomeAssertions;
using Pine.Core.Elm;
using System;
using Xunit;

namespace Pine.Core.Tests.Elm;

public class ElmValueRenderAsElmExpressionTests
{
    /// <summary>
    /// The default rendering of an <see cref="ElmValue.ElmPineBlob"/> only mentions the byte count.
    /// </summary>
    [Fact]
    public void Default_pineBlobRenderer_renders_byte_count_placeholder()
    {
        var blob = new ElmValue.ElmPineBlob(new byte[] { 0x12, 0x34, 0x56, 0x78 });

        var rendered = ElmValue.RenderAsElmExpression(blob);

        rendered.expressionString.Should().Be("<pine_blob 4 bytes>");
        rendered.needsParens.Should().BeTrue();
    }

    /// <summary>
    /// A custom <c>pineBlobRenderer</c> is used at the top level for an <see cref="ElmValue.ElmPineBlob"/>.
    /// </summary>
    [Fact]
    public void Custom_pineBlobRenderer_is_used_at_top_level()
    {
        var blob = new ElmValue.ElmPineBlob(new byte[] { 0x12, 0x34, 0x56, 0x78 });

        var rendered =
            ElmValue.RenderAsElmExpression(
                blob,
                pineBlobRenderer: bytes => "Blob 0x" + Convert.ToHexStringLower(bytes.Span));

        rendered.expressionString.Should().Be("Blob 0x12345678");
        rendered.needsParens.Should().BeTrue();
    }

    /// <summary>
    /// The supplied <c>pineBlobRenderer</c> is also used for <see cref="ElmValue.ElmPineBlob"/> nodes
    /// nested inside an <see cref="ElmValue.ElmList"/>.
    /// </summary>
    [Fact]
    public void Custom_pineBlobRenderer_is_used_inside_list()
    {
        var first = new ElmValue.ElmPineBlob(new byte[] { 0x01, 0x02, 0x03, 0x04 });
        var second = new ElmValue.ElmPineBlob(new byte[] { 0xAB, 0xCD });

        var list = new ElmValue.ElmList([first, second]);

        var rendered =
            ElmValue.RenderAsElmExpression(
                list,
                pineBlobRenderer: bytes => "Blob[" + bytes.Length + "]");

        rendered.expressionString.Should().Contain("Blob[4]").And.Contain("Blob[2]");
    }

    /// <summary>
    /// The supplied <c>pineBlobRenderer</c> is also used for <see cref="ElmValue.ElmPineBlob"/> nodes
    /// nested inside an <see cref="ElmValue.ElmTag"/>.
    /// </summary>
    [Fact]
    public void Custom_pineBlobRenderer_is_used_inside_tag_arguments()
    {
        var blob = new ElmValue.ElmPineBlob(new byte[] { 0xDE, 0xAD, 0xBE, 0xEF });

        var tag = ElmValue.TagInstance("Wrap", [blob]);

        var rendered =
            ElmValue.RenderAsElmExpression(
                tag,
                pineBlobRenderer: bytes => "Blob 0x" + Convert.ToHexStringLower(bytes.Span));

        rendered.expressionString.Should().Be("Wrap (Blob 0xdeadbeef)");
    }

    /// <summary>
    /// The supplied <c>pineBlobRenderer</c> is also used for <see cref="ElmValue.ElmPineBlob"/> nodes
    /// nested inside an <see cref="ElmValue.ElmRecord"/> field value.
    /// </summary>
    [Fact]
    public void Custom_pineBlobRenderer_is_used_inside_record_fields()
    {
        var blob = new ElmValue.ElmPineBlob(new byte[] { 0x01, 0x02 });

        var record = new ElmValue.ElmRecord([("payload", blob)]);

        var rendered =
            ElmValue.RenderAsElmExpression(
                record,
                pineBlobRenderer: bytes => "BLOB(" + bytes.Length + ")");

        rendered.expressionString.Should().Be("{ payload = BLOB(2) }");
    }

    /// <summary>
    /// The supplied <c>pineBlobRenderer</c> is threaded through deeply nested values: a list inside a
    /// tag inside a record field.
    /// </summary>
    [Fact]
    public void Custom_pineBlobRenderer_is_used_in_deeply_nested_value()
    {
        var innerBlob = new ElmValue.ElmPineBlob(new byte[] { 0xFF });

        var innerList = new ElmValue.ElmList([innerBlob, ElmValue.Integer(1)]);

        var tag = ElmValue.TagInstance("Inner", [innerList]);

        var record = new ElmValue.ElmRecord([("nested", tag)]);

        var capturedLengths = new System.Collections.Generic.List<int>();

        string PineBlobRenderer(ReadOnlyMemory<byte> bytes)
        {
            capturedLengths.Add(bytes.Length);
            return "BLOB(" + bytes.Length + ")";
        }

        var rendered = ElmValue.RenderAsElmExpression(record, pineBlobRenderer: PineBlobRenderer);

        rendered.expressionString.Should().Contain("BLOB(1)");
        capturedLengths.Should().ContainSingle().Which.Should().Be(1);
    }

    /// <summary>
    /// Non-blob nodes are unaffected by the supplied <c>pineBlobRenderer</c>.
    /// </summary>
    [Fact]
    public void Custom_pineBlobRenderer_does_not_affect_other_value_kinds()
    {
        var list =
            new ElmValue.ElmList(
                [
                ElmValue.Integer(7),
                ElmValue.StringInstance("hi"),
                ]);

        var renderedDefault = ElmValue.RenderAsElmExpression(list).expressionString;

        var renderedCustom =
            ElmValue.RenderAsElmExpression(
                list,
                pineBlobRenderer: _ => "should-not-be-used").expressionString;

        renderedCustom.Should().Be(renderedDefault);
    }
}
