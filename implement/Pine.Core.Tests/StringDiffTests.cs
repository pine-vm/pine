using AwesomeAssertions;
using AwesomeAssertions.Execution;
using Pine.Core.Elm.ElmSyntax;
using Pine.Core.Interpreter.IntermediateVM;
using Pine.Core.Testing;
using System;
using System.Linq;
using Xunit;

namespace Pine.Core.Tests;

public class StringDiffTests
{
    [Theory]
    [InlineData("")]
    [InlineData("same")]
    [InlineData("one\r\ntwo\nthree\r")]
    public void String_diff_equal_strings_have_no_hunks_or_message(string text)
    {
        StringDiff.FindHunks(text, text).Should().BeEmpty();
        StringDiff.ReportDifference(text, text).Should().BeNull();
    }

    [Fact]
    public void String_diff_defaults_and_models_preserve_original_text()
    {
        var options = new StringDiffOptions();
        options.MaxEqualLines.Should().Be(2);
        options.MaxHunks.Should().Be(1);

        var hunks =
            StringDiff.FindHunks(
                "same\none: 12\ntwo: 34\nsame\nA",
                "same\none: 56\ntwo: 78\nsame\nB",
                new(MaxEqualLines: 0));

        hunks.Should().ContainSingle();

        hunks[0].Lines.Should().Equal(
            new StringDiffLine(1, "one: 12\n", "one: 56\n"),
            new StringDiffLine(2, "two: 34\n", "two: 78\n"));

        hunks[0].DifferenceColumns.Should().Be(new StringDiffColumnRange(5, 7));
    }

    [Fact]
    public void String_diff_renders_whole_blocks_and_union_of_all_differing_columns()
    {
        var hunk = StringDiff.FindHunks("abc1xyz\nabcdef2", "abc3xyz\nabcdef4")[0];

        hunk.DifferenceColumns.Should().Be(new StringDiffColumnRange(3, 7));

        StringDiff.RenderHunk(hunk).Should().Be(
            """
                ↓  ↓ (actual)
            "abc1xyz"
            "abcdef2"
            <vs>
            "abc3xyz"
            "abcdef4"
                ↑  ↑ (expected)
            """);
    }

    [Fact]
    public void String_diff_single_line_is_compact()
    {
        var hunk = StringDiff.FindHunks("abc1xyz", "abc2xyz")[0];

        StringDiff.RenderHunk(hunk).Should().Be(
            StringDiff.RenderCoreStringDifference("abc1xyz", "abc2xyz", 3));
    }

    [Fact]
    public void String_diff_highlights_separated_changes_on_one_line()
    {
        var hunk = StringDiff.FindHunks("a1bc2z", "a3bc4z")[0];

        hunk.DifferenceColumns.Should().Be(new StringDiffColumnRange(1, 5));

        StringDiff.RenderHunk(hunk).Should().Be(
            "  ↓  ↓ (actual)\n\"a1bc2z\"\n<vs>\n\"a3bc4z\"\n  ↑  ↑ (expected)");
    }

    [Fact]
    public void String_diff_sparse_arrows_union_changes_without_marking_short_equal_lines()
    {
        var hunk =
            StringDiff.FindHunks(
            "a1bc2z\nx\nab3c4z", "a5bc6z\nx\nab7c8z")[0];

        var rendered = StringDiff.RenderHunk(hunk);

        rendered.Split('\n')[0].Should().Be("  ↓↓ ↓ (actual)");
        rendered.Split('\n')[^1].Should().Be("  ↑↑ ↑ (expected)");
        hunk.DifferenceColumns.Should().Be(new StringDiffColumnRange(1, 5));
    }

    [Theory]
    [InlineData("a1bc", "a2bcdef", "  ↓  ↓↓↓ (actual)")]
    [InlineData("a2bcdef", "a1bc", "  ↓  ↓↓↓ (actual)")]
    [InlineData("a1bc\n", "a2bc", " ↓↓  ↓↓ (actual)")]
    [InlineData("\t1ab2", "\t3ab4", "   ↓  ↓ (actual)")]
    [InlineData("a\0bc1", "a\u0001bc2", "       ↓  ↓ (actual)")]
    public void String_diff_sparse_arrows_preserve_extra_and_escaped_character_columns(
        string actual, string expected, string markers)
    {
        var rendered = StringDiff.RenderHunk(StringDiff.FindHunks(actual, expected)[0]);

        rendered.Split('\n')[0].Should().Be(markers);
        rendered.Split('\n')[^1].Should().Be(markers.Replace('↓', '↑').Replace("actual", "expected"));
    }

    [Fact]
    public void String_diff_sparse_arrows_mark_missing_empty_lines()
    {
        var hunk = StringDiff.FindHunks("a\n", "a\n\n")[0];

        StringDiff.RenderHunk(hunk).Should().Be(
            " ↓↓ (actual)\n\"\"\n<missing line>\n<vs>\n\"\\n\"\n\"\"\n ↑↑ (expected)");

        StringDiff.RenderHunk(new([new(0, null, "")], new(0, 1), 160)).Should().Be(
            " ↓ (actual)\n<missing line>\n<vs>\n\"\"\n ↑ (expected)");
    }

    [Fact]
    public void String_diff_sparse_arrows_keep_alignment_with_clipped_context()
    {
        var hunk = StringDiff.FindHunks("prefix1ab2suffix", "prefix3ab4suffix")[0];
        var rendered = StringDiff.RenderHunk(hunk, new(0, 0));

        rendered.Should().Be(
            "  ↓  ↓ (actual)\n\"…1ab2…\"\n<vs>\n\"…3ab4…\"\n  ↑  ↑ (expected)");
    }

    [Fact]
    public void String_diff_assertion_messages_use_sparse_arrows()
    {
        Action compare = () => "a1bc2z".ShouldBeWithDiff("a3bc4z");

        compare.Should().Throw<Exception>().Which.Message
            .Should().Contain("  ↓  ↓ (actual)").And.Contain("  ↑  ↑ (expected)");
    }

    [Theory]
    [InlineData(1, 5)]
    [InlineData(2, 3)]
    [InlineData(3, 2)]
    [InlineData(5, 1)]
    public void String_diff_line_limit_splits_runs_without_losing_differences(int maxLines, int hunkCount)
    {
        var hunks =
            StringDiff.FindHunks(
                "a\na\na\na\na",
                "b\nb\nb\nb\nb",
                new(MaxLines: maxLines, MaxHunks: 10));

        hunks.Should().HaveCount(hunkCount);
        hunks.Should().OnlyContain(hunk => hunk.Lines.Count <= maxLines);

        hunks.SelectMany(hunk => hunk.Lines).Select(line => line.LineIndex)
            .Should().Equal(0, 1, 2, 3, 4);
    }

    [Theory]
    [InlineData(0, 4)]
    [InlineData(1, 2)]
    [InlineData(2, 2)]
    [InlineData(3, 1)]
    public void String_diff_equal_line_budget_is_total_not_consecutive(int maxEqualLines, int hunkCount)
    {
        var hunks =
            StringDiff.FindHunks(
                "a\nsame\na\nsame\na\nsame\na",
                "b\nsame\nb\nsame\nb\nsame\nb",
                new(MaxEqualLines: maxEqualLines, MaxHunks: 10));

        hunks.Should().HaveCount(hunkCount);

        hunks.Should().OnlyContain(
            hunk => hunk.Lines.Count(line => line.Actual == line.Expected) <= maxEqualLines);

        hunks.Should().OnlyContain(
            hunk => hunk.Lines[0].Actual != hunk.Lines[0].Expected &&
                hunk.Lines[hunk.Lines.Count - 1].Actual != hunk.Lines[hunk.Lines.Count - 1].Expected);

        hunks.SelectMany(hunk => hunk.Lines)
            .Where(line => line.Actual != line.Expected).Select(line => line.LineIndex)
            .Should().Equal(0, 2, 4, 6);
    }

    [Fact]
    public void String_diff_equal_lines_are_rendered_only_between_changes()
    {
        var hunk =
            StringDiff.FindHunks(
            "prefix\na\nsame\nc\nsuffix", "prefix\nb\nsame\nd\nsuffix",
            new(MaxEqualLines: 2))[0];

        hunk.Lines.Select(line => line.LineIndex).Should().Equal(1, 2, 3);

        StringDiff.RenderHunk(hunk).Should().Be(
            """
             ↓ (actual)
            "a"
            "same"
            "c"
            <vs>
            "b"
            "same"
            "d"
             ↑ (expected)
            """);
    }

    [Fact]
    public void String_diff_line_limit_includes_equal_lines_and_trims_trailing_context()
    {
        var hunks =
            StringDiff.FindHunks(
                "a\nsame\nsame\nc",
                "b\nsame\nsame\nd",
                new(MaxLines: 3, MaxEqualLines: 2, MaxHunks: 2));

        hunks.Should().HaveCount(2);
        hunks[0].Lines.Should().ContainSingle();
        hunks[1].Lines[0].LineIndex.Should().Be(3);
    }

    [Theory]
    [InlineData(0, 0)]
    [InlineData(1, 1)]
    [InlineData(2, 2)]
    [InlineData(10, 3)]
    public void String_diff_hunk_limit_controls_model_and_messages(int maxHunks, int count)
    {
        var options = new StringDiffOptions(MaxEqualLines: 0, MaxHunks: maxHunks);
        var hunks = StringDiff.FindHunks("a\nsame\nc\nsame\ne", "b\nsame\nd\nsame\nf", options);
        var message = StringDiff.ReportDifference("a\nsame\nc\nsame\ne", "b\nsame\nd\nsame\nf", options)!;

        hunks.Should().HaveCount(count);
        message.Split("<vs>").Should().HaveCount(count + 1);

        if (count is 0)
            message.Should().Be("Strings differ.");

        else
            message.Should().StartWith("Strings differ at lines 1-1, columns 1-1:\n");

        if (count > 1)
            message.Should().Contain("\n\nStrings differ at lines 3-3");
    }

    [Fact]
    public void String_diff_exact_line_length_is_allowed_but_long_lines_end_recognition()
    {
        var hunks =
            StringDiff.FindHunks(
                "a\n1234\n12345\nc",
                "b\n5678\n56789\nd",
                new(MaxLineLength: 4, MaxHunks: 10));

        hunks.Select(hunk => hunk.Lines.Count).Should().Equal(2, 1, 1);
        hunks[1].Lines[0].Actual.Should().Be("12345\n");
        StringDiff.RenderHunk(hunks[1]).Should().Contain("\"12345\"").And.NotContain("…");
    }

    [Fact]
    public void String_diff_long_equal_line_is_also_a_boundary()
    {
        var hunks =
            StringDiff.FindHunks(
                "a\n12345\nc",
                "b\n12345\nd",
                new(MaxLineLength: 4, MaxEqualLines: 1, MaxHunks: 10));

        hunks.Select(hunk => hunk.Lines[0].LineIndex).Should().Equal(0, 2);
    }

    [Fact]
    public void String_diff_long_lines_use_aligned_slices_around_late_changes()
    {
        var prefix = new string('x', 200);

        var hunk =
            StringDiff.FindHunks(prefix + "123" + prefix, prefix + "456" + prefix,
            new(MaxLineLength: 8))[0];

        var rendered = StringDiff.RenderHunk(hunk, new(ContextColumnsLeft: 4, ContextColumnsRight: 1));

        hunk.DifferenceColumns.Should().Be(new StringDiffColumnRange(200, 203));

        rendered.Should().Be(
            """
                  ↓↓↓ (actual)
            "…xxxx123x…"
            <vs>
            "…xxxx456x…"
                  ↑↑↑ (expected)
            """);

        rendered.Length.Should().BeLessThan(100);
    }

    [Fact]
    public void String_diff_short_side_and_wide_difference_are_shown_completely()
    {
        var hunk =
            StringDiff.FindHunks("short", "short" + new string('x', 1000),
            new(MaxLineLength: 8))[0];

        StringDiff.RenderHunk(hunk, new(4, 1)).Should().Be(
            "      " + new string('↓', 1000) + " (actual)\n\"…hort\"\n<vs>\n\"…hort" +
            new string('x', 1000) + "\"\n      " + new string('↑', 1000) + " (expected)");
    }

    [Theory]
    [InlineData("", "x", 0, 1)]
    [InlineData("abc", "abcd", 3, 4)]
    [InlineData("abcdef", "abc", 3, 6)]
    [InlineData("abcd", "wxyz", 0, 4)]
    public void String_diff_marks_all_extra_and_changed_characters(
        string actual, string expected, int start, int end)
    {
        var hunk = StringDiff.FindHunks(actual, expected)[0];

        hunk.DifferenceColumns.Should().Be(new StringDiffColumnRange(start, end));
        StringDiff.RenderHunk(hunk).Should().Contain(new string('↓', end - start));
    }

    [Theory]
    [InlineData("a\nb\nc", "a\nb")]
    [InlineData("a\nb", "a\nb\nc")]
    [InlineData("", "\n")]
    [InlineData("\n", "")]
    public void String_diff_preserves_missing_lines_distinct_from_empty_lines(string actual, string expected)
    {
        var hunks = StringDiff.FindHunks(actual, expected, new(MaxHunks: 10));

        hunks.SelectMany(hunk => hunk.Lines).Should().Contain(
            line => line.Actual == null || line.Expected == null);

        string.Join("\n", hunks.Select(StringDiff.RenderHunk)).Should().Contain("<missing line>");
    }

    [Theory]
    [InlineData("a\n", "a", "\"a\\n\"")]
    [InlineData("a\r\n", "a\n", "\"a\\r\\n\"")]
    [InlineData("a\r", "a\n", "\"a\\r\"")]
    public void String_diff_exposes_line_ending_differences(string actual, string expected, string visible)
    {
        var hunk = StringDiff.FindHunks(actual, expected)[0];

        hunk.Lines[0].Actual.Should().Be(actual);
        StringDiff.RenderHunk(hunk).Should().Contain(visible);
        StringDiff.ReportDifference(actual, expected).Should().NotBeNull();
    }

    [Theory]
    [InlineData("\n")]
    [InlineData("\r")]
    [InlineData("\r\n")]
    public void String_diff_recognizes_all_common_line_endings(string ending)
    {
        var hunk = StringDiff.FindHunks("a" + ending + "c", "b" + ending + "d")[0];

        hunk.Lines.Should().HaveCount(2);
        StringDiff.RenderHunk(hunk).Should().Contain("\"a\"\n\"c\"");
    }

    [Fact]
    public void String_diff_escaped_characters_keep_marker_columns_aligned()
    {
        var hunk = StringDiff.FindHunks("\t\"\\a\0", "\t\"\\b\0")[0];

        hunk.DifferenceColumns.Should().Be(new StringDiffColumnRange(6, 7));

        StringDiff.RenderHunk(hunk).Should().Be(
            "       ↓ (actual)\n\"\\t\\\"\\\\a\\u0000\"\n<vs>\n" +
            "\"\\t\\\"\\\\b\\u0000\"\n       ↑ (expected)");
    }

    [Fact]
    public void String_diff_renderer_accepts_an_independently_constructed_model()
    {
        var hunk =
            new StringDiffHunk(
                [new(10, "a", "b"), new(11, "c", "d")],
                new(0, 1),
                160);

        StringDiff.RenderHunk(hunk).Should().Contain("\"a\"\n\"c\"\n<vs>\n\"b\"\n\"d\"");
        StringDiff.RenderHunk(new([], new(0, 0), 160)).Should().BeEmpty();
    }

    [Theory]
    [InlineData(-1, 1)]
    [InlineData(1, 0)]
    [InlineData(1, 1)]
    public void String_diff_renderer_rejects_invalid_column_ranges(int start, int end)
    {
        var hunk = new StringDiffHunk([new(0, "a", "b")], new(start, end), 160);
        Action render = () => StringDiff.RenderHunk(hunk);

        render.Should().Throw<ArgumentException>()
            .WithMessage("*difference column range*");
    }

    [Fact]
    public void String_diff_rejects_invalid_limits_and_null_strings()
    {
        StringDiffOptions[] invalid =
            [
            new(MaxLineLength: 0), new(MaxLineLength: -1), new(MaxLines: 0),
            new(MaxLines: -1), new(MaxEqualLines: -1), new(MaxHunks: -1)
            ];

        foreach (var options in invalid)
        {
            Action compare = () => StringDiff.FindHunks("a", "a", options);
            compare.Should().Throw<ArgumentOutOfRangeException>();
        }

        Action nullActual = () => StringDiff.FindHunks(null!, "");
        Action nullExpected = () => StringDiff.FindHunks("", null!);
        Action nullHunk = () => StringDiff.RenderHunk(null!);
        nullActual.Should().Throw<ArgumentNullException>();
        nullExpected.Should().Throw<ArgumentNullException>();
        nullHunk.Should().Throw<ArgumentNullException>();
    }

    [Fact]
    public void String_diff_assertion_helper_reports_all_changed_counters()
    {
        var actual = "Invocations: 123\nLists: 456\nLoops: 0\nInstructions: 789";
        var expected = "Invocations: 234\nLists: 567\nLoops: 0\nInstructions: 890";
        Action compare = () => actual.ShouldBeWithDiff(expected);
        var exception = compare.Should().Throw<Exception>().Which;

        foreach (var line in actual.Split('\n').Concat(expected.Split('\n')))
            exception.Message.Should().Contain("\"" + line + "\"");
    }

    [Theory]
    [InlineData(null)]
    [InlineData("")]
    [InlineData("one\r\ntwo\n")]
    public void String_diff_assertion_helper_accepts_exactly_equal_strings(string? text)
    {
        text.ShouldBeWithDiff(text);
    }

    [Theory]
    [InlineData(null, "")]
    [InlineData("", null)]
    [InlineData("a\n", "a")]
    [InlineData("a\r\n", "a\n")]
    public void String_diff_assertion_helper_does_not_normalize_inequality(string? actual, string? expected)
    {
        Action compare = () => actual.ShouldBeWithDiff(expected);
        compare.Should().Throw<Exception>();
    }

    [Fact]
    public void String_diff_assertion_helper_respects_options()
    {
        Action compare = () => "a\nc".ShouldBeWithDiff("b\nd", new(MaxLines: 1, MaxHunks: 1));
        var exception = compare.Should().Throw<Exception>().Which;

        exception.Message.Should().Contain("\"a\"").And.NotContain("\"c\"");
    }

    [Fact]
    public void String_diff_assertion_helper_preserves_braces_and_supports_assertion_scopes()
    {
        const string actual = "{reason}: 12\n{0}: 34";
        const string expected = "{reason}: 56\n{0}: 78";
        string[] failures;

        using (var scope = new AssertionScope())
        {
            actual.ShouldBeWithDiff(expected);
            "a".ShouldBeWithDiff("b");
            failures = scope.Discard();
        }

        failures.Should().HaveCount(2);
        failures[0].Should().Be(StringDiff.ReportDifference(actual, expected));
    }

    [Fact]
    public void String_diff_minimum_limits_work_with_empty_and_long_lines()
    {
        var hunks =
            StringDiff.FindHunks(
                "\nx\nab",
                "y\n\nac",
                new(MaxLineLength: 1, MaxLines: 1, MaxHunks: 3));

        hunks.Should().HaveCount(3);
        hunks.Should().OnlyContain(hunk => hunk.Lines.Count == 1);
        string.Join("\n", hunks.Select(StringDiff.RenderHunk)).Should().Contain("↓");
    }

    [Fact]
    public void String_diff_multiple_limits_cover_every_change_across_boundary_combinations()
    {
        string[] expectedLines = ["a", "bb", "c", "ddd", "e", "ff"];

        for (var changes = 0; changes < 64; changes++)
        {
            var actualLines =
                expectedLines.Select((line, index) => (changes & (1 << index)) == 0 ? line : line.ToUpperInvariant())
                .ToArray();

            var changedIndices =
                Enumerable.Range(0, expectedLines.Length)
                .Where(index => actualLines[index] != expectedLines[index]).ToArray();

            for (var maxLines = 1; maxLines <= 4; maxLines++)
            {
                for (var maxEqual = 0; maxEqual <= 2; maxEqual++)
                {
                    for (var maxLength = 1; maxLength <= 3; maxLength++)
                    {
                        var hunks =
                            StringDiff.FindHunks(
                                string.Join("\n", actualLines),
                                string.Join("\n", expectedLines),
                                new(maxLength, maxLines, maxEqual, MaxHunks: 10));

                        hunks.SelectMany(hunk => hunk.Lines)
                            .Where(line => line.Actual != line.Expected)
                            .Select(line => line.LineIndex).Should().Equal(changedIndices);

                        hunks.Should().OnlyContain(hunk => hunk.Lines.Count <= maxLines);

                        hunks.Should().OnlyContain(
                            hunk => hunk.Lines.Count(line => line.Actual == line.Expected) <= maxEqual);

                        foreach (var hunk in hunks)
                        {
                            hunk.Lines[0].Actual.Should().NotBe(hunk.Lines[0].Expected);
                            hunk.Lines[^1].Actual.Should().NotBe(hunk.Lines[^1].Expected);
                            StringDiff.RenderHunk(hunk).Should().Contain("<vs>");
                        }
                    }
                }
            }
        }
    }

    [Fact]
    public void String_diff_middle_insertion_is_compared_at_ordinal_indices()
    {
        var hunk = StringDiff.FindHunks("a\ninserted\nb\nc", "a\nb\nc")[0];

        hunk.Lines.Should().Equal(
            new StringDiffLine(1, "inserted\n", "b\n"),
            new StringDiffLine(2, "b\n", "c"),
            new StringDiffLine(3, "c", null));
    }

    [Theory]
    [InlineData(1, 1)]
    [InlineData(2, 1)]
    [InlineData(3, 2)]
    public void String_diff_default_equal_line_budget_bridges_at_most_two_lines(int equalLines, int count)
    {
        var context = string.Concat(Enumerable.Repeat("same\n", equalLines));

        var hunks =
            StringDiff.FindHunks(
                "a\n" + context + "c",
                "b\n" + context + "d",
                new(MaxHunks: 10));

        hunks.Should().HaveCount(count);
        hunks[0].Lines.Count.Should().Be(count == 1 ? equalLines + 2 : 1);
    }

    [Theory]
    [InlineData(0, 0, "\"…12…\"", 2)]
    [InlineData(1, 3, "\"…e12fgh…\"", 3)]
    [InlineData(3, 1, "\"…cde12f…\"", 5)]
    [InlineData(5, 5, "\"abcde12fghij\"", 6)]
    [InlineData(80, 80, "\"abcde12fghij\"", 6)]
    public void String_diff_rendering_context_controls_each_side_independently(
        int left, int right, string actualSlice, int indent)
    {
        var hunk = StringDiff.FindHunks("abcde12fghij", "abcde34fghij")[0];
        var rendered = StringDiff.RenderHunk(hunk, new(left, right));

        rendered.Should().Contain(actualSlice);
        rendered.Should().StartWith(new string(' ', indent) + "↓↓ (actual)\n");
        rendered.Should().EndWith(new string(' ', indent) + "↑↑ (expected)");
    }

    [Fact]
    public void String_diff_default_rendering_context_is_symmetric()
    {
        var options = new StringDiffRenderingOptions();
        options.ContextColumnsLeft.Should().Be(80);
        options.ContextColumnsRight.Should().Be(options.ContextColumnsLeft);
        var prefix = new string('a', 81);
        var suffix = new string('b', 81);
        var hunk = StringDiff.FindHunks(prefix + "1" + suffix, prefix + "2" + suffix)[0];

        StringDiff.RenderHunk(hunk).Should().Contain(
            "\"…" + new string('a', 80) + "1" + new string('b', 80) + "…\"");
    }

    [Fact]
    public void String_diff_rendering_context_does_not_change_hunk_recognition()
    {
        var hunks =
            StringDiff.FindHunks(
                "abcde12fghij\nabcde56fghij",
                "abcde34fghij\nabcde78fghij",
                new(MaxLineLength: 4, MaxHunks: 2));

        hunks.Should().HaveCount(2);
        StringDiff.RenderHunk(hunks[0]).Should().Contain("\"abcde12fghij\"").And.NotContain("…");
        StringDiff.RenderHunk(hunks[0], new(0, 0)).Should().Contain("\"…12…\"");
        hunks[0].DifferenceColumns.Should().Be(new StringDiffColumnRange(5, 7));
    }

    [Fact]
    public void String_diff_context_spans_the_full_multiline_difference_range()
    {
        var hunk = StringDiff.FindHunks("ab1defghi\nabcdefg2i", "ab3defghi\nabcdefg4i")[0];
        var rendered = StringDiff.RenderHunk(hunk, new(1, 0));

        rendered.Should().Be(
            """
               ↓    ↓ (actual)
            "…b1defgh…"
            "…bcdefg2…"
            <vs>
            "…b3defgh…"
            "…bcdefg4…"
               ↑    ↑ (expected)
            """);
    }

    [Fact]
    public void String_diff_context_is_measured_in_escaped_display_columns()
    {
        var hunk = StringDiff.FindHunks("\ta\\tail", "\tb\\tail")[0];

        StringDiff.RenderHunk(hunk, new(2, 2)).Should().Be(
            "   ↓ (actual)\n\"\\ta\\\\…\"\n<vs>\n\"\\tb\\\\…\"\n   ↑ (expected)");
    }

    [Fact]
    public void String_diff_rendering_context_accepts_large_values_without_overflow()
    {
        var hunk = StringDiff.FindHunks("prefix1suffix", "prefix2suffix")[0];

        StringDiff.RenderHunk(hunk, new(int.MaxValue, int.MaxValue))
            .Should().Be(StringDiff.RenderHunk(hunk));
    }

    [Theory]
    [InlineData(-1, 0)]
    [InlineData(0, -1)]
    public void String_diff_rendering_rejects_negative_context_even_without_hunks(int left, int right)
    {
        var options = new StringDiffRenderingOptions(left, right);
        Action render = () => StringDiff.RenderHunk(StringDiff.FindHunks("a", "b")[0], options);
        Action report = () => StringDiff.ReportDifference("a", "a", renderingOptions: options);
        Action empty = () => StringDiff.RenderHunk(new([], new(0, 0), 160), options);

        render.Should().Throw<ArgumentOutOfRangeException>();
        report.Should().Throw<ArgumentOutOfRangeException>();
        empty.Should().Throw<ArgumentOutOfRangeException>();
    }

    [Fact]
    public void String_diff_report_and_assertion_helper_forward_rendering_context()
    {
        const string actual = "abcde12fghij";
        const string expected = "abcde34fghij";
        var options = new StringDiffRenderingOptions(1, 3);
        var message = StringDiff.ReportDifference(actual, expected, renderingOptions: options)!;
        var hunk = StringDiff.FindHunks(actual, expected)[0];
        Action compare = () => actual.ShouldBeWithDiff(expected, renderingOptions: options);

        message.Should().EndWith(StringDiff.RenderHunk(hunk, options));
        compare.Should().Throw<Exception>().Which.Message.Should().Be(message);
    }

    [Fact]
    public void String_diff_default_context_shows_complete_performance_counter_snapshots()
    {
        var counters =
            new PerformanceCounters(
                long.MaxValue,
                long.MaxValue,
                long.MaxValue,
                long.MaxValue,
                long.MaxValue,
                long.MaxValue,
                long.MaxValue,
                long.MaxValue);

        var interpreterCounters =
            new ElmSyntaxInterpreterPerformanceCounters(
                long.MaxValue,
                long.MaxValue,
                long.MaxValue,
                long.MaxValue);

        string[] snapshots =
            [
            PerformanceCountersFormatting.FormatCounts(counters),
            PerformanceCountersFormatting.FormatAllCounts(counters),
            ElmSyntaxInterpreterPerformanceCountersFormatting.FormatCounts(interpreterCounters),
            InvocationCountReportFormatting.FormatCounts(InvocationCountReport.Empty)
            ];

        foreach (var actual in snapshots)
        {
            var expected = actual.Replace('7', '6').Replace('0', '1');
            var message = StringDiff.ReportDifference(actual, expected)!;
            Action compare = () => actual.ShouldBeWithDiff(expected);
            var failure = compare.Should().Throw<Exception>().Which.Message;

            message.Should().NotContain("…");
            failure.Should().NotContain("…");

            foreach (var line in actual.Split('\n').Concat(expected.Split('\n')))
            {
                message.Should().Contain("\"" + line + "\"");
                failure.Should().Contain("\"" + line + "\"");
            }
        }
    }
}
