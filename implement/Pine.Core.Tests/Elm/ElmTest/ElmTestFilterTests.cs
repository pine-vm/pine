using AwesomeAssertions;
using Pine.Core.Elm.Testing;
using System;
using System.Collections.Generic;
using System.Linq;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmTest;

public class ElmTestFilterTests
{
    [Fact]
    public void Exact_full_path_filters_treat_wildcards_and_description_separators_literally()
    {
        var test = new ListedTest("tests/Tests.elm", ["group/with\\separators"], "literal * name");
        new ElmTestFilter("=" + test.FullPath).Matches(test).Should().BeTrue();
        new ElmTestFilter("=" + test.FullPath).Matches(test with { Name = "literal other name" }).Should().BeFalse();
    }

    [Fact]
    public void Duplicate_paths_use_discovery_ordinals_for_exact_selection()
    {
        var test = new ListedTest("tests/Tests.elm", ["group"], "same");
        var selector = ElmTestFilter.ExactSelector(test, 2, [test, test]);
        selector.Should().Be("#2");
        new ElmTestFilter(selector).Matches(test, 1).Should().BeFalse();
        new ElmTestFilter(selector).Matches(test, 2).Should().BeTrue();
    }

    [Fact]
    public void Indexed_exact_selectors_preserve_literal_paths_case_and_duplicate_ordinals()
    {
        ListedTest[] tests =
            [
                new(@"tests\Tests.elm", ["group"], "same"),
                new("tests/Tests.elm", ["group"], "same"),
                new("tests/Tests.elm", ["group/with"], "separators"),
                new("tests/Tests.elm", ["group"], "with/separators"),
                new("tests/Tests.elm", ["group"], "Same"),
                new("tests/Tests.elm", ["group/with\\separators"], "literal * name")
            ];

        var counts = ElmTestFilter.CountFullPaths(tests);

        counts.Should().HaveCount(4);
        counts[tests[0].FullPath].Should().Be(2);
        counts[tests[2].FullPath].Should().Be(2);

        for (var index = 0; index < tests.Length; index++)
        {
            var selector = ElmTestFilter.ExactSelector(tests[index], index + 1, counts);
            selector.Should().Be(index < 4 ? "#" + (index + 1) : "=" + tests[index].FullPath);
            selector.Should().Be(ElmTestFilter.ExactSelector(tests[index], index + 1, tests));

            var matches =
                tests.Select(
                    (test, ordinal) => new ElmTestFilter(selector).Matches(test, ordinal + 1));

            matches.Should().Equal(Enumerable.Range(0, tests.Length).Select(ordinal => ordinal == index));
        }

        ElmTestFilter.ExactSelector(tests[0] with { Name = "missing" }, 7, counts).Should().Be("#7");
        ElmTestFilter.ExactSelector(tests[0], 1, ElmTestFilter.CountFullPaths([])).Should().Be("#1");
    }

    [Fact]
    public void Exact_path_frequencies_enumerate_the_inventory_once_for_all_selectors()
    {
        var enumerations = 0;

        var tests =
            Enumerable.Range(0, 2048)
            .Select(index => new ListedTest("tests/Tests.elm", ["group"], "test " + index)).ToArray();

        IEnumerable<ListedTest> Inventory()
        {
            enumerations++;

            if (enumerations > 1)
                throw new InvalidOperationException("The inventory must only be enumerated once.");

            foreach (var test in tests)
                yield return test;
        }

        var counts = ElmTestFilter.CountFullPaths(Inventory());

        for (var index = 0; index < tests.Length; index++)
        {
            ElmTestFilter.ExactSelector(tests[index], index + 1, counts)
                .Should().Be("=" + tests[index].FullPath);
        }

        enumerations.Should().Be(1);
        counts.Should().HaveCount(tests.Length);
    }

    private static readonly ListedTest s_test =
        new(
            "tests/nested/ConvertConcreteToAbstractTests.elm",
            ["convert concrete syntax to abstract syntax", "declarations"],
            "converts every declaration variant and drops documentation");

    [Theory]
    [InlineData("", true)]
    [InlineData("CONCRETE", true)]
    [InlineData("drops documentation", true)]
    [InlineData("nested", true)]
    [InlineData("unrelated", false)]
    [InlineData("tests/nested/ConvertConcreteToAbstractTests.elm", true)]
    [InlineData(@"tests\nested\ConvertConcreteToAbstractTests", true)]
    [InlineData(@"tests/nested\ConvertConcreteToAbstractTests", true)]
    [InlineData("/tests//nested/ConvertConcreteToAbstractTests/", true)]
    [InlineData("nested/ConvertConcreteToAbstractTests/convert concrete syntax to abstract syntax", true)]
    [InlineData("ConvertConcreteToAbstractTests/convert concrete syntax to abstract syntax/declarations/converts every declaration variant and drops documentation", true)]
    [InlineData("declarations/converts every declaration variant and drops documentation", true)]
    [InlineData("convert concrete syntax to abstract syntax/declarations", true)]
    [InlineData("nested/ConvertConcrete", false)]
    [InlineData("tests/ConvertConcreteToAbstractTests", false)]
    [InlineData("ConvertConcreteToAbstractTests/declarations", false)]
    [InlineData("declarations/convert concrete syntax to abstract syntax", false)]
    [InlineData("tests/*/Convert*Tests.elm", true)]
    [InlineData("tests/*/Convert*Tests", true)]
    [InlineData("tests/*/Convert*Tests.txt", false)]
    [InlineData("tests/*Tests.elm", false)]
    [InlineData("tests/**/Convert*Tests.elm", true)]
    [InlineData("nested/**/ConvertConcreteToAbstractTests", true)]
    [InlineData("ConvertConcreteToAbstractTests/**/converts every*", true)]
    [InlineData("tests/**/declarations/converts every*", true)]
    [InlineData("tests/**/missing/**/converts every*", false)]
    [InlineData("**/declarations/**", true)]
    [InlineData("**", true)]
    [InlineData("*", true)]
    [InlineData("convert*syntax", true)]
    [InlineData("convert**syntax", true)]
    [InlineData("*documentation", true)]
    [InlineData("documentation*", false)]
    [InlineData("converts every*variant*drops*", true)]
    [InlineData("*drops*variant*", false)]
    [InlineData("tests/****/converts every*", false)]
    public void Filter_matches_portable_consecutive_paths_and_wildcards(
        string expression,
        bool expected)
    {
        new ElmTestFilter(expression).Matches(s_test).Should().Be(expected);
    }

    [Fact]
    public void Description_separators_are_literal_characters_in_actual_path_items()
    {
        var test = new ListedTest("tests/Tests.elm", ["group/with\\separators"], "test");

        new ElmTestFilter("Tests/**/group*separators/test").Matches(test).Should().BeTrue();
        new ElmTestFilter("group/with/separators/test").Matches(test).Should().BeFalse();
    }

    [Fact]
    public void Only_filename_can_omit_its_final_extension()
    {
        var test = new ListedTest(@"tests\Nested.Name\Tests.extra.elm", ["Group.name"], "test.name");

        new ElmTestFilter("Nested.Name/Tests.extra/Group.name/test.name").Matches(test).Should().BeTrue();
        new ElmTestFilter("Nested.Name/Tests/Group.name").Matches(test).Should().BeFalse();
        new ElmTestFilter("tests/Nested/Tests.extra").Matches(test).Should().BeFalse();
        new ElmTestFilter("Tests.extra/Group/test").Matches(test).Should().BeFalse();
        test.FullPath.Should().Be("tests/Nested.Name/Tests.extra.elm/Group.name/test.name");
    }

    [Fact]
    public void Wildcards_match_empty_segments_and_do_not_interpret_regex_characters()
    {
        var test = new ListedTest("tests/Tests.elm", [""], "literal [x].name?");

        new ElmTestFilter("Tests/*/literal [x].name?").Matches(test).Should().BeTrue();
        new ElmTestFilter("Tests/**/literal [x].name?").Matches(test).Should().BeTrue();
        new ElmTestFilter("Tests/*/literal x.name?").Matches(test).Should().BeFalse();
    }

    [Fact]
    public void Boolean_operator_characters_are_literal_not_conjunctive_filters()
    {
        var test = new ListedTest("tests/Tests.elm", ["A & B"], "one && two");

        new ElmTestFilter("A & B/one && two").Matches(test).Should().BeTrue();
        new ElmTestFilter("A&B/one && two").Matches(test).Should().BeFalse();
    }

    [Fact]
    public void Closest_tests_account_for_full_expression_and_return_bounded_full_paths()
    {
        var closest = new ListedTest("tests/Target.elm", ["Root", "Chosen"], "First");
        var wrongFile = new ListedTest("tests/Other.elm", ["Root", "Chosen"], "First");
        var wrongGroup = new ListedTest("tests/Target.elm", ["Root", "Unrelated"], "First");
        var wrongName = new ListedTest("tests/Target.elm", ["Root", "Chosen"], "Unrelated");

        var suggestions =
            ElmTestFilter.FindClosestTests(
                [wrongFile, wrongGroup, wrongName, closest, closest],
                new ElmTestFilter(@"tests\Target\**\Chosen\*irstt"),
                count: 2);

        suggestions.Should().HaveCount(2);
        suggestions[0].Should().Be(closest);
        suggestions.Select(test => test.FullPath).Should().OnlyHaveUniqueItems();
    }

    [Fact]
    public void Closest_paths_favor_correct_level_order_and_support_globstar_and_extension_omission()
    {
        var correct = new ListedTest("tests/Target.elm", ["Root", "Chosen"], "First");
        var reversed = new ListedTest("tests/Target.elm", ["Chosen", "Root"], "First");

        var suggestions =
            ElmTestFilter.FindClosestTests(
                [reversed, correct],
                new ElmTestFilter("Target/**/Root/Chosen/Firstt"));

        suggestions[0].Should().Be(correct);
    }

    [Fact]
    public void Closest_tests_sort_equal_scores_by_path_and_limit_to_five()
    {
        var tests =
            Enumerable.Range(0, 10)
            .Reverse()
            .Select(index => new ListedTest($"tests/Tests{index}.elm", [], "same"));

        var suggestions =
            ElmTestFilter.FindClosestTests(tests, new ElmTestFilter("tests/*/same!"));

        suggestions.Select(test => test.FilePath).Should().Equal(
            Enumerable.Range(0, 5).Select(index => $"tests/Tests{index}.elm"));
    }

    [Fact]
    public void Closest_tests_handle_an_empty_test_set()
    {
        ElmTestFilter.FindClosestTests([], new ElmTestFilter("missing")).Should().BeEmpty();
    }

    [Fact]
    public void Filter_rejects_null_expression()
    {
        Action create = () => new ElmTestFilter(null!);

        create.Should().Throw<ArgumentNullException>();
    }
}
