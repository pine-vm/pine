using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Elm.ElmInElm;
using Pine.Core.Elm.ElmSyntax;
using Pine.Core.Files;
using Pine.Core.Tests.Elm.ElmSyntax.ElmSyntaxInterpreter;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.CoreLibraryModule;

public class CoreRegexFunctionTests
{
    private sealed record Scenario(string Name, string Expression, string Expected);

    private static string Str(string value) =>
        Rendering.RenderStringLiteral(value);

    private static Scenario Find(string name, string pattern, string input, string expected) =>
        new(name, $"find {Str(pattern)} {Str(input)}", "Just (" + expected + ")");

    private static Scenario Contains(string name, string pattern, string input, bool expected) =>
        new(name, $"contains {Str(pattern)} {Str(input)}", expected ? "Just True" : "Just False");

    private static readonly Scenario[] s_scenarios =
        [
            Find(
                "Literal search and match numbering",
                "cat",
                "cat dog cat",
                """[ ("cat", 0, (1, [])), ("cat", 8, (2, [])) ]"""),
            Find("Empty input and pattern", "", "", """[ ("", 0, (1, [])) ]"""),
            Find("Empty matches preserve upstream find termination", "", "ab", """[ ("", 0, (1, [])) ]"""),
            Find("Greedy repetition backtracks", "a*ab", "aaab", """[ ("aaab", 0, (1, [])) ]"""),
            Find(
                "Reluctant repetition",
                "a+?",
                "aaa",
                """[ ("a", 0, (1, [])), ("a", 1, (2, [])), ("a", 2, (3, [])) ]"""),
            Find("Reluctant repetition expands for the suffix", "a+?b", "aaab", """[ ("aaab", 0, (1, [])) ]"""),
            Find("Reluctant optional", "a??a", "aa", """[ ("a", 0, (1, [])), ("a", 1, (2, [])) ]"""),
            Find(
                "Alternation backtracks into following sequence",
                "(a|ab)c",
                "abc",
                """[ ("abc", 0, (1, [ Just "ab" ])) ]"""),
            Find("Alternative priority", "a|ab", "ab", """[ ("a", 0, (1, [])) ]"""),
            Find(
                "Nested capture numbering",
                "((a)(b))",
                "ab",
                """[ ("ab", 0, (1, [ Just "ab", Just "a", Just "b" ])) ]"""),
            Find("Non-capturing group", "(?:a|b)+", "abb", """[ ("abb", 0, (1, [])) ]"""),
            Find("Absent optional capture", "(a)?b", "b", """[ ("b", 0, (1, [ Nothing ])) ]"""),
            Find("Empty capture follows upstream behavior", "()a", "a", """[ ("a", 0, (1, [ Nothing ])) ]"""),
            Find(
                "Repeated captures are reset for the final iteration",
                "(a(b)?)+",
                "aba",
                """[ ("aba", 0, (1, [ Just "a", Nothing ])) ]"""),
            Find(
                "Nullable repetitions terminate without losing captures",
                "(a?)*",
                "a",
                """[ ("a", 0, (1, [ Just "a" ])) ]"""),
            Find("Required nullable repetition", "()+", "", """[ ("", 0, (1, [ Nothing ])) ]"""),
            Find(
                "Alternatives containing empty repetitions terminate",
                "(a*)*b",
                "aaab",
                """[ ("aaab", 0, (1, [ Just "aaa" ])) ]"""),
            Find(
                "Empty alternative",
                "(|within)\\s*([\\d\\.]+\\s*[km]+)",
                "within 12.5 km",
                """[ ("within 12.5 km", 0, (1, [ Just "within", Just "12.5 km" ])) ]"""),
            Find("Character ranges", "[A-Za-z_][A-Za-z0-9_]*", "9alpha_2", """[ ("alpha_2", 1, (1, [])) ]"""),
            Find("Negated class", "[^a-c]+", "abXYZc", """[ ("XYZ", 2, (1, [])) ]"""),
            Find("Empty class never matches", "[]", "x", "[]"),
            Find("Negated empty class matches line terminators", "[^]+", "x\n", """[ ("x\n", 0, (1, [])) ]"""),
            Find(
                "Leading and trailing class hyphens",
                "[-a]+|[b-]+",
                "--a b-",
                """[ ("--a", 0, (1, [])), ("b-", 4, (2, [])) ]"""),
            Find("Escaped class punctuation", "[\\]\\-]+", "]-", """[ ("]-", 0, (1, [])) ]"""),
            Find("Digits are ASCII", "\\d+", "\u0661\u0662 12", """[ ("12", 3, (1, [])) ]"""),
            Find("Complementary shorthand classes", "[\\d\\D]+", "a1\n", """[ ("a1\n", 0, (1, [])) ]"""),
            Find("Word shorthand", "\\w+", "-a_2-", """[ ("a_2", 1, (1, [])) ]"""),
            Find("Non-word shorthand", "\\W+", "a_2-!", """[ ("-!", 3, (1, [])) ]"""),
            Find(
                "Whitespace includes ECMAScript separators",
                "\\s+",
                "\u00A0\u202F\uFEFF",
                """[ ("\u{00A0}\u{202F}\u{FEFF}", 0, (1, [])) ]"""),
            Find("Non-whitespace", "\\S+", " \tword\n", """[ ("word", 2, (1, [])) ]"""),
            Find(
                "Control escapes",
                "\\t\\r\\n\\v\\f[\\b]\\0",
                "\t\r\n\v\f\b\0",
                """[ ("\t\r\n\u{000B}\u{000C}\u{0008}\u{0000}", 0, (1, [])) ]"""),
            Find("Identity escapes for punctuation", "\\,\\:\\/\\.", ",:/.", """[ (",:/.", 0, (1, [])) ]"""),
            Find(
                "Dot excludes all four line terminators",
                ".",
                "a\nb\rc\u2028d\u2029",
                """[ ("a", 0, (1, [])), ("b", 2, (2, [])), ("c", 4, (3, [])), ("d", 6, (4, [])) ]"""),
            Find("Dot consumes a full non-BMP scalar", ".", "\U0001F600", """[ ("\u{1F600}", 0, (1, [])) ]"""),
            Find(
                "Indices after non-BMP scalars",
                "(x)",
                "\U0001F600x\U0001F600x",
                """[ ("x", 1, (1, [ Just "x" ])), ("x", 3, (2, [ Just "x" ])) ]"""),
            Contains("Absolute anchors", "^a$", "a", true),
            Contains("Start anchor is not relative to the search cursor", "^a", "ba", false),
            Contains("Non-multiline end does not ignore a final newline", "a$", "a\n", false),
            new(
                "Multiline anchors",
                """Regex.fromStringWith { caseInsensitive = False, multiline = True } "^a$" |> Maybe.map (\r -> summarize (Regex.find r "b\na\rx\u{2028}a\u{2029}"))""",
                """Just [ ("a", 2, (1, [])), ("a", 6, (2, [])) ]"""),
            new(
                "Case-insensitive option is explicitly unsupported",
                """Regex.fromStringWith { caseInsensitive = True, multiline = False } "a" |> Maybe.map (\_ -> True)""",
                "Nothing"),
            new("Never contains", """Regex.contains Regex.never "" """, "False"),
            new("Never find", """Regex.find Regex.never "abc" """, "[]"),
            new(
                "Bounded find",
                """Regex.fromString "." |> Maybe.map (\r -> summarize (Regex.findAtMost 2 r "abcd"))""",
                """Just [ ("a", 0, (1, [])), ("b", 1, (2, [])) ]"""),
            new(
                "Zero find limit",
                """Regex.fromString "." |> Maybe.map (\r -> Regex.findAtMost 0 r "abc")""",
                "Just []"),
            new(
                "Negative find limit",
                """Regex.fromString "." |> Maybe.map (\r -> Regex.findAtMost -1 r "abc")""",
                "Just []"),
            new(
                "Replacement invokes closures with captures and numbering",
                """Regex.fromString "(\\d)" |> Maybe.map (\r -> Regex.replace r (\m -> "[" ++ String.fromInt m.number ++ ":" ++ m.match ++ "]") "a1b2")""",
                """Just "a[1:1]b[2:2]" """),
            new(
                "Replacement limit preserves untouched suffix",
                """Regex.fromString "\\d" |> Maybe.map (\r -> Regex.replaceAtMost 1 r (\_ -> "X") "a1b2")""",
                """Just "aXb2" """),
            new(
                "Replacement negative limit",
                """Regex.fromString "." |> Maybe.map (\r -> Regex.replaceAtMost -1 r (\_ -> "X") "abc")""",
                """Just "abc" """),
            new(
                "Empty replacement advances by scalars",
                """Regex.fromString "" |> Maybe.map (\r -> Regex.replace r (\m -> String.fromInt m.index) "\u{1F600}a")""",
                """Just "0\u{1F600}1a2" """),
            new(
                "Empty replacement input",
                """Regex.fromString "" |> Maybe.map (\r -> Regex.replace r (\_ -> "X") "")""",
                """Just "X" """),
            new(
                "Replacement callback can invoke the same regex",
                """Regex.fromString "(a)" |> Maybe.map (\r -> Regex.replace r (\m -> if Regex.contains r m.match then "X" else "Y") "aa")""",
                """Just "XX" """),
            new(
                "Split does not emit delimiter captures",
                """Regex.fromString "(,+)" |> Maybe.map (\r -> Regex.split r ",a,,b,")""",
                """Just [ "", "a", "b", "" ]"""),
            new(
                "Split limit counts delimiters",
                """Regex.fromString "," |> Maybe.map (\r -> Regex.splitAtMost 1 r "a,b,c")""",
                """Just [ "a", "b,c" ]"""),
            new(
                "Zero split limit",
                """Regex.fromString "," |> Maybe.map (\r -> Regex.splitAtMost 0 r "a,b")""",
                """Just [ "a,b" ]"""),
            new(
                "Negative split limit preserves upstream behavior",
                """Regex.fromString "," |> Maybe.map (\r -> Regex.splitAtMost -1 r "a,b")""",
                """Just [ "a", "b" ]"""),
            new(
                "Empty split makes finite scalar progress",
                """Regex.fromString "" |> Maybe.map (\r -> Regex.split r "\u{1F600}a")""",
                """Just [ "", "\u{1F600}", "a", "" ]"""),
            new("Never split", """Regex.split Regex.never "abc" """, """[ "abc" ]"""),
            new("Never replace", """Regex.replace Regex.never (\_ -> "X") "abc" """, """ "abc" """),
            Find(
                "Sanderling and production bots optimal range",
                "Optimal range (|within)\\s*([\\d\\.]+\\s*[km]+)",
                "Optimal range within 12.5 km",
                """[ ("Optimal range within 12.5 km", 0, (1, [ Just "within", Just "12.5 km" ])) ]"""),
            Find(
                "Optimal range without within",
                "Optimal range (|within)\\s*([\\d\\.]+\\s*[km]+)",
                "Optimal range 500 m",
                """[ ("Optimal range 500 m", 0, (1, [ Nothing, Just "500 m" ])) ]"""),
            Contains(
                "Production mining tooltip Unicode unit",
                "\\d\\s*m[\u00B33]\\s*\\/\\s*s",
                "125 m\u00B3 / s",
                true),
            Contains("Production mining tooltip ASCII unit", "\\d\\s*m[\u00B33]\\s*\\/\\s*s", "125 m3/s", true),
            Contains(
                "Production mining tooltip rejects unrelated text",
                "\\d\\s*m[\u00B33]\\s*\\/\\s*s",
                "125 km",
                false),
            Find(
                "GLSL declaration with absent precision capture",
                "^\\s*(uniform|attribute|varying)\\s+(highp\\s+|mediump\\s+|lowp\\s+)?([A-Za-z_][A-Za-z0-9_]*)\\s+([\\s\\S]+)",
                "uniform vec3 position",
                """[ ("uniform vec3 position", 0, (1, [ Just "uniform", Nothing, Just "vec3", Just "position" ])) ]"""),
            Find(
                "GLSL declaration with precision and multiline declarators",
                "^\\s*(uniform|attribute|varying)\\s+(highp\\s+|mediump\\s+|lowp\\s+)?([A-Za-z_][A-Za-z0-9_]*)\\s+([\\s\\S]+)",
                " attribute highp vec2 uv,\nuv2",
                """[ (" attribute highp vec2 uv,\nuv2", 0, (1, [ Just "attribute", Just "highp ", Just "vec2", Just "uv,\nuv2" ])) ]"""),
            Contains("GLSL variable name", "^[A-Za-z_][A-Za-z0-9_]*$", "u_position2", true),
            Contains("GLSL invalid variable name", "^[A-Za-z_][A-Za-z0-9_]*$", "2position", false),
            new(
                "GLSL line and reluctant block comments",
                """Regex.fromString "//[^\\n]*|/\\*[\\s\\S]*?\\*/" |> Maybe.map (\r -> Regex.replace r (\_ -> " ") "a/* first */b/* second\nline */c// tail\nend")""",
                """Just "a b c \nend" """),
            new(
                "Regex values serialize and can be reused",
                """Regex.fromString "(a)" |> Maybe.map (\r -> (Regex.find r "a" == Regex.find r "a", Regex.contains r "b"))""",
                "Just (True, False)"),
            .. new[] { "(", ")", "[", "[z-a]", "\\", "*a", "a**", "^*", "a{3}", "(?=a)", "(?!a)", "(?<=a)", "(a)\\1", "\\b", "\\x41", "\\u0041", "\\01" }
            .Select(
                pattern =>
                new Scenario(
                    "Invalid or unsupported: " + pattern,
                    $"Regex.fromString {Str(pattern)} |> Maybe.map (\\_ -> True)",
                    "Nothing")),
        ];

    public static IEnumerable<object[]> Scenarios() =>
        s_scenarios.Select((scenario, index) => new object[] { index, scenario.Name });

    private static string ModuleSource =>
        """
        module RegexScenarios exposing (..)

        import Regex
        import List
        import Maybe
        import String

        summarize matches =
            List.map (\m -> (m.match, m.index, (m.number, m.submatches))) matches

        find pattern input =
            Regex.fromString pattern |> Maybe.map (\regex -> summarize (Regex.find regex input))

        contains pattern input =
            Regex.fromString pattern |> Maybe.map (\regex -> Regex.contains regex input)

        buildRegex pattern =
            Regex.fromString pattern

        useRegex regex input =
            Regex.contains regex input

        """ + "\n" +
        string.Join(
            "\n\n",
            s_scenarios.Select(
                (scenario, index) =>
                $"scenario{index} _ =\n    ({scenario.Expression}) == ({scenario.Expected})"));

    private static readonly Lazy<ElmInteractiveEnvironment.ParsedInteractiveEnvironment> s_environment =
        new(
            () =>
            {
                var tree =
                    BundledFiles.ElmKernelModulesDefault.Value
                    .SetNodeAtPathSorted(["RegexScenarios.elm"], FileTree.File(Encoding.UTF8.GetBytes(ModuleSource)));

                var compiled =
                    ElmCompilerTestHelper.CompileInteractiveEnvironmentFromFiles(
                        tree,
                        rootFilePaths: [new[] { "RegexScenarios.elm" }],
                        syntaxOptimization: new ElmSyntaxOptimizationConfig.SyntaxOptimizationDisabled())
                    .Extract(error => throw new InvalidOperationException(error));

                return
                    ElmInteractiveEnvironment.ParseInteractiveEnvironment(compiled.compiledEnvValue)
                    .Extract(error => throw new InvalidOperationException(error));
            });

    private static readonly Lazy<Core.Elm.ElmSyntax.ElmSyntaxInterpreter.Prepared> s_prepared =
        new(
            () => InterpreterTestHelper.PrepareModulesFromSources(
                [
                    .. new[] { "Basics.elm", "List.elm", "Maybe.elm", "String.elm", "Char.elm", "Dict.elm", "Set.elm", "Tuple.elm", "Regex.elm" }
                    .Select(InterpreterTestHelper.LoadKernelModuleSource),
                    ModuleSource,
                ]));

    private static readonly Core.Interpreter.IntermediateVM.PineVM s_vm =
        Core.Interpreter.IntermediateVM.PineVM.CreateCustom(
            evalCache: null,
            evaluationConfigDefault: null,
            reportFunctionApplication: null,
            compilationEnvClasses: null,
            disableReductionInCompilation: true,
            selectPrecompiled: null,
            skipInlineForExpression: _ => false,
            enableTailRecursionOptimization: false,
            parseCache: null,
            precompiledLeaves: new Dictionary<PineValue, PrecompiledLeaf>(),
            reportEnterPrecompiledLeaf: null,
            reportExitPrecompiledLeaf: null,
            optimizationParametersSerial: null,
            cacheFileStore: null);

    [Theory]
    [MemberData(nameof(Scenarios))]
    public void Regex_scenarios_execute_as_Pine_without_precompiled_leaves(int index, string name)
    {
        var function =
            s_environment.Value.Modules.Single(module => module.moduleName == "RegexScenarios")
            .moduleContent.FunctionDeclarations["scenario" + index];

        CoreLibraryTestHelper.ApplyUnary(function, ElmValue.Integer(0), s_vm)
            .Should().Be(ElmValue.TrueValue, "{0}", name);
    }

    [Theory]
    [MemberData(nameof(Scenarios))]
    public void Regex_scenarios_execute_from_Elm_source_without_builtins(int index, string name)
    {
        InterpreterTestHelper.EvaluateInModulesWithoutBuiltinsToPineValue(
            $"RegexScenarios.scenario{index} 0",
            s_prepared.Value)
            .Should().Be(ElmValueEncoding.ElmValueAsPineValue(ElmValue.TrueValue), "{0}", name);
    }

    [Fact]
    public void Regex_values_roundtrip_as_plain_Pine_data_between_evaluations()
    {
        var functions =
            s_environment.Value.Modules.Single(module => module.moduleName == "RegexScenarios")
            .moduleContent.FunctionDeclarations;

        var parsed =
            CoreLibraryTestHelper.ApplyUnary(
                functions["buildRegex"],
                ElmValue.StringInstance("(a|\u00B3)+"),
                s_vm)
            .Should().BeOfType<ElmValue.ElmTag>().Which;

        parsed.TagName.Should().Be("Just");

        var encoded = ElmValueEncoding.ElmValueAsPineValue(parsed.Arguments.Single());

        var restored =
            ElmValueEncoding.PineValueAsElmValue(encoded, null, null)
            .Extract(error => throw new InvalidOperationException(error));

        CoreLibraryTestHelper.ApplyBinary(
            functions["useRegex"],
            restored,
            ElmValue.StringInstance("a\u00B3a"),
            s_vm)
            .Should().Be(ElmValue.TrueValue);
    }

    [Fact]
    public void Regex_scenarios_execute_with_default_compiler_optimizations()
    {
        var tree =
            BundledFiles.ElmKernelModulesDefault.Value
            .SetNodeAtPathSorted(["RegexScenarios.elm"], FileTree.File(Encoding.UTF8.GetBytes(ModuleSource)));

        var compiled =
            ElmCompilerTestHelper.CompileInteractiveEnvironmentFromFiles(
                tree,
                rootFilePaths: [new[] { "RegexScenarios.elm" }])
            .Extract(error => throw new InvalidOperationException(error));

        var environment =
            ElmInteractiveEnvironment.ParseInteractiveEnvironment(compiled.compiledEnvValue)
            .Extract(error => throw new InvalidOperationException(error));

        var functions =
            environment.Modules.Single(module => module.moduleName == "RegexScenarios")
            .moduleContent.FunctionDeclarations;

        for (var index = 0; index < s_scenarios.Length; index++)
        {
            CoreLibraryTestHelper.ApplyUnary(functions["scenario" + index], ElmValue.Integer(0), s_vm)
                .Should().Be(ElmValue.TrueValue, "{0}", s_scenarios[index].Name);
        }
    }

    [Fact]
    public async Task Regex_scenarios_execute_through_namespaced_package_substitution()
    {
        var tree =
            FileTree.FromSetOfFilesWithStringPath(
                [
                    (new[] { "elm.json" },
                    (ReadOnlyMemory<byte>)
                    """
                    {
                        "type": "application",
                        "source-directories": ["src"],
                        "elm-version": "0.19.1",
                        "dependencies": {
                            "direct": {"elm/core": "1.0.5", "elm/regex": "1.0.0"},
                            "indirect": {}
                        },
                        "test-dependencies": {"direct": {}, "indirect": {}}
                    }
                    """u8.ToArray()),
                    (new[] { "src", "RegexScenarios.elm" }, (ReadOnlyMemory<byte>)Encoding.UTF8.GetBytes(ModuleSource)),
                ]);

        var build =
            await ElmResolvedBuildPreparation.PrepareAsync(
                tree,
                ["elm.json"],
                [new[] { "src", "RegexScenarios.elm" }],
                ElmPackageSubstitutions.DefaultBuild.Value with { Offline = true });

        var compiled =
            ElmCompiler.CompileResolvedEnvironment(
                build,
                rootDeclarations:
                [
                    .. s_scenarios.Select((_, index) => DeclQualifiedName.Create(["RegexScenarios"], "scenario" + index))
                ])
            .Extract(error => throw new InvalidOperationException(error));

        var environment =
            ElmInteractiveEnvironment.ParseInteractiveEnvironment(compiled.compiledEnvValue)
            .Extract(error => throw new InvalidOperationException(error));

        var functions =
            environment.Modules.Single(module => module.moduleName == "RegexScenarios")
            .moduleContent.FunctionDeclarations;

        for (var index = 0; index < s_scenarios.Length; index++)
        {
            CoreLibraryTestHelper.ApplyUnary(functions["scenario" + index], ElmValue.Integer(0), s_vm)
                .Should().Be(ElmValue.TrueValue, "{0}", s_scenarios[index].Name);
        }
    }
}
