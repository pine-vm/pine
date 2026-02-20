using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Elm.ElmInElm;
using System;
using System.Collections.Generic;
using System.Linq;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.CoreLibraryModule;

public class CoreStringFunctionTests
{
    private static readonly Lazy<ElmInteractiveEnvironment.ParsedInteractiveEnvironment> s_kernelEnv =
        new(
            () =>
            {
                var kernelModulesTree =
                    BundledFiles.CompilerSourceContainerFilesDefault.Value
                    .GetNodeAtPath(["elm-kernel-modules"])
                    ?? throw new Exception("Did not find elm-kernel-modules");

                var rootFilePaths =
                    kernelModulesTree.EnumerateFilesTransitive()
                    .Where(
                        b =>
                        b.path[^1].Equals("String.elm", StringComparison.OrdinalIgnoreCase))
                    .Select(b => (IReadOnlyList<string>)b.path)
                    .ToList();

                var compiledEnv =
                    ElmCompiler.CompileInteractiveEnvironment(
                        kernelModulesTree,
                        rootFilePaths: rootFilePaths,
                        disableInlining: false)
                    .Extract(err => throw new Exception("Failed compiling elm-kernel-modules: " + err));

                return
                    ElmInteractiveEnvironment.ParseInteractiveEnvironment(compiledEnv)
                    .Extract(err => throw new Exception("Failed parsing interactive environment: " + err));
            });

    private static PineValue GetStringFunction(string name) =>
        s_kernelEnv.Value.Modules
        .First(m => m.moduleName is "String")
        .moduleContent.FunctionDeclarations[name];

    private static PineValue GetCharFunction(string name) =>
        s_kernelEnv.Value.Modules
        .First(m => m.moduleName is "Char")
        .moduleContent.FunctionDeclarations[name];

    private static ElmValue ApplyUnary(PineValue functionValue, ElmValue argument) =>
        CoreLibraryTestHelper.ApplyDirectUnary(functionValue, argument);

    private static ElmValue ApplyBinary(
        PineValue functionValue, ElmValue arg1, ElmValue arg2) =>
        CoreLibraryTestHelper.ApplyGeneric(functionValue, [arg1, arg2]);

    private static ElmValue ApplyTernary(
        PineValue functionValue, ElmValue arg1, ElmValue arg2, ElmValue arg3) =>
        CoreLibraryTestHelper.ApplyGeneric(functionValue, [arg1, arg2, arg3]);

    private static ElmValue S(string s) =>
        ElmValue.StringInstance(s);

    private static ElmValue I(long i) =>
        ElmValue.Integer(i);

    private static ElmValue C(char c) =>
        ElmValue.CharInstance(c);

    private static ElmValue JustOf(ElmValue inner) =>
        ElmValue.TagInstance("Just", [inner]);

    private static readonly ElmValue s_nothing =
        ElmValue.TagInstance("Nothing", []);

    private static ElmValue ElmList(params ElmValue[] items) =>
        ElmValue.ListInstance([.. items]);

    private static PineValue ToPine(ElmValue value) =>
        ElmValueEncoding.ElmValueAsPineValue(value);

    private static ElmValue FromPine(PineValue value) =>
        ElmValueEncoding.PineValueAsElmValue(value, null, null)
        .Extract(err => throw new Exception("Failed decode as Elm value: " + err));

    private static ElmValue ApplyWithPineArgs(
        PineValue functionValue, params PineValue[] pineArgs) =>
        FromPine(CoreLibraryTestHelper.ApplyGenericPine(functionValue, pineArgs));

    // ========== Tests for isEmpty ==========
    // isEmpty "" == True
    // isEmpty "the world" == False

    [Fact]
    public void IsEmpty_empty_string()
    {
        var result = ApplyUnary(GetStringFunction("isEmpty"), S(""));
        result.Should().Be(ElmValue.TrueValue);
    }

    [Fact]
    public void IsEmpty_non_empty_string()
    {
        var result = ApplyUnary(GetStringFunction("isEmpty"), S("the world"));
        result.Should().Be(ElmValue.FalseValue);
    }

    // ========== Tests for length ==========
    // length "innumerable" == 11
    // length "" == 0

    [Fact]
    public void Length_innumerable()
    {
        var result = ApplyUnary(GetStringFunction("length"), S("innumerable"));
        result.Should().Be(I(11));
    }

    [Fact]
    public void Length_empty()
    {
        var result = ApplyUnary(GetStringFunction("length"), S(""));
        result.Should().Be(I(0));
    }

    // ========== Tests for reverse ==========
    // reverse "stressed" == "desserts"

    [Fact]
    public void Reverse_stressed()
    {
        var result = ApplyUnary(GetStringFunction("reverse"), S("stressed"));
        result.Should().Be(S("desserts"));
    }

    // ========== Tests for repeat ==========
    // repeat 3 "ha" == "hahaha"

    [Fact]
    public void Repeat_3_ha()
    {
        var result = ApplyBinary(GetStringFunction("repeat"), I(3), S("ha"));
        result.Should().Be(S("hahaha"));
    }

    // ========== Tests for replace ==========
    // replace "." "-" "Json.Decode.succeed" == "Json-Decode-succeed"
    // replace "," "/" "a,b,c,d,e"           == "a/b/c/d/e"

    [Fact]
    public void Replace_dot_with_dash()
    {
        var result =
            ApplyTernary(
                GetStringFunction("replace"),
                S("."),
                S("-"),
                S("Json.Decode.succeed"));

        result.Should().Be(S("Json-Decode-succeed"));
    }

    [Fact]
    public void Replace_comma_with_slash()
    {
        var result =
            ApplyTernary(
                GetStringFunction("replace"),
                S(","),
                S("/"),
                S("a,b,c,d,e"));

        result.Should().Be(S("a/b/c/d/e"));
    }

    // ========== Tests for append ==========
    // append "butter" "fly" == "butterfly"

    [Fact]
    public void Append_butterfly()
    {
        var result =
            ApplyBinary(GetStringFunction("append"), S("butter"), S("fly"));

        result.Should().Be(S("butterfly"));
    }

    // ========== Tests for concat ==========
    // concat ["never","the","less"] == "nevertheless"

    [Fact]
    public void Concat_nevertheless()
    {
        var result =
            ApplyUnary(
                GetStringFunction("concat"),
                ElmList(S("never"), S("the"), S("less")));

        result.Should().Be(S("nevertheless"));
    }

    // ========== Tests for split ==========
    // split "," "cat,dog,cow"        == ["cat","dog","cow"]
    // split "/" "home/evan/Desktop/" == ["home","evan","Desktop", ""]

    [Fact]
    public void Split_comma()
    {
        var result =
            ApplyBinary(GetStringFunction("split"), S(","), S("cat,dog,cow"));

        result.Should().Be(ElmList(S("cat"), S("dog"), S("cow")));
    }

    [Fact]
    public void Split_slash_trailing()
    {
        var result =
            ApplyBinary(GetStringFunction("split"), S("/"), S("home/evan/Desktop/"));

        result.Should().Be(ElmList(S("home"), S("evan"), S("Desktop"), S("")));
    }

    // ========== Tests for join ==========
    // join "a" ["H","w","ii","n"]        == "Hawaiian"
    // join " " ["cat","dog","cow"]       == "cat dog cow"
    // join "/" ["home","evan","Desktop"] == "home/evan/Desktop"

    [Fact]
    public void Join_Hawaiian()
    {
        var result =
            ApplyBinary(
                GetStringFunction("join"),
                S("a"),
                ElmList(S("H"), S("w"), S("ii"), S("n")));

        result.Should().Be(S("Hawaiian"));
    }

    [Fact]
    public void Join_space()
    {
        var result =
            ApplyBinary(
                GetStringFunction("join"),
                S(" "),
                ElmList(S("cat"), S("dog"), S("cow")));

        result.Should().Be(S("cat dog cow"));
    }

    [Fact]
    public void Join_slash()
    {
        var result =
            ApplyBinary(
                GetStringFunction("join"),
                S("/"),
                ElmList(S("home"), S("evan"), S("Desktop")));

        result.Should().Be(S("home/evan/Desktop"));
    }

    // ========== Tests for words ==========
    // words "How are \t you? \n Good?" == ["How","are","you?","Good?"]

    [Fact]
    public void Words_split_whitespace()
    {
        var result =
            ApplyUnary(
                GetStringFunction("words"),
                S("How are \t you? \n Good?"));

        result.Should().Be(ElmList(S("How"), S("are"), S("you?"), S("Good?")));
    }

    // ========== Tests for lines ==========
    // lines "How are you?\nGood?" == ["How are you?", "Good?"]

    [Fact]
    public void Lines_split_newline()
    {
        var result =
            ApplyUnary(
                GetStringFunction("lines"),
                S("How are you?\nGood?"));

        result.Should().Be(ElmList(S("How are you?"), S("Good?")));
    }

    // ========== Tests for slice ==========
    // slice  7  9 "snakes on a plane!" == "on"
    // slice  0  6 "snakes on a plane!" == "snakes"
    // slice  0 -7 "snakes on a plane!" == "snakes on a"
    // slice -6 -1 "snakes on a plane!" == "plane"

    [Fact]
    public void Slice_7_9()
    {
        var result =
            ApplyTernary(
                GetStringFunction("slice"),
                I(7),
                I(9),
                S("snakes on a plane!"));

        result.Should().Be(S("on"));
    }

    [Fact]
    public void Slice_0_6()
    {
        var result =
            ApplyTernary(
                GetStringFunction("slice"),
                I(0),
                I(6),
                S("snakes on a plane!"));

        result.Should().Be(S("snakes"));
    }

    [Fact]
    public void Slice_0_neg7()
    {
        var result =
            ApplyTernary(
                GetStringFunction("slice"),
                I(0),
                I(-7),
                S("snakes on a plane!"));

        result.Should().Be(S("snakes on a"));
    }

    [Fact]
    public void Slice_neg6_neg1()
    {
        var result =
            ApplyTernary(
                GetStringFunction("slice"),
                I(-6),
                I(-1),
                S("snakes on a plane!"));

        result.Should().Be(S("plane"));
    }

    // ========== Tests for left ==========
    // left 2 "Mulder" == "Mu"

    [Fact]
    public void Left_2()
    {
        var result =
            ApplyBinary(GetStringFunction("left"), I(2), S("Mulder"));

        result.Should().Be(S("Mu"));
    }

    // ========== Tests for right ==========
    // right 2 "Scully" == "ly"

    [Fact]
    public void Right_2()
    {
        var result =
            ApplyBinary(GetStringFunction("right"), I(2), S("Scully"));

        result.Should().Be(S("ly"));
    }

    // ========== Tests for dropLeft ==========
    // dropLeft 2 "The Lone Gunmen" == "e Lone Gunmen"

    [Fact]
    public void DropLeft_2()
    {
        var result =
            ApplyBinary(
                GetStringFunction("dropLeft"),
                I(2),
                S("The Lone Gunmen"));

        result.Should().Be(S("e Lone Gunmen"));
    }

    // ========== Tests for dropRight ==========
    // dropRight 2 "Cigarette Smoking Man" == "Cigarette Smoking M"

    [Fact]
    public void DropRight_2()
    {
        var result =
            ApplyBinary(
                GetStringFunction("dropRight"),
                I(2),
                S("Cigarette Smoking Man"));

        result.Should().Be(S("Cigarette Smoking M"));
    }

    // ========== Tests for contains ==========
    // contains "the" "theory" == True
    // contains "hat" "theory" == False
    // contains "THE" "theory" == False

    [Fact]
    public void Contains_the_in_theory()
    {
        var result =
            ApplyBinary(GetStringFunction("contains"), S("the"), S("theory"));

        result.Should().Be(ElmValue.TrueValue);
    }

    [Fact]
    public void Contains_hat_in_theory()
    {
        var result =
            ApplyBinary(GetStringFunction("contains"), S("hat"), S("theory"));

        result.Should().Be(ElmValue.FalseValue);
    }

    [Fact]
    public void Contains_THE_in_theory()
    {
        var result =
            ApplyBinary(GetStringFunction("contains"), S("THE"), S("theory"));

        result.Should().Be(ElmValue.FalseValue);
    }

    // ========== Tests for startsWith ==========
    // startsWith "the" "theory" == True
    // startsWith "ory" "theory" == False

    [Fact]
    public void StartsWith_the()
    {
        var result =
            ApplyBinary(
                GetStringFunction("startsWith"),
                S("the"),
                S("theory"));

        result.Should().Be(ElmValue.TrueValue);
    }

    [Fact]
    public void StartsWith_ory()
    {
        var result =
            ApplyBinary(
                GetStringFunction("startsWith"),
                S("ory"),
                S("theory"));

        result.Should().Be(ElmValue.FalseValue);
    }

    // ========== Tests for endsWith ==========
    // endsWith "the" "theory" == False
    // endsWith "ory" "theory" == True

    [Fact]
    public void EndsWith_the()
    {
        var result =
            ApplyBinary(
                GetStringFunction("endsWith"),
                S("the"),
                S("theory"));

        result.Should().Be(ElmValue.FalseValue);
    }

    [Fact]
    public void EndsWith_ory()
    {
        var result =
            ApplyBinary(
                GetStringFunction("endsWith"),
                S("ory"),
                S("theory"));

        result.Should().Be(ElmValue.TrueValue);
    }

    // ========== Tests for indexes ==========
    // indexes "i" "Mississippi"   == [1,4,7,10]
    // indexes "ss" "Mississippi"  == [2,5]
    // indexes "needle" "haystack" == []

    [Fact]
    public void Indexes_i_Mississippi()
    {
        var result =
            ApplyBinary(
                GetStringFunction("indexes"),
                S("i"),
                S("Mississippi"));

        result.Should().Be(ElmList(I(1), I(4), I(7), I(10)));
    }

    [Fact]
    public void Indexes_ss_Mississippi()
    {
        var result =
            ApplyBinary(
                GetStringFunction("indexes"),
                S("ss"),
                S("Mississippi"));

        result.Should().Be(ElmList(I(2), I(5)));
    }

    [Fact]
    public void Indexes_needle_haystack()
    {
        var result =
            ApplyBinary(
                GetStringFunction("indexes"),
                S("needle"),
                S("haystack"));

        result.Should().Be(ElmList());
    }

    // ========== Tests for indices (alias for indexes) ==========

    [Fact]
    public void Indices_i_Mississippi()
    {
        var result =
            ApplyBinary(
                GetStringFunction("indices"),
                S("i"),
                S("Mississippi"));

        result.Should().Be(ElmList(I(1), I(4), I(7), I(10)));
    }

    // ========== Tests for toInt ==========
    // String.toInt "123" == Just 123
    // String.toInt "-42" == Just -42
    // String.toInt "3.1" == Nothing
    // String.toInt "31a" == Nothing

    [Fact]
    public void ToInt_123()
    {
        var result = ApplyUnary(GetStringFunction("toInt"), S("123"));
        result.Should().Be(JustOf(I(123)));
    }

    [Fact]
    public void ToInt_neg42()
    {
        var result = ApplyUnary(GetStringFunction("toInt"), S("-42"));
        result.Should().Be(JustOf(I(-42)));
    }

    [Fact]
    public void ToInt_3_1()
    {
        var result = ApplyUnary(GetStringFunction("toInt"), S("3.1"));
        result.Should().Be(s_nothing);
    }

    [Fact]
    public void ToInt_31a()
    {
        var result = ApplyUnary(GetStringFunction("toInt"), S("31a"));
        result.Should().Be(s_nothing);
    }

    // ========== Tests for fromInt ==========
    // String.fromInt 123 == "123"
    // String.fromInt -42 == "-42"

    [Fact]
    public void FromInt_123()
    {
        var result = ApplyUnary(GetStringFunction("fromInt"), I(123));
        result.Should().Be(S("123"));
    }

    [Fact]
    public void FromInt_neg42()
    {
        var result = ApplyUnary(GetStringFunction("fromInt"), I(-42));
        result.Should().Be(S("-42"));
    }

    // ========== Tests for toFloat ==========
    // String.toFloat "123" == Just 123.0
    // String.toFloat "-42" == Just -42.0
    // String.toFloat "3.1" == Just 3.1
    // String.toFloat "31a" == Nothing

    [Fact]
    public void ToFloat_123()
    {
        var result = ApplyUnary(GetStringFunction("toFloat"), S("123"));
        var justTag = result as ElmValue.ElmTag;
        justTag.Should().NotBeNull();
        justTag!.TagName.Should().Be("Just");
    }

    [Fact]
    public void ToFloat_neg42()
    {
        var result = ApplyUnary(GetStringFunction("toFloat"), S("-42"));
        var justTag = result as ElmValue.ElmTag;
        justTag.Should().NotBeNull();
        justTag!.TagName.Should().Be("Just");
    }

    [Fact]
    public void ToFloat_3_1()
    {
        var result = ApplyUnary(GetStringFunction("toFloat"), S("3.1"));
        var justTag = result as ElmValue.ElmTag;
        justTag.Should().NotBeNull();
        justTag!.TagName.Should().Be("Just");
    }

    [Fact]
    public void ToFloat_31a()
    {
        var result = ApplyUnary(GetStringFunction("toFloat"), S("31a"));
        result.Should().Be(s_nothing);
    }

    // ========== Tests for fromFloat ==========
    // String.fromFloat 123 == "123"
    // String.fromFloat -42 == "-42"
    // String.fromFloat 3.9 == "3.9"

    [Fact]
    public void FromFloat_123()
    {
        var result = ApplyUnary(GetStringFunction("fromFloat"), I(123));
        result.Should().Be(S("123"));
    }

    [Fact]
    public void FromFloat_neg42()
    {
        var result = ApplyUnary(GetStringFunction("fromFloat"), I(-42));
        result.Should().Be(S("-42"));
    }

    // ========== Tests for toList ==========
    // toList "abc" == ['a','b','c']

    [Fact]
    public void ToList_abc()
    {
        var result = ApplyUnary(GetStringFunction("toList"), S("abc"));
        result.Should().Be(ElmList(C('a'), C('b'), C('c')));
    }

    // ========== Tests for fromList ==========
    // fromList ['a','b','c'] == "abc"

    [Fact]
    public void FromList_abc()
    {
        var result =
            ApplyUnary(
                GetStringFunction("fromList"),
                ElmList(C('a'), C('b'), C('c')));

        result.Should().Be(S("abc"));
    }

    // ========== Tests for fromChar ==========
    // fromChar 'a' == "a"

    [Fact]
    public void FromChar_a()
    {
        var result = ApplyUnary(GetStringFunction("fromChar"), C('a'));
        result.Should().Be(S("a"));
    }

    // ========== Tests for cons ==========
    // cons 'T' "he truth is out there" == "The truth is out there"

    [Fact]
    public void Cons_T()
    {
        var result =
            ApplyBinary(
                GetStringFunction("cons"),
                C('T'),
                S("he truth is out there"));

        result.Should().Be(S("The truth is out there"));
    }

    // ========== Tests for uncons ==========
    // uncons "abc" == Just ('a',"bc")
    // uncons ""    == Nothing

    [Fact]
    public void Uncons_abc()
    {
        var result = ApplyUnary(GetStringFunction("uncons"), S("abc"));
        result.Should().Be(JustOf(ElmList(C('a'), S("bc"))));
    }

    [Fact]
    public void Uncons_empty()
    {
        var result = ApplyUnary(GetStringFunction("uncons"), S(""));
        result.Should().Be(s_nothing);
    }

    // ========== Tests for toUpper ==========
    // toUpper "skinner" == "SKINNER"

    [Fact]
    public void ToUpper_skinner()
    {
        var result = ApplyUnary(GetStringFunction("toUpper"), S("skinner"));
        result.Should().Be(S("SKINNER"));
    }

    // ========== Tests for toLower ==========
    // toLower "X-FILES" == "x-files"

    [Fact]
    public void ToLower_XFILES()
    {
        var result = ApplyUnary(GetStringFunction("toLower"), S("X-FILES"));
        result.Should().Be(S("x-files"));
    }

    // ========== Tests for pad ==========
    // pad 5 ' ' "1"   == "  1  "
    // pad 5 ' ' "11"  == "  11 "
    // pad 5 ' ' "121" == " 121 "

    [Fact]
    public void Pad_5_space_1()
    {
        var result =
            ApplyTernary(
                GetStringFunction("pad"),
                I(5),
                C(' '),
                S("1"));

        result.Should().Be(S("  1  "));
    }

    [Fact]
    public void Pad_5_space_11()
    {
        var result =
            ApplyTernary(
                GetStringFunction("pad"),
                I(5),
                C(' '),
                S("11"));

        result.Should().Be(S("  11 "));
    }

    [Fact]
    public void Pad_5_space_121()
    {
        var result =
            ApplyTernary(
                GetStringFunction("pad"),
                I(5),
                C(' '),
                S("121"));

        result.Should().Be(S(" 121 "));
    }

    // ========== Tests for padLeft ==========
    // padLeft 5 '.' "1"   == "....1"
    // padLeft 5 '.' "11"  == "...11"
    // padLeft 5 '.' "121" == "..121"

    [Fact]
    public void PadLeft_5_dot_1()
    {
        var result =
            ApplyTernary(
                GetStringFunction("padLeft"),
                I(5),
                C('.'),
                S("1"));

        result.Should().Be(S("....1"));
    }

    [Fact]
    public void PadLeft_5_dot_11()
    {
        var result =
            ApplyTernary(
                GetStringFunction("padLeft"),
                I(5),
                C('.'),
                S("11"));

        result.Should().Be(S("...11"));
    }

    [Fact]
    public void PadLeft_5_dot_121()
    {
        var result =
            ApplyTernary(
                GetStringFunction("padLeft"),
                I(5),
                C('.'),
                S("121"));

        result.Should().Be(S("..121"));
    }

    // ========== Tests for padRight ==========
    // padRight 5 '.' "1"   == "1...."
    // padRight 5 '.' "11"  == "11..."
    // padRight 5 '.' "121" == "121.."

    [Fact]
    public void PadRight_5_dot_1()
    {
        var result =
            ApplyTernary(
                GetStringFunction("padRight"),
                I(5),
                C('.'),
                S("1"));

        result.Should().Be(S("1...."));
    }

    [Fact]
    public void PadRight_5_dot_11()
    {
        var result =
            ApplyTernary(
                GetStringFunction("padRight"),
                I(5),
                C('.'),
                S("11"));

        result.Should().Be(S("11..."));
    }

    [Fact]
    public void PadRight_5_dot_121()
    {
        var result =
            ApplyTernary(
                GetStringFunction("padRight"),
                I(5),
                C('.'),
                S("121"));

        result.Should().Be(S("121.."));
    }

    // ========== Tests for trim ==========
    // trim "  hats  \n" == "hats"

    [Fact]
    public void Trim_whitespace()
    {
        var result =
            ApplyUnary(GetStringFunction("trim"), S("  hats  \n"));

        result.Should().Be(S("hats"));
    }

    // ========== Tests for trimLeft ==========
    // trimLeft "  hats  \n" == "hats  \n"

    [Fact]
    public void TrimLeft_whitespace()
    {
        var result =
            ApplyUnary(GetStringFunction("trimLeft"), S("  hats  \n"));

        result.Should().Be(S("hats  \n"));
    }

    // ========== Tests for trimRight ==========
    // trimRight "  hats  \n" == "  hats"

    [Fact]
    public void TrimRight_whitespace()
    {
        var result =
            ApplyUnary(GetStringFunction("trimRight"), S("  hats  \n"));

        result.Should().Be(S("  hats"));
    }

    // ========== Tests for map ==========
    // map (\c -> if c == '/' then '.' else c) "a/b/c" == "a.b.c"

    [Fact]
    public void Map_toUpper()
    {
        // Use Char.toUpper as a simpler function to test map.
        var toUpperFn = GetCharFunction("toUpper");

        var result =
            ApplyWithPineArgs(
                GetStringFunction("map"),
                toUpperFn,
                ToPine(S("abc")));

        result.Should().Be(S("ABC"));
    }

    // ========== Tests for filter ==========
    // filter isDigit "R2-D2" == "22"

    [Fact]
    public void Filter_isDigit()
    {
        var isDigitFn = GetCharFunction("isDigit");

        var result =
            ApplyWithPineArgs(
                GetStringFunction("filter"),
                isDigitFn,
                ToPine(S("R2-D2")));

        result.Should().Be(S("22"));
    }

    // ========== Tests for foldl ==========
    // foldl cons "" "time" == "emit"

    [Fact]
    public void Foldl_cons()
    {
        var consFn = GetStringFunction("cons");

        var result =
            ApplyWithPineArgs(
                GetStringFunction("foldl"),
                consFn,
                ToPine(S("")),
                ToPine(S("time")));

        result.Should().Be(S("emit"));
    }

    // ========== Tests for foldr ==========
    // foldr cons "" "time" == "time"

    [Fact]
    public void Foldr_cons()
    {
        var consFn = GetStringFunction("cons");

        var result =
            ApplyWithPineArgs(
                GetStringFunction("foldr"),
                consFn,
                ToPine(S("")),
                ToPine(S("time")));

        result.Should().Be(S("time"));
    }

    // ========== Tests for any ==========
    // any isDigit "90210" == True
    // any isDigit "R2-D2" == True
    // any isDigit "heart" == False

    [Fact]
    public void Any_isDigit_90210()
    {
        var isDigitFn = GetCharFunction("isDigit");

        var result =
            ApplyWithPineArgs(
                GetStringFunction("any"),
                isDigitFn,
                ToPine(S("90210")));

        result.Should().Be(ElmValue.TrueValue);
    }

    [Fact]
    public void Any_isDigit_R2D2()
    {
        var isDigitFn = GetCharFunction("isDigit");

        var result =
            ApplyWithPineArgs(
                GetStringFunction("any"),
                isDigitFn,
                ToPine(S("R2-D2")));

        result.Should().Be(ElmValue.TrueValue);
    }

    [Fact]
    public void Any_isDigit_heart()
    {
        var isDigitFn = GetCharFunction("isDigit");

        var result =
            ApplyWithPineArgs(
                GetStringFunction("any"),
                isDigitFn,
                ToPine(S("heart")));

        result.Should().Be(ElmValue.FalseValue);
    }

    // ========== Tests for all ==========
    // all isDigit "90210" == True
    // all isDigit "R2-D2" == False
    // all isDigit "heart" == False

    [Fact]
    public void All_isDigit_90210()
    {
        var isDigitFn = GetCharFunction("isDigit");

        var result =
            ApplyWithPineArgs(
                GetStringFunction("all"),
                isDigitFn,
                ToPine(S("90210")));

        result.Should().Be(ElmValue.TrueValue);
    }

    [Fact]
    public void All_isDigit_R2D2()
    {
        var isDigitFn = GetCharFunction("isDigit");

        var result =
            ApplyWithPineArgs(
                GetStringFunction("all"),
                isDigitFn,
                ToPine(S("R2-D2")));

        result.Should().Be(ElmValue.FalseValue);
    }

    [Fact]
    public void All_isDigit_heart()
    {
        var isDigitFn = GetCharFunction("isDigit");

        var result =
            ApplyWithPineArgs(
                GetStringFunction("all"),
                isDigitFn,
                ToPine(S("heart")));

        result.Should().Be(ElmValue.FalseValue);
    }
}
