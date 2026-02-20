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

    private static readonly Core.Interpreter.IntermediateVM.PineVM s_vm =
        ElmCompilerTestHelper.PineVMForProfiling(_ => { });

    private static ElmValue ApplyUnary(PineValue functionValue, ElmValue argument) =>
        CoreLibraryTestHelper.ApplyUnary(functionValue, argument, s_vm);

    private static ElmValue ApplyBinary(
        PineValue functionValue, ElmValue arg1, ElmValue arg2) =>
        CoreLibraryTestHelper.ApplyBinary(functionValue, arg1, arg2, s_vm);

    private static ElmValue ApplyTernary(
        PineValue functionValue, ElmValue arg1, ElmValue arg2, ElmValue arg3) =>
        CoreLibraryTestHelper.ApplyTernary(functionValue, arg1, arg2, arg3, s_vm);

    private static ElmValue String(string s) =>
        ElmValue.StringInstance(s);

    private static ElmValue Integer(long i) =>
        ElmValue.Integer(i);

    private static ElmValue Char(char c) =>
        ElmValue.CharInstance(c);

    private static ElmValue JustOf(ElmValue inner) =>
        ElmValue.TagInstance("Just", [inner]);

    private static readonly ElmValue s_nothing =
        ElmValue.TagInstance("Nothing", []);

    private static ElmValue ElmList(params ElmValue[] items) =>
        ElmValue.ListInstance([.. items]);

    private static PineValue ToPine(ElmValue value) =>
        ElmValueEncoding.ElmValueAsPineValue(value);

    private static ElmValue ApplyWithPineArgs(
        PineValue functionValue, params PineValue[] pineArgs) =>
        CoreLibraryTestHelper.ApplyWithPineArgs(s_vm, functionValue, pineArgs);

    // ========== Tests for isEmpty ==========
    // isEmpty "" == True
    // isEmpty "the world" == False

    [Fact]
    public void IsEmpty_empty_string()
    {
        var result = ApplyUnary(GetStringFunction("isEmpty"), String(""));
        result.Should().Be(ElmValue.TrueValue);
    }

    [Fact]
    public void IsEmpty_non_empty_string()
    {
        var result = ApplyUnary(GetStringFunction("isEmpty"), String("the world"));
        result.Should().Be(ElmValue.FalseValue);
    }

    // ========== Tests for length ==========
    // length "innumerable" == 11
    // length "" == 0

    [Fact]
    public void Length_innumerable()
    {
        var result = ApplyUnary(GetStringFunction("length"), String("innumerable"));
        result.Should().Be(Integer(11));
    }

    [Fact]
    public void Length_empty()
    {
        var result = ApplyUnary(GetStringFunction("length"), String(""));
        result.Should().Be(Integer(0));
    }

    // ========== Tests for reverse ==========
    // reverse "stressed" == "desserts"

    [Fact]
    public void Reverse_stressed()
    {
        var result = ApplyUnary(GetStringFunction("reverse"), String("stressed"));
        result.Should().Be(String("desserts"));
    }

    // ========== Tests for repeat ==========
    // repeat 3 "ha" == "hahaha"

    [Fact]
    public void Repeat_3_ha()
    {
        var result = ApplyBinary(GetStringFunction("repeat"), Integer(3), String("ha"));
        result.Should().Be(String("hahaha"));
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
                String("."),
                String("-"),
                String("Json.Decode.succeed"));

        result.Should().Be(String("Json-Decode-succeed"));
    }

    [Fact]
    public void Replace_comma_with_slash()
    {
        var result =
            ApplyTernary(
                GetStringFunction("replace"),
                String(","),
                String("/"),
                String("a,b,c,d,e"));

        result.Should().Be(String("a/b/c/d/e"));
    }

    // ========== Tests for append ==========
    // append "butter" "fly" == "butterfly"

    [Fact]
    public void Append_butterfly()
    {
        var result =
            ApplyBinary(GetStringFunction("append"), String("butter"), String("fly"));

        result.Should().Be(String("butterfly"));
    }

    // ========== Tests for concat ==========
    // concat ["never","the","less"] == "nevertheless"

    [Fact]
    public void Concat_nevertheless()
    {
        var result =
            ApplyUnary(
                GetStringFunction("concat"),
                ElmList(String("never"), String("the"), String("less")));

        result.Should().Be(String("nevertheless"));
    }

    // ========== Tests for split ==========
    // split "," "cat,dog,cow"        == ["cat","dog","cow"]
    // split "/" "home/evan/Desktop/" == ["home","evan","Desktop", ""]

    [Fact]
    public void Split_comma()
    {
        var result =
            ApplyBinary(GetStringFunction("split"), String(","), String("cat,dog,cow"));

        result.Should().Be(ElmList(String("cat"), String("dog"), String("cow")));
    }

    [Fact]
    public void Split_slash_trailing()
    {
        var result =
            ApplyBinary(GetStringFunction("split"), String("/"), String("home/evan/Desktop/"));

        result.Should().Be(ElmList(String("home"), String("evan"), String("Desktop"), String("")));
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
                String("a"),
                ElmList(String("H"), String("w"), String("ii"), String("n")));

        result.Should().Be(String("Hawaiian"));
    }

    [Fact]
    public void Join_space()
    {
        var result =
            ApplyBinary(
                GetStringFunction("join"),
                String(" "),
                ElmList(String("cat"), String("dog"), String("cow")));

        result.Should().Be(String("cat dog cow"));
    }

    [Fact]
    public void Join_slash()
    {
        var result =
            ApplyBinary(
                GetStringFunction("join"),
                String("/"),
                ElmList(String("home"), String("evan"), String("Desktop")));

        result.Should().Be(String("home/evan/Desktop"));
    }

    // ========== Tests for words ==========
    // words "How are \t you? \n Good?" == ["How","are","you?","Good?"]

    [Fact]
    public void Words_split_whitespace()
    {
        var result =
            ApplyUnary(
                GetStringFunction("words"),
                String("How are \t you? \n Good?"));

        result.Should().Be(ElmList(String("How"), String("are"), String("you?"), String("Good?")));
    }

    // ========== Tests for lines ==========
    // lines "How are you?\nGood?" == ["How are you?", "Good?"]

    [Fact]
    public void Lines_split_newline()
    {
        var result =
            ApplyUnary(
                GetStringFunction("lines"),
                String("How are you?\nGood?"));

        result.Should().Be(ElmList(String("How are you?"), String("Good?")));
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
                Integer(7),
                Integer(9),
                String("snakes on a plane!"));

        result.Should().Be(String("on"));
    }

    [Fact]
    public void Slice_0_6()
    {
        var result =
            ApplyTernary(
                GetStringFunction("slice"),
                Integer(0),
                Integer(6),
                String("snakes on a plane!"));

        result.Should().Be(String("snakes"));
    }

    [Fact]
    public void Slice_0_neg7()
    {
        var result =
            ApplyTernary(
                GetStringFunction("slice"),
                Integer(0),
                Integer(-7),
                String("snakes on a plane!"));

        result.Should().Be(String("snakes on a"));
    }

    [Fact]
    public void Slice_neg6_neg1()
    {
        var result =
            ApplyTernary(
                GetStringFunction("slice"),
                Integer(-6),
                Integer(-1),
                String("snakes on a plane!"));

        result.Should().Be(String("plane"));
    }

    // ========== Tests for left ==========
    // left 2 "Mulder" == "Mu"

    [Fact]
    public void Left_2()
    {
        var result =
            ApplyBinary(GetStringFunction("left"), Integer(2), String("Mulder"));

        result.Should().Be(String("Mu"));
    }

    // ========== Tests for right ==========
    // right 2 "Scully" == "ly"

    [Fact]
    public void Right_2()
    {
        var result =
            ApplyBinary(GetStringFunction("right"), Integer(2), String("Scully"));

        result.Should().Be(String("ly"));
    }

    // ========== Tests for dropLeft ==========
    // dropLeft 2 "The Lone Gunmen" == "e Lone Gunmen"

    [Fact]
    public void DropLeft_2()
    {
        var result =
            ApplyBinary(
                GetStringFunction("dropLeft"),
                Integer(2),
                String("The Lone Gunmen"));

        result.Should().Be(String("e Lone Gunmen"));
    }

    // ========== Tests for dropRight ==========
    // dropRight 2 "Cigarette Smoking Man" == "Cigarette Smoking M"

    [Fact]
    public void DropRight_2()
    {
        var result =
            ApplyBinary(
                GetStringFunction("dropRight"),
                Integer(2),
                String("Cigarette Smoking Man"));

        result.Should().Be(String("Cigarette Smoking M"));
    }

    // ========== Tests for contains ==========
    // contains "the" "theory" == True
    // contains "hat" "theory" == False
    // contains "THE" "theory" == False

    [Fact]
    public void Contains_the_in_theory()
    {
        var result =
            ApplyBinary(GetStringFunction("contains"), String("the"), String("theory"));

        result.Should().Be(ElmValue.TrueValue);
    }

    [Fact]
    public void Contains_hat_in_theory()
    {
        var result =
            ApplyBinary(GetStringFunction("contains"), String("hat"), String("theory"));

        result.Should().Be(ElmValue.FalseValue);
    }

    [Fact]
    public void Contains_THE_in_theory()
    {
        var result =
            ApplyBinary(GetStringFunction("contains"), String("THE"), String("theory"));

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
                String("the"),
                String("theory"));

        result.Should().Be(ElmValue.TrueValue);
    }

    [Fact]
    public void StartsWith_ory()
    {
        var result =
            ApplyBinary(
                GetStringFunction("startsWith"),
                String("ory"),
                String("theory"));

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
                String("the"),
                String("theory"));

        result.Should().Be(ElmValue.FalseValue);
    }

    [Fact]
    public void EndsWith_ory()
    {
        var result =
            ApplyBinary(
                GetStringFunction("endsWith"),
                String("ory"),
                String("theory"));

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
                String("i"),
                String("Mississippi"));

        result.Should().Be(ElmList(Integer(1), Integer(4), Integer(7), Integer(10)));
    }

    [Fact]
    public void Indexes_ss_Mississippi()
    {
        var result =
            ApplyBinary(
                GetStringFunction("indexes"),
                String("ss"),
                String("Mississippi"));

        result.Should().Be(ElmList(Integer(2), Integer(5)));
    }

    [Fact]
    public void Indexes_needle_haystack()
    {
        var result =
            ApplyBinary(
                GetStringFunction("indexes"),
                String("needle"),
                String("haystack"));

        result.Should().Be(ElmList());
    }

    // ========== Tests for indices (alias for indexes) ==========

    [Fact]
    public void Indices_i_Mississippi()
    {
        var result =
            ApplyBinary(
                GetStringFunction("indices"),
                String("i"),
                String("Mississippi"));

        result.Should().Be(ElmList(Integer(1), Integer(4), Integer(7), Integer(10)));
    }

    // ========== Tests for toInt ==========
    // String.toInt "123" == Just 123
    // String.toInt "-42" == Just -42
    // String.toInt "3.1" == Nothing
    // String.toInt "31a" == Nothing

    [Fact]
    public void ToInt_123()
    {
        var result = ApplyUnary(GetStringFunction("toInt"), String("123"));
        result.Should().Be(JustOf(Integer(123)));
    }

    [Fact]
    public void ToInt_neg42()
    {
        var result = ApplyUnary(GetStringFunction("toInt"), String("-42"));
        result.Should().Be(JustOf(Integer(-42)));
    }

    [Fact]
    public void ToInt_3_1()
    {
        var result = ApplyUnary(GetStringFunction("toInt"), String("3.1"));
        result.Should().Be(s_nothing);
    }

    [Fact]
    public void ToInt_31a()
    {
        var result = ApplyUnary(GetStringFunction("toInt"), String("31a"));
        result.Should().Be(s_nothing);
    }

    // ========== Tests for fromInt ==========
    // String.fromInt 123 == "123"
    // String.fromInt -42 == "-42"

    [Fact]
    public void FromInt_123()
    {
        var result = ApplyUnary(GetStringFunction("fromInt"), Integer(123));
        result.Should().Be(String("123"));
    }

    [Fact]
    public void FromInt_neg42()
    {
        var result = ApplyUnary(GetStringFunction("fromInt"), Integer(-42));
        result.Should().Be(String("-42"));
    }

    // ========== Tests for toFloat ==========
    // String.toFloat "123" == Just 123.0
    // String.toFloat "-42" == Just -42.0
    // String.toFloat "3.1" == Just 3.1
    // String.toFloat "31a" == Nothing

    [Fact]
    public void ToFloat_123()
    {
        var result = ApplyUnary(GetStringFunction("toFloat"), String("123"));
        var justTag = result as ElmValue.ElmTag;
        justTag.Should().NotBeNull();
        justTag!.TagName.Should().Be("Just");
    }

    [Fact]
    public void ToFloat_neg42()
    {
        var result = ApplyUnary(GetStringFunction("toFloat"), String("-42"));
        var justTag = result as ElmValue.ElmTag;
        justTag.Should().NotBeNull();
        justTag!.TagName.Should().Be("Just");
    }

    [Fact]
    public void ToFloat_3_1()
    {
        var result = ApplyUnary(GetStringFunction("toFloat"), String("3.1"));
        var justTag = result as ElmValue.ElmTag;
        justTag.Should().NotBeNull();
        justTag!.TagName.Should().Be("Just");
    }

    [Fact]
    public void ToFloat_31a()
    {
        var result = ApplyUnary(GetStringFunction("toFloat"), String("31a"));
        result.Should().Be(s_nothing);
    }

    // ========== Tests for fromFloat ==========
    // String.fromFloat 123 == "123"
    // String.fromFloat -42 == "-42"
    // String.fromFloat 3.9 == "3.9"

    [Fact]
    public void FromFloat_123()
    {
        var result = ApplyUnary(GetStringFunction("fromFloat"), Integer(123));
        result.Should().Be(String("123"));
    }

    [Fact]
    public void FromFloat_neg42()
    {
        var result = ApplyUnary(GetStringFunction("fromFloat"), Integer(-42));
        result.Should().Be(String("-42"));
    }

    // ========== Tests for toList ==========
    // toList "abc" == ['a','b','c']

    [Fact]
    public void ToList_abc()
    {
        var result = ApplyUnary(GetStringFunction("toList"), String("abc"));
        result.Should().Be(ElmList(Char('a'), Char('b'), Char('c')));
    }

    // ========== Tests for fromList ==========
    // fromList ['a','b','c'] == "abc"

    [Fact]
    public void FromList_abc()
    {
        var result =
            ApplyUnary(
                GetStringFunction("fromList"),
                ElmList(Char('a'), Char('b'), Char('c')));

        result.Should().Be(String("abc"));
    }

    // ========== Tests for fromChar ==========
    // fromChar 'a' == "a"

    [Fact]
    public void FromChar_a()
    {
        var result = ApplyUnary(GetStringFunction("fromChar"), Char('a'));
        result.Should().Be(String("a"));
    }

    // ========== Tests for cons ==========
    // cons 'T' "he truth is out there" == "The truth is out there"

    [Fact]
    public void Cons_T()
    {
        var result =
            ApplyBinary(
                GetStringFunction("cons"),
                Char('T'),
                String("he truth is out there"));

        result.Should().Be(String("The truth is out there"));
    }

    // ========== Tests for uncons ==========
    // uncons "abc" == Just ('a',"bc")
    // uncons ""    == Nothing

    [Fact]
    public void Uncons_abc()
    {
        var result = ApplyUnary(GetStringFunction("uncons"), String("abc"));
        result.Should().Be(JustOf(ElmList(Char('a'), String("bc"))));
    }

    [Fact]
    public void Uncons_empty()
    {
        var result = ApplyUnary(GetStringFunction("uncons"), String(""));
        result.Should().Be(s_nothing);
    }

    // ========== Tests for toUpper ==========
    // toUpper "skinner" == "SKINNER"

    [Fact]
    public void ToUpper_skinner()
    {
        var result = ApplyUnary(GetStringFunction("toUpper"), String("skinner"));
        result.Should().Be(String("SKINNER"));
    }

    // ========== Tests for toLower ==========
    // toLower "X-FILES" == "x-files"

    [Fact]
    public void ToLower_XFILES()
    {
        var result = ApplyUnary(GetStringFunction("toLower"), String("X-FILES"));
        result.Should().Be(String("x-files"));
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
                Integer(5),
                Char(' '),
                String("1"));

        result.Should().Be(String("  1  "));
    }

    [Fact]
    public void Pad_5_space_11()
    {
        var result =
            ApplyTernary(
                GetStringFunction("pad"),
                Integer(5),
                Char(' '),
                String("11"));

        result.Should().Be(String("  11 "));
    }

    [Fact]
    public void Pad_5_space_121()
    {
        var result =
            ApplyTernary(
                GetStringFunction("pad"),
                Integer(5),
                Char(' '),
                String("121"));

        result.Should().Be(String(" 121 "));
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
                Integer(5),
                Char('.'),
                String("1"));

        result.Should().Be(String("....1"));
    }

    [Fact]
    public void PadLeft_5_dot_11()
    {
        var result =
            ApplyTernary(
                GetStringFunction("padLeft"),
                Integer(5),
                Char('.'),
                String("11"));

        result.Should().Be(String("...11"));
    }

    [Fact]
    public void PadLeft_5_dot_121()
    {
        var result =
            ApplyTernary(
                GetStringFunction("padLeft"),
                Integer(5),
                Char('.'),
                String("121"));

        result.Should().Be(String("..121"));
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
                Integer(5),
                Char('.'),
                String("1"));

        result.Should().Be(String("1...."));
    }

    [Fact]
    public void PadRight_5_dot_11()
    {
        var result =
            ApplyTernary(
                GetStringFunction("padRight"),
                Integer(5),
                Char('.'),
                String("11"));

        result.Should().Be(String("11..."));
    }

    [Fact]
    public void PadRight_5_dot_121()
    {
        var result =
            ApplyTernary(
                GetStringFunction("padRight"),
                Integer(5),
                Char('.'),
                String("121"));

        result.Should().Be(String("121.."));
    }

    // ========== Tests for trim ==========
    // trim "  hats  \n" == "hats"

    [Fact]
    public void Trim_whitespace()
    {
        var result =
            ApplyUnary(GetStringFunction("trim"), String("  hats  \n"));

        result.Should().Be(String("hats"));
    }

    // ========== Tests for trimLeft ==========
    // trimLeft "  hats  \n" == "hats  \n"

    [Fact]
    public void TrimLeft_whitespace()
    {
        var result =
            ApplyUnary(GetStringFunction("trimLeft"), String("  hats  \n"));

        result.Should().Be(String("hats  \n"));
    }

    // ========== Tests for trimRight ==========
    // trimRight "  hats  \n" == "  hats"

    [Fact]
    public void TrimRight_whitespace()
    {
        var result =
            ApplyUnary(GetStringFunction("trimRight"), String("  hats  \n"));

        result.Should().Be(String("  hats"));
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
                ToPine(String("abc")));

        result.Should().Be(String("ABC"));
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
                ToPine(String("R2-D2")));

        result.Should().Be(String("22"));
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
                ToPine(String("")),
                ToPine(String("time")));

        result.Should().Be(String("emit"));
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
                ToPine(String("")),
                ToPine(String("time")));

        result.Should().Be(String("time"));
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
                ToPine(String("90210")));

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
                ToPine(String("R2-D2")));

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
                ToPine(String("heart")));

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
                ToPine(String("90210")));

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
                ToPine(String("R2-D2")));

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
                ToPine(String("heart")));

        result.Should().Be(ElmValue.FalseValue);
    }
}
