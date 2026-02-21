using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Elm.ElmInElm;
using Pine.Core.Files;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.CoreLibraryModule;

public class KernelParserFunctionTests
{
    /// <summary>
    /// Elm test module that imports Parser and Parser.Advanced and provides
    /// test functions. Parsers stay inside Elm source so we never have to
    /// pass closures through C#.
    /// </summary>
    private const string TestModuleSource =
        """"
        module ParserTest exposing (..)

        import Parser exposing ((|.), (|=))
        import Parser.Advanced as PA
        import Set


        -- ====== Parser.run + succeed ======

        -- run (succeed 90210) "mississippi" == Ok 90210
        runSucceedInt : Int -> Result (List Parser.DeadEnd) Int
        runSucceedInt _ =
            Parser.run (Parser.succeed 90210) "mississippi"

        -- run (succeed ()) "mississippi" == Ok ()
        runSucceedUnit : Int -> Result (List Parser.DeadEnd) ()
        runSucceedUnit _ =
            Parser.run (Parser.succeed ()) "mississippi"


        -- ====== Parser.int ======

        -- run int "123456" == Ok 123456
        parseInt123456 : Int -> Result (List Parser.DeadEnd) Int
        parseInt123456 _ =
            Parser.run Parser.int "123456"

        -- run int "1" == Ok 1
        parseInt1 : Int -> Result (List Parser.DeadEnd) Int
        parseInt1 _ =
            Parser.run Parser.int "1"

        -- run int "1234" == Ok 1234
        parseInt1234 : Int -> Result (List Parser.DeadEnd) Int
        parseInt1234 _ =
            Parser.run Parser.int "1234"

        parseIntEmpty : Int -> Bool
        parseIntEmpty _ =
            case Parser.run Parser.int "" of
                Err _ ->
                    True

                Ok _ ->
                    False

        parseIntLetters : Int -> Bool
        parseIntLetters _ =
            case Parser.run Parser.int "abc" of
                Err _ ->
                    True

                Ok _ ->
                    False


        -- ====== Parser.symbol ======

        -- run (symbol "[") "[" == Ok ()
        symbolBracket : Int -> Result (List Parser.DeadEnd) ()
        symbolBracket _ =
            Parser.run (Parser.symbol "[") "["

        -- run (symbol "[") "4" == Err ...
        symbolBracketFail : Int -> Bool
        symbolBracketFail _ =
            case Parser.run (Parser.symbol "[") "4" of
                Err _ ->
                    True

                Ok _ ->
                    False


        -- ====== Parser.keyword ======

        -- run (keyword "let") "let" == Ok ()
        keywordLet : Int -> Result (List Parser.DeadEnd) ()
        keywordLet _ =
            Parser.run (Parser.keyword "let") "let"

        -- run (keyword "let") "var" == Err ...
        keywordLetVar : Int -> Bool
        keywordLetVar _ =
            case Parser.run (Parser.keyword "let") "var" of
                Err _ ->
                    True

                Ok _ ->
                    False

        -- run (keyword "let") "letters" == Err ... -- because of subsequent letters
        keywordLetLetters : Int -> Bool
        keywordLetLetters _ =
            case Parser.run (Parser.keyword "let") "letters" of
                Err _ ->
                    True

                Ok _ ->
                    False

        -- run (keyword "true") "true" == Ok ()
        keywordTrue : Int -> Result (List Parser.DeadEnd) ()
        keywordTrue _ =
            Parser.run (Parser.keyword "true") "true"

        -- run (keyword "true") "true!" == Ok ()
        keywordTrueBang : Int -> Result (List Parser.DeadEnd) ()
        keywordTrueBang _ =
            Parser.run (Parser.keyword "true") "true!"


        -- ====== Parser.token ======

        tokenHello : Int -> Result (List Parser.DeadEnd) ()
        tokenHello _ =
            Parser.run (Parser.token "hello") "hello world"

        tokenFail : Int -> Bool
        tokenFail _ =
            case Parser.run (Parser.token "hello") "world" of
                Err _ ->
                    True

                Ok _ ->
                    False


        -- ====== Parser.end ======

        endAfterInt : Int -> Result (List Parser.DeadEnd) Int
        endAfterInt _ =
            Parser.run
                (Parser.succeed identity
                    |= Parser.int
                    |. Parser.end
                )
                "123"

        endFail : Int -> Bool
        endFail _ =
            case
                Parser.run
                    (Parser.succeed identity
                        |= Parser.int
                        |. Parser.end
                    )
                    "123abc"
            of
                Err _ ->
                    True

                Ok _ ->
                    False


        -- ====== Parser.map ======

        -- run nullOrInt "0"    == Ok (Just 0)
        -- run nullOrInt "13"   == Ok (Just 13)
        -- run nullOrInt "null" == Ok Nothing
        mapJustInt0 : Int -> Result (List Parser.DeadEnd) (Maybe Int)
        mapJustInt0 _ =
            Parser.run
                (Parser.oneOf
                    [ Parser.map Just Parser.int
                    , Parser.map (\_ -> Nothing) (Parser.keyword "null")
                    ]
                )
                "0"

        mapJustInt13 : Int -> Result (List Parser.DeadEnd) (Maybe Int)
        mapJustInt13 _ =
            Parser.run
                (Parser.oneOf
                    [ Parser.map Just Parser.int
                    , Parser.map (\_ -> Nothing) (Parser.keyword "null")
                    ]
                )
                "13"

        mapNull : Int -> Result (List Parser.DeadEnd) (Maybe Int)
        mapNull _ =
            Parser.run
                (Parser.oneOf
                    [ Parser.map Just Parser.int
                    , Parser.map (\_ -> Nothing) (Parser.keyword "null")
                    ]
                )
                "null"

        mapZero : Int -> Bool
        mapZero _ =
            case
                Parser.run
                    (Parser.oneOf
                        [ Parser.map Just Parser.int
                        , Parser.map (\_ -> Nothing) (Parser.keyword "null")
                        ]
                    )
                    "zero"
            of
                Err _ ->
                    True

                Ok _ ->
                    False


        -- ====== Parser.oneOf ======

        oneOfFirstMatch : Int -> Result (List Parser.DeadEnd) String
        oneOfFirstMatch _ =
            Parser.run
                (Parser.oneOf
                    [ Parser.map (\_ -> "symbol") (Parser.symbol "(")
                    , Parser.map (\_ -> "keyword") (Parser.keyword "let")
                    ]
                )
                "let"

        oneOfSecondMatch : Int -> Result (List Parser.DeadEnd) String
        oneOfSecondMatch _ =
            Parser.run
                (Parser.oneOf
                    [ Parser.map (\_ -> "int") Parser.int
                    , Parser.map (\_ -> "keyword") (Parser.keyword "hello")
                    ]
                )
                "hello"


        -- ====== Parser.andThen ======

        checkLen : String -> Parser.Parser String
        checkLen code =
            if String.length code == 5 then
                Parser.succeed code
            else
                Parser.problem "a U.S. zip code has exactly 5 digits"

        zipCode : Parser.Parser String
        zipCode =
            Parser.getChompedString (Parser.chompWhile Char.isDigit)
                |> Parser.andThen checkLen

        zipCodeValid : Int -> Result (List Parser.DeadEnd) String
        zipCodeValid _ =
            Parser.run zipCode "12345"

        zipCodeInvalid : Int -> Bool
        zipCodeInvalid _ =
            case Parser.run zipCode "1234" of
                Err _ ->
                    True

                Ok _ ->
                    False

        zipCodeTooLong : Int -> Bool
        zipCodeTooLong _ =
            case Parser.run zipCode "123456" of
                Err _ ->
                    True

                Ok _ ->
                    False


        -- ====== Parser.lazy ======

        type Expr
            = Literal Int
            | Negate Expr

        exprParser : Parser.Parser Expr
        exprParser =
            Parser.oneOf
                [ Parser.succeed Negate
                    |. Parser.symbol "-"
                    |= Parser.lazy (\_ -> exprParser)
                , Parser.map Literal Parser.int
                ]

        lazySimple : Int -> Result (List Parser.DeadEnd) Expr
        lazySimple _ =
            Parser.run exprParser "42"

        lazyNested : Int -> Result (List Parser.DeadEnd) Expr
        lazyNested _ =
            Parser.run exprParser "-42"

        lazyDoubleNested : Int -> Result (List Parser.DeadEnd) Expr
        lazyDoubleNested _ =
            Parser.run exprParser "--42"


        -- ====== Pipeline: |= and |. ======

        type alias Point =
            { x : Int, y : Int }

        pointParser : Parser.Parser Point
        pointParser =
            Parser.succeed Point
                |. Parser.symbol "("
                |. Parser.spaces
                |= Parser.int
                |. Parser.spaces
                |. Parser.symbol ","
                |. Parser.spaces
                |= Parser.int
                |. Parser.spaces
                |. Parser.symbol ")"

        pipelinePoint : Int -> Result (List Parser.DeadEnd) Point
        pipelinePoint _ =
            Parser.run pointParser "( 3 , 4 )"

        pipelinePointNoSpaces : Int -> Result (List Parser.DeadEnd) Point
        pipelinePointNoSpaces _ =
            Parser.run pointParser "(10,20)"


        -- ====== Parser.chompIf ======

        chompIfDigit : Int -> Result (List Parser.DeadEnd) String
        chompIfDigit _ =
            Parser.run
                (Parser.getChompedString (Parser.chompIf Char.isDigit))
                "5abc"

        chompIfFail : Int -> Bool
        chompIfFail _ =
            case Parser.run (Parser.chompIf Char.isDigit) "abc" of
                Err _ ->
                    True

                Ok _ ->
                    False


        -- ====== Parser.chompWhile ======

        chompWhileDigits : Int -> Result (List Parser.DeadEnd) String
        chompWhileDigits _ =
            Parser.run
                (Parser.getChompedString (Parser.chompWhile Char.isDigit))
                "12345abc"

        chompWhileEmpty : Int -> Result (List Parser.DeadEnd) String
        chompWhileEmpty _ =
            Parser.run
                (Parser.getChompedString (Parser.chompWhile Char.isDigit))
                "abc"


        -- ====== Parser.chompUntil ======

        chompUntilSemicolon : Int -> Result (List Parser.DeadEnd) String
        chompUntilSemicolon _ =
            Parser.run
                (Parser.getChompedString (Parser.chompUntil ";"))
                "hello;"

        chompUntilMultiChar : Int -> Result (List Parser.DeadEnd) String
        chompUntilMultiChar _ =
            Parser.run
                (Parser.getChompedString (Parser.chompUntil "*/"))
                "comment content */"


        -- ====== Parser.chompUntilEndOr ======

        chompUntilEndOrNewline : Int -> Result (List Parser.DeadEnd) String
        chompUntilEndOrNewline _ =
            Parser.run
                (Parser.getChompedString (Parser.chompUntilEndOr "\n"))
                "first line"

        chompUntilEndOrFound : Int -> Result (List Parser.DeadEnd) String
        chompUntilEndOrFound _ =
            Parser.run
                (Parser.getChompedString (Parser.chompUntilEndOr ";"))
                "hello;world"


        -- ====== Parser.getChompedString ======

        getChompedDigits : Int -> Result (List Parser.DeadEnd) String
        getChompedDigits _ =
            Parser.run
                (Parser.getChompedString (Parser.chompWhile Char.isDigit))
                "123abc"


        -- ====== Parser.backtrackable ======

        backtrackableTest : Int -> Result (List Parser.DeadEnd) String
        backtrackableTest _ =
            Parser.run
                (Parser.oneOf
                    [ Parser.backtrackable
                        (Parser.succeed (\_ -> "parens")
                            |. Parser.symbol "("
                            |= Parser.int
                            |. Parser.symbol ")"
                        )
                    , Parser.succeed (\_ -> "ident")
                        |= Parser.getChompedString (Parser.chompWhile Char.isAlpha)
                    ]
                )
                "hello"


        -- ====== Parser.loop ======

        type alias IntList =
            List Int

        intListHelper : IntList -> Parser.Parser (Parser.Step IntList IntList)
        intListHelper revItems =
            Parser.oneOf
                [ Parser.backtrackable
                    (Parser.succeed (\item -> Parser.Loop (item :: revItems))
                        |= Parser.int
                        |. Parser.spaces
                        |. Parser.symbol ","
                        |. Parser.spaces
                    )
                , Parser.succeed (\item -> Parser.Done (List.reverse (item :: revItems)))
                    |= Parser.int
                ]

        loopIntList : Int -> Result (List Parser.DeadEnd) IntList
        loopIntList _ =
            Parser.run
                (Parser.loop [] intListHelper)
                "1, 2, 3"


        -- ====== Parser.spaces ======

        spacesTest : Int -> Result (List Parser.DeadEnd) Int
        spacesTest _ =
            Parser.run
                (Parser.succeed identity
                    |. Parser.spaces
                    |= Parser.int
                )
                "   42"


        -- ====== Parser.getPosition / getRow / getCol / getOffset / getSource ======

        posAfterSymbol : Int -> Result (List Parser.DeadEnd) ( Int, Int )
        posAfterSymbol _ =
            Parser.run
                (Parser.succeed identity
                    |. Parser.symbol "ab"
                    |= Parser.getPosition
                )
                "ab"

        getRowTest : Int -> Result (List Parser.DeadEnd) Int
        getRowTest _ =
            Parser.run
                (Parser.succeed identity
                    |. Parser.symbol "a"
                    |= Parser.getRow
                )
                "a"

        getColTest : Int -> Result (List Parser.DeadEnd) Int
        getColTest _ =
            Parser.run
                (Parser.succeed identity
                    |. Parser.symbol "abc"
                    |= Parser.getCol
                )
                "abc"

        getOffsetTest : Int -> Result (List Parser.DeadEnd) Int
        getOffsetTest _ =
            Parser.run
                (Parser.succeed identity
                    |. Parser.symbol "hello"
                    |= Parser.getOffset
                )
                "hello world"

        getSourceTest : Int -> Result (List Parser.DeadEnd) String
        getSourceTest _ =
            Parser.run
                (Parser.succeed identity
                    |. Parser.symbol "a"
                    |= Parser.getSource
                )
                "abc"


        -- ====== Parser.variable ======

        elmVar : Parser.Parser String
        elmVar =
            Parser.variable
                { start = Char.isLower
                , inner = \c -> Char.isAlphaNum c || c == '_'
                , reserved = Set.fromList [ "let", "in", "case", "of" ]
                }

        variableSimple : Int -> Result (List Parser.DeadEnd) String
        variableSimple _ =
            Parser.run elmVar "myVar123"

        variableReserved : Int -> Bool
        variableReserved _ =
            case Parser.run elmVar "let" of
                Err _ ->
                    True

                Ok _ ->
                    False

        variableUnderscore : Int -> Result (List Parser.DeadEnd) String
        variableUnderscore _ =
            Parser.run elmVar "my_var"

        variableStartUpper : Int -> Bool
        variableStartUpper _ =
            case Parser.run elmVar "MyVar" of
                Err _ ->
                    True

                Ok _ ->
                    False


        -- ====== Parser.lineComment ======

        lineCommentTest : Int -> Result (List Parser.DeadEnd) String
        lineCommentTest _ =
            Parser.run
                (Parser.succeed identity
                    |= Parser.getChompedString (Parser.chompWhile Char.isAlpha)
                    |. Parser.lineComment "--"
                )
                "hello-- this is a comment"


        -- ====== Parser.multiComment ======

        multiCommentTest : Int -> Result (List Parser.DeadEnd) String
        multiCommentTest _ =
            Parser.run
                (Parser.succeed identity
                    |= Parser.getChompedString (Parser.chompWhile Char.isAlpha)
                    |. Parser.multiComment "{-" "-}" Parser.Nestable
                )
                "hello{- comment -}"


        -- ====== Parser.sequence ======

        sequenceForbidden : Int -> Result (List Parser.DeadEnd) (List Int)
        sequenceForbidden _ =
            Parser.run
                (Parser.sequence
                    { start = "["
                    , separator = ","
                    , end = "]"
                    , spaces = Parser.spaces
                    , item = Parser.int
                    , trailing = Parser.Forbidden
                    }
                )
                "[ 1 , 2 , 3 ]"

        sequenceEmpty : Int -> Result (List Parser.DeadEnd) (List Int)
        sequenceEmpty _ =
            Parser.run
                (Parser.sequence
                    { start = "["
                    , separator = ","
                    , end = "]"
                    , spaces = Parser.spaces
                    , item = Parser.int
                    , trailing = Parser.Forbidden
                    }
                )
                "[]"

        sequenceSingle : Int -> Result (List Parser.DeadEnd) (List Int)
        sequenceSingle _ =
            Parser.run
                (Parser.sequence
                    { start = "["
                    , separator = ","
                    , end = "]"
                    , spaces = Parser.spaces
                    , item = Parser.int
                    , trailing = Parser.Forbidden
                    }
                )
                "[42]"


        -- ====== Parser.mapChompedString ======

        mapChompedStringTest : Int -> Result (List Parser.DeadEnd) ( String, Int )
        mapChompedStringTest _ =
            Parser.run
                (Parser.mapChompedString Tuple.pair (Parser.chompWhile Char.isDigit)
                    |> Parser.andThen
                        (\( str, _ ) ->
                            case String.toInt str of
                                Just n ->
                                    Parser.succeed ( str, n )

                                Nothing ->
                                    Parser.problem "not a number"
                        )
                )
                "42abc"


        -- ====== Parser.problem ======

        problemTest : Int -> Bool
        problemTest _ =
            case Parser.run (Parser.problem "custom error") "anything" of
                Err _ ->
                    True

                Ok _ ->
                    False


        -- ====== Parser.commit ======

        commitTest : Int -> Result (List Parser.DeadEnd) Int
        commitTest _ =
            Parser.run
                (Parser.succeed identity
                    |. Parser.commit ()
                    |= Parser.int
                )
                "42"


        -- ====== Parser.Advanced specific tests ======

        -- Test PA.run with custom problem types
        type MyProblem
            = ExpectingNumber
            | ExpectingComma

        advancedSucceed : Int -> Result (List (PA.DeadEnd Never MyProblem)) Int
        advancedSucceed _ =
            PA.run (PA.succeed 42) "anything"

        advancedIntParse : Int -> Result (List (PA.DeadEnd Never MyProblem)) Int
        advancedIntParse _ =
            PA.run (PA.int ExpectingNumber ExpectingNumber) "123"

        advancedSymbol : Int -> Result (List (PA.DeadEnd Never MyProblem)) ()
        advancedSymbol _ =
            PA.run (PA.symbol (PA.Token "," ExpectingComma)) ","

        advancedEnd : Int -> Result (List (PA.DeadEnd Never MyProblem)) ()
        advancedEnd _ =
            PA.run (PA.end ExpectingNumber) ""


        """"
        ;

    // ---------- compilation + environment ----------

    private static readonly Lazy<ElmInteractiveEnvironment.ParsedInteractiveEnvironment> s_env =
        new(
            () =>
            {
                var kernelModulesTree =
                    BundledFiles.CompilerSourceContainerFilesDefault.Value
                    .GetNodeAtPath(["elm-kernel-modules"])
                    ?? throw new Exception("Did not find elm-kernel-modules");

                // Add our test module to the kernel modules tree
                var treeWithTest =
                    kernelModulesTree.SetNodeAtPathSorted(
                        ["ParserTest.elm"],
                        FileTree.File(Encoding.UTF8.GetBytes(TestModuleSource)));

                var rootFilePaths =
                    treeWithTest.EnumerateFilesTransitive()
                    .Where(
                        b =>
                        b.path[^1].Equals("ParserTest.elm", StringComparison.OrdinalIgnoreCase))
                    .Select(b => (IReadOnlyList<string>)b.path)
                    .ToList();

                var compiledEnv =
                    ElmCompiler.CompileInteractiveEnvironment(
                        treeWithTest,
                        rootFilePaths: rootFilePaths,
                        disableInlining: false)
                    .Extract(err => throw new Exception("Failed compiling: " + err));

                return
                    ElmInteractiveEnvironment.ParseInteractiveEnvironment(compiledEnv)
                    .Extract(err => throw new Exception("Failed parsing: " + err));
            });

    private static PineValue GetTestFunction(string name) =>
        s_env.Value.Modules
        .First(m => m.moduleName is "ParserTest")
        .moduleContent.FunctionDeclarations[name];

    private static readonly Core.Interpreter.IntermediateVM.PineVM s_vm =
        ElmCompilerTestHelper.PineVMForProfiling(_ => { });

    private static ElmValue ApplyUnary(PineValue functionValue, ElmValue argument) =>
        CoreLibraryTestHelper.ApplyUnary(functionValue, argument, s_vm);

    private static ElmValue Integer(long i) =>
        ElmValue.Integer(i);

    private static ElmValue String(string s) =>
        ElmValue.StringInstance(s);

    private static ElmValue ElmList(params ElmValue[] items) =>
        ElmValue.ListInstance([.. items]);

    private static ElmValue OkOf(ElmValue inner) =>
        ElmValue.TagInstance("Ok", [inner]);

    private static ElmValue ErrOf(ElmValue inner) =>
        ElmValue.TagInstance("Err", [inner]);

    private static readonly ElmValue s_true = ElmValue.TagInstance("True", []);
    private static readonly ElmValue s_false = ElmValue.TagInstance("False", []);
    private static readonly ElmValue s_nothing = ElmValue.TagInstance("Nothing", []);
    private static readonly ElmValue s_unit = ElmValue.ListInstance([]);

    private static ElmValue JustOf(ElmValue inner) =>
        ElmValue.TagInstance("Just", [inner]);

    private static ElmValue ElmTuple(ElmValue a, ElmValue b) =>
        ElmValue.ListInstance([a, b]);

    private static ElmValue CallThunk(string name) =>
        ApplyUnary(GetTestFunction(name), Integer(0));

    // ===== Parser.run + succeed =====

    [Fact]
    public void Run_succeed_int()
    {
        CallThunk("runSucceedInt").Should().Be(OkOf(Integer(90210)));
    }

    [Fact]
    public void Run_succeed_unit()
    {
        CallThunk("runSucceedUnit").Should().Be(OkOf(s_unit));
    }

    // ===== Parser.int =====

    [Fact]
    public void Parse_int_123456()
    {
        CallThunk("parseInt123456").Should().Be(OkOf(Integer(123456)));
    }

    [Fact]
    public void Parse_int_1()
    {
        CallThunk("parseInt1").Should().Be(OkOf(Integer(1)));
    }

    [Fact]
    public void Parse_int_1234()
    {
        CallThunk("parseInt1234").Should().Be(OkOf(Integer(1234)));
    }

    [Fact]
    public void Parse_int_empty_fails()
    {
        CallThunk("parseIntEmpty").Should().Be(s_true);
    }

    [Fact]
    public void Parse_int_letters_fails()
    {
        CallThunk("parseIntLetters").Should().Be(s_true);
    }

    // ===== Parser.symbol =====

    [Fact]
    public void Symbol_bracket_ok()
    {
        CallThunk("symbolBracket").Should().Be(OkOf(s_unit));
    }

    [Fact]
    public void Symbol_bracket_fail()
    {
        CallThunk("symbolBracketFail").Should().Be(s_true);
    }

    // ===== Parser.keyword =====

    [Fact]
    public void Keyword_let_ok()
    {
        CallThunk("keywordLet").Should().Be(OkOf(s_unit));
    }

    [Fact]
    public void Keyword_let_var_fail()
    {
        CallThunk("keywordLetVar").Should().Be(s_true);
    }

    [Fact]
    public void Keyword_let_letters_fail()
    {
        CallThunk("keywordLetLetters").Should().Be(s_true);
    }

    [Fact]
    public void Keyword_true_ok()
    {
        CallThunk("keywordTrue").Should().Be(OkOf(s_unit));
    }

    [Fact]
    public void Keyword_true_bang_ok()
    {
        CallThunk("keywordTrueBang").Should().Be(OkOf(s_unit));
    }

    // ===== Parser.token =====

    [Fact]
    public void Token_hello_ok()
    {
        CallThunk("tokenHello").Should().Be(OkOf(s_unit));
    }

    [Fact]
    public void Token_fail()
    {
        CallThunk("tokenFail").Should().Be(s_true);
    }

    // ===== Parser.end =====

    [Fact]
    public void End_after_int_ok()
    {
        CallThunk("endAfterInt").Should().Be(OkOf(Integer(123)));
    }

    [Fact]
    public void End_fail()
    {
        CallThunk("endFail").Should().Be(s_true);
    }

    // ===== Parser.map =====

    [Fact]
    public void Map_just_int_0()
    {
        CallThunk("mapJustInt0").Should().Be(OkOf(JustOf(Integer(0))));
    }

    [Fact]
    public void Map_just_int_13()
    {
        CallThunk("mapJustInt13").Should().Be(OkOf(JustOf(Integer(13))));
    }

    [Fact]
    public void Map_null()
    {
        CallThunk("mapNull").Should().Be(OkOf(s_nothing));
    }

    [Fact]
    public void Map_zero_fail()
    {
        CallThunk("mapZero").Should().Be(s_true);
    }

    // ===== Parser.oneOf =====

    [Fact]
    public void OneOf_first_match()
    {
        CallThunk("oneOfFirstMatch").Should().Be(OkOf(String("keyword")));
    }

    [Fact]
    public void OneOf_second_match()
    {
        CallThunk("oneOfSecondMatch").Should().Be(OkOf(String("keyword")));
    }

    // ===== Parser.andThen =====

    [Fact]
    public void ZipCode_valid()
    {
        CallThunk("zipCodeValid").Should().Be(OkOf(String("12345")));
    }

    [Fact]
    public void ZipCode_invalid()
    {
        CallThunk("zipCodeInvalid").Should().Be(s_true);
    }

    [Fact]
    public void ZipCode_too_long()
    {
        CallThunk("zipCodeTooLong").Should().Be(s_true);
    }

    // ===== Parser.lazy =====

    [Fact]
    public void Lazy_simple()
    {
        CallThunk("lazySimple").Should().Be(OkOf(ElmValue.TagInstance("Literal", [Integer(42)])));
    }

    [Fact]
    public void Lazy_nested()
    {
        CallThunk("lazyNested").Should().Be(
            OkOf(ElmValue.TagInstance("Negate", [ElmValue.TagInstance("Literal", [Integer(42)])])));
    }

    [Fact]
    public void Lazy_double_nested()
    {
        CallThunk("lazyDoubleNested").Should().Be(
            OkOf(ElmValue.TagInstance("Negate",
                [ElmValue.TagInstance("Negate", [ElmValue.TagInstance("Literal", [Integer(42)])])])));
    }

    // ===== Pipeline: |= and |. =====

    [Fact]
    public void Pipeline_point()
    {
        var result = CallThunk("pipelinePoint");
        var expected = OkOf(new ElmValue.ElmRecord(
            [("x", Integer(3)), ("y", Integer(4))]));
        result.Should().Be(expected);
    }

    [Fact]
    public void Pipeline_point_no_spaces()
    {
        var result = CallThunk("pipelinePointNoSpaces");
        var expected = OkOf(new ElmValue.ElmRecord(
            [("x", Integer(10)), ("y", Integer(20))]));
        result.Should().Be(expected);
    }

    // ===== Parser.chompIf =====

    [Fact]
    public void ChompIf_digit()
    {
        CallThunk("chompIfDigit").Should().Be(OkOf(String("5")));
    }

    [Fact]
    public void ChompIf_fail()
    {
        CallThunk("chompIfFail").Should().Be(s_true);
    }

    // ===== Parser.chompWhile =====

    [Fact]
    public void ChompWhile_digits()
    {
        CallThunk("chompWhileDigits").Should().Be(OkOf(String("12345")));
    }

    [Fact]
    public void ChompWhile_empty()
    {
        CallThunk("chompWhileEmpty").Should().Be(OkOf(String("")));
    }

    // ===== Parser.chompUntil =====

    [Fact]
    public void ChompUntil_semicolon()
    {
        CallThunk("chompUntilSemicolon").Should().Be(OkOf(String("hello")));
    }

    [Fact]
    public void ChompUntil_multi_char()
    {
        CallThunk("chompUntilMultiChar").Should().Be(OkOf(String("comment content ")));
    }

    // ===== Parser.chompUntilEndOr =====

    [Fact]
    public void ChompUntilEndOr_newline()
    {
        CallThunk("chompUntilEndOrNewline").Should().Be(OkOf(String("first line")));
    }

    [Fact]
    public void ChompUntilEndOr_found()
    {
        CallThunk("chompUntilEndOrFound").Should().Be(OkOf(String("hello")));
    }

    // ===== Parser.getChompedString =====

    [Fact]
    public void GetChompedString_digits()
    {
        CallThunk("getChompedDigits").Should().Be(OkOf(String("123")));
    }

    // ===== Parser.backtrackable =====

    [Fact]
    public void Backtrackable_test()
    {
        // The Elm lambda (\_ -> "ident") discards the chomped string "hello" and returns "ident".
        // The first oneOf branch (backtrackable parens parser) fails on "hello" and backtracks,
        // so the second branch matches, chomping "hello" and applying the lambda.
        CallThunk("backtrackableTest").Should().Be(OkOf(String("ident")));
    }

    // ===== Parser.loop =====

    [Fact]
    public void Loop_int_list()
    {
        CallThunk("loopIntList").Should().Be(
            OkOf(ElmList(Integer(1), Integer(2), Integer(3))));
    }

    // ===== Parser.spaces =====

    [Fact]
    public void Spaces_test()
    {
        CallThunk("spacesTest").Should().Be(OkOf(Integer(42)));
    }

    // ===== Parser.getPosition / getRow / getCol / getOffset / getSource =====

    [Fact]
    public void GetPosition_after_symbol()
    {
        CallThunk("posAfterSymbol").Should().Be(OkOf(ElmTuple(Integer(1), Integer(3))));
    }

    [Fact]
    public void GetRow_test()
    {
        CallThunk("getRowTest").Should().Be(OkOf(Integer(1)));
    }

    [Fact]
    public void GetCol_test()
    {
        CallThunk("getColTest").Should().Be(OkOf(Integer(4)));
    }

    [Fact]
    public void GetOffset_test()
    {
        // "hello" is 5 chars; the parser tracks offset in bytes (4 bytes per code point)
        CallThunk("getOffsetTest").Should().Be(OkOf(Integer(20)));
    }

    [Fact]
    public void GetSource_test()
    {
        CallThunk("getSourceTest").Should().Be(OkOf(String("abc")));
    }

    // ===== Parser.variable =====

    [Fact]
    public void Variable_simple()
    {
        CallThunk("variableSimple").Should().Be(OkOf(String("myVar123")));
    }

    [Fact]
    public void Variable_reserved()
    {
        CallThunk("variableReserved").Should().Be(s_true);
    }

    [Fact]
    public void Variable_underscore()
    {
        CallThunk("variableUnderscore").Should().Be(OkOf(String("my_var")));
    }

    [Fact]
    public void Variable_start_upper()
    {
        CallThunk("variableStartUpper").Should().Be(s_true);
    }

    // ===== Parser.lineComment =====

    [Fact]
    public void LineComment_test()
    {
        CallThunk("lineCommentTest").Should().Be(OkOf(String("hello")));
    }

    // ===== Parser.multiComment =====

    [Fact]
    public void MultiComment_test()
    {
        CallThunk("multiCommentTest").Should().Be(OkOf(String("hello")));
    }

    // ===== Parser.sequence =====

    [Fact]
    public void Sequence_forbidden()
    {
        CallThunk("sequenceForbidden").Should().Be(
            OkOf(ElmList(Integer(1), Integer(2), Integer(3))));
    }

    [Fact]
    public void Sequence_empty()
    {
        CallThunk("sequenceEmpty").Should().Be(OkOf(ElmList()));
    }

    [Fact]
    public void Sequence_single()
    {
        CallThunk("sequenceSingle").Should().Be(OkOf(ElmList(Integer(42))));
    }

    // ===== Parser.mapChompedString =====

    [Fact]
    public void MapChompedString_test()
    {
        CallThunk("mapChompedStringTest").Should().Be(
            OkOf(ElmTuple(String("42"), Integer(42))));
    }

    // ===== Parser.problem =====

    [Fact]
    public void Problem_test()
    {
        CallThunk("problemTest").Should().Be(s_true);
    }

    // ===== Parser.commit =====

    [Fact]
    public void Commit_test()
    {
        CallThunk("commitTest").Should().Be(OkOf(Integer(42)));
    }

    // ===== Parser.Advanced =====

    [Fact]
    public void Advanced_succeed()
    {
        CallThunk("advancedSucceed").Should().Be(OkOf(Integer(42)));
    }

    [Fact]
    public void Advanced_int_parse()
    {
        CallThunk("advancedIntParse").Should().Be(OkOf(Integer(123)));
    }

    [Fact]
    public void Advanced_symbol()
    {
        CallThunk("advancedSymbol").Should().Be(OkOf(s_unit));
    }

    [Fact]
    public void Advanced_end()
    {
        CallThunk("advancedEnd").Should().Be(OkOf(s_unit));
    }
}
