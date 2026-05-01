using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
using Pine.Core.Elm;
using System;
using System.Linq;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.ElmCompilerTests;

public class SimpleRecordPatternTests
{
    [Fact]
    public void Record_pattern_destructuring_three_fields()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            decl { alfa, gamma, beta } =
                [ alfa, beta, gamma ]

            """";

        var parseCache = new PineVMParseCache();

        var (parsedEnv, _) =
            ElmCompilerTestHelper.CompileElmModules(
                [elmModuleText],
                disableInlining: false);

        var testModule =
            parsedEnv.Modules.FirstOrDefault(c => c.moduleName is "Test");

        var declValue =
            testModule.moduleContent.FunctionDeclarations
            .FirstOrDefault(decl => decl.Key is "decl");

        var declParsed =
            FunctionRecord.ParseFunctionRecordTagged(declValue.Value, parseCache)
            .Extract(err => throw new Exception("Failed parsing " + nameof(declValue) + ": " + err));

        var invokeFunction = ElmCompilerTestHelper.CreateFunctionInvocationDelegate(declParsed);

        PineValue ApplyForArgument(PineValue argument)
        {
            var (applyRunResult, _) = invokeFunction([argument]);

            return applyRunResult.ReturnValue.Evaluate();
        }

        string ApplyForArgumentAsExpressionString(PineValue argument)
        {
            var applyRunResult = ApplyForArgument(argument);

            var resultAsElmValue =
                ElmValueEncoding.PineValueAsElmValue(applyRunResult, null, null);

            var resultAsElmExpr =
                ElmValue.RenderAsElmExpression(
                    resultAsElmValue
                    .Extract(err => throw new Exception("Failed decoding result as Elm value: " + err)));

            return resultAsElmExpr.expressionString;
        }

        // Helper to create a record with three fields: { alfa, beta, gamma }
        // Record encoding: ["Elm_Record", [[["alfa", alfaValue], ["beta", betaValue], ["gamma", gammaValue]]]]
        // Fields are sorted alphabetically
        PineValue CreateRecord(int alfa, int beta, int gamma)
        {
            return
                ElmValueEncoding.ElmRecordAsPineValue(
                    [
                    ("alfa", IntegerEncoding.EncodeSignedInteger(alfa)),
                    ("beta", IntegerEncoding.EncodeSignedInteger(beta)),
                    ("gamma", IntegerEncoding.EncodeSignedInteger(gamma))
                    ]);
        }

        // Test case: { alfa = 1, beta = 2, gamma = 3 } -> [1, 2, 3]
        // Note: decl { alfa, gamma, beta } = [ alfa, beta, gamma ]
        // So output is [alfa, beta, gamma] which with the input becomes [1, 2, 3]
        {
            var result =
                ApplyForArgumentAsExpressionString(CreateRecord(1, 2, 3));

            result.Should().Be("[ 1, 2, 3 ]");
        }

        // Test case: { alfa = 10, beta = 20, gamma = 30 }
        {
            var result =
                ApplyForArgumentAsExpressionString(CreateRecord(10, 20, 30));

            result.Should().Be("[ 10, 20, 30 ]");
        }

        // Test case: { alfa = -5, beta = 0, gamma = 100 }
        {
            var result =
                ApplyForArgumentAsExpressionString(CreateRecord(-5, 0, 100));

            result.Should().Be("[ -5, 0, 100 ]");
        }
    }

    [Fact]
    public void Record_pattern_with_two_fields()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            decl { x, y } =
                Pine_builtin.int_add [ x, y ]

            """";

        var parseCache = new PineVMParseCache();

        var (parsedEnv, _) =
            ElmCompilerTestHelper.CompileElmModules(
                [elmModuleText],
                disableInlining: false);

        var testModule =
            parsedEnv.Modules.FirstOrDefault(c => c.moduleName is "Test");

        var declValue =
            testModule.moduleContent.FunctionDeclarations
            .FirstOrDefault(decl => decl.Key is "decl");

        var declParsed =
            FunctionRecord.ParseFunctionRecordTagged(declValue.Value, parseCache)
            .Extract(err => throw new Exception("Failed parsing " + nameof(declValue) + ": " + err));

        var invokeFunction = ElmCompilerTestHelper.CreateFunctionInvocationDelegate(declParsed);

        PineValue ApplyForArgument(PineValue argument)
        {
            var (applyRunResult, _) = invokeFunction([argument]);

            return applyRunResult.ReturnValue.Evaluate();
        }

        string ApplyForArgumentAsExpressionString(PineValue argument)
        {
            var applyRunResult = ApplyForArgument(argument);

            var resultAsElmValue =
                ElmValueEncoding.PineValueAsElmValue(applyRunResult, null, null);

            var resultAsElmExpr =
                ElmValue.RenderAsElmExpression(
                    resultAsElmValue
                    .Extract(err => throw new Exception("Failed decoding result as Elm value: " + err)));

            return resultAsElmExpr.expressionString;
        }

        PineValue CreateRecord(int x, int y)
        {
            return
                ElmValueEncoding.ElmRecordAsPineValue(
                    [
                    ("x", IntegerEncoding.EncodeSignedInteger(x)),
                    ("y", IntegerEncoding.EncodeSignedInteger(y))
                    ]);
        }

        // Test case: { x = 3, y = 5 } -> 8
        {
            var result =
                ApplyForArgumentAsExpressionString(CreateRecord(3, 5));

            result.Should().Be("8");
        }

        // Test case: { x = 10, y = 20 } -> 30
        {
            var result =
                ApplyForArgumentAsExpressionString(CreateRecord(10, 20));

            result.Should().Be("30");
        }

        // Test case: { x = -7, y = 12 } -> 5
        {
            var result =
                ApplyForArgumentAsExpressionString(CreateRecord(-7, 12));

            result.Should().Be("5");
        }
    }

    [Fact]
    public void Record_pattern_single_field()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            decl { value } =
                Pine_builtin.int_mul [ value, 2 ]

            """";

        var parseCache = new PineVMParseCache();

        var (parsedEnv, _) =
            ElmCompilerTestHelper.CompileElmModules(
                [elmModuleText],
                disableInlining: false);

        var testModule =
            parsedEnv.Modules.FirstOrDefault(c => c.moduleName is "Test");

        var declValue =
            testModule.moduleContent.FunctionDeclarations
            .FirstOrDefault(decl => decl.Key is "decl");

        var declParsed =
            FunctionRecord.ParseFunctionRecordTagged(declValue.Value, parseCache)
            .Extract(err => throw new Exception("Failed parsing " + nameof(declValue) + ": " + err));

        var invokeFunction = ElmCompilerTestHelper.CreateFunctionInvocationDelegate(declParsed);

        PineValue ApplyForArgument(PineValue argument)
        {
            var (applyRunResult, _) = invokeFunction([argument]);

            return applyRunResult.ReturnValue.Evaluate();
        }

        string ApplyForArgumentAsExpressionString(PineValue argument)
        {
            var applyRunResult = ApplyForArgument(argument);

            var resultAsElmValue =
                ElmValueEncoding.PineValueAsElmValue(applyRunResult, null, null);

            var resultAsElmExpr =
                ElmValue.RenderAsElmExpression(
                    resultAsElmValue
                    .Extract(err => throw new Exception("Failed decoding result as Elm value: " + err)));

            return resultAsElmExpr.expressionString;
        }

        PineValue CreateRecord(int value)
        {
            return
                ElmValueEncoding.ElmRecordAsPineValue(
                    [
                    ("value", IntegerEncoding.EncodeSignedInteger(value))
                    ]);
        }

        // Test case: { value = 7 } -> 14
        {
            var result =
                ApplyForArgumentAsExpressionString(CreateRecord(7));

            result.Should().Be("14");
        }

        // Test case: { value = 25 } -> 50
        {
            var result =
                ApplyForArgumentAsExpressionString(CreateRecord(25));

            result.Should().Be("50");
        }
    }

    [Fact]
    public void Record_pattern_in_let_binding()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            decl arg =
                let
                    { a, b } =
                        arg
                in
                Pine_builtin.int_mul [ a, b ]

            """";

        var parseCache = new PineVMParseCache();

        var (parsedEnv, _) =
            ElmCompilerTestHelper.CompileElmModules(
                [elmModuleText],
                disableInlining: false);

        var testModule =
            parsedEnv.Modules.FirstOrDefault(c => c.moduleName is "Test");

        var declValue =
            testModule.moduleContent.FunctionDeclarations
            .FirstOrDefault(decl => decl.Key is "decl");

        var declParsed =
            FunctionRecord.ParseFunctionRecordTagged(declValue.Value, parseCache)
            .Extract(err => throw new Exception("Failed parsing " + nameof(declValue) + ": " + err));

        var invokeFunction = ElmCompilerTestHelper.CreateFunctionInvocationDelegate(declParsed);

        PineValue ApplyForArgument(PineValue argument)
        {
            var (applyRunResult, _) = invokeFunction([argument]);

            return applyRunResult.ReturnValue.Evaluate();
        }

        string ApplyForArgumentAsExpressionString(PineValue argument)
        {
            var applyRunResult = ApplyForArgument(argument);

            var resultAsElmValue =
                ElmValueEncoding.PineValueAsElmValue(applyRunResult, null, null);

            var resultAsElmExpr =
                ElmValue.RenderAsElmExpression(
                    resultAsElmValue
                    .Extract(err => throw new Exception("Failed decoding result as Elm value: " + err)));

            return resultAsElmExpr.expressionString;
        }

        PineValue CreateRecord(int a, int b)
        {
            return
                ElmValueEncoding.ElmRecordAsPineValue(
                    [
                    ("a", IntegerEncoding.EncodeSignedInteger(a)),
                    ("b", IntegerEncoding.EncodeSignedInteger(b))
                    ]);
        }

        // Test case: { a = 3, b = 4 } -> 12
        {
            var result =
                ApplyForArgumentAsExpressionString(CreateRecord(3, 4));

            result.Should().Be("12");
        }

        // Test case: { a = 5, b = 6 } -> 30
        {
            var result =
                ApplyForArgumentAsExpressionString(CreateRecord(5, 6));

            result.Should().Be("30");
        }
    }

    /// <summary>
    /// Compiles <paramref name="elmModuleText"/>, looks up the
    /// <c>decl</c> top-level declaration in module <c>Test</c>, applies it
    /// to <paramref name="argument"/>, and returns the rendered Elm
    /// expression form of the result.
    /// </summary>
    private static string CompileAndApplyDecl(string elmModuleText, PineValue argument)
    {
        var parseCache = new PineVMParseCache();

        var (parsedEnv, _) =
            ElmCompilerTestHelper.CompileElmModules(
                [elmModuleText],
                disableInlining: false);

        var declValue =
            parsedEnv.Modules
            .First(c => c.moduleName is "Test")
            .moduleContent.FunctionDeclarations
            .First(decl => decl.Key is "decl");

        var declParsed =
            FunctionRecord.ParseFunctionRecordTagged(declValue.Value, parseCache)
            .Extract(err => throw new Exception("Failed parsing decl: " + err));

        var invokeFunction = ElmCompilerTestHelper.CreateFunctionInvocationDelegate(declParsed);

        var (applyRunResult, _) = invokeFunction([argument]);

        var resultPine = applyRunResult.ReturnValue.Evaluate();

        return
            ElmValue.RenderAsElmExpression(
                ElmValueEncoding.PineValueAsElmValue(resultPine, null, null)
                .Extract(err => throw new Exception("Failed decoding result: " + err)))
            .expressionString;
    }

    /// <summary>
    /// Builds an Elm record value with the given int-valued fields.
    /// </summary>
    private static PineValue IntRecord(params (string name, int value)[] fields) =>
        ElmValueEncoding.ElmRecordAsPineValue(
            [
            .. fields.Select(
                f =>
                (f.name, IntegerEncoding.EncodeSignedInteger(f.value)))
            ]);

    // ===================================================================
    // Coverage for the bug described in
    // RecordDestructureLetBindingRegressionTests:
    // `AnalyzeRecordPattern` used the pattern's local loop index `i` to
    // index into the record's full fields list. The mis-indexing only
    // mattered when the pattern named a *strict subset* of the record's
    // fields, and is silently masked when the pattern lists every field
    // (which the original tests above all did). The cases below cover the
    // bug specifically and extrapolate to scenarios that exercise nearby
    // code paths.
    // ===================================================================

    /// <summary>
    /// Direct minimal reproducer: a 2-field record destructured by a
    /// pattern that names ONLY the second field. Before the fix, the
    /// pattern's loop index 0 read field 0 of the record and bound the
    /// name <c>second</c> to the value of <c>first</c>.
    /// </summary>
    [Fact]
    public void Record_pattern_in_let_binds_named_field_when_pattern_omits_first_field()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            decl arg =
                let
                    { second } =
                        arg
                in
                second

            """";

        var rendered =
            CompileAndApplyDecl(
                elmModuleText,
                IntRecord(("first", 100), ("second", 200)));

        rendered.Should().Be("200");
    }

    /// <summary>
    /// Mirror of the LanguageService scenario the original bisection
    /// surfaced: two fields, the pattern names only the second, and the
    /// extracted value is a list. Before the fix, this returned the value
    /// of <c>fromDeclarations</c> bound to the name <c>hoverItems</c>.
    /// </summary>
    [Fact]
    public void Record_pattern_in_let_binds_named_list_field_not_earlier_list_field()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            buildRecord : Int -> { fromDeclarations : List Int, hoverItems : List Int }
            buildRecord _ =
                { fromDeclarations = [ 1, 2, 3 ]
                , hoverItems = [ 7, 8 ]
                }


            decl arg =
                let
                    { hoverItems } =
                        buildRecord arg
                in
                hoverItems

            """";

        var rendered =
            CompileAndApplyDecl(elmModuleText, IntegerEncoding.EncodeSignedInteger(0));

        rendered.Should().Be("[ 7, 8 ]");
    }

    /// <summary>
    /// 4-field record, pattern names only the last (alphabetically). With
    /// the buggy index-by-position the bound name would receive the
    /// value of the first record field.
    /// </summary>
    [Fact]
    public void Record_pattern_in_let_binds_last_alphabetical_field_when_pattern_names_only_one()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            decl arg =
                let
                    { delta } =
                        arg
                in
                delta

            """";

        var rendered =
            CompileAndApplyDecl(
                elmModuleText,
                IntRecord(
                    ("alpha", 1),
                    ("beta", 2),
                    ("gamma", 3),
                    ("delta", 4)));

        rendered.Should().Be("4");
    }

    /// <summary>
    /// Pattern names a non-contiguous subset of a 4-field record. Verifies
    /// each name is independently looked up: the buggy code would line up
    /// the pattern's two names with the record's first two slots.
    /// </summary>
    [Fact]
    public void Record_pattern_in_let_binds_non_contiguous_subset()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            decl arg =
                let
                    { beta, delta } =
                        arg
                in
                Pine_builtin.int_add [ beta, delta ]

            """";

        var rendered =
            CompileAndApplyDecl(
                elmModuleText,
                IntRecord(
                    ("alpha", 100),
                    ("beta", 20),
                    ("gamma", 300),
                    ("delta", 4)));

        // Must add the named fields beta + delta = 24, NOT
        // alpha + beta = 120 (which is what the buggy positional code
        // would produce after sorting the pattern's names alphabetically:
        // ["beta","delta"] mapped to record slots 0 and 1).
        rendered.Should().Be("24");
    }

    /// <summary>
    /// Same subset shape but the destructure happens directly in the
    /// function's parameter pattern rather than in a <c>let</c>. The
    /// fix has to apply to every record-pattern site, not only the
    /// surface-syntactic <c>let</c> form.
    /// </summary>
    [Fact]
    public void Record_pattern_as_function_parameter_binds_named_subset()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            decl { beta, delta } =
                Pine_builtin.int_add [ beta, delta ]

            """";

        var rendered =
            CompileAndApplyDecl(
                elmModuleText,
                IntRecord(
                    ("alpha", 100),
                    ("beta", 20),
                    ("gamma", 300),
                    ("delta", 4)));

        rendered.Should().Be("24");
    }

    /// <summary>
    /// Record-pattern destructuring inside a lambda parameter. Mirrors
    /// the lang-server <c>List.map (\{ hoverItems } -&gt; ...)</c> shape.
    /// </summary>
    [Fact]
    public void Record_pattern_as_lambda_parameter_binds_named_subset()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            decl arg =
                (\{ hoverItems } -> hoverItems) arg

            """";

        var rendered =
            CompileAndApplyDecl(
                elmModuleText,
                IntRecord(("fromDeclarations", 999), ("hoverItems", 11)));

        rendered.Should().Be("11");
    }

    /// <summary>
    /// Record-pattern destructuring inside a <c>case</c> branch. Verifies
    /// the pattern compiler's <c>case</c> entry point uses the same
    /// fixed name-based lookup as <c>let</c>.
    /// </summary>
    [Fact]
    public void Record_pattern_in_case_branch_binds_named_subset()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            decl arg =
                case arg of
                    { hoverItems } ->
                        hoverItems

            """";

        var rendered =
            CompileAndApplyDecl(
                elmModuleText,
                IntRecord(("fromDeclarations", 999), ("hoverItems", 42)));

        rendered.Should().Be("42");
    }

    /// <summary>
    /// Record-pattern destructure of a record returned by a function
    /// call (rather than the function's argument). This is the exact
    /// shape that surfaced the bug in the LanguageService:
    /// <c>let { hoverItems } = hoverItemsFromParsedModule ...</c>.
    /// </summary>
    [Fact]
    public void Record_pattern_in_let_destructures_function_call_result()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            buildRecord : Int -> { fromDeclarations : Int, hoverItems : Int }
            buildRecord seed =
                { fromDeclarations = Pine_builtin.int_mul [ seed, 10 ]
                , hoverItems = Pine_builtin.int_add [ seed, 1 ]
                }


            decl arg =
                let
                    { hoverItems } =
                        buildRecord arg
                in
                hoverItems

            """";

        var rendered =
            CompileAndApplyDecl(elmModuleText, IntegerEncoding.EncodeSignedInteger(5));

        // Must be `seed + 1` = 6, NOT `seed * 10` = 50.
        rendered.Should().Be("6");
    }

    /// <summary>
    /// Two record-pattern <c>let</c> bindings on the same record, naming
    /// disjoint subsets. Verifies the second destructure does not reuse
    /// stale indices from the first.
    /// </summary>
    [Fact]
    public void Record_pattern_in_let_two_disjoint_subsets_on_same_record()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            decl arg =
                let
                    { alpha } =
                        arg

                    { gamma } =
                        arg
                in
                Pine_builtin.int_add [ alpha, gamma ]

            """";

        var rendered =
            CompileAndApplyDecl(
                elmModuleText,
                IntRecord(("alpha", 7), ("beta", 9999), ("gamma", 13)));

        rendered.Should().Be("20");
    }

    /// <summary>
    /// Nested scenario: pattern names a single field, that field is
    /// itself a record which we destructure again. Both layers must use
    /// name-based lookup for this to round-trip.
    /// </summary>
    [Fact]
    public void Record_pattern_in_let_nested_destructure_through_record_field()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            decl arg =
                let
                    { inner } =
                        arg

                    { y } =
                        inner
                in
                y

            """";

        var inner =
            ElmValueEncoding.ElmRecordAsPineValue(
                [
                ("x", IntegerEncoding.EncodeSignedInteger(111)),
                ("y", IntegerEncoding.EncodeSignedInteger(222))
                ]);

        var outer =
            ElmValueEncoding.ElmRecordAsPineValue(
                [
                ("filler", IntegerEncoding.EncodeSignedInteger(0)),
                ("inner", inner)
                ]);

        var rendered = CompileAndApplyDecl(elmModuleText, outer);

        rendered.Should().Be("222");
    }

    /// <summary>
    /// A 5-field record where the pattern names exactly one field at
    /// each alphabetical position (first, middle, last). Parametrizes
    /// the bug: the buggy positional code produced a different wrong
    /// answer for each of these but always agreed with the correct
    /// answer on the first slot.
    /// </summary>
    [Theory]
    [InlineData("alfa", 1)]
    [InlineData("bravo", 2)]
    [InlineData("charlie", 3)]
    [InlineData("delta", 4)]
    [InlineData("echo", 5)]
    public void Record_pattern_in_let_binds_each_field_individually(string fieldName, int expected)
    {
        var elmModuleText =
            $$""""
            module Test exposing (..)


            decl arg =
                let
                    { {{fieldName}} } =
                        arg
                in
                {{fieldName}}

            """";

        var rendered =
            CompileAndApplyDecl(
                elmModuleText,
                IntRecord(
                    ("alfa", 1),
                    ("bravo", 2),
                    ("charlie", 3),
                    ("delta", 4),
                    ("echo", 5)));

        rendered.Should().Be(expected.ToString());
    }

    /// <summary>
    /// Focused semantic regression for the
    /// <c>elm-syntax/src/Elm/Syntax/Node.elm</c> <c>combine</c> idiom:
    /// <code>
    /// combine f ((Node { start } _) as a) ((Node { end } _) as b) =
    ///     Node { start = start, end = end } (f a b)
    /// </code>
    /// Combines the start of <c>a</c>'s range with the end of <c>b</c>'s
    /// range. Under the buggy positional fallback both
    /// <c>{ start }</c> and <c>{ end }</c> resolved to slot 0 of the
    /// alphabetically-sorted record (= the <c>end</c> field), so the
    /// combined range had both endpoints drawn from <c>end</c>. This
    /// test exercises the exact shape (record-pattern subset inside an
    /// <c>as</c> pattern) without any type annotations on the
    /// declaration, so the compiler must take the unknown-type
    /// runtime-lookup fallback.
    /// </summary>
    [Fact]
    public void Record_pattern_subset_in_as_pattern_drives_node_combine_semantics()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            combineRanges (({ start } as a)) (({ end } as b)) =
                { aStart = start
                , bEnd = end
                , aEnd = a.end
                , bStart = b.start
                }


            decl _ =
                let
                    rangeA =
                        { start = 11, end = 22 }

                    rangeB =
                        { start = 33, end = 44 }

                    combined =
                        combineRanges rangeA rangeB
                in
                Pine_builtin.int_add
                    [ Pine_builtin.int_mul [ combined.aStart, 1000 ]
                    , Pine_builtin.int_mul [ combined.bEnd, 100 ]
                    , Pine_builtin.int_mul [ combined.aEnd, 10 ]
                    , combined.bStart
                    ]

            """";

        var rendered =
            CompileAndApplyDecl(
                elmModuleText,
                IntegerEncoding.EncodeSignedInteger(0));

        // Expected: aStart = 11, bEnd = 44, aEnd = 22, bStart = 33
        // 11 * 1000 + 44 * 100 + 22 * 10 + 33 = 11000 + 4400 + 220 + 33 = 15653.
        // Under the buggy positional code (subset patterns reading slot 0
        // of the alphabetically-sorted record, which is `end`):
        //   aStart -> a.end = 22, bEnd -> b.end = 44 (matches by accident),
        //   aEnd  -> a.end = 22 (matches),  bStart -> b.start = 33 (matches).
        // The aStart slot is the discriminator: 22 * 1000 + 4400 + 220 + 33 = 26653.
        rendered.Should().Be("15653");
    }
}
