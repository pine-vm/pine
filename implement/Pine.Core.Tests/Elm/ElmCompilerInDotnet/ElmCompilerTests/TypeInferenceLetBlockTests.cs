using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.ElmCompilerTests;

public class TypeInferenceLetBlockTests
{
    /*
     * In these tests, we compose expressions using applications of operators that accept operands of the `number` type class.
     * Therefore, without additional typing information, the compiler could not replace these operations with integer-specific builtins.
     * */

    [Fact]
    public void Int_mul_for_operand_from_let_binding_alias()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            alfa : Int -> Int -> Int
            alfa x y =
                let
                    a = x
                in
                a * (y + 3)

            """";

        var parseCache = new PineVMParseCache();

        var (parsedEnv, _) =
            ElmCompilerTestHelper.CompileElmModules(
                [elmModuleText],
                disableInlining: true);

        var wholeProgramText =
            ElmCompilerTestHelper.ParseAndRenderStaticProgram(
                parsedEnv,
                includeDeclaration: qualifiedName => qualifiedName.DeclName is "alfa",
                parseCache: parseCache);

        wholeProgramText.Trim().Should().Be(
            """"

            Test.alfa param_1_0 param_1_1 =
                Pine_builtin.int_mul
                    [ param_1_0
                    , Pine_builtin.int_add
                        [ param_1_1
                        , 3
                        ]
                    ]
            
            """"
            .Trim());
    }

    [Fact]
    public void Int_mul_for_operand_from_let_binding_chained_alias()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            alfa : Int -> Int -> Int
            alfa x y =
                let
                    a = x

                    b = a
                in
                b * (y + 3)

            """";

        var parseCache = new PineVMParseCache();

        var (parsedEnv, _) =
            ElmCompilerTestHelper.CompileElmModules(
                [elmModuleText],
                disableInlining: true);

        var wholeProgramText =
            ElmCompilerTestHelper.ParseAndRenderStaticProgram(
                parsedEnv,
                includeDeclaration: qualifiedName => qualifiedName.DeclName is "alfa",
                parseCache: parseCache);

        wholeProgramText.Trim().Should().Be(
            """"

            Test.alfa param_1_0 param_1_1 =
                Pine_builtin.int_mul
                    [ param_1_0
                    , Pine_builtin.int_add
                        [ param_1_1
                        , 3
                        ]
                    ]
            
            """"
            .Trim());
    }

    [Fact]
    public void Int_mul_for_operand_from_chained_let_binding_alias()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            alfa : Int -> Int -> Int
            alfa x y =
                let
                    a = x
                in
                let
                    b = a
                in
                b * (y + 3)

            """";

        var parseCache = new PineVMParseCache();

        var (parsedEnv, _) =
            ElmCompilerTestHelper.CompileElmModules(
                [elmModuleText],
                disableInlining: true);

        var wholeProgramText =
            ElmCompilerTestHelper.ParseAndRenderStaticProgram(
                parsedEnv,
                includeDeclaration: qualifiedName => qualifiedName.DeclName is "alfa",
                parseCache: parseCache);

        wholeProgramText.Trim().Should().Be(
            """"

            Test.alfa param_1_0 param_1_1 =
                Pine_builtin.int_mul
                    [ param_1_0
                    , Pine_builtin.int_add
                        [ param_1_1
                        , 3
                        ]
                    ]
            
            """"
            .Trim());
    }

    [Fact]
    public void Int_mul_for_operand_from_let_binding_sum()
    {
        // Should infer that 'a' is constrained to 'Int' as result from (Int + number)

        var elmModuleText =
            """"
            module Test exposing (..)


            alfa : Int -> Int -> Int
            alfa x y =
                let
                    a =
                        x + 13
                in
                a * (y + 7)

            """";

        var parseCache = new PineVMParseCache();

        var (parsedEnv, _) =
            ElmCompilerTestHelper.CompileElmModules(
                [elmModuleText],
                disableInlining: true);

        var wholeProgramText =
            ElmCompilerTestHelper.ParseAndRenderStaticProgram(
                parsedEnv,
                includeDeclaration: qualifiedName => qualifiedName.DeclName is "alfa",
                parseCache: parseCache);

        wholeProgramText.Trim().Should().Be(
            """"

            Test.alfa param_1_0 param_1_1 =
                Pine_builtin.int_mul
                    [ Pine_builtin.int_add
                        [ param_1_0
                        , 13
                        ]
                    , Pine_builtin.int_add
                        [ param_1_1
                        , 7
                        ]
                    ]
            
            """"
            .Trim());
    }

    /// <summary>
    /// Mirrors the exact form that drives the
    /// <c>References_request_finds_usage_across_modules</c> language
    /// service scenario: a let-destructuring pattern names a single
    /// field of a record returned by a multi-arg function call whose
    /// signature explicitly declares the record return type. The
    /// compiler must emit a static field-access (<c>Pine_builtin.head</c>
    /// / <c>Pine_builtin.skip</c>) rather than the runtime-lookup
    /// fallback that walks every (name,value) pair via
    /// <see cref="Core.Elm.ElmCompilerInDotnet.RecordRuntime.PineFunctionForRecordAccessAsValue"/>.
    /// </summary>
    [Fact]
    public void Let_destructure_named_field_of_record_returned_by_multi_arg_call_uses_static_field_access()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            buildRecord : Int -> Int -> { fromDeclarations : List Int, hoverItems : List Int }
            buildRecord _ _ =
                { fromDeclarations = [ 1, 2, 3 ]
                , hoverItems = [ 7, 8 ]
                }


            decl : Int -> Int -> List Int
            decl x y =
                let
                    { hoverItems } =
                        buildRecord x y
                in
                hoverItems

            """";

        var parseCache = new PineVMParseCache();

        var (parsedEnv, _) =
            ElmCompilerTestHelper.CompileElmModules(
                [elmModuleText],
                disableInlining: true);

        var wholeProgramText =
            ElmCompilerTestHelper.ParseAndRenderStaticProgram(
                parsedEnv,
                includeDeclaration: qualifiedName => qualifiedName.DeclName is "decl",
                parseCache: parseCache);

        // The body must reach the `hoverItems` field by static slot,
        // not via a runtime-lookup `Pine_builtin.parse_and_eval` of
        // RecordRuntime.PineFunctionForRecordAccessAsValue.
        wholeProgramText.Should().NotContain("parse_and_eval");
    }

    /// <summary>
    /// Same as
    /// <see cref="Let_destructure_named_field_of_record_returned_by_multi_arg_call_uses_static_field_access"/>
    /// but the let-destructuring lives inside nested <c>case</c>
    /// branches whose patterns introduce the bindings used as the
    /// arguments to the call. This is the shape used in
    /// <c>LanguageService.elm</c>'s <c>provideHover</c>,
    /// <c>provideReferences</c>, and <c>findReferencesInModule</c>
    /// helpers.
    /// </summary>
    [Fact]
    public void Let_destructure_named_field_inside_nested_case_branches_uses_static_field_access()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            type CacheItem
                = NoCacheItem
                | CacheItem { syntax : Int }


            type Outer
                = Empty
                | HasItem CacheItem


            buildRecord : Int -> Int -> { fromDeclarations : List Int, hoverItems : List Int }
            buildRecord _ _ =
                { fromDeclarations = [ 1, 2, 3 ]
                , hoverItems = [ 7, 8 ]
                }


            decl : Outer -> Int -> List Int
            decl outer state =
                case outer of
                    Empty ->
                        []

                    HasItem item ->
                        case item of
                            NoCacheItem ->
                                []

                            CacheItem record ->
                                let
                                    { hoverItems } =
                                        buildRecord record.syntax state
                                in
                                hoverItems

            """";

        var parseCache = new PineVMParseCache();

        var (parsedEnv, _) =
            ElmCompilerTestHelper.CompileElmModules(
                [elmModuleText],
                disableInlining: true);

        var wholeProgramText =
            ElmCompilerTestHelper.ParseAndRenderStaticProgram(
                parsedEnv,
                includeDeclaration: qualifiedName => qualifiedName.DeclName is "decl",
                parseCache: parseCache);

        wholeProgramText.Should().NotContain("parse_and_eval");
    }

    /// <summary>
    /// The function parameter pattern destructures a record whose
    /// type is a <em>type alias</em> for a record type. Mirrors the
    /// <c>rangeFromRecordRange : Elm.Syntax.Range.Range -&gt; Range</c>
    /// helper in <c>LanguageService.elm</c>. The compiler must
    /// recognize the alias <c>Layout</c> as a record type and emit a
    /// static field-access; otherwise the runtime-lookup fallback
    /// kicks in and every call pays an O(n) field walk per pattern
    /// field, which is the source of the runtime-efficiency
    /// regression in
    /// <c>References_request_finds_usage_across_modules</c>.
    /// </summary>
    [Fact]
    public void Function_parameter_record_pattern_through_record_type_alias_uses_static_field_access()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            type alias Layout =
                { start : Int, end : Int }


            spread : Layout -> Int
            spread { start, end } =
                end - start

            """";

        var parseCache = new PineVMParseCache();

        var (parsedEnv, _) =
            ElmCompilerTestHelper.CompileElmModules(
                [elmModuleText],
                disableInlining: true);

        var wholeProgramText =
            ElmCompilerTestHelper.ParseAndRenderStaticProgram(
                parsedEnv,
                includeDeclaration: qualifiedName => qualifiedName.DeclName is "spread",
                parseCache: parseCache);

        wholeProgramText.Should().NotContain("parse_and_eval");
    }

    /// <summary>
    /// The let-destructured expression has a return type that is a
    /// <em>type alias</em> for a record type (declared in the same
    /// module). Mirrors the
    /// <c>let { start, end } = someFunctionReturningRange ...</c>
    /// shape that appears in
    /// <c>Elm.Syntax.Range.combineHelp</c>.
    /// </summary>
    [Fact]
    public void Let_destructure_through_record_type_alias_return_uses_static_field_access()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            type alias Layout =
                { start : Int, end : Int }


            mkLayout : Int -> Int -> Layout
            mkLayout s e =
                { start = s, end = e }


            spread : Int -> Int -> Int
            spread a b =
                let
                    { start, end } =
                        mkLayout a b
                in
                end - start

            """";

        var parseCache = new PineVMParseCache();

        var (parsedEnv, _) =
            ElmCompilerTestHelper.CompileElmModules(
                [elmModuleText],
                disableInlining: true);

        var wholeProgramText =
            ElmCompilerTestHelper.ParseAndRenderStaticProgram(
                parsedEnv,
                includeDeclaration: qualifiedName => qualifiedName.DeclName is "spread",
                parseCache: parseCache);

        wholeProgramText.Should().NotContain("parse_and_eval");
    }
}
