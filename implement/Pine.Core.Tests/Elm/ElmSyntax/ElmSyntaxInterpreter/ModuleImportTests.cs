using AwesomeAssertions;
using Pine.Core.Elm.ElmInElm;
using Xunit;

using ElmInterpreter = Pine.Core.Elm.ElmSyntax.ElmSyntaxInterpreter;

namespace Pine.Core.Tests.Elm.ElmSyntax.ElmSyntaxInterpreter;

/// <summary>
/// End-to-end coverage of the multi-module overload of
/// <see cref="ElmInterpreter.ParseAndInterpret(string, System.Collections.Generic.IReadOnlyList{string})"/>,
/// which wraps the supplied root expression in a dedicated synthetic module
/// (with no explicit <c>import</c> statements) and reuses the Elm compiler's
/// <see cref="Core.Elm.ElmCompilerInDotnet.Canonicalization"/> pass to qualify
/// references in every module — including the synthetic root module — before
/// dispatching the interpreter.
///
/// <para>
/// Because the synthetic root module has no explicit imports, no user-supplied
/// module's <c>import ... as Alias</c> or <c>exposing (..)</c> clause leaks into
/// the root expression's lexical scope. This gives REPL-like semantics: the root
/// expression must reference declarations from user-supplied modules using their
/// fully-qualified canonical names (e.g. <c>Foo.bar</c>, never <c>F.bar</c> or a
/// bare <c>bar</c>). The Elm compiler's <em>implicit imports</em>
/// (<c>Basics</c> exposed; canonical names of <c>List</c>, <c>Char</c>,
/// <c>String</c>, <c>Maybe</c>, <c>Result</c>, <c>Debug</c>, <c>Tuple</c>,
/// <c>Platform</c>; aliases <c>Cmd</c>/<c>Sub</c>; and a small set of exposed
/// names listed in <c>guide/elm-programming-language-semantics.md</c>) <em>do</em>
/// apply to the synthetic root module as they would to any regular module.
/// </para>
///
/// Each test exercises one form of <c>import</c> statement found in
/// <c>implement/Pine.Core/Elm/elm-in-elm</c> (qualified-only, alias, exposing names,
/// exposing types with and without constructors, exposing operators, open exposing,
/// dotted module names, and combinations of alias + exposing). To exercise the
/// alias / exposing forms (which only affect a module's <em>own</em> body, never
/// the root expression's scope), the alias / exposing usage is placed inside an
/// intermediate module and the root expression calls into that intermediate
/// module by its fully-qualified name.
/// </summary>
public class ModuleImportTests
{
    private static string EvaluateRendered(string expression, params string[] modules)
    {
        var result = ElmInterpreter.ParseAndInterpret(expression, modules);

        var value = result.Extract(err => throw new System.Exception(err.ToString()));

        return Core.Elm.ElmValue.RenderAsElmExpression(value).expressionString;
    }

    // ============================================================
    // Form: `import Module`
    //   - Reference must be fully qualified at the call site.
    // ============================================================

    [Fact]
    public void Plain_qualified_reference_resolves_to_dependency_module()
    {
        EvaluateRendered(
            "Foo.bar",
            """
            module Foo exposing (..)


            bar : Int
            bar =
                42
            """)
            .Should().Be("42");
    }

    [Fact]
    public void Plain_qualified_function_application_resolves()
    {
        EvaluateRendered(
            "Foo.identity 7",
            """
            module Foo exposing (..)


            identity : a -> a
            identity x =
                x
            """)
            .Should().Be("7");
    }

    // ============================================================
    // Form: `import Module as Alias`
    //   - The alias only applies inside the module that declares the import.
    //   - Root expression cannot use the alias; it must use the canonical name.
    // ============================================================

    [Fact]
    public void Aliased_import_resolves_inside_declaring_module_and_root_calls_via_canonical_name()
    {
        // The alias `F` only exists inside `Mid`. The root expression accesses
        // the result via `Mid.value`, the canonical name of the intermediate
        // module's exposed declaration.
        EvaluateRendered(
            "Mid.value",
            """
            module Mid exposing (..)

            import Foo as F


            value : Int
            value =
                F.bar
            """,
            """
            module Foo exposing (..)


            bar : Int
            bar =
                42
            """)
            .Should().Be("42");
    }

    [Fact]
    public void Aliased_dotted_import_resolves_inside_declaring_module()
    {
        EvaluateRendered(
            "Mid.value",
            """
            module Mid exposing (..)

            import Foo.Sub.Deep as B


            value : Int
            value =
                B.bar
            """,
            """
            module Foo.Sub.Deep exposing (..)


            bar : Int
            bar =
                11
            """)
            .Should().Be("11");
    }

    [Fact]
    public void Root_expression_cannot_reference_declaration_via_alias_from_user_module()
    {
        // Even though `Mid` imports `Foo as F`, the alias `F` is not in scope
        // for the root expression — only inside `Mid`. The interpreter must
        // therefore fail when the root expression tries to use `F.bar`.
        var result =
            ElmInterpreter.ParseAndInterpret(
                "F.bar",
                [
                    """
                    module Mid exposing (..)

                    import Foo as F
                    """,
                    """
                    module Foo exposing (..)


                    bar : Int
                    bar =
                        42
                    """,
                ]);

        result.IsErrOrNull().Should().NotBeNull();
    }

    // ============================================================
    // Form: `import Module exposing (a, b)`
    //   - Listed names available unqualified inside the importing module only.
    // ============================================================

    [Fact]
    public void Exposing_specific_names_brings_them_unqualified_into_declaring_module()
    {
        EvaluateRendered(
            "Mid.value",
            """
            module Mid exposing (..)

            import Foo exposing (bar)


            value : Int
            value =
                bar
            """,
            """
            module Foo exposing (..)


            bar : Int
            bar =
                42


            other : Int
            other =
                7
            """)
            .Should().Be("42");
    }

    [Fact]
    public void Exposing_names_does_not_expose_unlisted_names_in_declaring_module()
    {
        // Inside `Mid`, `bar` is not exposed (only `other` is). `Mid.value`
        // tries to use the bare `bar`, which the canonicalizer cannot resolve.
        var result =
            ElmInterpreter.ParseAndInterpret(
                "Mid.value",
                [
                    """
                    module Mid exposing (..)

                    import Foo exposing (other)


                    value : Int
                    value =
                        bar
                    """,
                    """
                    module Foo exposing (..)


                    bar : Int
                    bar =
                        42


                    other : Int
                    other =
                        7
                    """,
                ]);

        result.IsErrOrNull().Should().NotBeNull();
    }

    [Fact]
    public void Root_expression_cannot_reference_unqualified_name_exposed_only_in_user_module()
    {
        // `Mid` exposes `bar` from `Foo`, but the root expression's lexical
        // scope is independent of `Mid`'s imports. A bare `bar` in the root
        // expression is therefore unresolved.
        var result =
            ElmInterpreter.ParseAndInterpret(
                "bar",
                [
                    """
                    module Mid exposing (..)

                    import Foo exposing (bar)
                    """,
                    """
                    module Foo exposing (..)


                    bar : Int
                    bar =
                        42
                    """,
                ]);

        result.IsErrOrNull().Should().NotBeNull();
    }

    // ============================================================
    // Form: `import Module exposing (Type)`
    //   - Type name in scope inside the importing module, but constructors only
    //     via the qualified path.
    // ============================================================

    [Fact]
    public void Exposing_type_only_without_constructors_qualified_constructor_works()
    {
        // `MyMaybe` (the type) is exposed in `Mid`, but the constructor `Just`
        // is only reachable as `MyMaybe.Just` because the import does not use
        // the `(..)` constructor expansion. The root expression accesses the
        // result via `Mid.value`.
        EvaluateRendered(
            "Mid.value",
            """
            module Mid exposing (..)

            import MyMaybe exposing (MyMaybe)


            value : MyMaybe Int
            value =
                MyMaybe.Just 5
            """,
            """
            module MyMaybe exposing (..)


            type MyMaybe a
                = Just a
                | Nothing
            """)
            .Should().Be("Just 5");
    }

    // ============================================================
    // Form: `import Module exposing (Type(..))`
    //   - Type and ALL its constructors in scope unqualified inside the
    //     importing module.
    // ============================================================

    [Fact]
    public void Exposing_type_with_all_constructors_in_declaring_module()
    {
        EvaluateRendered(
            "Mid.value",
            """
            module Mid exposing (..)

            import MyMaybe exposing (MyMaybe(..))


            value : MyMaybe Int
            value =
                Just 7
            """,
            """
            module MyMaybe exposing (..)


            type MyMaybe a
                = Just a
                | Nothing
            """)
            .Should().Be("Just 7");
    }

    [Fact]
    public void Exposing_type_with_all_constructors_supports_pattern_match_in_declaring_module()
    {
        EvaluateRendered(
            "Mid.classify (MyMaybe.Just 9)",
            """
            module Mid exposing (..)

            import MyMaybe exposing (MyMaybe(..))


            classify : MyMaybe Int -> Int
            classify v =
                case v of
                    Just n ->
                        n

                    Nothing ->
                        0
            """,
            """
            module MyMaybe exposing (..)


            type MyMaybe a
                = Just a
                | Nothing
            """)
            .Should().Be("9");
    }

    // ============================================================
    // Form: `import Module exposing (..)` (open exposing)
    //   - Every exposed name from the module in scope unqualified inside the
    //     importing module.
    // ============================================================

    [Fact]
    public void Open_exposing_brings_all_values_into_declaring_module()
    {
        EvaluateRendered(
            "Mid.value",
            """
            module Mid exposing (..)

            import Foo exposing (..)


            value : Int
            value =
                bar
            """,
            """
            module Foo exposing (..)


            bar : Int
            bar =
                7
            """)
            .Should().Be("7");
    }

    [Fact]
    public void Open_exposing_brings_constructors_into_declaring_module()
    {
        EvaluateRendered(
            "Mid.value",
            """
            module Mid exposing (..)

            import MyMaybe exposing (..)


            value : MyMaybe Int
            value =
                Just 3
            """,
            """
            module MyMaybe exposing (..)


            type MyMaybe a
                = Just a
                | Nothing
            """)
            .Should().Be("Just 3");
    }

    // ============================================================
    // Form: `import Module.Sub.Path` (multi-segment module names)
    // ============================================================

    [Fact]
    public void Dotted_module_name_resolves_qualified_reference_from_root()
    {
        EvaluateRendered(
            "Foo.Sub.bar",
            """
            module Foo.Sub exposing (..)


            bar : Int
            bar =
                100
            """)
            .Should().Be("100");
    }

    [Fact]
    public void Dotted_module_name_with_open_exposing_in_declaring_module()
    {
        EvaluateRendered(
            "Mid.value",
            """
            module Mid exposing (..)

            import Foo.Sub.Deep exposing (..)


            value : Int
            value =
                bar
            """,
            """
            module Foo.Sub.Deep exposing (..)


            bar : Int
            bar =
                21
            """)
            .Should().Be("21");
    }

    // ============================================================
    // Form: `import Module as Alias exposing (...)`
    //   - Combination: alias for qualified path AND specific names unqualified
    //     (both effective only inside the importing module).
    // ============================================================

    [Fact]
    public void Alias_combined_with_exposing_uses_alias_qualified_in_declaring_module()
    {
        EvaluateRendered(
            "Mid.value",
            """
            module Mid exposing (..)

            import Foo as F exposing (baz)


            value : Int
            value =
                F.bar
            """,
            """
            module Foo exposing (..)


            bar : Int
            bar =
                1


            baz : Int
            baz =
                2
            """)
            .Should().Be("1");
    }

    [Fact]
    public void Alias_combined_with_exposing_uses_unqualified_exposed_name_in_declaring_module()
    {
        EvaluateRendered(
            "Mid.value",
            """
            module Mid exposing (..)

            import Foo as F exposing (baz)


            value : Int
            value =
                baz
            """,
            """
            module Foo exposing (..)


            bar : Int
            bar =
                1


            baz : Int
            baz =
                2
            """)
            .Should().Be("2");
    }

    [Fact]
    public void Alias_combined_with_exposing_type_constructors_in_declaring_module()
    {
        EvaluateRendered(
            "Mid.classify (MyMaybe.Just 4)",
            """
            module Mid exposing (..)

            import MyMaybe as M exposing (MyMaybe(..))


            classify : MyMaybe Int -> Int
            classify v =
                case v of
                    Just n ->
                        n

                    Nothing ->
                        0
            """,
            """
            module MyMaybe exposing (..)


            type MyMaybe a
                = Just a
                | Nothing
            """)
            .Should().Be("4");
    }

    // ============================================================
    // Transitive imports: A imports B, evaluating uses A which uses B.
    // ============================================================

    [Fact]
    public void Transitive_import_resolves_through_intermediate_module()
    {
        EvaluateRendered(
            "Foo.value",
            """
            module Foo exposing (..)

            import Bar


            value : Int
            value =
                Bar.something
            """,
            """
            module Bar exposing (..)


            something : Int
            something =
                99
            """)
            .Should().Be("99");
    }

    [Fact]
    public void Multiple_qualified_references_from_root_resolve_independently()
    {
        // The root expression references two unrelated modules using their
        // fully-qualified canonical names. `Pine_builtin.int_add` is a
        // language-level primitive recognised by the interpreter.
        EvaluateRendered(
            "Pine_builtin.int_add [ Foo.x, Bar.y ]",
            """
            module Foo exposing (..)


            x : Int
            x =
                10
            """,
            """
            module Bar exposing (..)


            y : Int
            y =
                32
            """)
            .Should().Be("42");
    }

    // ============================================================
    // Sanity: declarations sharing a simple name in two modules are kept
    // distinct under their fully-qualified canonical names.
    // ============================================================

    [Fact]
    public void Same_name_in_two_modules_disambiguated_by_canonical_name()
    {
        EvaluateRendered(
            "Pine_builtin.int_add [ ModuleA.value, ModuleB.value ]",
            """
            module ModuleA exposing (..)


            value : Int
            value =
                3
            """,
            """
            module ModuleB exposing (..)


            value : Int
            value =
                4
            """)
            .Should().Be("7");
    }

    // ============================================================
    // Implicit imports apply to the synthetic root module.
    //
    // `guide/elm-programming-language-semantics.md` specifies that several
    // declarations from the core library are implicitly exposed (and several
    // modules are implicitly imported under their canonical names). These
    // tests verify that those names are also reachable from the root
    // expression even though the synthetic root module has no explicit
    // `import` statements.
    // ============================================================

    private static readonly string s_basicsSource =
        System.Text.Encoding.UTF8.GetString(
            ((Files.FileTree.FileNode)BundledFiles.ElmKernelModulesDefault.Value
                .GetNodeAtPath(["Basics.elm"])!).Bytes.Span);

    private static readonly string s_listSource =
        System.Text.Encoding.UTF8.GetString(
            ((Files.FileTree.FileNode)BundledFiles.ElmKernelModulesDefault.Value
                .GetNodeAtPath(["List.elm"])!).Bytes.Span);

    private static readonly string s_maybeSource =
        System.Text.Encoding.UTF8.GetString(
            ((Files.FileTree.FileNode)BundledFiles.ElmKernelModulesDefault.Value
                .GetNodeAtPath(["Maybe.elm"])!).Bytes.Span);

    private static readonly string s_resultSource =
        System.Text.Encoding.UTF8.GetString(
            ((Files.FileTree.FileNode)BundledFiles.ElmKernelModulesDefault.Value
                .GetNodeAtPath(["Result.elm"])!).Bytes.Span);

    [Fact]
    public void Implicit_import_brings_True_into_root_scope()
    {
        // `True` is implicitly exposed from `Basics` per the spec.
        EvaluateRendered("True", s_basicsSource)
            .Should().Be("True");
    }

    [Fact]
    public void Implicit_import_brings_Basics_operators_into_root_scope()
    {
        // `+` is the implicit operator alias for `Basics.add`.
        EvaluateRendered("1 + 2", s_basicsSource)
            .Should().Be("3");
    }

    [Fact]
    public void Implicit_import_brings_Just_constructor_into_root_scope()
    {
        // `Just` is implicitly exposed from `Maybe` per the spec.
        EvaluateRendered("Just 7", s_basicsSource, s_maybeSource)
            .Should().Be("Just 7");
    }

    [Fact]
    public void Implicit_import_brings_Nothing_constructor_into_root_scope()
    {
        EvaluateRendered("Nothing", s_basicsSource, s_maybeSource)
            .Should().Be("Nothing");
    }

    [Fact]
    public void Implicit_import_brings_Ok_constructor_into_root_scope()
    {
        // `Ok` is implicitly exposed from `Result` per the spec.
        EvaluateRendered("Ok 5", s_basicsSource, s_resultSource)
            .Should().Be("Ok 5");
    }

    [Fact]
    public void Implicit_import_brings_Err_constructor_into_root_scope()
    {
        EvaluateRendered("Err \"boom\"", s_basicsSource, s_resultSource)
            .Should().Be("Err \"boom\"");
    }

    [Fact]
    public void Implicit_import_makes_List_module_qualified_access_available()
    {
        // `List` is implicitly imported under its canonical name, so qualified
        // access like `List.length` requires no explicit import in the root
        // module.
        EvaluateRendered("List.length [ 1, 2, 3 ]", s_basicsSource, s_listSource)
            .Should().Be("3");
    }

    [Fact]
    public void Implicit_import_brings_cons_operator_into_root_scope()
    {
        // The `::` infix is implicitly available from `List`.
        EvaluateRendered("1 :: [ 2, 3 ]", s_basicsSource, s_listSource)
            .Should().Be("[ 1, 2, 3 ]");
    }
}
