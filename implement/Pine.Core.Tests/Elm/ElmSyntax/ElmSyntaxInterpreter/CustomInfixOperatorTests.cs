using AwesomeAssertions;
using Pine.Core.Elm;
using Xunit;

using ElmInterpreter = Pine.Core.Elm.ElmSyntax.ElmSyntaxInterpreter;

namespace Pine.Core.Tests.Elm.ElmSyntax.ElmSyntaxInterpreter;

/// <summary>
/// Verifies that <see cref="ElmInterpreter"/> dispatches infix-operator usage through the
/// <c>infix … = funcName</c> declarations actually present in the parsed Elm source, rather
/// than through any hard-coded operator table. To make the difference observable, every
/// operator in this test module is bound to a <em>counterfactual</em> implementation: e.g.
/// the <c>(+)</c> operator is wired to a function that returns <c>a - b</c>, the <c>(*)</c>
/// operator to one that returns <c>a + b</c>, and a custom <c>(&lt;/&gt;)</c> operator to a
/// function that doubles its left operand before adding. If the interpreter were using a
/// built-in operator table, these tests would observe the standard arithmetic results
/// instead.
/// <para />
/// The same module also exposes the operators in the prefix form <c>(&lt;op&gt;)</c> as
/// values, used both as fully-applied prefix calls and as partially-applied / higher-order
/// arguments, to exercise the <c>PrefixOperator</c> AST branch.
/// </summary>
public class CustomInfixOperatorTests
{
    /// <summary>
    /// A single Elm module that defines four custom operators with implementations that
    /// disagree with the standard <c>Basics</c> module on purpose:
    /// <list type="bullet">
    /// <item><c>+</c> is implemented as subtraction.</item>
    /// <item><c>*</c> is implemented as addition.</item>
    /// <item><c>-</c> is implemented as multiplication.</item>
    /// <item><c>&lt;/&gt;</c> is implemented as <c>2 * a + b</c>.</item>
    /// </list>
    /// Every test below uses the same module so the operator-to-function dispatch is the
    /// only thing under test.
    /// </summary>
    private const string ModuleText =
        """
        module Test exposing (..)


        infix left  6 (+)   = customAdd
        infix left  7 (*)   = customMul
        infix left  6 (-)   = customSub
        infix left  6 (</>) = weightedAdd


        customAdd : Int -> Int -> Int
        customAdd a b =
            Pine_kernel.int_add [ a, Pine_kernel.int_mul [ b, -1 ] ]


        customMul : Int -> Int -> Int
        customMul a b =
            Pine_kernel.int_add [ a, b ]


        customSub : Int -> Int -> Int
        customSub a b =
            Pine_kernel.int_mul [ a, b ]


        weightedAdd : Int -> Int -> Int
        weightedAdd a b =
            Pine_kernel.int_add [ Pine_kernel.int_mul [ 2, a ], b ]


        applyOp : (Int -> Int -> Int) -> Int -> Int -> Int
        applyOp f a b =
            f a b


        useCustomInfix : Int -> Int -> Int
        useCustomInfix a b =
            a </> b
        """;

    /// <summary>
    /// Renders the result of evaluating <paramref name="expression"/> against the test module
    /// to its Elm-expression form, using the same snapshot-style assertion pattern as
    /// <c>NamedPattern_with_shuffled_order_in_source_code</c>.
    /// </summary>
    private static string Evaluate(string expression) =>
        ElmValue.RenderAsElmExpression(
            InterpreterTestHelper.EvaluateInModuleOrCrash(expression, ModuleText))
        .expressionString;

    // ============================================================
    // Infix usage – proves dispatch goes through the declared functions
    // ============================================================

    [Fact]
    public void Plus_operator_dispatches_to_customAdd_which_subtracts()
    {
        // If `(+)` were hard-coded, this would be 12. The infix decl routes it through
        // `customAdd`, which subtracts.
        Evaluate("7 + 5").Should().Be("2");
    }

    [Fact]
    public void Star_operator_dispatches_to_customMul_which_adds()
    {
        // `customMul a b = a + b`, so 3 * 4 must be 7, not 12.
        Evaluate("3 * 4").Should().Be("7");
    }

    [Fact]
    public void Minus_operator_dispatches_to_customSub_which_multiplies()
    {
        // `customSub a b = a * b`, so 3 - 4 must be 12, not -1.
        Evaluate("3 - 4").Should().Be("12");
    }

    [Fact]
    public void Custom_operator_uses_its_declared_implementation()
    {
        // `weightedAdd a b = 2 * a + b` (using kernel arithmetic), so `5 </> 3 = 13`.
        // The interpreter's free-standing root-expression parser doesn't know which
        // module's infix declarations are in scope, so we exercise the OperatorApplication
        // path through a helper function defined alongside the infix decl in the same
        // module.
        Evaluate("useCustomInfix 5 3").Should().Be("13");
    }

    [Fact]
    public void Multiple_operators_in_one_expression_each_use_their_declared_function()
    {
        // 7 + 5 (= 7 - 5 = 2), then 2 * 4 (= 2 + 4 = 6).
        // Using the standard Elm precedence the parser pairs `*` tighter than `+`, but
        // since both implementations have been redefined, the result still depends purely
        // on the declared functions.
        // (7 + 5) * 4 with custom semantics = (7 - 5) + 4 = 6
        Evaluate("(7 + 5) * 4").Should().Be("6");
    }

    [Fact]
    public void Operator_left_associativity_is_preserved_with_custom_function()
    {
        // `customSub` (multiply) is left-associative at precedence 6.
        // 2 - 3 - 4 parses as (2 - 3) - 4 = (2 * 3) * 4 = 24.
        Evaluate("2 - 3 - 4").Should().Be("24");
    }

    // ============================================================
    // Prefix-form usage – the (<op>) syntax as a fully-applied call
    // ============================================================

    [Fact]
    public void Prefix_form_of_plus_calls_customAdd()
    {
        Evaluate("(+) 10 3").Should().Be("7");
    }

    [Fact]
    public void Prefix_form_of_star_calls_customMul()
    {
        Evaluate("(*) 10 3").Should().Be("13");
    }

    [Fact]
    public void Prefix_form_of_custom_operator_calls_weightedAdd()
    {
        Evaluate("(</>) 5 3").Should().Be("13");
    }

    // ============================================================
    // Partial application of operators in prefix form
    // ============================================================

    [Fact]
    public void Partially_applied_prefix_operator_returns_a_function_value()
    {
        // `(+) 10` produces a closure of arity 1; applying it to 4 yields customAdd(10, 4) = 6.
        Evaluate(
            """
            let
                addTo10 =
                    (+) 10
            in
            addTo10 4
            """)
            .Should().Be("6");
    }

    [Fact]
    public void Partially_applied_custom_operator_threads_through_let_binding()
    {
        // `(</>) 5` is `\b -> 2 * 5 + b = 10 + b`, so applied to 7 yields 17.
        Evaluate(
            """
            let
                f = (</>) 5
            in
            f 7
            """)
            .Should().Be("17");
    }

    // ============================================================
    // Higher-order: operator value passed as an argument
    // ============================================================

    [Fact]
    public void Operator_passed_as_higher_order_argument_uses_declared_function()
    {
        // `applyOp f a b = f a b`. When we pass `(*)` as `f`, the call inside `applyOp`
        // resolves to `customMul`, so applyOp (*) 10 3 must return 13, not 30.
        Evaluate("applyOp (*) 10 3").Should().Be("13");
    }

    [Fact]
    public void Different_operator_values_can_be_chosen_by_higher_order_dispatch()
    {
        // Pick `(+)` (= customAdd, subtract) vs `(*)` (= customMul, add) at runtime by the
        // same `applyOp` helper, demonstrating that each prefix-operator value is
        // independently bound to its own infix declaration's function.
        Evaluate("applyOp (+) 10 3").Should().Be("7");
        Evaluate("applyOp (-) 10 3").Should().Be("30");
        Evaluate("applyOp (</>) 10 3").Should().Be("23");
    }

    [Fact]
    public void Operator_value_can_be_returned_from_a_function()
    {
        // A let-bound chain that picks an operator function via a value-dependent expression
        // and immediately applies it. The actual implementation must be the user-declared one.
        Evaluate(
            """
            let
                op =
                    (+)
            in
            op 9 4
            """)
            .Should().Be("5");
    }
}
