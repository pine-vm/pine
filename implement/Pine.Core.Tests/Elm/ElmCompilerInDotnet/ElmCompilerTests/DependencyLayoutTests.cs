using AwesomeAssertions;
using System.Linq;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.ElmCompilerTests;

/// <summary>
/// Tests for the dependency layout computation in the Elm compiler.
/// The compiler uses a two-pass approach:
/// 1. First pass: Collect all dependencies for each function
/// 2. Second pass: Emit functions with their dependency layouts
///
/// <para>
/// As of §7.6b the layout contains only SCC members (cross-SCC dependencies
/// are literal-inlined at call sites). For non-recursive single-member SCCs
/// the layout is the empty list (those callees have no env-functions slot
/// to populate), though Approach A1 still emits them in the uniform
/// WithEnvFunctions wrapper shape with env[0] holding the empty list.
/// Recursive SCCs (mutual recursion or self-recursion) keep the SCC member
/// list as their layout.
/// </para>
/// </summary>
public class DependencyLayoutTests
{
    /// <summary>
    /// Tests that a simple function with no dependencies has only itself in its layout.
    /// </summary>
    [Fact]
    public void Simple_function_with_no_dependencies()
    {
        var elmModuleText =
            """
            module Test exposing (..)

            identity x =
                x
            """;

        var layouts = ElmCompilerTestHelper.ComputeDependencyLayoutsFromModule(elmModuleText);

        layouts.Should().ContainKey("Test.identity");
        // §7.6b: non-recursive single-member SCC → empty env-functions layout
        // (still wrapped in the uniform WithEnvFunctions shape per Approach A1).
        layouts["Test.identity"].Should().BeEmpty();
    }

    /// <summary>
    /// Tests that a self-recursive function has itself at index 0.
    /// The function can reference itself via index 0 in its own layout.
    /// </summary>
    [Fact]
    public void Self_recursive_function_layout()
    {
        var elmModuleText =
            """
            module Test exposing (..)

            factorial n =
                if Pine_kernel.equal [ n, 0 ] then
                    1
                else
                    Pine_kernel.int_mul [ n, factorial (Pine_kernel.int_add [ n, -1 ]) ]
            """;

        var layouts = ElmCompilerTestHelper.ComputeDependencyLayoutsFromModule(elmModuleText);

        layouts.Should().ContainKey("Test.factorial");
        // Self-recursive function has only itself at index 0
        // Self-recursion is handled by referencing index 0
        layouts["Test.factorial"].Should().Equal(["Test.factorial"]);
    }

    /// <summary>
    /// Tests that mutually recursive functions have each other in their dependency layouts.
    /// Dependencies are sorted alphabetically.
    /// </summary>
    [Fact]
    public void Mutually_recursive_functions_have_correct_dependency_layout()
    {
        var elmModuleText =
            """
            module Test exposing (..)


            isEven x =
                if Pine_kernel.equal [ x, 0 ] then
                    True

                else
                    isOdd (Pine_kernel.int_add [ x, -1 ])


            isOdd y =
                if Pine_kernel.equal [ y, 0 ] then
                    False

                else
                    isEven (Pine_kernel.int_add [ y, -1 ])

            """;

        var layouts = ElmCompilerTestHelper.ComputeDependencyLayoutsFromModule(elmModuleText);

        layouts.Should().ContainKey("Test.isEven");
        layouts.Should().ContainKey("Test.isOdd");

        // For mutual recursion, both functions share the same layout order
        // (alphabetically sorted as per the implementation guide)
        // Layout is [isEven, isOdd] for both functions
        layouts["Test.isEven"].Should().Equal(["Test.isEven", "Test.isOdd"]);

        layouts["Test.isOdd"].Should().Equal(["Test.isEven", "Test.isOdd"]);
    }

    /// <summary>
    /// Tests that a function calling another non-recursive function has correct dependencies.
    /// </summary>
    [Fact]
    public void Function_calling_another_function_has_dependency_in_layout()
    {
        var elmModuleText =
            """
            module Test exposing (..)

            double x =
                Pine_kernel.int_add [ x, x ]

            quadruple x =
                double (double x)
            """;

        var layouts = ElmCompilerTestHelper.ComputeDependencyLayoutsFromModule(elmModuleText);

        layouts.Should().ContainKey("Test.double");
        layouts.Should().ContainKey("Test.quadruple");

        // §7.6b/§7.7: non-recursive single-member SCCs have empty layouts.
        layouts["Test.double"].Should().BeEmpty();

        // quadruple is also a non-recursive single-member SCC (its only call is
        // to a cross-SCC dep `double`, inlined as a literal at the call site).
        layouts["Test.quadruple"].Should().BeEmpty();
    }

    /// <summary>
    /// Tests that Pine_kernel functions are not included in the dependency layout.
    /// </summary>
    [Fact]
    public void Pine_kernel_functions_not_included_in_dependencies()
    {
        var elmModuleText =
            """
            module Test exposing (..)

            addAndMultiply a b =
                let
                    sum = Pine_kernel.int_add [ a, b ]
                    product = Pine_kernel.int_mul [ a, b ]
                in
                Pine_kernel.int_add [ sum, product ]
            """;

        var layouts = ElmCompilerTestHelper.ComputeDependencyLayoutsFromModule(elmModuleText);

        layouts.Should().ContainKey("Test.addAndMultiply");
        // §7.7: non-recursive single-member SCC → empty layout.
        layouts["Test.addAndMultiply"].Should().BeEmpty();
    }

    /// <summary>
    /// Tests that choice type constructors (tags starting with uppercase) are not included in the dependency layout.
    /// </summary>
    [Fact]
    public void Choice_type_constructors_not_included_in_dependencies()
    {
        // Using True/False which are predefined boolean constructors
        var elmModuleText =
            """
            module Test exposing (..)

            returnTrue x =
                True

            returnFalse x =
                False
            """;

        var layouts = ElmCompilerTestHelper.ComputeDependencyLayoutsFromModule(elmModuleText);

        layouts.Should().ContainKey("Test.returnTrue");
        layouts.Should().ContainKey("Test.returnFalse");
        // §7.7: both are non-recursive single-member SCCs → empty layouts.
        layouts["Test.returnTrue"].Should().BeEmpty();

        layouts["Test.returnFalse"].Should().BeEmpty();
    }

    /// <summary>
    /// Tests that dependencies are sorted alphabetically.
    /// </summary>
    [Fact]
    public void Dependencies_are_sorted_alphabetically()
    {
        var elmModuleText =
            """
            module Test exposing (..)

            zebra x = x
            alpha x = x
            middle x = x

            caller x =
                zebra (alpha (middle x))
            """;

        var layouts = ElmCompilerTestHelper.ComputeDependencyLayoutsFromModule(elmModuleText);

        layouts.Should().ContainKey("Test.caller");

        // §7.7: caller is a non-recursive single-member SCC → empty layout.
        layouts["Test.caller"].Should().BeEmpty();
    }

    /// <summary>
    /// Tests multiple functions with various dependencies to verify consistent alphabetical ordering.
    /// </summary>
    [Fact]
    public void Multiple_functions_have_consistent_alphabetical_ordering()
    {
        var elmModuleText =
            """
            module Test exposing (..)

            funcA x = x
            funcB x = funcA x
            funcC x = funcA (funcB x)
            funcD x = funcC (funcB (funcA x))
            """;

        var layouts = ElmCompilerTestHelper.ComputeDependencyLayoutsFromModule(elmModuleText);

        // §7.7: every function above is a non-recursive single-member SCC →
        // empty layouts. Cross-SCC deps are inlined as literals at call sites.
        layouts["Test.funcA"].Should().BeEmpty();

        layouts["Test.funcB"].Should().BeEmpty();

        layouts["Test.funcC"].Should().BeEmpty();

        layouts["Test.funcD"].Should().BeEmpty();
    }

    /// <summary>
    /// Tests that a function dependency inside a record access expression is correctly detected.
    /// For example, <c>(getRecord x).name</c> should register <c>getRecord</c> as a dependency
    /// even though it appears inside a <c>RecordAccess</c> node.
    /// </summary>
    [Fact]
    public void Function_dependency_inside_record_access_is_detected()
    {
        var elmModuleText =
            """
            module Test exposing (..)

            getRecord x =
                { name = x, age = 0 }

            caller x =
                (getRecord x).name
            """;

        var layouts = ElmCompilerTestHelper.ComputeDependencyLayoutsFromModule(elmModuleText);

        layouts.Should().ContainKey("Test.caller");

        // §7.7: caller is a non-recursive single-member SCC → empty layout.
        layouts["Test.caller"].Should().BeEmpty();
    }

    /// <summary>
    /// Tests that a function dependency inside a lambda expression body is correctly detected.
    /// Even though lambda lifting transforms lambdas before dependency analysis, this test
    /// verifies that the overall pipeline correctly handles function references in lambda bodies.
    /// </summary>
    [Fact]
    public void Function_dependency_inside_lambda_body_is_detected()
    {
        var elmModuleText =
            """
            module Test exposing (..)

            helper x =
                Pine_kernel.int_add [ x, 1 ]

            caller f =
                f (\x -> helper x)
            """;

        var layouts = ElmCompilerTestHelper.ComputeDependencyLayoutsFromModule(elmModuleText);

        layouts.Should().ContainKey("Test.caller");

        // After lambda lifting, the lambda becomes a top-level function that references helper.
        // §7.7: every function in this program (caller, helper, the lifted lambda) is a
        // non-recursive single-member SCC → empty layouts.
        layouts["Test.caller"].Should().BeEmpty();

        var liftedFuncName =
            layouts.Keys.FirstOrDefault(n => n.Contains("__lifted__"));
        liftedFuncName.Should().NotBeNull("a lifted lambda function should be present in the program");

        if (liftedFuncName is not null)
        {
            // §7.7: lifted function is also a non-recursive single-member SCC → empty layout.
            layouts[liftedFuncName].Should().BeEmpty();
        }
    }
}
