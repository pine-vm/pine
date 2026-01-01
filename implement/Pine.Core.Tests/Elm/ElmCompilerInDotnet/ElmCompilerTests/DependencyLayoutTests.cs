using AwesomeAssertions;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.ElmCompilerTests;

/// <summary>
/// Tests for the dependency layout computation in the Elm compiler.
/// The compiler uses a two-pass approach:
/// 1. First pass: Collect all dependencies for each function
/// 2. Second pass: Emit functions with their dependency layouts
/// 
/// Each function's layout contains [self, sorted_dependencies...] where dependencies are sorted alphabetically.
/// Self is always at index 0 for snapshot test name rendering.
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
        // Self is always at index 0, no other dependencies
        layouts["Test.identity"].Should().Equal(["Test.identity"]);
    }

    /// <summary>
    /// Tests that a self-recursive function has itself at index 0 and also in the sorted dependencies.
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
        // Self-recursive function has itself at index 0 AND in sorted dependencies
        // Layout: [self, self] since factorial calls factorial
        layouts["Test.factorial"].Should().Equal(["Test.factorial", "Test.factorial"]);
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

        // isEven depends on isOdd: layout is [self, isOdd]
        layouts["Test.isEven"].Should().Equal(["Test.isEven", "Test.isOdd"]);

        // isOdd depends on isEven: layout is [self, isEven]
        layouts["Test.isOdd"].Should().Equal(["Test.isOdd", "Test.isEven"]);
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

        // double has no user function dependencies, only self
        layouts["Test.double"].Should().Equal(["Test.double"]);

        // quadruple depends on double: [self, double]
        layouts["Test.quadruple"].Should().Equal(["Test.quadruple", "Test.double"]);
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
        // Only uses Pine_kernel functions, no user function dependencies, only self
        layouts["Test.addAndMultiply"].Should().Equal(["Test.addAndMultiply"]);
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
        // True and False are choice type constructors (boolean), not function dependencies, only self
        layouts["Test.returnTrue"].Should().Equal(["Test.returnTrue"]);
        layouts["Test.returnFalse"].Should().Equal(["Test.returnFalse"]);
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

        // Layout: [self, alpha, middle, zebra] - dependencies sorted alphabetically
        layouts["Test.caller"].Should().Equal(["Test.caller", "Test.alpha", "Test.middle", "Test.zebra"]);
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

        // funcA has no dependencies, only self
        layouts["Test.funcA"].Should().Equal(["Test.funcA"]);

        // funcB depends on funcA: [self, funcA]
        layouts["Test.funcB"].Should().Equal(["Test.funcB", "Test.funcA"]);

        // funcC depends on funcA and funcB, sorted alphabetically: [self, funcA, funcB]
        layouts["Test.funcC"].Should().Equal(["Test.funcC", "Test.funcA", "Test.funcB"]);

        // funcD depends on funcA, funcB, funcC, sorted alphabetically: [self, funcA, funcB, funcC]
        layouts["Test.funcD"].Should().Equal(["Test.funcD", "Test.funcA", "Test.funcB", "Test.funcC"]);
    }
}
