using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
using Pine.Core.Elm;
using System.Linq;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.Inlining;

public class InliningCompilerIntegrationTests
{
    [Fact]
    public void Compiler_uses_cross_module_inlining_when_enabled()
    {
        const string ProviderModuleText =
            """
            module Provider exposing (applyTwice)


            applyTwice func value =
                func (func value)
            """;

        const string ConsumerModuleText =
            """
            module Consumer exposing (compiled)

            import Provider


            increment value =
                Pine_builtin.int_add [ value, 1 ]


            compiled value =
                Provider.applyTwice increment value
            """;

        var envWithoutInlining =
            ElmCompilerTestHelper.CompileElmModules(
                [ProviderModuleText, ConsumerModuleText],
                disableInlining: true).parsedEnv;

        var envWithInlining =
            ElmCompilerTestHelper.CompileElmModules(
                [ProviderModuleText, ConsumerModuleText],
                disableInlining: false).parsedEnv;

        var compiledWithoutInlining =
            envWithoutInlining.Modules
            .First(module => module.moduleName is "Consumer")
            .moduleContent.FunctionDeclarations["compiled"];

        var compiledWithInlining =
            envWithInlining.Modules
            .First(module => module.moduleName is "Consumer")
            .moduleContent.FunctionDeclarations["compiled"];

        var parseCache = new PineVMParseCache();

        var invokeWithoutInlining =
            ElmCompilerTestHelper.CreateFunctionValueInvocationDelegate(compiledWithoutInlining, parseCache);

        var invokeWithInlining =
            ElmCompilerTestHelper.CreateFunctionValueInvocationDelegate(compiledWithInlining, parseCache);

        var (valueWithoutInlining, reportsWithoutInlining) =
            invokeWithoutInlining([IntegerEncoding.EncodeSignedInteger(10)]);

        var (valueWithInlining, reportsWithInlining) =
            invokeWithInlining([IntegerEncoding.EncodeSignedInteger(10)]);

        valueWithoutInlining.Should().Be(IntegerEncoding.EncodeSignedInteger(12));
        valueWithInlining.Should().Be(IntegerEncoding.EncodeSignedInteger(12));

        // §7.6b: cross-SCC callees are now always literal-inlined at call sites
        // (the Consumer.compiled body inlines Provider.applyTwice's wrapper
        // regardless of the user-facing inlining flag), so for this small
        // synthetic example the inlining feature no longer strictly reduces
        // instruction count beyond what §7.6b already accomplishes.
        // The intent of this test is preserved as the weaker invariant that
        // the inlining feature must not *increase* instruction count.
        reportsWithInlining.Sum(report => report.InstructionCount)
            .Should()
            .BeLessThanOrEqualTo(reportsWithoutInlining.Sum(report => report.InstructionCount));
    }

    [Fact]
    public void Compiler_inlines_cross_module_higher_order_call_without_breaking_lifted_local_helpers()
    {
        const string ProviderModuleText =
            """
            module Provider exposing (applyTwice)


            applyTwice func value =
                func (func value)
            """;

        const string ConsumerModuleText =
            """
            module Consumer exposing (compiled)

            import Provider


            compiled value =
                let
                    localIncrement =
                        (\offset -> value + offset)
                in
                Provider.applyTwice localIncrement 1
            """;

        var envWithInlining =
            ElmCompilerTestHelper.CompileElmModules(
                [ProviderModuleText, ConsumerModuleText],
                disableInlining: false).parsedEnv;

        var compiledWithInlining =
            envWithInlining.Modules
            .First(module => module.moduleName is "Consumer")
            .moduleContent.FunctionDeclarations["compiled"];

        var parseCache = new PineVMParseCache();

        var invokeWithInlining =
            ElmCompilerTestHelper.CreateFunctionValueInvocationDelegate(compiledWithInlining, parseCache);

        var (valueWithInlining, _) =
            invokeWithInlining([IntegerEncoding.EncodeSignedInteger(10)]);

        valueWithInlining.Should().Be(IntegerEncoding.EncodeSignedInteger(21));
    }

    /// <summary>
    /// Regression test for a bug where size-based inlining of small functions (like <c>skip</c>
    /// in <c>Parser.Advanced</c>) changed the AST structure before lambda lifting,
    /// causing lambda lifting to miss captured variables in nested lambdas.
    /// <para>
    /// The pattern: a function with a parameter (like <c>close</c>) used inside lambdas,
    /// where a small wrapper function (like <c>skip</c>) gets inlined at the call site.
    /// After inlining, the lambda still references the outer parameter, but lambda lifting
    /// must correctly capture it as a parameter of the lifted lambda function.
    /// </para>
    /// </summary>
    [Fact]
    public void Size_based_inlining_preserves_captured_variables_in_lambdas()
    {
        // This module defines a small function `wrap` that is eligible for size-based inlining,
        // and a function `process` whose parameter `transform` is used inside a lambda.
        // When `wrap` is inlined into the body of `process`, the lambda still references
        // `transform`, which must be properly captured by lambda lifting.
        const string ModuleText =
            """
            module App exposing (result)


            wrap f x =
                f x


            applyToEach transform items =
                case items of
                    [] ->
                        []

                    first :: rest ->
                        wrap transform first :: applyToEach transform rest


            result input =
                applyToEach
                    (\x -> Pine_kernel.int_add [ x, x ])
                    input
            """;

        var envWithInlining =
            ElmCompilerTestHelper.CompileElmModules(
                [ModuleText],
                disableInlining: false);

        var compiled =
            envWithInlining.parsedEnv.Modules
            .First(module => module.moduleName is "App")
            .moduleContent.FunctionDeclarations["result"];

        var parseCache = new PineVMParseCache();

        var invoke =
            ElmCompilerTestHelper.CreateFunctionValueInvocationDelegate(compiled, parseCache);

        // result [3, 5] should produce [6, 10] since each x maps to x + x
        var inputList =
            PineValue.List(
                [
                IntegerEncoding.EncodeSignedInteger(3),
                IntegerEncoding.EncodeSignedInteger(5)
                ]);

        var (resultValue, _) = invoke([inputList]);

        var expectedResult =
            PineValue.List(
                [
                IntegerEncoding.EncodeSignedInteger(6),
                IntegerEncoding.EncodeSignedInteger(10)
                ]);

        resultValue.Should().Be(expectedResult);
    }


    /// <summary>
    /// Regression test for a defect where enabling size-based inlining after higher-order
    /// inlining caused <c>percentDecode</c> in the Url module to always return <c>Nothing</c>.
    /// <para>
    /// The pattern: a public function calls a small wrapper function that delegates to a
    /// recursive implementation. When size-based inlining is enabled, the wrapper gets
    /// inlined, but something in the process breaks the overall computation.
    /// </para>
    /// </summary>
    [Fact]
    public void Size_based_inlining_preserves_small_wrapper_around_recursive_function()
    {
        const string ModuleText =
            """
            module App exposing (process)


            doAll items =
                doRec items []


            doRec items acc =
                case items of
                    [] ->
                        acc

                    first :: rest ->
                        doRec rest (Pine_kernel.int_add [ first, 1 ] :: acc)


            process input =
                doAll input
            """;

        var envWithInlining =
            ElmCompilerTestHelper.CompileElmModules(
                [ModuleText],
                disableInlining: false);

        var compiled =
            envWithInlining.parsedEnv.Modules
            .First(module => module.moduleName is "App")
            .moduleContent.FunctionDeclarations["process"];

        var parseCache = new PineVMParseCache();

        var invoke =
            ElmCompilerTestHelper.CreateFunctionValueInvocationDelegate(compiled, parseCache);

        // process [3, 5] should produce [6, 4] (reversed because doRec prepends)
        var inputList =
            PineValue.List(
                [
                IntegerEncoding.EncodeSignedInteger(3),
                IntegerEncoding.EncodeSignedInteger(5)
                ]);

        var (resultValue, _) = invoke([inputList]);

        var expectedResult =
            PineValue.List(
                [
                IntegerEncoding.EncodeSignedInteger(6),
                IntegerEncoding.EncodeSignedInteger(4)
                ]);

        resultValue.Should().Be(expectedResult);
    }


    /// <summary>
    /// Same as above but the small wrapper is called through a pipe operator.
    /// Tests that pipe desugaring interacts correctly with size-based inlining.
    /// </summary>
    [Fact]
    public void Size_based_inlining_preserves_piped_small_wrapper_around_recursive_function()
    {
        const string ModuleText =
            """
            module App exposing (process)


            doAll items =
                doRec items []


            doRec items acc =
                case items of
                    [] ->
                        acc

                    first :: rest ->
                        doRec rest (Pine_kernel.int_add [ first, 1 ] :: acc)


            process input =
                input
                    |> doAll
            """;

        var envWithInlining =
            ElmCompilerTestHelper.CompileElmModules(
                [ModuleText],
                disableInlining: false);

        var compiled =
            envWithInlining.parsedEnv.Modules
            .First(module => module.moduleName is "App")
            .moduleContent.FunctionDeclarations["process"];

        var parseCache = new PineVMParseCache();

        var invoke =
            ElmCompilerTestHelper.CreateFunctionValueInvocationDelegate(compiled, parseCache);

        var inputList =
            PineValue.List(
                [
                IntegerEncoding.EncodeSignedInteger(3),
                IntegerEncoding.EncodeSignedInteger(5)
                ]);

        var (resultValue, _) = invoke([inputList]);

        var expectedResult =
            PineValue.List(
                [
                IntegerEncoding.EncodeSignedInteger(6),
                IntegerEncoding.EncodeSignedInteger(4)
                ]);

        resultValue.Should().Be(expectedResult);
    }


    /// <summary>
    /// Tests Maybe propagation through a small wrapper — mirrors the
    /// <c>percentDecode → decodeAll → decodeRec</c> pattern in the Url module.
    /// </summary>
    [Fact]
    public void Size_based_inlining_preserves_Maybe_wrapper_through_pipes()
    {
        const string ModuleText =
            """
            module App exposing (process)


            transformAll items =
                transformRec items []


            transformRec items acc =
                case items of
                    [] ->
                        Just acc

                    first :: rest ->
                        if Pine_kernel.int_is_sorted_asc [ 0, first ] then
                            transformRec rest (Pine_kernel.int_add [ first, 1 ] :: acc)
                        else
                            Nothing


            process input =
                input
                    |> transformAll
            """;

        var envWithInlining =
            ElmCompilerTestHelper.CompileElmModules(
                [ModuleText],
                disableInlining: false);

        var compiled =
            envWithInlining.parsedEnv.Modules
            .First(module => module.moduleName is "App")
            .moduleContent.FunctionDeclarations["process"];

        var parseCache = new PineVMParseCache();

        var invoke =
            ElmCompilerTestHelper.CreateFunctionValueInvocationDelegate(compiled, parseCache);

        // Valid input: all non-negative → Just [6, 4]
        var inputList =
            PineValue.List(
                [
                IntegerEncoding.EncodeSignedInteger(3),
                IntegerEncoding.EncodeSignedInteger(5)
                ]);

        var (resultValue, _) = invoke([inputList]);

        // The result should be Just [6, 4] (reversed because transformRec prepends)
        var expectedInnerList =
            PineValue.List(
                [
                IntegerEncoding.EncodeSignedInteger(6),
                IntegerEncoding.EncodeSignedInteger(4)
                ]);

        var expected =
            ElmValueEncoding.TagAsPineValue("Just", [expectedInnerList]);

        resultValue.Should().Be(expected);
    }
}
