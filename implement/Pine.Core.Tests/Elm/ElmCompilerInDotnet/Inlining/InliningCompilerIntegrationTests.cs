using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
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

        reportsWithInlining.Sum(report => report.InstructionCount)
            .Should()
            .BeLessThan(reportsWithoutInlining.Sum(report => report.InstructionCount));
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
}
