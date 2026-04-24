using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm;
using System.Linq;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.ElmCompilerTests;

/// <summary>
/// Tests for the Elm <em>prefix record-field-accessor function</em>
/// (<c>.fieldName</c>) — the function form of record field access, as
/// distinct from the postfix dot-syntax (<c>record.fieldName</c>) which is
/// covered by <see cref="RecordAccessTests"/>. These two forms are compiled
/// down two different code paths in
/// <c>ExpressionCompiler.CompileRecordAccessFunction</c> /
/// <c>CompileRecordAccess</c>, so they need separate coverage.
///
/// <para>
/// The minimal reproduction
/// <see cref="Prefix_accessor_on_inline_record_with_closure_field_invoked_with_arg"/>
/// is the smallest known stand-alone trigger of the bytecode-emission defect
/// also surfaced by the bundled-source reproductions
/// <c>ElmParserExpressionTests.Expression_int_plus_int</c> /
/// <c>Expression_value_pipeRight_value</c> /
/// <c>Expression_value_eq_value</c> — see
/// <c>expression-infix-emission-defect-analysis.md</c>.
/// </para>
/// </summary>
public class RecordAccessFunctionTests
{
    private static readonly Core.Interpreter.IntermediateVM.PineVM s_vm =
        ElmCompilerTestHelper.PineVMForProfiling(_ => { });

    private static ElmInteractiveEnvironment.ParsedInteractiveEnvironment CompileModule(
        string elmModuleText)
    {
        return
            ElmCompilerTestHelper.CompileElmModules(
                [elmModuleText],
                disableInlining: true).parsedEnv;
    }

    private static PineValue GetFunction(
        ElmInteractiveEnvironment.ParsedInteractiveEnvironment env,
        string moduleName,
        string funcName)
    {
        return
            env.Modules
            .First(m => m.moduleName == moduleName)
            .moduleContent.FunctionDeclarations[funcName];
    }

    private static ElmValue Invoke1(PineValue func, ElmValue arg) =>
        CoreLibraryModule.CoreLibraryTestHelper.ApplyUnary(func, arg, s_vm);

    /// <summary>
    /// The minimal known reproduction of an open compile-to-PineVM defect:
    /// the prefix record-field-accessor function (<c>.f</c>) applied to an
    /// inline record literal whose <c>f</c> field is a non-capturing
    /// closure, then applied to a single integer argument.
    ///
    /// <para>
    /// Expected behaviour: <c>use 5</c> evaluates to <c>5</c>, since the
    /// closure stored in the record is the identity function on integers.
    /// </para>
    ///
    /// <para>
    /// Observed behaviour at the time of writing: the runtime fails with
    /// <c>Failed eval: Failed to parse expression from value: Unexpected
    /// number of items in list: Not 2 but 0 - expressionValue is string ''
    /// - environmentValue is not a string</c>, with stack frames = 0,
    /// instructions = 3, invocations = 1 — i.e. the very first
    /// <c>ParseAndEval</c> the VM attempts already has the empty-string
    /// <see cref="PineValue.BlobValue"/> as its expression operand. This
    /// is the same diagnostic the bundled-source reproductions
    /// <c>ElmParserExpressionTests.Expression_int_plus_int</c> /
    /// <c>Expression_value_pipeRight_value</c> /
    /// <c>Expression_value_eq_value</c> surface, only ~2500× smaller in
    /// execution and source size.
    /// </para>
    /// </summary>
    [Fact]
    public void Prefix_accessor_on_inline_record_with_closure_field_invoked_with_arg()
    {
        var env = CompileModule(
            """"
            module Test exposing (..)


            use : Int -> Int
            use x =
                .f { f = \y -> y } x

            """");

        var func = GetFunction(env, "Test", "use");

        Invoke1(func, ElmValue.Integer(5)).Should().Be(ElmValue.Integer(5));
    }

    /// <summary>
    /// Companion to
    /// <see cref="Prefix_accessor_on_inline_record_with_closure_field_invoked_with_arg"/>
    /// using the same prefix accessor shape but with an <c>Int</c>-typed
    /// field instead of a closure-typed field. This case does <b>not</b>
    /// raise the <c>expressionValue is string ''</c> runtime error; instead
    /// the prefix accessor returns the empty list <c>[]</c> where the
    /// field's actual integer value (<c>42</c>) is expected. That is a
    /// related but distinct emission bug for the prefix
    /// record-field-accessor function applied to a non-function field.
    /// </summary>
    [Fact]
    public void Prefix_accessor_on_record_with_int_field_returns_field_value()
    {
        var env = CompileModule(
            """"
            module Test exposing (..)


            type alias R =
                { f : Int }


            r : R
            r =
                { f = 42 }


            use : Int -> Int
            use offset =
                Pine_kernel.int_add [ .f r, offset ]

            """");

        var func = GetFunction(env, "Test", "use");

        Invoke1(func, ElmValue.Integer(3)).Should().Be(ElmValue.Integer(45));
    }
}
