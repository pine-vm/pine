using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
using System.Collections.Generic;
using Xunit;

using PineCodeAnalysis = Pine.Core.CodeAnalysis.CodeAnalysis;

namespace Pine.Core.Tests.CodeAnalysis;

/// <summary>
/// Focused tests for <see cref="PineCodeAnalysis.TryComputeConsolidatedFormTemplate(PineValue, int, PineVMParseCache)"/>
/// and <see cref="PineCodeAnalysis.TryMatchExpressionTemplate(Expression, Expression, IReadOnlyList{Expression})"/>:
/// the helpers used by the static program parser to recognize the consolidated form
/// produced by the generic-application chain consolidation optimization.
/// </summary>
public class ConsolidatedFormTemplateTests
{
    private static readonly PineVMParseCache s_parseCache = new();

    private static PineValue TestFunctionValueWithParamCount(int parameterCount) =>
        FunctionValueBuilder.EmitFunctionValueWithEnvFunctions(
            innerExpression: Expression.EnvironmentInstance,
            parameterCount: parameterCount,
            envFunctions: []);

    [Fact]
    public void Returns_null_when_depth_is_below_optimization_threshold()
    {
        var functionValue = TestFunctionValueWithParamCount(3);

        PineCodeAnalysis.TryComputeConsolidatedFormTemplate(functionValue, depthK: 0, s_parseCache)
            .Should().BeNull();

        PineCodeAnalysis.TryComputeConsolidatedFormTemplate(functionValue, depthK: 1, s_parseCache)
            .Should().BeNull();
    }

    [Fact]
    public void Template_round_trips_via_match_for_partial_2_of_3()
    {
        var functionValue = TestFunctionValueWithParamCount(3);

        var template =
            PineCodeAnalysis.TryComputeConsolidatedFormTemplate(functionValue, depthK: 2, s_parseCache);

        template.Should().NotBeNull();
        template!.Placeholders.Count.Should().Be(2);

        // Build a real consolidated form by running the optimization on a fresh chain with
        // distinct concrete arguments. The encoded literal must equal the template's, and the
        // matcher must recover the concrete arguments at the placeholder positions.
        var arg0 = Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(11));
        var arg1 = Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(13));

        var chain =
            (Expression.ParseAndEval)PineCodeAnalysis.BuildGenericFunctionApplication(
                Expression.LiteralInstance(functionValue),
                new Expression[] { arg0, arg1 });

        var consolidated =
            ReducePineExpression.TryConsolidateGenericFunctionApplicationChain(chain, s_parseCache);

        consolidated.Should().NotBeNull();
        var consolidatedPaE = (Expression.ParseAndEval)consolidated!;
        var encodedLit = (Expression.Literal)consolidatedPaE.Encoded;
        encodedLit.Value.Should().Be(template.EncodedLiteral);

        var bindings =
            PineCodeAnalysis.TryMatchExpressionTemplate(
                template: template.EnvTemplate,
                actual: consolidatedPaE.Environment,
                placeholders: template.Placeholders);

        bindings.Should().NotBeNull();
        bindings!.Count.Should().Be(2);
        bindings[0].Should().Be(arg0);
        bindings[1].Should().Be(arg1);
    }

    [Fact]
    public void Template_round_trips_via_match_for_partial_3_of_4()
    {
        var functionValue = TestFunctionValueWithParamCount(4);

        var template =
            PineCodeAnalysis.TryComputeConsolidatedFormTemplate(functionValue, depthK: 3, s_parseCache);

        template.Should().NotBeNull();
        template!.Placeholders.Count.Should().Be(3);

        var arg0 = Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(11));
        var arg1 = Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(13));
        var arg2 = Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(17));

        var chain =
            (Expression.ParseAndEval)PineCodeAnalysis.BuildGenericFunctionApplication(
                Expression.LiteralInstance(functionValue),
                new Expression[] { arg0, arg1, arg2 });

        var consolidated =
            ReducePineExpression.TryConsolidateGenericFunctionApplicationChain(chain, s_parseCache);

        consolidated.Should().NotBeNull();
        var consolidatedPaE = (Expression.ParseAndEval)consolidated!;
        var encodedLit = (Expression.Literal)consolidatedPaE.Encoded;
        encodedLit.Value.Should().Be(template.EncodedLiteral);

        var bindings =
            PineCodeAnalysis.TryMatchExpressionTemplate(
                template: template.EnvTemplate,
                actual: consolidatedPaE.Environment,
                placeholders: template.Placeholders);

        bindings.Should().NotBeNull();
        bindings!.Count.Should().Be(3);
        bindings[0].Should().Be(arg0);
        bindings[1].Should().Be(arg1);
        bindings[2].Should().Be(arg2);
    }

    [Fact]
    public void Template_round_trips_for_partial_with_non_literal_args()
    {
        // Use environment-access expressions as the concrete arguments — the same kind of
        // shape produced by a partial-application helper compiled by the Elm compiler.
        var functionValue = TestFunctionValueWithParamCount(3);

        var template =
            PineCodeAnalysis.TryComputeConsolidatedFormTemplate(functionValue, depthK: 2, s_parseCache);

        template.Should().NotBeNull();

        Expression EnvAccess(int idx)
        {
            Expression cur = Expression.EnvironmentInstance;

            for (var i = 0; i < idx; ++i)
            {
                cur =
                    Expression.KernelApplicationInstance(
                        "skip",
                        Expression.ListInstance(
                            new[]
                            {
                                Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(1)),
                                cur
                            }));
            }

            return Expression.KernelApplicationInstance("head", cur);
        }

        var arg0 = EnvAccess(0);
        var arg1 = EnvAccess(1);

        var chain =
            (Expression.ParseAndEval)PineCodeAnalysis.BuildGenericFunctionApplication(
                Expression.LiteralInstance(functionValue),
                new Expression[] { arg0, arg1 });

        var consolidated =
            ReducePineExpression.TryConsolidateGenericFunctionApplicationChain(chain, s_parseCache);

        consolidated.Should().NotBeNull();
        var consolidatedPaE = (Expression.ParseAndEval)consolidated!;
        var encodedLit = (Expression.Literal)consolidatedPaE.Encoded;
        encodedLit.Value.Should().Be(template!.EncodedLiteral);

        var bindings =
            PineCodeAnalysis.TryMatchExpressionTemplate(
                template: template.EnvTemplate,
                actual: consolidatedPaE.Environment,
                placeholders: template.Placeholders);

        bindings.Should().NotBeNull();
        bindings![0].Should().Be(arg0);
        bindings[1].Should().Be(arg1);
    }

    [Fact]
    public void Template_for_saturated_application_round_trips()
    {
        // K=N (saturated): the optimization may produce a single ParseAndEval whose
        // Encoded is the function's encoded body. Verify the template captures this and
        // matches with concrete arguments.
        var functionValue = TestFunctionValueWithParamCount(3);

        var template =
            PineCodeAnalysis.TryComputeConsolidatedFormTemplate(functionValue, depthK: 3, s_parseCache);

        template.Should().NotBeNull();
        template!.Placeholders.Count.Should().Be(3);

        var arg0 = Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(11));
        var arg1 = Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(13));
        var arg2 = Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(17));

        var chain =
            (Expression.ParseAndEval)PineCodeAnalysis.BuildGenericFunctionApplication(
                Expression.LiteralInstance(functionValue),
                new Expression[] { arg0, arg1, arg2 });

        var consolidated =
            ReducePineExpression.TryConsolidateGenericFunctionApplicationChain(chain, s_parseCache);

        consolidated.Should().NotBeNull();

        // For depthK == parameterCount the consolidation might produce either a
        // single ParseAndEval (with the encoded body as Encoded) or a fully inlined body
        // (no remaining ParseAndEval). The template helper requires a ParseAndEval shape;
        // when the consolidation collapses below that shape the template is null.
        if (consolidated is Expression.ParseAndEval consolidatedPaE)
        {
            var encodedLit = (Expression.Literal)consolidatedPaE.Encoded;
            encodedLit.Value.Should().Be(template.EncodedLiteral);

            var bindings =
                PineCodeAnalysis.TryMatchExpressionTemplate(
                    template: template.EnvTemplate,
                    actual: consolidatedPaE.Environment,
                    placeholders: template.Placeholders);

            bindings.Should().NotBeNull();
            bindings![0].Should().Be(arg0);
            bindings[1].Should().Be(arg1);
            bindings[2].Should().Be(arg2);
        }
    }

    [Fact]
    public void Match_returns_null_on_structural_mismatch()
    {
        var functionValue = TestFunctionValueWithParamCount(3);

        var template =
            PineCodeAnalysis.TryComputeConsolidatedFormTemplate(functionValue, depthK: 2, s_parseCache);

        template.Should().NotBeNull();

        // Use an actual env that obviously cannot have come from this function's optimized
        // application — e.g. a single literal where a List structure is expected.
        var bogusActual = Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(0));

        var bindings =
            PineCodeAnalysis.TryMatchExpressionTemplate(
                template: template!.EnvTemplate,
                actual: bogusActual,
                placeholders: template.Placeholders);

        bindings.Should().BeNull();
    }
}
