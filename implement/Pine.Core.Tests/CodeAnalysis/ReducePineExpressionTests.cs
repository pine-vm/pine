using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.PopularEncodings;
using System.Linq;
using Xunit;

namespace Pine.Core.Tests.CodeAnalysis;

public class ReducePineExpressionTests
{
    private static readonly PineVMParseCache s_parseCache = new();

    [Fact]
    public void TryEvaluateExpressionIndependent_EvaluatesLiteral_ListAndLengthKernel()
    {
        // literal
        var lit = Expression.LiteralInstance(PineValue.Blob([1, 2]));

        ReducePineExpression.TryEvaluateExpressionIndependent(lit, s_parseCache)
            .IsOkOrNull().Should().Be(PineValue.Blob([1, 2]));

        // list of literals
        var list =
            Expression.ListInstance(
                [
                Expression.LiteralInstance(PineValue.Blob([3])),
                Expression.LiteralInstance(PineValue.EmptyList)
                ]);

        var evalList = ReducePineExpression.TryEvaluateExpressionIndependent(list, s_parseCache).IsOkOrNull();

        (evalList is PineValue.ListValue).Should().BeTrue();

        var evalListItems = ((PineValue.ListValue)evalList!).Items;

        evalListItems.Length.Should().Be(2);

        // kernel length([x,y]) -> 2
        var lengthOfList =
            Expression.KernelApplicationInstance(
                nameof(KernelFunction.length),
                list);

        var lengthValue = ReducePineExpression.TryEvaluateExpressionIndependent(lengthOfList, s_parseCache).IsOkOrNull();
        lengthValue.Should().Be(IntegerEncoding.EncodeSignedInteger(2));
    }

    [Fact]
    public void ReduceExpressionBottomUp_Folds_Length_On_ListLiteral_ToInteger()
    {
        var expr =
            Expression.KernelApplicationInstance(
                nameof(KernelFunction.length),
                Expression.ListInstance(
                    [
                    Expression.LiteralInstance(PineValue.Blob([1])),
                    Expression.LiteralInstance(PineValue.Blob([2]))
                    ]));

        var reduced = ReducePineExpression.ReduceExpressionBottomUp(expr, s_parseCache);

        (reduced is Expression.Literal).Should().BeTrue();

        var lit = (Expression.Literal)reduced;
        lit.Value.Should().Be(IntegerEncoding.EncodeSignedInteger(2));
    }

    [Fact]
    public void ReduceExpressionBottomUp_ConditionalWithTrueLiteral_SelectsTrueBranch()
    {
        var trueBranch = Expression.LiteralInstance(PineValue.Blob([9]));
        var falseBranch = Expression.LiteralInstance(PineValue.Blob([7]));

        var conditional = Expression.ConditionalInstance(
            condition: Expression.LiteralInstance(Core.PineVM.PineKernelValues.TrueValue),
            falseBranch: falseBranch,
            trueBranch: trueBranch);

        var reduced = ReducePineExpression.ReduceExpressionBottomUp(conditional, s_parseCache);

        reduced.Should().Be(trueBranch);
    }

    [Fact]
    public void TryEvaluateExpressionIndependent_ParseAndEval_WithLiteralArgs_EvaluatesInnerExpression()
    {
        // Build encoded expression value representing a simple literal
        var innerLiteral = Expression.LiteralInstance(PineValue.Blob([42]));
        var encodedValue = ExpressionEncoding.EncodeExpressionAsValue(innerLiteral);

        var parseAndEval = new Expression.ParseAndEval(
            encoded: Expression.LiteralInstance(encodedValue),
            environment: Expression.LiteralInstance(PineValue.EmptyList));

        var result = ReducePineExpression.TryEvaluateExpressionIndependent(parseAndEval, s_parseCache);

        result.IsOkOrNull().Should().Be(PineValue.Blob([42]));
    }

    [Fact]
    public void Transform_ReplacingEnvironmentWithLiteral_RemovesEnvAndReportsNoReference()
    {
        var expr = Expression.ListInstance(
            [
            Expression.EnvironmentInstance,

            Expression.KernelApplicationInstance(
                nameof(KernelFunction.head),
                Expression.LiteralInstance(PineValue.List([
                    PineValue.Blob([1])
                ])))
            ]);

        var (mapped, referencesOriginalEnv) =
            ReducePineExpression.TransformPineExpressionWithOptionalReplacement(
                e => e is Expression.Environment ? Expression.LiteralInstance(PineValue.EmptyList) : null,
                expr);

        referencesOriginalEnv.Should().BeFalse();

        // Ensure no Environment nodes remain
        foreach (var node in Expression.EnumerateSelfAndDescendants(mapped))
        {
            (node is Expression.Environment).Should().BeFalse();
        }
    }

    [Fact]
    public void TryInferListLengthLowerBounds_SkipConst_OnLiteralList_ReturnsLowerBound()
    {
        var list =
            Expression.LiteralInstance(
                PineValue.List(
                    [
                    PineValue.Blob([1]),
                    PineValue.Blob([2]),
                    PineValue.Blob([3]),
                    PineValue.Blob([4]),
                    PineValue.Blob([5])
                    ]));

        var skip =
            Expression.KernelApplicationInstance(
                nameof(KernelFunction.skip),
                Expression.ListInstance(
                    [
                    Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(2)),
                    list
                    ]));

        var dummyClass = PineValueClass.CreateEquals(PineValue.EmptyList);

        var bounds = ReducePineExpression.EnumerateInferListLengthBounds(skip, dummyClass, s_parseCache).ToArray();

        bounds.Select(b => b.lower).Should().Contain(3);
    }

    [Fact]
    public void Reduce_HeadOverConditional_PushesIntoBranches_SelectsFirstElement()
    {
        // condition references the environment to prevent collapsing the conditional
        var condition = Expression.EnvironmentInstance;

        var falseFirst = Expression.LiteralInstance(PineValue.Blob([10]));
        var falseSecond = Expression.LiteralInstance(PineValue.Blob([11]));
        var trueFirst = Expression.LiteralInstance(PineValue.Blob([20]));
        var trueSecond = Expression.LiteralInstance(PineValue.Blob([21]));

        var falseList = Expression.ListInstance([falseFirst, falseSecond]);
        var trueList = Expression.ListInstance([trueFirst, trueSecond]);

        var conditional =
            Expression.ConditionalInstance(
                condition: condition,
                falseBranch: falseList,
                trueBranch: trueList);

        var root =
            Expression.KernelApplicationInstance(
                nameof(KernelFunction.head),
                conditional);

        var reduced = ReducePineExpression.ReduceExpressionBottomUp(root, s_parseCache);

        // Expect: head disappears, conditional remains, branches reduced to the head element of each list
        (reduced is Expression.Conditional).Should().BeTrue();

        var reducedConditional = (Expression.Conditional)reduced;
        reducedConditional.TrueBranch.Should().Be(trueFirst);
        reducedConditional.FalseBranch.Should().Be(falseFirst);
    }

    [Fact]
    public void Reduce_HeadOverSkip_WithConstCount_SelectsElementAtIndex()
    {
        var i0 = Expression.LiteralInstance(PineValue.Blob([1]));
        var i1 = Expression.LiteralInstance(PineValue.Blob([2]));
        var i2 = Expression.LiteralInstance(PineValue.Blob([3]));
        var i3 = Expression.LiteralInstance(PineValue.Blob([4]));

        var listExpr = Expression.ListInstance([i0, i1, i2, i3]);

        var skipArgs =
            Expression.ListInstance(
                [
                Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(2)),
                listExpr
                ]);

        var skipApp = Expression.KernelApplicationInstance(nameof(KernelFunction.skip), skipArgs);
        var root = Expression.KernelApplicationInstance(nameof(KernelFunction.head), skipApp);

        var reduced = ReducePineExpression.ReduceExpressionBottomUp(root, s_parseCache);

        // Expect: head(skip(2, [i0,i1,i2,i3])) -> i2
        reduced.Should().Be(i2);
    }

    [Fact]
    public void Reduce_HeadOverSkipConst_ConditionalList_PushesIntoBranches_SelectsIndex()
    {
        // condition references the environment to prevent evaluating away
        var condition = Expression.EnvironmentInstance;

        var f0 = Expression.LiteralInstance(PineValue.Blob([1]));
        var f1 = Expression.LiteralInstance(PineValue.Blob([2]));
        var f2 = Expression.LiteralInstance(PineValue.Blob([3]));

        var t0 = Expression.LiteralInstance(PineValue.Blob([4]));
        var t1 = Expression.LiteralInstance(PineValue.Blob([5]));
        var t2 = Expression.LiteralInstance(PineValue.Blob([6]));

        var falseList = Expression.ListInstance([f0, f1, f2]);
        var trueList = Expression.ListInstance([t0, t1, t2]);

        var listConditional =
            Expression.ConditionalInstance(
                condition: condition,
                falseBranch: falseList,
                trueBranch: trueList);

        var skipArgs =
            Expression.ListInstance(
                [
                Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(2)),
                listConditional
                ]);

        var root =
            Expression.KernelApplicationInstance(
                nameof(KernelFunction.head),
                Expression.KernelApplicationInstance(nameof(KernelFunction.skip), skipArgs));

        var reduced = ReducePineExpression.ReduceExpressionBottomUp(root, s_parseCache);

        // Expect: head(skip(2, conditional([f0,f1,f2],[t0,t1,t2]))) -> conditional(f2, t2)
        (reduced is Expression.Conditional).Should().BeTrue();

        var reducedConditional = (Expression.Conditional)reduced;
        reducedConditional.TrueBranch.Should().Be(t2);
        reducedConditional.FalseBranch.Should().Be(f2);
    }

    [Fact]
    public void Reduce_HeadOverSkip_NegativeCount_ClampedToZero_PushesThroughNestedConditionalsAndTags()
    {
        // Two conditions referencing environment so the conditional stays
        var condA =
            Expression.KernelApplicationInstance(
                nameof(KernelFunction.head),
                Expression.KernelApplicationInstance(
                    nameof(KernelFunction.skip),
                    Expression.ListInstance([
                        Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(11)),
                        Expression.EnvironmentInstance
                ])));

        var condB =
            Expression.KernelApplicationInstance(
                nameof(KernelFunction.head),
                Expression.KernelApplicationInstance(
                    nameof(KernelFunction.skip),
                    Expression.ListInstance(
                        [
                        Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(13)),
                        Expression.EnvironmentInstance
                        ])));

        // Leaves: one branch with string tag around list, another with literal(list), third with direct list
        var sf0 = Expression.LiteralInstance(PineValue.Blob([101]));
        var sf1 = Expression.LiteralInstance(PineValue.Blob([102]));
        var taggedFalse = new Expression.StringTag("tag-false", Expression.ListInstance([sf0, sf1]));

        var lt0 = PineValue.Blob([201]);
        var lt1 = PineValue.Blob([202]);
        var literalTrue = Expression.LiteralInstance(PineValue.List([lt0, lt1]));

        var dl0 = Expression.LiteralInstance(PineValue.Blob([11]));
        var dl1 = Expression.LiteralInstance(PineValue.Blob([12]));
        var directList = Expression.ListInstance([dl0, dl1]);

        // Nested conditional as false branch
        var nestedFalse = Expression.ConditionalInstance(
            condition: condB,
            falseBranch: taggedFalse,
            trueBranch: literalTrue);

        // Outer conditional
        var outer = Expression.ConditionalInstance(
            condition: condA,
            falseBranch: nestedFalse,
            trueBranch: directList);

        // Use a negative skip count which should clamp to zero
        var skipArgs = Expression.ListInstance([
            Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(-3)),
            outer
        ]);

        var root =
            Expression.KernelApplicationInstance(
                nameof(KernelFunction.head),
                Expression.KernelApplicationInstance(nameof(KernelFunction.skip), skipArgs));

        var reduced = ReducePineExpression.ReduceExpressionBottomUp(root, s_parseCache);

        // Expect: selection index becomes 0; result is conditional with true branch dl0 and false branch another conditional
        (reduced is Expression.Conditional).Should().BeTrue();
        var reducedOuter = (Expression.Conditional)reduced;

        reducedOuter.TrueBranch.Should().Be(dl0);

        (reducedOuter.FalseBranch is Expression.Conditional).Should().BeTrue();
        var reducedInner = (Expression.Conditional)reducedOuter.FalseBranch;

        // From string-tagged list -> first element sf0
        reducedInner.FalseBranch.Should().Be(sf0);
        // From literal(list) -> first element lt0
        reducedInner.TrueBranch.Should().Be(Expression.LiteralInstance(lt0));
    }

    [Fact]
    public void Reduce_NestedHeadSkip_MultiLevel_FoldsToExpectedElement()
    {
        // Build nested lists: [[a0,a1,a2],[b0,b1,b2],[c0,c1,c2]]
        var a0 = Expression.LiteralInstance(PineValue.Blob([1]));
        var a1 = Expression.LiteralInstance(PineValue.Blob([2]));
        var a2 = Expression.LiteralInstance(PineValue.Blob([3]));
        var b0 = Expression.LiteralInstance(PineValue.Blob([4]));
        var b1 = Expression.LiteralInstance(PineValue.Blob([5]));
        var b2 = Expression.LiteralInstance(PineValue.Blob([6]));
        var c0 = Expression.LiteralInstance(PineValue.Blob([7]));
        var c1 = Expression.LiteralInstance(PineValue.Blob([8]));
        var c2 = Expression.LiteralInstance(PineValue.Blob([9]));

        var subA = Expression.ListInstance([a0, a1, a2]);
        var subB = Expression.ListInstance([b0, b1, b2]);
        var subC = Expression.ListInstance([c0, c1, c2]);

        var top = Expression.ListInstance([subA, subB, subC]);

        // Expression: head(skip(2, head(skip(1, top))))
        var innerSkipArgs = Expression.ListInstance([
            Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(1)),
            top
        ]);

        var inner =
            Expression.KernelApplicationInstance(
                nameof(KernelFunction.head),
                Expression.KernelApplicationInstance(nameof(KernelFunction.skip), innerSkipArgs));

        var outerSkipArgs =
            Expression.ListInstance(
                [
                Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(2)),
                inner
                ]);

        var root =
            Expression.KernelApplicationInstance(
                nameof(KernelFunction.head),
                Expression.KernelApplicationInstance(nameof(KernelFunction.skip), outerSkipArgs));

        var reduced = ReducePineExpression.ReduceExpressionBottomUp(root, s_parseCache);

        // Expect: select sublistB via inner, then select index 2 -> b2
        reduced.Should().Be(b2);
    }

    [Fact]
    public void Reduce_HeadOverReverse_SelectsLastElement()
    {
        var i0 = Expression.LiteralInstance(PineValue.Blob([1]));
        var i1 = Expression.LiteralInstance(PineValue.Blob([2]));
        var i2 = Expression.LiteralInstance(PineValue.Blob([3]));
        var listExpr = Expression.ListInstance([i0, i1, i2]);

        var root =
            Expression.KernelApplicationInstance(
                nameof(KernelFunction.head),
                Expression.KernelApplicationInstance(nameof(KernelFunction.reverse), listExpr));

        var reduced = ReducePineExpression.ReduceExpressionBottomUp(root, s_parseCache);
        reduced.Should().Be(i2);
    }

    [Fact]
    public void Reduce_HeadOverReverseTake_SelectsLastOfTake()
    {
        // head(reverse(take(2, [a,b,c]))) -> b
        var a = Expression.LiteralInstance(PineValue.Blob([1]));
        var b = Expression.LiteralInstance(PineValue.Blob([2]));
        var c = Expression.LiteralInstance(PineValue.Blob([3]));
        var listExpr = Expression.ListInstance([a, b, c]);

        var takeArgs =
            Expression.ListInstance(
                [
                Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(2)),
                listExpr
                ]);

        var root =
            Expression.KernelApplicationInstance(
                nameof(KernelFunction.head),
                Expression.KernelApplicationInstance(
                    nameof(KernelFunction.reverse),
                    Expression.KernelApplicationInstance(nameof(KernelFunction.take), takeArgs)));

        var reduced = ReducePineExpression.ReduceExpressionBottomUp(root, s_parseCache);
        reduced.Should().Be(b);
    }

    [Fact]
    public void Reduce_TakeLast_AsReverseTakeReverse_SelectsExpected()
    {
        // take-last 2 from [a,b,c,d] = reverse(take(2, reverse([a,b,c,d]))) -> [c,d]; head(...) -> c when further taking head
        var a = Expression.LiteralInstance(PineValue.Blob([1]));
        var b = Expression.LiteralInstance(PineValue.Blob([2]));
        var c = Expression.LiteralInstance(PineValue.Blob([3]));
        var d = Expression.LiteralInstance(PineValue.Blob([4]));

        var listExpr = Expression.ListInstance([a, b, c, d]);

        var reversed =
            Expression.KernelApplicationInstance(nameof(KernelFunction.reverse), listExpr);

        var takeArgs =
            Expression.ListInstance(
                [
                Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(2)),
                reversed
                ]);

        var takeOnReversed =
            Expression.KernelApplicationInstance(nameof(KernelFunction.take), takeArgs);

        var takeLast =
            Expression.KernelApplicationInstance(nameof(KernelFunction.reverse), takeOnReversed);

        // head(take-last 2) -> c
        var root =
            Expression.KernelApplicationInstance(nameof(KernelFunction.head), takeLast);

        var reduced =
            ReducePineExpression.ReduceExpressionBottomUp(root, s_parseCache);

        reduced.Should().Be(c);
    }

    [Fact]
    public void Reduce_TakeLastOverConditional_PushesIntoBranches_SelectsTail()
    {
        // Build conditional list with env-referencing condition to avoid collapsing
        var condition = Expression.EnvironmentInstance;

        var falseList =
            Expression.LiteralInstance(PineValue.List(
                [
                PineValue.Blob([10]),
                PineValue.Blob([11]),
                PineValue.Blob([12]),
                PineValue.Blob([13])
                ]));

        var trueList =
            Expression.LiteralInstance(PineValue.List([
                PineValue.Blob([20]),
                PineValue.Blob([21]),
                PineValue.Blob([22]),
                PineValue.Blob([23])
        ]));

        var listConditional =
            Expression.ConditionalInstance(condition, falseList, trueList);

        // take-last 2 -> reverse(take(2, reverse(x)))
        var rev1 =
            Expression.KernelApplicationInstance(nameof(KernelFunction.reverse), listConditional);

        var takeArgs =
            Expression.ListInstance(
                [
                Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(2)),
                rev1
                ]);

        var take =
            Expression.KernelApplicationInstance(nameof(KernelFunction.take), takeArgs);

        var takeLast =
            Expression.KernelApplicationInstance(nameof(KernelFunction.reverse), take);

        var reduced = ReducePineExpression.ReduceExpressionBottomUp(takeLast, s_parseCache);

        (reduced is Expression.Conditional).Should().BeTrue();
        var reducedCond = (Expression.Conditional)reduced;

        reducedCond.FalseBranch.Should().Be(
            Expression.LiteralInstance(
                PineValue.List(
                    [
                    PineValue.Blob([12]),
                    PineValue.Blob([13])
                    ])));

        reducedCond.TrueBranch.Should().Be(
            Expression.LiteralInstance(
                PineValue.List(
                    [
                    PineValue.Blob([22]),
                    PineValue.Blob([23])
                    ])));
    }

    [Fact]
    public void Reduce_SkipLastOverConditional_PushesIntoBranches_DropsTail()
    {
        // Build conditional list with env-referencing condition to avoid collapsing
        var condition = Expression.EnvironmentInstance;

        var falseList =
            Expression.LiteralInstance(PineValue.List(
                [
                PineValue.Blob([10]),
                PineValue.Blob([11]),
                PineValue.Blob([12])
                ]));

        var trueList =
            Expression.LiteralInstance(PineValue.List(
                [
                PineValue.Blob([20]),
                PineValue.Blob([21]),
                PineValue.Blob([22])
                ]));

        var listConditional = Expression.ConditionalInstance(condition, falseList, trueList);

        // skip-last 1 -> reverse(skip(1, reverse(x)))
        var rev1 = Expression.KernelApplicationInstance(nameof(KernelFunction.reverse), listConditional);
        var skipArgs = Expression.ListInstance([
            Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(1)),
            rev1
        ]);

        var skip = Expression.KernelApplicationInstance(nameof(KernelFunction.skip), skipArgs);
        var skipLast = Expression.KernelApplicationInstance(nameof(KernelFunction.reverse), skip);

        var reduced = ReducePineExpression.ReduceExpressionBottomUp(skipLast, s_parseCache);

        (reduced is Expression.Conditional).Should().BeTrue();

        var reducedCond = (Expression.Conditional)reduced;

        reducedCond.FalseBranch.Should().Be(
            Expression.LiteralInstance(
                PineValue.List(
                    [
                    PineValue.Blob([10]),
                    PineValue.Blob([11])
                    ])));

        reducedCond.TrueBranch.Should().Be(
            Expression.LiteralInstance(
                PineValue.List(
                    [
                    PineValue.Blob([20]),
                    PineValue.Blob([21])
                    ])));
    }
}
