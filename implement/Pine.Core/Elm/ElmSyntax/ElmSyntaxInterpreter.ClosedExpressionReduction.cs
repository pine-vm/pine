using Pine.Core.CodeAnalysis;
using Pine.Core.Internal;
using System.Collections.Generic;

using AbstractExpression = Pine.Core.Elm.ElmSyntax.ElmSyntaxAbstract.Expression;

namespace Pine.Core.Elm.ElmSyntax;

public partial class ElmSyntaxInterpreter
{
    private sealed class ClosedExpressionReducer(
        DeclQualifiedName currentTopLevel,
        System.Func<Application, ApplicationResolution> resolveApplication,
        IReadOnlyDictionary<string, DeclQualifiedName> infixOperators)
    {
        private const int MaxInstructionCount = 100_000;

        private const int MaxLiteralNodeCount = 100_000;

        private const int MaxLiteralBlobByteCount = 16 * 1024 * 1024;

        public PreparedExpression Reduce(
            AbstractExpression abstractExpression,
            PreparedExpression preparedExpression)
        {
            if (preparedExpression is PreparedExpression.ValueLiteral ||
                ElmSyntaxAbstract.SyntaxAnalysis.CollectRemainingFreeVariables(abstractExpression).Count is not 0)
            {
                return preparedExpression;
            }

            try
            {
                var result =
                    RunTrampoline(
                        initialExpression: preparedExpression,
                        initialEnv:
                        new ApplicationContext(
                            CurrentTopLevel: currentTopLevel,
                            localBindings: LocalBindingEnvironment.Empty),
                        initialApplication: null,
                        resolveApplication: resolveApplication,
                        infixOperators: infixOperators,
                        invocationLogger: new InvocationCounter(),
                        evaluationConfig:
                        new EvaluationConfig(
                            InstructionCountLimit: MaxInstructionCount,
                            ContinuationDepthLimit: EvaluationConfig.Default.ContinuationDepthLimit));

                if (result.IsOkOrNull() is { } value &&
                    TryMaterializeLiteral(
                        value,
                        MaxLiteralNodeCount,
                        MaxLiteralBlobByteCount,
                        out var literalValue))
                {
                    return PrepareValueLiteral(literalValue);
                }
            }
            catch (System.Exception)
            {
            }

            return preparedExpression;
        }

        private static bool TryMaterializeLiteral(
            PineValueInProcess value,
            int nodeCountLimit,
            int blobByteCountLimit,
            out PineValue literalValue)
        {
            var remainingNodeCount = nodeCountLimit;
            var remainingBlobByteCount = blobByteCountLimit;
            var pending = new Stack<object>();
            pending.Push(value);

            while (pending.TryPop(out var current))
            {
                if (--remainingNodeCount < 0)
                {
                    literalValue = default!;
                    return false;
                }

                if (current is PineValueInProcess inProcess)
                {
                    if (IsOpaque(inProcess))
                    {
                        literalValue = default!;
                        return false;
                    }

                    if (inProcess.EvaluatedOrNull is { } evaluated)
                    {
                        pending.Push(evaluated);
                        continue;
                    }

                    if (inProcess.ListItemsOrNull() is { } listItems)
                    {
                        for (var i = 0; i < listItems.Count; ++i)
                            pending.Push(listItems[i]);

                        continue;
                    }

                    if (!inProcess.IsBlob())
                    {
                        literalValue = default!;
                        return false;
                    }

                    remainingBlobByteCount -= inProcess.GetLength();

                    if (remainingBlobByteCount < 0)
                    {
                        literalValue = default!;
                        return false;
                    }

                    continue;
                }

                var pineValue = (PineValue)current;

                switch (pineValue)
                {
                    case PineValue.BlobValue blobValue:
                        remainingBlobByteCount -= blobValue.Bytes.Length;
                        break;

                    case PineValue.ListValue listValue:
                        for (var i = 0; i < listValue.Items.Length; ++i)
                            pending.Push(listValue.Items.Span[i]);

                        break;

                    default:
                        throw new System.NotImplementedException(
                            "TryMaterializeLiteral does not handle Pine value variant: " +
                            pineValue.GetType().Name);
                }

                if (remainingBlobByteCount < 0)
                {
                    literalValue = default!;
                    return false;
                }
            }

            literalValue = value.Evaluate();
            return true;
        }
    }

}
