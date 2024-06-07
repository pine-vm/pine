using Pine.Json;
using System;
using System.Text.Json.Serialization;

namespace Pine.PineVM;


[JsonConverter(typeof(JsonConverterForChoiceType))]
public abstract record StackInstruction
{
    public static StackInstruction Eval(Expression expression) =>
        new EvalInstruction(expression);

    public static readonly StackInstruction Return = new ReturnInstruction();

    public record EvalInstruction(
        Expression Expression)
        : StackInstruction;

    public record ConditionalJumpRefInstruction(
        Expression Condition,
        Expression IfTrueExpr)
        : StackInstruction;

    public record ConditionalJumpInstruction(
        Expression Condition,
        int IfTrueOffset)
        : StackInstruction;

    public record ReturnInstruction
        : StackInstruction;

    public static StackInstruction TransformExpressionWithOptionalReplacement(
        Func<Expression, Expression?> findReplacement,
        StackInstruction instruction)
    {
        switch (instruction)
        {
            case EvalInstruction evalInstruction:
                var (newExpression, _) =
                        CompilePineToDotNet.ReducePineExpression.TransformPineExpressionWithOptionalReplacement(
                            findReplacement,
                            evalInstruction.Expression);

                return new EvalInstruction(newExpression);

            case ConditionalJumpRefInstruction conditionalJumpRef:
                {
                    var newCondition =
                        CompilePineToDotNet.ReducePineExpression.TransformPineExpressionWithOptionalReplacement(
                            findReplacement,
                            conditionalJumpRef.Condition).expr;

                    var newIfTrueExpr =
                        CompilePineToDotNet.ReducePineExpression.TransformPineExpressionWithOptionalReplacement(
                            findReplacement,
                            conditionalJumpRef.IfTrueExpr).expr;

                    return new ConditionalJumpRefInstruction(newCondition, newIfTrueExpr);
                }

            case ConditionalJumpInstruction conditionalJump:
                {
                    var newCondition =
                        CompilePineToDotNet.ReducePineExpression.TransformPineExpressionWithOptionalReplacement(
                            findReplacement,
                            conditionalJump.Condition).expr;

                    return new ConditionalJumpInstruction(newCondition, conditionalJump.IfTrueOffset);
                }

            case ReturnInstruction returnInstruction:
                return Return;

            default:
                throw new NotImplementedException(
                    "Unexpected instruction type: " + instruction.GetType().FullName);
        }
    }
}
