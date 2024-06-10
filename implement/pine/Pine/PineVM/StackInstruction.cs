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

    public record ConditionalJumpInstruction(
        Expression Condition,
        int IfFalseOffset,
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

            case ConditionalJumpInstruction conditionalJump:
                {
                    var newCondition =
                        CompilePineToDotNet.ReducePineExpression.TransformPineExpressionWithOptionalReplacement(
                            findReplacement,
                            conditionalJump.Condition).expr;

                    return new ConditionalJumpInstruction(
                        newCondition,
                        IfFalseOffset: conditionalJump.IfFalseOffset,
                        IfTrueOffset: conditionalJump.IfTrueOffset);
                }

            case ReturnInstruction:
                return Return;

            default:
                throw new NotImplementedException(
                    "Unexpected instruction type: " + instruction.GetType().FullName);
        }
    }
}
