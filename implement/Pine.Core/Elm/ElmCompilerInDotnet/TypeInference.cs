using System.Collections.Generic;
using SyntaxTypes = Pine.Core.Elm.ElmSyntax.SyntaxTreeClassic;

namespace Pine.Core.Elm.ElmCompilerInDotnet;

/// <summary>
/// Type inference for Elm expressions.
/// This module determines the types of expressions to enable proper code generation.
/// </summary>
public static class TypeInference
{
    /// <summary>
    /// Represents an inferred type for an Elm expression.
    /// </summary>
    public abstract record InferredType
    {
        /// <summary>Integer type.</summary>
        public sealed record IntType : InferredType;

        /// <summary>Float type.</summary>
        public sealed record FloatType : InferredType;

        /// <summary>String type.</summary>
        public sealed record StringType : InferredType;

        /// <summary>Char type.</summary>
        public sealed record CharType : InferredType;

        /// <summary>Boolean type.</summary>
        public sealed record BoolType : InferredType;

        /// <summary>Number type (polymorphic numeric type that could be Int or Float).</summary>
        public sealed record NumberType : InferredType;

        /// <summary>Unknown or unresolved type.</summary>
        public sealed record UnknownType : InferredType;
    }

    private static readonly InferredType.IntType s_intType = new();
    private static readonly InferredType.FloatType s_floatType = new();
    private static readonly InferredType.StringType s_stringType = new();
    private static readonly InferredType.CharType s_charType = new();
    private static readonly InferredType.BoolType s_boolType = new();
    private static readonly InferredType.NumberType s_numberType = new();
    private static readonly InferredType.UnknownType s_unknownType = new();

    /// <summary>
    /// Describes type constraints for an infix operator.
    /// </summary>
    private record OperatorTypeConstraints(
        InferredType? ResultType = null,
        InferredType? LeftOperandType = null,
        InferredType? RightOperandType = null);

    /// <summary>
    /// Maps operators to their type constraints.
    /// </summary>
    private static readonly Dictionary<string, OperatorTypeConstraints> s_operatorConstraints = new()
    {
        // Integer division forces Int type
        ["//"] = new(ResultType: s_intType, LeftOperandType: s_intType, RightOperandType: s_intType),

        // Float division forces Float type
        ["/"] = new(ResultType: s_floatType, LeftOperandType: s_floatType, RightOperandType: s_floatType),

        // Comparison operators force Bool result
        ["=="] = new(ResultType: s_boolType),
        ["/="] = new(ResultType: s_boolType),
        ["<"] = new(ResultType: s_boolType),
        [">"] = new(ResultType: s_boolType),
        ["<="] = new(ResultType: s_boolType),
        [">="] = new(ResultType: s_boolType),

        // Logical operators force Bool on everything
        ["&&"] = new(ResultType: s_boolType, LeftOperandType: s_boolType, RightOperandType: s_boolType),
        ["||"] = new(ResultType: s_boolType, LeftOperandType: s_boolType, RightOperandType: s_boolType),
    };

    /// <summary>
    /// Converts an Elm type annotation to an inferred type.
    /// </summary>
    public static InferredType TypeAnnotationToInferredType(SyntaxTypes.TypeAnnotation typeAnnotation)
    {
        if (typeAnnotation is SyntaxTypes.TypeAnnotation.Typed typed)
        {
            var (moduleName, name) =
                typed.TypeName.Value;

            if (typed.TypeArguments.Count is 0)
            {
                if (moduleName.Count is 0)
                {
                    switch (name)
                    {
                        case "Int":
                            return s_intType;

                        case "String":
                            return s_stringType;

                        case "Char":
                            return s_charType;

                        case "Bool":
                            return s_boolType;

                        case "Float":
                            return s_floatType;
                    }
                }

                if (moduleName.Count is 1 && moduleName[0] is "Basics")
                {
                    switch (name)
                    {
                        case "Int":
                            return s_intType;

                        case "Bool":
                            return s_boolType;

                        case "Float":
                            return s_floatType;
                    }
                }

                if (moduleName.Count is 1 && moduleName[0] is "String" &&
                    name is "String")
                {
                    return s_stringType;
                }

                if (moduleName.Count is 1 && moduleName[0] is "Char" &&
                    name is "Char")
                {
                    return s_charType;

                }
            }
        }

        return s_unknownType;
    }

    /// <summary>
    /// Infers the type of an Elm expression based on its structure and operands.
    /// </summary>
    /// <param name="expression">The expression to analyze.</param>
    /// <param name="parameterNames">Map of parameter names to their indices.</param>
    /// <param name="parameterTypes">Map of parameter names to their types from type annotations.</param>
    /// <returns>The inferred type of the expression.</returns>
    public static InferredType InferExpressionType(
        SyntaxTypes.Expression expression,
        IReadOnlyDictionary<string, int> parameterNames,
        IReadOnlyDictionary<string, InferredType> parameterTypes)
    {
        // Integer literal - in Elm, this has type "number" (polymorphic), not forced to Int
        if (expression is SyntaxTypes.Expression.Integer)
        {
            return s_numberType;
        }

        // Float literal
        if (expression is SyntaxTypes.Expression.Floatable)
        {
            return s_floatType;
        }

        // String literal
        if (expression is SyntaxTypes.Expression.Literal)
        {
            return s_stringType;
        }

        // Char literal
        if (expression is SyntaxTypes.Expression.CharLiteral)
        {
            return s_charType;
        }

        // Boolean values
        if (expression is SyntaxTypes.Expression.FunctionOrValue funcOrValue &&
            funcOrValue.ModuleName.Count is 0 &&
            (funcOrValue.Name is "True" || funcOrValue.Name is "False"))
        {
            return s_boolType;
        }

        // Parameter reference - look up the type from annotations
        if (expression is SyntaxTypes.Expression.FunctionOrValue paramRef &&
            paramRef.ModuleName.Count is 0 &&
            parameterTypes.TryGetValue(paramRef.Name, out var paramType))
        {
            return paramType;
        }

        // Operator application - infer from operands
        if (expression is SyntaxTypes.Expression.OperatorApplication operatorApp)
        {
            return InferOperatorApplicationType(operatorApp, parameterNames, parameterTypes);
        }

        // Function application - infer from the function's return type
        if (expression is SyntaxTypes.Expression.Application application &&
            application.Arguments.Count >= 2 &&
            application.Arguments[0].Value is SyntaxTypes.Expression.FunctionOrValue funcRef)
        {
            // For now, we can't easily look up the return type of the called function
            // without access to the full compilation context
            // Return UnknownType for now
            return s_unknownType;
        }

        // For other expressions, we cannot infer the type yet
        return s_unknownType;
    }

    /// <summary>
    /// Infers the type of an operator application by examining its operands and operator constraints.
    /// Uses the organized operator constraints to determine forced types.
    /// </summary>
    private static InferredType InferOperatorApplicationType(
        SyntaxTypes.Expression.OperatorApplication operatorApp,
        IReadOnlyDictionary<string, int> parameterNames,
        IReadOnlyDictionary<string, InferredType> parameterTypes)
    {
        // Check if operator has explicit type constraints
        if (s_operatorConstraints.TryGetValue(operatorApp.Operator, out var constraints))
        {
            // If the operator forces a result type, return it
            if (constraints.ResultType is not null)
            {
                return constraints.ResultType;
            }
        }

        // For operators without explicit constraints, infer from operands
        var leftType = InferExpressionType(operatorApp.Left.Value, parameterNames, parameterTypes);
        var rightType = InferExpressionType(operatorApp.Right.Value, parameterNames, parameterTypes);

        // For arithmetic operators without explicit constraints (+, -, *, etc.), 
        // resolve the type based on concrete operand types
        if (operatorApp.Operator is "+" or "-" or "*" or "^" or "%")
        {
            // If either operand is explicitly Int (from type annotation), the operation is Int
            if (leftType is InferredType.IntType || rightType is InferredType.IntType)
            {
                return s_intType;
            }

            // If either operand is Float, the operation is Float
            if (leftType is InferredType.FloatType || rightType is InferredType.FloatType)
            {
                return s_floatType;
            }

            // If both are NumberType (polymorphic numeric literals), we can't determine yet
            return s_numberType;
        }

        return s_unknownType;
    }

    /// <summary>
    /// Determines if an expression is known to be of integer type.
    /// </summary>
    public static bool IsIntegerType(
        SyntaxTypes.Expression expression,
        IReadOnlyDictionary<string, int> parameterNames,
        IReadOnlyDictionary<string, InferredType> parameterTypes)
    {
        return InferExpressionType(expression, parameterNames, parameterTypes) is InferredType.IntType;
    }
}
