using System.Collections.Generic;
using SyntaxTypes = Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;

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

        /// <summary>Tuple type with element types.</summary>
        public sealed record TupleType(IReadOnlyList<InferredType> ElementTypes) : InferredType;

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

        // Handle tuple type annotations
        if (typeAnnotation is SyntaxTypes.TypeAnnotation.Tupled tupled)
        {
            var elementTypes = new List<InferredType>();
            foreach (var elementNode in tupled.TypeAnnotations)
            {
                elementTypes.Add(TypeAnnotationToInferredType(elementNode.Value));
            }
            return new InferredType.TupleType(elementTypes);
        }

        return s_unknownType;
    }

    /// <summary>
    /// Extracts the return type from a function's type signature.
    /// For a function like `alfa : List a -> Int`, this returns Int.
    /// </summary>
    /// <param name="function">The function structure containing the signature.</param>
    /// <returns>The inferred return type, or UnknownType if not determinable.</returns>
    public static InferredType GetFunctionReturnType(SyntaxTypes.FunctionStruct function)
    {
        if (function.Signature?.Value is not { } signature)
        {
            return s_unknownType;
        }

        var typeAnnotation = signature.TypeAnnotation.Value;
        var paramCount = function.Declaration.Value.Arguments.Count;

        // Walk through the function type annotation to find the return type
        var currentType = typeAnnotation;

        for (var i = 0; i < paramCount; i++)
        {
            if (currentType is SyntaxTypes.TypeAnnotation.FunctionTypeAnnotation funcType)
            {
                currentType = funcType.ReturnType.Value;
            }
            else
            {
                // Not enough function arrows for the parameters
                return s_unknownType;
            }
        }

        // currentType is now the return type
        return TypeAnnotationToInferredType(currentType);
    }

    /// <summary>
    /// Extracts the return type from a function declaration.
    /// </summary>
    /// <param name="declaration">The function declaration.</param>
    /// <returns>The inferred return type, or UnknownType if not determinable.</returns>
    public static InferredType GetFunctionReturnType(SyntaxTypes.Declaration.FunctionDeclaration declaration)
    {
        return GetFunctionReturnType(declaration.Function);
    }

    /// <summary>
    /// Infers the type of an Elm expression based on its structure and operands.
    /// </summary>
    /// <param name="expression">The expression to analyze.</param>
    /// <param name="parameterNames">Map of parameter names to their indices.</param>
    /// <param name="parameterTypes">Map of parameter names to their types from type annotations.</param>
    /// <param name="localBindingTypes">Map of local binding names to their types.</param>
    /// <returns>The inferred type of the expression.</returns>
    public static InferredType InferExpressionType(
        SyntaxTypes.Expression expression,
        IReadOnlyDictionary<string, int> parameterNames,
        IReadOnlyDictionary<string, InferredType> parameterTypes,
        IReadOnlyDictionary<string, InferredType>? localBindingTypes = null) =>
        InferExpressionType(expression, parameterNames, parameterTypes, localBindingTypes, null, null);

    /// <summary>
    /// Infers the type of an Elm expression based on its structure, operands, and context.
    /// </summary>
    /// <param name="expression">The expression to analyze.</param>
    /// <param name="parameterNames">Map of parameter names to their indices.</param>
    /// <param name="parameterTypes">Map of parameter names to their types from type annotations.</param>
    /// <param name="localBindingTypes">Map of local binding names to their types.</param>
    /// <param name="currentModuleName">The current module name for resolving unqualified function references.</param>
    /// <param name="functionReturnTypes">Map of qualified function names to their return types.</param>
    /// <returns>The inferred type of the expression.</returns>
    public static InferredType InferExpressionType(
        SyntaxTypes.Expression expression,
        IReadOnlyDictionary<string, int> parameterNames,
        IReadOnlyDictionary<string, InferredType> parameterTypes,
        IReadOnlyDictionary<string, InferredType>? localBindingTypes,
        string? currentModuleName,
        IReadOnlyDictionary<string, InferredType>? functionReturnTypes)
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

        // Local binding reference - look up the type from local binding types
        if (expression is SyntaxTypes.Expression.FunctionOrValue localRef &&
            localRef.ModuleName.Count is 0 &&
            localBindingTypes is not null &&
            localBindingTypes.TryGetValue(localRef.Name, out var localType))
        {
            return localType;
        }

        // Operator application - infer from operands
        if (expression is SyntaxTypes.Expression.OperatorApplication operatorApp)
        {
            return InferOperatorApplicationType(operatorApp, parameterNames, parameterTypes, localBindingTypes, currentModuleName, functionReturnTypes);
        }

        // Tuple expression - infer type from element types
        if (expression is SyntaxTypes.Expression.TupledExpression tupledExpr)
        {
            var elementTypes = new List<InferredType>();
            foreach (var elem in tupledExpr.Elements)
            {
                elementTypes.Add(InferExpressionType(elem.Value, parameterNames, parameterTypes, localBindingTypes, currentModuleName, functionReturnTypes));
            }
            return new InferredType.TupleType(elementTypes);
        }

        // Parenthesized expression - infer from inner expression
        if (expression is SyntaxTypes.Expression.ParenthesizedExpression parenExpr)
        {
            return InferExpressionType(parenExpr.Expression.Value, parameterNames, parameterTypes, localBindingTypes, currentModuleName, functionReturnTypes);
        }

        // Function application - infer from the function's return type
        if (expression is SyntaxTypes.Expression.Application application &&
            application.Arguments.Count >= 1 &&
            application.Arguments[0].Value is SyntaxTypes.Expression.FunctionOrValue funcRef &&
            functionReturnTypes is not null)
        {
            // Build the qualified function name
            string qualifiedName;
            if (funcRef.ModuleName.Count > 0)
            {
                qualifiedName = string.Join(".", funcRef.ModuleName) + "." + funcRef.Name;
            }
            else if (currentModuleName is not null)
            {
                qualifiedName = currentModuleName + "." + funcRef.Name;
            }
            else
            {
                qualifiedName = funcRef.Name;
            }

            // Look up the return type
            if (functionReturnTypes.TryGetValue(qualifiedName, out var returnType))
            {
                return returnType;
            }
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
        IReadOnlyDictionary<string, InferredType> parameterTypes,
        IReadOnlyDictionary<string, InferredType>? localBindingTypes,
        string? currentModuleName,
        IReadOnlyDictionary<string, InferredType>? functionReturnTypes)
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
        var leftType = InferExpressionType(operatorApp.Left.Value, parameterNames, parameterTypes, localBindingTypes, currentModuleName, functionReturnTypes);
        var rightType = InferExpressionType(operatorApp.Right.Value, parameterNames, parameterTypes, localBindingTypes, currentModuleName, functionReturnTypes);

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
        IReadOnlyDictionary<string, InferredType> parameterTypes,
        IReadOnlyDictionary<string, InferredType>? localBindingTypes = null)
    {
        return InferExpressionType(expression, parameterNames, parameterTypes, localBindingTypes) is InferredType.IntType;
    }

    /// <summary>
    /// Extracts binding types from a pattern given a type annotation.
    /// For example, a tuple pattern (x, y) with type (Int, Int) will extract types {x: Int, y: Int}.
    /// </summary>
    /// <param name="pattern">The pattern to analyze.</param>
    /// <param name="typeAnnotation">The type annotation for the pattern.</param>
    /// <param name="bindingTypes">Dictionary to populate with binding name -> type mappings.</param>
    public static void ExtractPatternBindingTypes(
        SyntaxTypes.Pattern pattern,
        SyntaxTypes.TypeAnnotation typeAnnotation,
        Dictionary<string, InferredType> bindingTypes)
    {
        switch (pattern)
        {
            case SyntaxTypes.Pattern.VarPattern varPattern:
                // Simple variable binding - convert the type annotation to inferred type
                bindingTypes[varPattern.Name] = TypeAnnotationToInferredType(typeAnnotation);
                break;

            case SyntaxTypes.Pattern.TuplePattern tuplePattern:
                // Tuple pattern - match against Tupled type annotation
                if (typeAnnotation is SyntaxTypes.TypeAnnotation.Tupled tupledType &&
                    tupledType.TypeAnnotations.Count == tuplePattern.Elements.Count)
                {
                    for (var i = 0; i < tuplePattern.Elements.Count; i++)
                    {
                        ExtractPatternBindingTypes(
                            tuplePattern.Elements[i].Value,
                            tupledType.TypeAnnotations[i].Value,
                            bindingTypes);
                    }
                }
                break;

            case SyntaxTypes.Pattern.ParenthesizedPattern parenthesized:
                // Unwrap parenthesized pattern and recurse
                ExtractPatternBindingTypes(parenthesized.Pattern.Value, typeAnnotation, bindingTypes);
                break;

            case SyntaxTypes.Pattern.AllPattern:
                // Wildcard pattern - no bindings
                break;

                // Additional patterns can be added as needed
        }
    }

    /// <summary>
    /// Extracts binding types from a pattern given an inferred type.
    /// For example, a tuple pattern (x, y) with type TupleType([Int, Int]) will extract types {x: Int, y: Int}.
    /// </summary>
    /// <param name="pattern">The pattern to analyze.</param>
    /// <param name="inferredType">The inferred type for the pattern.</param>
    /// <param name="bindingTypes">Dictionary to populate with binding name -> type mappings.</param>
    public static void ExtractPatternBindingTypesFromInferred(
        SyntaxTypes.Pattern pattern,
        InferredType inferredType,
        Dictionary<string, InferredType> bindingTypes)
    {
        switch (pattern)
        {
            case SyntaxTypes.Pattern.VarPattern varPattern:
                // Simple variable binding
                bindingTypes[varPattern.Name] = inferredType;
                break;

            case SyntaxTypes.Pattern.TuplePattern tuplePattern:
                // Tuple pattern - match against TupleType
                if (inferredType is InferredType.TupleType tupleType &&
                    tupleType.ElementTypes.Count == tuplePattern.Elements.Count)
                {
                    for (var i = 0; i < tuplePattern.Elements.Count; i++)
                    {
                        ExtractPatternBindingTypesFromInferred(
                            tuplePattern.Elements[i].Value,
                            tupleType.ElementTypes[i],
                            bindingTypes);
                    }
                }
                break;

            case SyntaxTypes.Pattern.ParenthesizedPattern parenthesized:
                // Unwrap parenthesized pattern and recurse
                ExtractPatternBindingTypesFromInferred(parenthesized.Pattern.Value, inferredType, bindingTypes);
                break;

            case SyntaxTypes.Pattern.AllPattern:
                // Wildcard pattern - no bindings
                break;

                // Additional patterns can be added as needed
        }
    }
}
