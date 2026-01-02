using System.Collections.Generic;
using System.Collections.Immutable;
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
    /// Extracts the parameter types from a function's type signature.
    /// For a function like `alfa : Int -> String -> Bool`, this returns [Int, String].
    /// </summary>
    /// <param name="function">The function structure containing the signature.</param>
    /// <returns>List of parameter types, or empty list if not determinable.</returns>
    public static IReadOnlyList<InferredType> GetFunctionParameterTypes(SyntaxTypes.FunctionStruct function)
    {
        if (function.Signature?.Value is not { } signature)
        {
            return [];
        }

        var typeAnnotation = signature.TypeAnnotation.Value;
        var paramCount = function.Declaration.Value.Arguments.Count;

        var parameterTypes = new List<InferredType>();

        // Walk through the function type annotation to extract parameter types
        var currentType = typeAnnotation;

        for (var i = 0; i < paramCount; i++)
        {
            if (currentType is SyntaxTypes.TypeAnnotation.FunctionTypeAnnotation funcType)
            {
                parameterTypes.Add(TypeAnnotationToInferredType(funcType.ArgumentType.Value));
                currentType = funcType.ReturnType.Value;
            }
            else
            {
                // Not enough function arrows for the parameters
                break;
            }
        }

        return parameterTypes;
    }

    /// <summary>
    /// Extracts the parameter types from a function declaration.
    /// </summary>
    /// <param name="declaration">The function declaration.</param>
    /// <returns>List of parameter types, or empty list if not determinable.</returns>
    public static IReadOnlyList<InferredType> GetFunctionParameterTypes(SyntaxTypes.Declaration.FunctionDeclaration declaration)
    {
        return GetFunctionParameterTypes(declaration.Function);
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
    /// Extracts type constraints from tag constructor applications in an expression.
    /// When a variable is used as an argument to a tag constructor, its type can be inferred from the constructor's signature.
    /// For example, in `TagAlfa a` where `TagAlfa` takes an `Int`, we infer that `a` is `Int`.
    /// </summary>
    /// <param name="expression">The expression to analyze.</param>
    /// <param name="constructorArgumentTypes">Map of constructor names to their argument types.</param>
    /// <param name="existingConstraints">Existing type constraints to merge with.</param>
    /// <returns>A new dictionary containing all constraints (existing plus newly discovered).</returns>
    public static ImmutableDictionary<string, InferredType> ExtractTypeConstraintsFromTagApplications(
        SyntaxTypes.Expression expression,
        IReadOnlyDictionary<string, IReadOnlyList<InferredType>>? constructorArgumentTypes,
        ImmutableDictionary<string, InferredType> existingConstraints)
    {
        if (constructorArgumentTypes is null)
            return existingConstraints;

        return ExtractTypeConstraintsFromTagApplicationsInternal(
            expression,
            constructorArgumentTypes,
            existingConstraints);
    }

    private static ImmutableDictionary<string, InferredType> ExtractTypeConstraintsFromTagApplicationsInternal(
        SyntaxTypes.Expression expression,
        IReadOnlyDictionary<string, IReadOnlyList<InferredType>> constructorArgumentTypes,
        ImmutableDictionary<string, InferredType> constraints)
    {
        switch (expression)
        {
            case SyntaxTypes.Expression.Application application:
                // Check if this is a tag constructor application
                // Applications have the form [func, arg1, arg2, ...], so we need at least 2 elements
                // (the tag constructor and at least one argument)
                if (application.Arguments.Count >= 2 &&
                    application.Arguments[0].Value is SyntaxTypes.Expression.FunctionOrValue tagFuncRef &&
                    ElmValueEncoding.StringIsValidTagName(tagFuncRef.Name))
                {
                    // This is a tag constructor application
                    if (constructorArgumentTypes.TryGetValue(tagFuncRef.Name, out var argTypes))
                    {
                        // Match arguments to constructor argument types
                        // Arguments start at index 1 (index 0 is the constructor itself)
                        for (var i = 1; i < application.Arguments.Count && i - 1 < argTypes.Count; i++)
                        {
                            var argIndex = i - 1;
                            var argExpr = application.Arguments[i].Value;
                            var argType = argTypes[argIndex];

                            // If the argument is a simple variable reference, constrain its type
                            // (only if not already constrained, to avoid overwriting existing constraints)
                            if (argExpr is SyntaxTypes.Expression.FunctionOrValue tagVarRef &&
                                tagVarRef.ModuleName.Count is 0 &&
                                !ElmValueEncoding.StringIsValidTagName(tagVarRef.Name) &&
                                !constraints.ContainsKey(tagVarRef.Name))
                            {
                                constraints = constraints.SetItem(tagVarRef.Name, argType);
                            }

                            // Recurse into the argument expression
                            constraints = ExtractTypeConstraintsFromTagApplicationsInternal(argExpr, constructorArgumentTypes, constraints);
                        }
                    }
                }

                // Recurse into all arguments
                foreach (var arg in application.Arguments)
                {
                    constraints = ExtractTypeConstraintsFromTagApplicationsInternal(arg.Value, constructorArgumentTypes, constraints);
                }
                return constraints;

            case SyntaxTypes.Expression.OperatorApplication opApp:
                constraints = ExtractTypeConstraintsFromTagApplicationsInternal(opApp.Left.Value, constructorArgumentTypes, constraints);
                constraints = ExtractTypeConstraintsFromTagApplicationsInternal(opApp.Right.Value, constructorArgumentTypes, constraints);
                return constraints;

            case SyntaxTypes.Expression.IfBlock ifBlock:
                constraints = ExtractTypeConstraintsFromTagApplicationsInternal(ifBlock.Condition.Value, constructorArgumentTypes, constraints);
                constraints = ExtractTypeConstraintsFromTagApplicationsInternal(ifBlock.ThenBlock.Value, constructorArgumentTypes, constraints);
                constraints = ExtractTypeConstraintsFromTagApplicationsInternal(ifBlock.ElseBlock.Value, constructorArgumentTypes, constraints);
                return constraints;

            case SyntaxTypes.Expression.LetExpression letExpr:
                foreach (var decl in letExpr.Value.Declarations)
                {
                    switch (decl.Value)
                    {
                        case SyntaxTypes.Expression.LetDeclaration.LetFunction letFunc:
                            constraints = ExtractTypeConstraintsFromTagApplicationsInternal(letFunc.Function.Declaration.Value.Expression.Value, constructorArgumentTypes, constraints);
                            break;

                        case SyntaxTypes.Expression.LetDeclaration.LetDestructuring letDestr:
                            constraints = ExtractTypeConstraintsFromTagApplicationsInternal(letDestr.Expression.Value, constructorArgumentTypes, constraints);
                            break;
                    }
                }
                constraints = ExtractTypeConstraintsFromTagApplicationsInternal(letExpr.Value.Expression.Value, constructorArgumentTypes, constraints);
                return constraints;

            case SyntaxTypes.Expression.CaseExpression caseExpr:
                constraints = ExtractTypeConstraintsFromTagApplicationsInternal(caseExpr.CaseBlock.Expression.Value, constructorArgumentTypes, constraints);
                foreach (var caseItem in caseExpr.CaseBlock.Cases)
                {
                    constraints = ExtractTypeConstraintsFromTagApplicationsInternal(caseItem.Expression.Value, constructorArgumentTypes, constraints);
                }
                return constraints;

            case SyntaxTypes.Expression.ParenthesizedExpression parenExpr:
                return ExtractTypeConstraintsFromTagApplicationsInternal(parenExpr.Expression.Value, constructorArgumentTypes, constraints);

            case SyntaxTypes.Expression.ListExpr listExpr:
                foreach (var elem in listExpr.Elements)
                {
                    constraints = ExtractTypeConstraintsFromTagApplicationsInternal(elem.Value, constructorArgumentTypes, constraints);
                }
                return constraints;

            case SyntaxTypes.Expression.TupledExpression tupleExpr:
                foreach (var elem in tupleExpr.Elements)
                {
                    constraints = ExtractTypeConstraintsFromTagApplicationsInternal(elem.Value, constructorArgumentTypes, constraints);
                }
                return constraints;

            case SyntaxTypes.Expression.Negation negation:
                return ExtractTypeConstraintsFromTagApplicationsInternal(negation.Expression.Value, constructorArgumentTypes, constraints);

            default:
                // Other expression types that don't introduce constraints
                return constraints;
        }
    }

    /// <summary>
    /// Extracts type constraints from function applications in an expression.
    /// When a variable is used as an argument to a function with a known parameter type, 
    /// its type can be inferred from the function's signature.
    /// For example, in `alfa a` where `alfa : Int -> String`, we infer that `a` is `Int`.
    /// </summary>
    /// <param name="expression">The expression to analyze.</param>
    /// <param name="functionParameterTypes">Map of qualified function names to their parameter types.</param>
    /// <param name="currentModuleName">The current module name for resolving unqualified function references.</param>
    /// <param name="existingConstraints">Existing type constraints to merge with.</param>
    /// <returns>A new dictionary containing all constraints (existing plus newly discovered).</returns>
    public static ImmutableDictionary<string, InferredType> ExtractTypeConstraintsFromFunctionApplications(
        SyntaxTypes.Expression expression,
        IReadOnlyDictionary<string, IReadOnlyList<InferredType>>? functionParameterTypes,
        string currentModuleName,
        ImmutableDictionary<string, InferredType> existingConstraints)
    {
        if (functionParameterTypes is null)
            return existingConstraints;

        return ExtractTypeConstraintsFromFunctionApplicationsInternal(
            expression,
            functionParameterTypes,
            currentModuleName,
            existingConstraints);
    }

    private static ImmutableDictionary<string, InferredType> ExtractTypeConstraintsFromFunctionApplicationsInternal(
        SyntaxTypes.Expression expression,
        IReadOnlyDictionary<string, IReadOnlyList<InferredType>> functionParameterTypes,
        string currentModuleName,
        ImmutableDictionary<string, InferredType> constraints)
    {
        switch (expression)
        {
            case SyntaxTypes.Expression.Application application:
                // Check if this is a function application (not a tag constructor)
                if (application.Arguments.Count >= 2 &&
                    application.Arguments[0].Value is SyntaxTypes.Expression.FunctionOrValue funcRef &&
                    !ElmValueEncoding.StringIsValidTagName(funcRef.Name))
                {
                    // Build qualified function name
                    var qualifiedFuncName = funcRef.ModuleName.Count > 0
                        ? string.Join(".", funcRef.ModuleName) + "." + funcRef.Name
                        : currentModuleName + "." + funcRef.Name;

                    // Check if we have parameter types for this function
                    if (functionParameterTypes.TryGetValue(qualifiedFuncName, out var paramTypes))
                    {
                        // Match arguments to function parameter types
                        // Arguments start at index 1 (index 0 is the function itself)
                        for (var i = 1; i < application.Arguments.Count && i - 1 < paramTypes.Count; i++)
                        {
                            var argIndex = i - 1;
                            var argExpr = application.Arguments[i].Value;
                            var paramType = paramTypes[argIndex];

                            // If the argument is a simple variable reference, constrain its type
                            // (only if not already constrained and param type is known)
                            if (argExpr is SyntaxTypes.Expression.FunctionOrValue varRef &&
                                varRef.ModuleName.Count is 0 &&
                                !ElmValueEncoding.StringIsValidTagName(varRef.Name) &&
                                !constraints.ContainsKey(varRef.Name) &&
                                paramType is not InferredType.UnknownType)
                            {
                                constraints = constraints.SetItem(varRef.Name, paramType);
                            }
                        }
                    }
                }

                // Recurse into all arguments to find nested function applications
                foreach (var arg in application.Arguments)
                {
                    constraints = ExtractTypeConstraintsFromFunctionApplicationsInternal(arg.Value, functionParameterTypes, currentModuleName, constraints);
                }
                return constraints;

            case SyntaxTypes.Expression.OperatorApplication opApp:
                constraints = ExtractTypeConstraintsFromFunctionApplicationsInternal(opApp.Left.Value, functionParameterTypes, currentModuleName, constraints);
                constraints = ExtractTypeConstraintsFromFunctionApplicationsInternal(opApp.Right.Value, functionParameterTypes, currentModuleName, constraints);
                return constraints;

            case SyntaxTypes.Expression.IfBlock ifBlock:
                constraints = ExtractTypeConstraintsFromFunctionApplicationsInternal(ifBlock.Condition.Value, functionParameterTypes, currentModuleName, constraints);
                constraints = ExtractTypeConstraintsFromFunctionApplicationsInternal(ifBlock.ThenBlock.Value, functionParameterTypes, currentModuleName, constraints);
                constraints = ExtractTypeConstraintsFromFunctionApplicationsInternal(ifBlock.ElseBlock.Value, functionParameterTypes, currentModuleName, constraints);
                return constraints;

            case SyntaxTypes.Expression.LetExpression letExpr:
                foreach (var decl in letExpr.Value.Declarations)
                {
                    switch (decl.Value)
                    {
                        case SyntaxTypes.Expression.LetDeclaration.LetFunction letFunc:
                            constraints = ExtractTypeConstraintsFromFunctionApplicationsInternal(letFunc.Function.Declaration.Value.Expression.Value, functionParameterTypes, currentModuleName, constraints);
                            break;

                        case SyntaxTypes.Expression.LetDeclaration.LetDestructuring letDestr:
                            constraints = ExtractTypeConstraintsFromFunctionApplicationsInternal(letDestr.Expression.Value, functionParameterTypes, currentModuleName, constraints);
                            break;
                    }
                }
                constraints = ExtractTypeConstraintsFromFunctionApplicationsInternal(letExpr.Value.Expression.Value, functionParameterTypes, currentModuleName, constraints);
                return constraints;

            case SyntaxTypes.Expression.CaseExpression caseExpr:
                constraints = ExtractTypeConstraintsFromFunctionApplicationsInternal(caseExpr.CaseBlock.Expression.Value, functionParameterTypes, currentModuleName, constraints);
                foreach (var caseItem in caseExpr.CaseBlock.Cases)
                {
                    constraints = ExtractTypeConstraintsFromFunctionApplicationsInternal(caseItem.Expression.Value, functionParameterTypes, currentModuleName, constraints);
                }
                return constraints;

            case SyntaxTypes.Expression.ParenthesizedExpression parenExpr:
                return ExtractTypeConstraintsFromFunctionApplicationsInternal(parenExpr.Expression.Value, functionParameterTypes, currentModuleName, constraints);

            case SyntaxTypes.Expression.ListExpr listExpr:
                foreach (var elem in listExpr.Elements)
                {
                    constraints = ExtractTypeConstraintsFromFunctionApplicationsInternal(elem.Value, functionParameterTypes, currentModuleName, constraints);
                }
                return constraints;

            case SyntaxTypes.Expression.TupledExpression tupleExpr:
                foreach (var elem in tupleExpr.Elements)
                {
                    constraints = ExtractTypeConstraintsFromFunctionApplicationsInternal(elem.Value, functionParameterTypes, currentModuleName, constraints);
                }
                return constraints;

            case SyntaxTypes.Expression.Negation negation:
                return ExtractTypeConstraintsFromFunctionApplicationsInternal(negation.Expression.Value, functionParameterTypes, currentModuleName, constraints);

            default:
                // Other expression types that don't introduce constraints
                return constraints;
        }
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
    /// <param name="existingBindings">Existing binding types to merge with.</param>
    /// <returns>A new dictionary containing all bindings (existing plus newly discovered).</returns>
    public static ImmutableDictionary<string, InferredType> ExtractPatternBindingTypes(
        SyntaxTypes.Pattern pattern,
        SyntaxTypes.TypeAnnotation typeAnnotation,
        ImmutableDictionary<string, InferredType> existingBindings)
    {
        return ExtractPatternBindingTypesInternal(pattern, typeAnnotation, existingBindings);
    }

    private static ImmutableDictionary<string, InferredType> ExtractPatternBindingTypesInternal(
        SyntaxTypes.Pattern pattern,
        SyntaxTypes.TypeAnnotation typeAnnotation,
        ImmutableDictionary<string, InferredType> bindings)
    {
        switch (pattern)
        {
            case SyntaxTypes.Pattern.VarPattern varPattern:
                // Simple variable binding - convert the type annotation to inferred type
                return bindings.SetItem(varPattern.Name, TypeAnnotationToInferredType(typeAnnotation));

            case SyntaxTypes.Pattern.TuplePattern tuplePattern:
                // Tuple pattern - match against Tupled type annotation
                if (typeAnnotation is SyntaxTypes.TypeAnnotation.Tupled tupledType &&
                    tupledType.TypeAnnotations.Count == tuplePattern.Elements.Count)
                {
                    for (var i = 0; i < tuplePattern.Elements.Count; i++)
                    {
                        bindings = ExtractPatternBindingTypesInternal(
                            tuplePattern.Elements[i].Value,
                            tupledType.TypeAnnotations[i].Value,
                            bindings);
                    }
                }
                return bindings;

            case SyntaxTypes.Pattern.ParenthesizedPattern parenthesized:
                // Unwrap parenthesized pattern and recurse
                return ExtractPatternBindingTypesInternal(parenthesized.Pattern.Value, typeAnnotation, bindings);

            case SyntaxTypes.Pattern.AllPattern:
                // Wildcard pattern - no bindings
                return bindings;

            default:
                // Additional patterns can be added as needed
                return bindings;
        }
    }

    /// <summary>
    /// Extracts binding types from a pattern using constructor argument types.
    /// This is used for function parameters that are NamedPatterns, where we don't have a type annotation.
    /// </summary>
    /// <param name="pattern">The pattern to analyze.</param>
    /// <param name="constructorArgumentTypes">Map of constructor names to their argument types.</param>
    /// <param name="existingBindings">Existing binding types to merge with.</param>
    /// <returns>A new dictionary containing all bindings (existing plus newly discovered).</returns>
    public static ImmutableDictionary<string, InferredType> ExtractPatternBindingTypesWithConstructors(
        SyntaxTypes.Pattern pattern,
        IReadOnlyDictionary<string, IReadOnlyList<InferredType>>? constructorArgumentTypes,
        ImmutableDictionary<string, InferredType> existingBindings)
    {
        if (constructorArgumentTypes is null)
            return existingBindings;

        return ExtractPatternBindingTypesWithConstructorsInternal(pattern, constructorArgumentTypes, existingBindings);
    }

    private static ImmutableDictionary<string, InferredType> ExtractPatternBindingTypesWithConstructorsInternal(
        SyntaxTypes.Pattern pattern,
        IReadOnlyDictionary<string, IReadOnlyList<InferredType>> constructorArgumentTypes,
        ImmutableDictionary<string, InferredType> bindings)
    {
        switch (pattern)
        {
            case SyntaxTypes.Pattern.NamedPattern namedPattern:
                // NamedPattern - look up constructor argument types
                if (constructorArgumentTypes.TryGetValue(namedPattern.Name.Name, out var argTypes) &&
                    argTypes.Count == namedPattern.Arguments.Count)
                {
                    for (var i = 0; i < namedPattern.Arguments.Count; i++)
                    {
                        bindings = ExtractPatternBindingTypesFromInferredInternal(
                            namedPattern.Arguments[i].Value,
                            argTypes[i],
                            bindings,
                            constructorArgumentTypes);
                    }
                }
                return bindings;

            case SyntaxTypes.Pattern.ParenthesizedPattern parenthesized:
                // Unwrap parenthesized pattern and recurse
                return ExtractPatternBindingTypesWithConstructorsInternal(parenthesized.Pattern.Value, constructorArgumentTypes, bindings);

            case SyntaxTypes.Pattern.TuplePattern tuplePattern:
                // Recurse into tuple elements (they might contain NamedPatterns)
                foreach (var elem in tuplePattern.Elements)
                {
                    bindings = ExtractPatternBindingTypesWithConstructorsInternal(elem.Value, constructorArgumentTypes, bindings);
                }
                return bindings;

            default:
                // Other patterns don't introduce type constraints from constructors without type annotation
                return bindings;
        }
    }

    /// <summary>
    /// Extracts binding types from a pattern given an inferred type.
    /// For example, a tuple pattern (x, y) with type TupleType([Int, Int]) will extract types {x: Int, y: Int}.
    /// </summary>
    /// <param name="pattern">The pattern to analyze.</param>
    /// <param name="inferredType">The inferred type for the pattern.</param>
    /// <param name="existingBindings">Existing binding types to merge with.</param>
    /// <returns>A new dictionary containing all bindings (existing plus newly discovered).</returns>
    public static ImmutableDictionary<string, InferredType> ExtractPatternBindingTypesFromInferred(
        SyntaxTypes.Pattern pattern,
        InferredType inferredType,
        ImmutableDictionary<string, InferredType> existingBindings)
    {
        return ExtractPatternBindingTypesFromInferredInternal(pattern, inferredType, existingBindings, null);
    }

    /// <summary>
    /// Extracts binding types from a pattern given an inferred type and constructor argument types.
    /// For example, a tuple pattern (x, y) with type TupleType([Int, Int]) will extract types {x: Int, y: Int}.
    /// For NamedPattern like (TagAlfa a), it looks up the constructor's argument types.
    /// </summary>
    /// <param name="pattern">The pattern to analyze.</param>
    /// <param name="inferredType">The inferred type for the pattern.</param>
    /// <param name="existingBindings">Existing binding types to merge with.</param>
    /// <param name="constructorArgumentTypes">Optional map of constructor names to their argument types.</param>
    /// <returns>A new dictionary containing all bindings (existing plus newly discovered).</returns>
    public static ImmutableDictionary<string, InferredType> ExtractPatternBindingTypesFromInferred(
        SyntaxTypes.Pattern pattern,
        InferredType inferredType,
        ImmutableDictionary<string, InferredType> existingBindings,
        IReadOnlyDictionary<string, IReadOnlyList<InferredType>>? constructorArgumentTypes)
    {
        return ExtractPatternBindingTypesFromInferredInternal(pattern, inferredType, existingBindings, constructorArgumentTypes);
    }

    private static ImmutableDictionary<string, InferredType> ExtractPatternBindingTypesFromInferredInternal(
        SyntaxTypes.Pattern pattern,
        InferredType inferredType,
        ImmutableDictionary<string, InferredType> bindings,
        IReadOnlyDictionary<string, IReadOnlyList<InferredType>>? constructorArgumentTypes)
    {
        switch (pattern)
        {
            case SyntaxTypes.Pattern.VarPattern varPattern:
                // Simple variable binding
                return bindings.SetItem(varPattern.Name, inferredType);

            case SyntaxTypes.Pattern.TuplePattern tuplePattern:
                // Tuple pattern - match against TupleType
                if (inferredType is InferredType.TupleType tupleType &&
                    tupleType.ElementTypes.Count == tuplePattern.Elements.Count)
                {
                    for (var i = 0; i < tuplePattern.Elements.Count; i++)
                    {
                        bindings = ExtractPatternBindingTypesFromInferredInternal(
                            tuplePattern.Elements[i].Value,
                            tupleType.ElementTypes[i],
                            bindings,
                            constructorArgumentTypes);
                    }
                }
                return bindings;

            case SyntaxTypes.Pattern.ParenthesizedPattern parenthesized:
                // Unwrap parenthesized pattern and recurse
                return ExtractPatternBindingTypesFromInferredInternal(
                    parenthesized.Pattern.Value,
                    inferredType,
                    bindings,
                    constructorArgumentTypes);

            case SyntaxTypes.Pattern.NamedPattern namedPattern:
                // NamedPattern - look up constructor argument types
                if (constructorArgumentTypes is not null &&
                    constructorArgumentTypes.TryGetValue(namedPattern.Name.Name, out var argTypes) &&
                    argTypes.Count == namedPattern.Arguments.Count)
                {
                    for (var i = 0; i < namedPattern.Arguments.Count; i++)
                    {
                        bindings = ExtractPatternBindingTypesFromInferredInternal(
                            namedPattern.Arguments[i].Value,
                            argTypes[i],
                            bindings,
                            constructorArgumentTypes);
                    }
                }
                return bindings;

            case SyntaxTypes.Pattern.AllPattern:
                // Wildcard pattern - no bindings
                return bindings;

            default:
                // Additional patterns can be added as needed
                return bindings;
        }
    }
}
