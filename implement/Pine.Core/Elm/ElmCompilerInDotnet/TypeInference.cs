using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
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
        public sealed record IntType
            : InferredType;

        /// <summary>Float type.</summary>
        public sealed record FloatType
            : InferredType;

        /// <summary>String type.</summary>
        public sealed record StringType
            : InferredType;

        /// <summary>Char type.</summary>
        public sealed record CharType
            : InferredType;

        /// <summary>Boolean type.</summary>
        public sealed record BoolType
            : InferredType;

        /// <summary>Number type (polymorphic numeric type that could be Int or Float).</summary>
        public sealed record NumberType
            : InferredType;

        /// <summary>Tuple type with element types.</summary>
        public sealed record TupleType(IReadOnlyList<InferredType> ElementTypes)
            : InferredType;

        /// <summary>Record type with field names and their types (sorted alphabetically by field name).</summary>
        public sealed record RecordType(IReadOnlyList<(string FieldName, InferredType FieldType)> Fields)
            : InferredType;

        /// <summary>Function type with argument type and return type.</summary>
        public sealed record FunctionType(InferredType ArgumentType, InferredType ReturnType)
            : InferredType;

        /// <summary>List type with element type.</summary>
        public sealed record ListType(InferredType ElementType)
            : InferredType;

        /// <summary>Choice type with module name, type name, and type arguments.</summary>
        public sealed record ChoiceType(IReadOnlyList<string> ModuleName, string TypeName, IReadOnlyList<InferredType> TypeArguments)
            : InferredType;

        /// <summary>Type variable (e.g., 'a', 'b' in generic type definitions).</summary>
        public sealed record TypeVariable(string Name)
            : InferredType;

        /// <summary>Unknown or unresolved type.</summary>
        public sealed record UnknownType
            : InferredType;

        /// <summary>
        /// Returns the inferred type that represents an integer.
        /// </summary>
        public static InferredType Int() =>
            s_intType;

        /// <summary>
        /// Returns the inferred type that represents a floating-point number.
        /// </summary>
        public static InferredType Float() =>
            s_floatType;

        /// <summary>
        /// Returns the inferred type that represents a string.
        /// </summary>
        public static InferredType String() =>
            s_stringType;

        /// <summary>
        /// Returns the inferred type that represents a character.
        /// </summary>
        public static InferredType Char() =>
            s_charType;

        /// <summary>
        /// Returns the inferred type that represents a boolean.
        /// </summary>
        public static InferredType Bool() =>
            s_boolType;

        /// <summary>
        /// Returns the inferred type that represents a number (polymorphic numeric type).
        /// </summary>
        public static InferredType Number() =>
            s_numberType;

        /// <summary>
        /// Build a function type that represents a mapping from the specified argument type to the specified return type.
        /// </summary>
        public static InferredType Function(InferredType argType, InferredType returnType) =>
            new FunctionType(argType, returnType);
    }

    private static readonly InferredType.IntType s_intType = new();
    private static readonly InferredType.FloatType s_floatType = new();
    private static readonly InferredType.StringType s_stringType = new();
    private static readonly InferredType.CharType s_charType = new();
    private static readonly InferredType.BoolType s_boolType = new();
    private static readonly InferredType.NumberType s_numberType = new();
    private static readonly InferredType.UnknownType s_unknownType = new();

    /// <summary>
    /// Unifies two inferred types, returning the more specific type when possible.
    /// For example, Int and number unify to Int (Int is more specific than number).
    /// </summary>
    private static InferredType UnifyTypes(InferredType type1, InferredType type2)
    {
        // If types are equal, return either
        if (type1.Equals(type2))
            return type1;

        // Unknown can unify with anything - return the other type
        if (type1 is InferredType.UnknownType)
            return type2;
        if (type2 is InferredType.UnknownType)
            return type1;

        // Number is the polymorphic numeric type - more specific numeric types take precedence
        // Int is more specific than number
        if (type1 is InferredType.NumberType && type2 is InferredType.IntType)
            return type2;
        if (type1 is InferredType.IntType && type2 is InferredType.NumberType)
            return type1;

        // Float is more specific than number
        if (type1 is InferredType.NumberType && type2 is InferredType.FloatType)
            return type2;
        if (type1 is InferredType.FloatType && type2 is InferredType.NumberType)
            return type1;

        // If one is Int and other is Float, they don't unify (this is a type error in Elm)
        // Return the first one (arbitrary choice since this shouldn't happen in valid Elm code)
        if ((type1 is InferredType.IntType && type2 is InferredType.FloatType) ||
            (type1 is InferredType.FloatType && type2 is InferredType.IntType))
            return type1;

        // For list types, unify element types
        if (type1 is InferredType.ListType list1 && type2 is InferredType.ListType list2)
        {
            var unifiedElement = UnifyTypes(list1.ElementType, list2.ElementType);
            return new InferredType.ListType(unifiedElement);
        }

        // For tuple types, unify element types
        if (type1 is InferredType.TupleType tuple1 && type2 is InferredType.TupleType tuple2 &&
            tuple1.ElementTypes.Count == tuple2.ElementTypes.Count)
        {
            var unifiedElements = new List<InferredType>();
            for (var i = 0; i < tuple1.ElementTypes.Count; i++)
            {
                unifiedElements.Add(UnifyTypes(tuple1.ElementTypes[i], tuple2.ElementTypes[i]));
            }
            return new InferredType.TupleType(unifiedElements);
        }

        // For function types, unify argument and return types
        if (type1 is InferredType.FunctionType func1 && type2 is InferredType.FunctionType func2)
        {
            var unifiedArg = UnifyTypes(func1.ArgumentType, func2.ArgumentType);
            var unifiedRet = UnifyTypes(func1.ReturnType, func2.ReturnType);
            return new InferredType.FunctionType(unifiedArg, unifiedRet);
        }

        // For record types with same fields, unify field types (comparing by name, not position)
        if (type1 is InferredType.RecordType record1 && type2 is InferredType.RecordType record2 &&
            record1.Fields.Count == record2.Fields.Count)
        {
            // Create a lookup for record2 fields by name
            var record2FieldsByName = record2.Fields.ToDictionary(f => f.FieldName, f => f.FieldType);

            // Check if all fields from record1 exist in record2 and unify their types
            var unifiedFields = new List<(string FieldName, InferredType FieldType)>();
            foreach (var field1 in record1.Fields)
            {
                if (!record2FieldsByName.TryGetValue(field1.FieldName, out var field2Type))
                {
                    // Field name not found in record2 - records have different structure
                    // Return type1 as fallback since we can't unify incompatible record types
                    return type1;
                }
                unifiedFields.Add((field1.FieldName, UnifyTypes(field1.FieldType, field2Type)));
            }
            return new InferredType.RecordType(unifiedFields);
        }

        // Type variables can unify with any type - return the concrete type
        if (type1 is InferredType.TypeVariable)
            return type2;
        if (type2 is InferredType.TypeVariable)
            return type1;

        // For choice types with the same name, unify type arguments
        if (type1 is InferredType.ChoiceType choice1 && type2 is InferredType.ChoiceType choice2 &&
            choice1.TypeName == choice2.TypeName &&
            choice1.ModuleName.SequenceEqual(choice2.ModuleName) &&
            choice1.TypeArguments.Count == choice2.TypeArguments.Count)
        {
            var unifiedArgs = new List<InferredType>();
            for (var i = 0; i < choice1.TypeArguments.Count; i++)
            {
                unifiedArgs.Add(UnifyTypes(choice1.TypeArguments[i], choice2.TypeArguments[i]));
            }
            return new InferredType.ChoiceType(choice1.ModuleName, choice1.TypeName, unifiedArgs);
        }

        // Types don't unify - return the first one as a fallback
        // (in a full type system, this would typically be a type error)
        return type1;
    }

    /// <summary>
    /// Collects substitutions for type variables by matching an expected type against an actual type.
    /// For example, if expected is `TypeVariable("a")` and actual is `StringType`, adds `a -> String` to substitutions.
    /// </summary>
    private static void CollectTypeVariableSubstitutions(
        InferredType expected,
        InferredType actual,
        Dictionary<string, InferredType> substitutions)
    {
        if (expected is InferredType.TypeVariable typeVar)
        {
            // If we haven't seen this type variable yet, or if we can unify with the existing substitution
            if (!substitutions.TryGetValue(typeVar.Name, out var existingType))
            {
                substitutions[typeVar.Name] = actual;
            }
            else
            {
                // Unify with existing substitution
                substitutions[typeVar.Name] = UnifyTypes(existingType, actual);
            }
            return;
        }

        // Recursively match nested types
        switch ((expected, actual))
        {
            case (InferredType.ListType expList, InferredType.ListType actList):
                CollectTypeVariableSubstitutions(expList.ElementType, actList.ElementType, substitutions);
                break;

            case (InferredType.FunctionType expFunc, InferredType.FunctionType actFunc):
                CollectTypeVariableSubstitutions(expFunc.ArgumentType, actFunc.ArgumentType, substitutions);
                CollectTypeVariableSubstitutions(expFunc.ReturnType, actFunc.ReturnType, substitutions);
                break;

            case (InferredType.TupleType expTuple, InferredType.TupleType actTuple)
                when expTuple.ElementTypes.Count == actTuple.ElementTypes.Count:
                for (var i = 0; i < expTuple.ElementTypes.Count; i++)
                {
                    CollectTypeVariableSubstitutions(expTuple.ElementTypes[i], actTuple.ElementTypes[i], substitutions);
                }
                break;

            case (InferredType.ChoiceType expChoice, InferredType.ChoiceType actChoice)
                when expChoice.TypeName == actChoice.TypeName &&
                     expChoice.TypeArguments.Count == actChoice.TypeArguments.Count:
                for (var i = 0; i < expChoice.TypeArguments.Count; i++)
                {
                    CollectTypeVariableSubstitutions(expChoice.TypeArguments[i], actChoice.TypeArguments[i], substitutions);
                }
                break;
        }
    }

    /// <summary>
    /// Applies type variable substitutions to a type, replacing type variables with their concrete types.
    /// </summary>
    private static InferredType ApplyTypeSubstitutions(
        InferredType type,
        IReadOnlyDictionary<string, InferredType> substitutions)
    {
        if (substitutions.Count is 0)
            return type;

        return type switch
        {
            InferredType.TypeVariable typeVar when substitutions.TryGetValue(typeVar.Name, out var subst) =>
                subst,

            InferredType.ListType listType =>
                new InferredType.ListType(ApplyTypeSubstitutions(listType.ElementType, substitutions)),

            InferredType.FunctionType funcType =>
                new InferredType.FunctionType(
                    ApplyTypeSubstitutions(funcType.ArgumentType, substitutions),
                    ApplyTypeSubstitutions(funcType.ReturnType, substitutions)),

            InferredType.TupleType tupleType =>
                new InferredType.TupleType(
                    [.. tupleType.ElementTypes.Select(et => ApplyTypeSubstitutions(et, substitutions))]),

            InferredType.ChoiceType choiceType =>
                new InferredType.ChoiceType(
                    choiceType.ModuleName,
                    choiceType.TypeName,
                    [.. choiceType.TypeArguments.Select(ta => ApplyTypeSubstitutions(ta, substitutions))]),

            InferredType.RecordType recordType =>
                new InferredType.RecordType(
                    recordType.Fields.Select(f => (f.FieldName, ApplyTypeSubstitutions(f.FieldType, substitutions))).ToList()),

            _ => type
        };
    }

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

            // Handle List type specially
            if (name is "List" && moduleName.Count is 0 && typed.TypeArguments.Count is 1)
            {
                var elementType = TypeAnnotationToInferredType(typed.TypeArguments[0].Value);
                return new InferredType.ListType(elementType);
            }

            // Handle custom types (including those with type arguments)
            var typeArguments = typed.TypeArguments
                .Select(arg => TypeAnnotationToInferredType(arg.Value))
                .ToList();

            return new InferredType.ChoiceType([.. moduleName], name, typeArguments);
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

        // Handle record type annotations
        if (typeAnnotation is SyntaxTypes.TypeAnnotation.Record recordType)
        {
            // Extract field names and types, sorted alphabetically (as Elm records are stored)
            var fields =
                recordType.RecordDefinition.Fields
                .Select(f => (f.Value.FieldName.Value, TypeAnnotationToInferredType(f.Value.FieldType.Value)))
                .OrderBy(f => f.Value, StringComparer.Ordinal)
                .ToList();

            return new InferredType.RecordType(fields);
        }

        // Handle function type annotations
        if (typeAnnotation is SyntaxTypes.TypeAnnotation.FunctionTypeAnnotation funcType)
        {
            var argType =
                TypeAnnotationToInferredType(funcType.ArgumentType.Value);

            var returnType =
                TypeAnnotationToInferredType(funcType.ReturnType.Value);

            return new InferredType.FunctionType(argType, returnType);
        }

        // Handle generic type (type variable like 'a', 'b', etc.)
        if (typeAnnotation is SyntaxTypes.TypeAnnotation.GenericType genericType)
        {
            return new InferredType.TypeVariable(genericType.Name);
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

        // Function or constructor reference - look up type from function signatures
        // This handles cases like `ChoiceD` which should return `Bool -> Beta`
        if (expression is SyntaxTypes.Expression.FunctionOrValue funcOrValueRef &&
            functionReturnTypes is not null)
        {
            // Build the qualified name
            string qualifiedFuncName;
            if (funcOrValueRef.ModuleName.Count > 0)
            {
                qualifiedFuncName = string.Join(".", funcOrValueRef.ModuleName) + "." + funcOrValueRef.Name;
            }
            else if (currentModuleName is not null)
            {
                qualifiedFuncName = currentModuleName + "." + funcOrValueRef.Name;
            }
            else
            {
                qualifiedFuncName = funcOrValueRef.Name;
            }

            if (functionReturnTypes.TryGetValue(qualifiedFuncName, out var funcOrValueType))
            {
                return funcOrValueType;
            }
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
            application.Arguments.Count >= 1)
        {
            // Check if the first argument is a FunctionOrValue
            if (application.Arguments[0].Value is SyntaxTypes.Expression.FunctionOrValue funcRef)
            {
                // Check if this is a qualified Basics module function with known return type
                // Only use core library type info for qualified references (e.g., Basics.modBy)
                // Unqualified references could be shadowed by local parameters, so canonicalization
                // must resolve them first
                if (funcRef.ModuleName.Count is 1 && funcRef.ModuleName[0] is "Basics")
                {
                    if (CoreLibraryModule.CoreBasics.GetBasicsFunctionInfo(funcRef.Name) is { } basicsFuncInfo &&
                        basicsFuncInfo.FunctionType.Count > 0)
                    {
                        // FunctionType contains [arg1Type, arg2Type, ..., returnType]
                        // We need to compute the result type after applying the given arguments
                        var appliedArgCount = application.Arguments.Count - 1; // Exclude the function itself
                        var functionTypes = basicsFuncInfo.FunctionType;
                        var totalArgCount = functionTypes.Count - 1; // Last element is return type

                        if (appliedArgCount >= totalArgCount)
                        {
                            // Full application - return the final return type
                            return functionTypes[^1];
                        }
                        else
                        {
                            // Partial application - build a function type from remaining arguments
                            // For modBy 7: functionTypes = [Int, Int, Int], appliedArgCount = 1
                            // We need to return Int -> Int (functionTypes[1] -> functionTypes[2])
                            var remainingTypes = functionTypes.Skip(appliedArgCount).ToList();
                            return BuildFunctionTypeFromList(remainingTypes);
                        }
                    }
                }

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

                // Look up the function type
                // After canonicalization, all references should be fully qualified
                if (functionReturnTypes is not null && functionReturnTypes.TryGetValue(qualifiedName, out var funcType))
                {
                    // Build a substitution map from type variables to concrete types
                    var substitution = new Dictionary<string, InferredType>();

                    // Compute the return type by "applying" arguments to the function type
                    // For a function type A -> B -> C with 2 arguments, the return type is C
                    // Also infer type variable substitutions from argument types
                    var appliedArgCount = application.Arguments.Count - 1; // Exclude the function itself
                    var resultType = funcType;

                    for (var i = 0; i < appliedArgCount && resultType is InferredType.FunctionType ft; i++)
                    {
                        // Infer the actual argument type
                        var argExpr = application.Arguments[i + 1].Value; // +1 to skip the function itself
                        var actualArgType = InferExpressionType(argExpr, parameterNames, parameterTypes, localBindingTypes, currentModuleName, functionReturnTypes);

                        // Collect type variable substitutions from matching expected vs actual arg types
                        CollectTypeVariableSubstitutions(ft.ArgumentType, actualArgType, substitution);

                        resultType = ft.ReturnType;
                    }

                    // Apply substitutions to the result type
                    return ApplyTypeSubstitutions(resultType, substitution);
                }
            }
            // Check if the first argument is a PrefixOperator (e.g., (+), (-), (*), (//))
            else if (application.Arguments[0].Value is SyntaxTypes.Expression.PrefixOperator prefixOp)
            {
                // Map the operator to a function name and get its type info
                if (CoreLibraryModule.CoreBasics.OperatorToFunctionName(prefixOp.Operator) is { } funcName)
                {
                    if (CoreLibraryModule.CoreBasics.GetBasicsFunctionInfo(funcName) is { } basicsFuncInfo &&
                        basicsFuncInfo.FunctionType.Count > 0)
                    {
                        // FunctionType contains [arg1Type, arg2Type, ..., returnType]
                        var appliedArgCount = application.Arguments.Count - 1; // Exclude the operator itself
                        var functionTypes = basicsFuncInfo.FunctionType;
                        var totalArgCount = functionTypes.Count - 1;

                        if (appliedArgCount >= totalArgCount)
                        {
                            // Full application - return the final return type
                            return functionTypes[^1];
                        }
                        else
                        {
                            // Partial application - build a function type from remaining arguments
                            var remainingTypes = functionTypes.Skip(appliedArgCount).ToList();
                            return BuildFunctionTypeFromList(remainingTypes);
                        }
                    }
                }
            }
        }

        // Record expression - infer type from field names and values
        if (expression is SyntaxTypes.Expression.RecordExpr recordExpr)
        {
            var fields = new List<(string FieldName, InferredType FieldType)>();
            foreach (var field in recordExpr.Fields)
            {
                var fieldName = field.Value.fieldName.Value;
                var fieldType = InferExpressionType(field.Value.valueExpr.Value, parameterNames, parameterTypes, localBindingTypes, currentModuleName, functionReturnTypes);
                fields.Add((fieldName, fieldType));
            }
            // Sort fields alphabetically by field name
            fields.Sort((a, b) => string.Compare(a.FieldName, b.FieldName, StringComparison.Ordinal));
            return new InferredType.RecordType(fields);
        }

        // Let expression - infer types for local bindings and then the body
        if (expression is SyntaxTypes.Expression.LetExpression letExpr)
        {
            // Build local binding types from the let declarations
            var extendedLocalBindings =
                localBindingTypes?.ToImmutableDictionary() ?? [];

            foreach (var decl in letExpr.Value.Declarations)
            {
                if (decl.Value is SyntaxTypes.Expression.LetDeclaration.LetFunction letFunc)
                {
                    var funcName = letFunc.Function.Declaration.Value.Name.Value;

                    var funcType = InferExpressionType(
                        letFunc.Function.Declaration.Value.Expression.Value,
                        parameterNames,
                        parameterTypes,
                        extendedLocalBindings,
                        currentModuleName,
                        functionReturnTypes);
                    extendedLocalBindings = extendedLocalBindings.SetItem(funcName, funcType);
                }
                else if (decl.Value is SyntaxTypes.Expression.LetDeclaration.LetDestructuring letDestr)
                {
                    // For destructuring, we would need pattern matching support
                    // For now, just recurse into the expression
                    var exprType = InferExpressionType(
                        letDestr.Expression.Value,
                        parameterNames,
                        parameterTypes,
                        extendedLocalBindings,
                        currentModuleName,
                        functionReturnTypes);

                    // Try to extract binding names from the pattern
                    foreach (var binding in ExtractBindingsFromPattern(letDestr.Pattern.Value))
                    {
                        extendedLocalBindings = extendedLocalBindings.SetItem(binding, exprType);
                    }
                }
            }

            // The type of the let expression is the type of its body
            return InferExpressionType(
                letExpr.Value.Expression.Value,
                parameterNames,
                parameterTypes,
                extendedLocalBindings,
                currentModuleName,
                functionReturnTypes);
        }

        // List expression - infer type from element types
        if (expression is SyntaxTypes.Expression.ListExpr listExpr)
        {
            if (listExpr.Elements.Count is 0)
            {
                // Empty list - cannot determine element type
                return s_unknownType;
            }

            // Infer type from first element
            var firstElementType = InferExpressionType(
                listExpr.Elements[0].Value,
                parameterNames,
                parameterTypes,
                localBindingTypes,
                currentModuleName,
                functionReturnTypes);

            return new InferredType.ListType(firstElementType);
        }

        // If-block expression - unify types from all branches
        if (expression is SyntaxTypes.Expression.IfBlock ifBlock)
        {
            var thenType = InferExpressionType(
                ifBlock.ThenBlock.Value,
                parameterNames,
                parameterTypes,
                localBindingTypes,
                currentModuleName,
                functionReturnTypes);

            var elseType = InferExpressionType(
                ifBlock.ElseBlock.Value,
                parameterNames,
                parameterTypes,
                localBindingTypes,
                currentModuleName,
                functionReturnTypes);

            // Unify types: If one branch is more specific (e.g., Int vs number), use the more specific type
            return UnifyTypes(thenType, elseType);
        }

        // Case expression - unify types from all case arm expressions
        if (expression is SyntaxTypes.Expression.CaseExpression caseExpr)
        {
            if (caseExpr.CaseBlock.Cases.Count is 0)
            {
                return s_unknownType;
            }

            // Start with the type of the first case arm
            var unifiedType = InferExpressionType(
                caseExpr.CaseBlock.Cases[0].Expression.Value,
                parameterNames,
                parameterTypes,
                localBindingTypes,
                currentModuleName,
                functionReturnTypes);

            // Unify with remaining case arm types
            for (var i = 1; i < caseExpr.CaseBlock.Cases.Count; i++)
            {
                var armType = InferExpressionType(
                    caseExpr.CaseBlock.Cases[i].Expression.Value,
                    parameterNames,
                    parameterTypes,
                    localBindingTypes,
                    currentModuleName,
                    functionReturnTypes);

                unifiedType = UnifyTypes(unifiedType, armType);
            }

            return unifiedType;
        }

        // For other expressions, we cannot infer the type yet
        return s_unknownType;
    }

    /// <summary>
    /// Extracts binding names from a pattern.
    /// </summary>
    private static IEnumerable<string> ExtractBindingsFromPattern(SyntaxTypes.Pattern pattern)
    {
        switch (pattern)
        {
            case SyntaxTypes.Pattern.VarPattern var:
                yield return var.Name;
                break;

            case SyntaxTypes.Pattern.TuplePattern tuple:
                foreach (var elem in tuple.Elements)
                {
                    foreach (var binding in ExtractBindingsFromPattern(elem.Value))
                    {
                        yield return binding;
                    }
                }
                break;

            case SyntaxTypes.Pattern.RecordPattern record:
                foreach (var field in record.Fields)
                {
                    yield return field.Value;
                }
                break;

            case SyntaxTypes.Pattern.AsPattern asPattern:
                yield return asPattern.Name.Value;
                foreach (var binding in ExtractBindingsFromPattern(asPattern.Pattern.Value))
                {
                    yield return binding;
                }
                break;

            case SyntaxTypes.Pattern.ParenthesizedPattern paren:
                foreach (var binding in ExtractBindingsFromPattern(paren.Pattern.Value))
                {
                    yield return binding;
                }
                break;
        }
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

                    // First, check if this is a Basics module function with known types
                    IReadOnlyList<InferredType>? paramTypes = null;

                    if (funcRef.ModuleName.Count is 1 && funcRef.ModuleName[0] is "Basics")
                    {
                        if (CoreLibraryModule.CoreBasics.GetBasicsFunctionInfo(funcRef.Name) is { } basicsFuncInfo)
                        {
                            // The FunctionType contains [arg1Type, arg2Type, ..., returnType]
                            // We need just the parameter types (excluding return type)
                            paramTypes =
                                [.. basicsFuncInfo.FunctionType.Take(basicsFuncInfo.FunctionType.Count - 1)];
                        }
                    }

                    // If not a known Basics function, check the user-defined function parameter types
                    if (paramTypes is null)
                    {
                        functionParameterTypes.TryGetValue(qualifiedFuncName, out paramTypes);
                    }

                    if (paramTypes is not null)
                    {
                        // Match arguments to function parameter types
                        // Arguments start at index 1 (index 0 is the function itself)
                        for (var i = 1; i < application.Arguments.Count && i - 1 < paramTypes.Count; i++)
                        {
                            var argIndex = i - 1;
                            var argExpr = application.Arguments[i].Value;
                            var paramType = paramTypes[argIndex];

                            // Extract type constraints from the argument expression
                            // This handles both simple variable references and operator expressions
                            if (paramType is not InferredType.UnknownType)
                            {
                                constraints = ExtractTypeConstraintsFromExpression(argExpr, paramType, constraints);
                            }
                        }
                    }
                }
                // Also handle PrefixOperator applications (e.g., (//) a b)
                else if (application.Arguments.Count >= 2 &&
                    application.Arguments[0].Value is SyntaxTypes.Expression.PrefixOperator prefixOp)
                {
                    // Map the operator to a function name and get its type info
                    if (CoreLibraryModule.CoreBasics.OperatorToFunctionName(prefixOp.Operator) is { } funcName)
                    {
                        if (CoreLibraryModule.CoreBasics.GetBasicsFunctionInfo(funcName) is { } basicsFuncInfo)
                        {
                            // The FunctionType contains [arg1Type, arg2Type, ..., returnType]
                            // We need just the parameter types (excluding return type)
                            var paramTypes =
                                basicsFuncInfo.FunctionType
                                .Take(basicsFuncInfo.FunctionType.Count - 1)
                                .ToList();

                            // Match arguments to function parameter types
                            // Arguments start at index 1 (index 0 is the operator itself)
                            for (var i = 1; i < application.Arguments.Count && i - 1 < paramTypes.Count; i++)
                            {
                                var argIndex = i - 1;
                                var argExpr = application.Arguments[i].Value;
                                var paramType = paramTypes[argIndex];

                                // Extract type constraints from the argument expression
                                if (paramType is not InferredType.UnknownType)
                                {
                                    constraints = ExtractTypeConstraintsFromExpression(argExpr, paramType, constraints);
                                }
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
    /// Extracts type constraints from an expression given a target type.
    /// For example, if the expression is `x + 7` and the target type is Int,
    /// we infer that `x` must be Int (since Int + number = Int).
    /// This recursively processes operator expressions, parenthesized expressions, etc.
    /// </summary>
    private static ImmutableDictionary<string, InferredType> ExtractTypeConstraintsFromExpression(
        SyntaxTypes.Expression expression,
        InferredType targetType,
        ImmutableDictionary<string, InferredType> constraints)
    {
        switch (expression)
        {
            case SyntaxTypes.Expression.FunctionOrValue varRef:
                // Simple variable reference - constrain its type if not already constrained
                if (varRef.ModuleName.Count is 0 &&
                    !ElmValueEncoding.StringIsValidTagName(varRef.Name) &&
                    !constraints.ContainsKey(varRef.Name))
                {
                    constraints = constraints.SetItem(varRef.Name, targetType);
                }
                return constraints;

            case SyntaxTypes.Expression.OperatorApplication opApp:
                // For arithmetic operators where result type is known, propagate to operands
                if (targetType is InferredType.IntType)
                {
                    // When the result is Int, operands must also be Int (for arithmetic operators)
                    // Integer division (//) and modulo operators also force Int
                    if (opApp.Operator is "+" or "-" or "*" or "//" or "%" or "^")
                    {
                        constraints = ExtractTypeConstraintsFromExpression(opApp.Left.Value, targetType, constraints);
                        constraints = ExtractTypeConstraintsFromExpression(opApp.Right.Value, targetType, constraints);
                    }
                }
                else if (targetType is InferredType.FloatType)
                {
                    // When the result is Float, operands must also be Float (for float division)
                    if (opApp.Operator is "/")
                    {
                        constraints = ExtractTypeConstraintsFromExpression(opApp.Left.Value, targetType, constraints);
                        constraints = ExtractTypeConstraintsFromExpression(opApp.Right.Value, targetType, constraints);
                    }
                }
                return constraints;

            case SyntaxTypes.Expression.ParenthesizedExpression parenExpr:
                // Propagate through parentheses
                return ExtractTypeConstraintsFromExpression(parenExpr.Expression.Value, targetType, constraints);

            case SyntaxTypes.Expression.Negation negation:
                // Negation preserves numeric type
                return ExtractTypeConstraintsFromExpression(negation.Expression.Value, targetType, constraints);

            default:
                // Other expression types - no additional constraints to extract
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

    /// <summary>
    /// Builds a map of function signatures from a file for type inference.
    /// Includes function declarations, choice type constructors, and type alias record constructors.
    /// </summary>
    /// <param name="file">The parsed Elm file.</param>
    /// <param name="moduleName">The module name for qualifying function names.</param>
    /// <returns>A dictionary mapping qualified function names to their inferred types.</returns>
    public static ImmutableDictionary<string, InferredType> BuildFunctionSignaturesMap(
        SyntaxTypes.File file,
        string moduleName)
    {
        var signatures = ImmutableDictionary<string, InferredType>.Empty;

        foreach (var declaration in file.Declarations)
        {
            if (declaration.Value is SyntaxTypes.Declaration.FunctionDeclaration funcDecl)
            {
                var funcName = funcDecl.Function.Declaration.Value.Name.Value;
                var qualifiedName = moduleName + "." + funcName;

                if (funcDecl.Function.Signature?.Value is { } signature)
                {
                    var inferredType = TypeAnnotationToInferredType(signature.TypeAnnotation.Value);
                    signatures = signatures.Add(qualifiedName, inferredType);
                }
            }
            else if (declaration.Value is SyntaxTypes.Declaration.CustomTypeDeclaration choiceTypeDecl)
            {
                var typeName = choiceTypeDecl.TypeDeclaration.Name.Value;

                // Get the type parameters (generics) for this type
                var typeParams = choiceTypeDecl.TypeDeclaration.Generics
                    .Select(g => g.Value)
                    .ToList();

                // Build constructor types for each value constructor
                foreach (var constructorNode in choiceTypeDecl.TypeDeclaration.Constructors)
                {
                    var constructorName = constructorNode.Value.Name.Value;
                    var qualifiedConstructorName = moduleName + "." + constructorName;

                    // The constructor result type is the choice type with type variables as arguments
                    // e.g., for `type ChoiceType a b = ...`, the result type is `ChoiceType a b`
                    var typeArguments = typeParams
                        .Select(p => (InferredType)new InferredType.TypeVariable(p))
                        .ToList();

                    InferredType resultType = new InferredType.ChoiceType(
                        ModuleName: [],
                        TypeName: typeName,
                        TypeArguments: typeArguments);

                    // Build the constructor type: arg1 -> arg2 -> ... -> ResultType
                    var constructorType = resultType;

                    // Process arguments in reverse to build the function type correctly
                    for (var i = constructorNode.Value.Arguments.Count - 1; i >= 0; i--)
                    {
                        var argTypeAnnotation = constructorNode.Value.Arguments[i].Value;
                        var argType = TypeAnnotationToInferredType(argTypeAnnotation);
                        constructorType = new InferredType.FunctionType(argType, constructorType);
                    }

                    signatures = signatures.Add(qualifiedConstructorName, constructorType);
                }
            }
            else if (declaration.Value is SyntaxTypes.Declaration.AliasDeclaration aliasDecl)
            {
                // Type alias record constructors - if the type alias is for a record type,
                // it creates an implicit constructor: FieldType1 -> FieldType2 -> ... -> { field1: T1, field2: T2, ... }
                var aliasName = aliasDecl.TypeAlias.Name.Value;
                var qualifiedAliasName = moduleName + "." + aliasName;

                // Check if the type annotation is a record type
                if (aliasDecl.TypeAlias.TypeAnnotation.Value is SyntaxTypes.TypeAnnotation.Record recordType &&
                    recordType.RecordDefinition.Fields.Count > 0)
                {
                    // Convert the full record type annotation to InferredType for the result type
                    var resultType = TypeAnnotationToInferredType(aliasDecl.TypeAlias.TypeAnnotation.Value);

                    // Collect all fields in order
                    var allFields = recordType.RecordDefinition.Fields;

                    // Build the constructor type: field1Type -> field2Type -> ... -> RecordType
                    // Fields must be in declaration order
                    var constructorType = resultType;

                    // Process fields in reverse to build the function type correctly
                    for (var i = allFields.Count - 1; i >= 0; i--)
                    {
                        var field = allFields[i];
                        var fieldTypeAnnotation = field.Value.FieldType.Value;
                        var fieldType = TypeAnnotationToInferredType(fieldTypeAnnotation);
                        constructorType = new InferredType.FunctionType(fieldType, constructorType);
                    }

                    signatures = signatures.Add(qualifiedAliasName, constructorType);
                }
            }
        }

        return signatures;
    }

    /// <summary>
    /// Infers the type of a function declaration, including parameter types inferred from usage context.
    /// </summary>
    /// <param name="expression">The function body expression.</param>
    /// <param name="arguments">The function arguments (patterns).</param>
    /// <param name="moduleName">The current module name.</param>
    /// <param name="functionSignatures">Map of known function signatures.</param>
    /// <returns>A tuple containing the return type and inferred parameter types.</returns>
    public static (InferredType returnType, ImmutableDictionary<string, InferredType> parameterTypes) InferFunctionDeclarationType(
        SyntaxTypes.Expression expression,
        IReadOnlyList<SyntaxTypes.Node<SyntaxTypes.Pattern>> arguments,
        string moduleName,
        IReadOnlyDictionary<string, InferredType> functionSignatures)
    {
        // Build parameter names dictionary
        var parameterNames = new Dictionary<string, int>();
        for (var i = 0; i < arguments.Count; i++)
        {
            if (arguments[i].Value is SyntaxTypes.Pattern.VarPattern varPattern)
            {
                parameterNames[varPattern.Name] = i;
            }
        }

        // Infer parameter types from usage context (function applications)
        var parameterTypes = InferParameterTypesFromUsage(
            expression,
            parameterNames,
            functionSignatures);

        var localBindingTypes = ImmutableDictionary<string, InferredType>.Empty;

        var returnType = InferExpressionType(
            expression,
            parameterNames,
            parameterTypes,
            localBindingTypes,
            moduleName,
            functionSignatures);

        return (returnType, parameterTypes);
    }

    /// <summary>
    /// Builds a full function type from parameter types and return type.
    /// For a function with parameters (a, b) and return type T, creates: paramTypeA -> paramTypeB -> T
    /// </summary>
    /// <param name="arguments">The function arguments (patterns).</param>
    /// <param name="parameterTypes">Map of parameter names to their inferred types.</param>
    /// <param name="returnType">The return type of the function.</param>
    /// <returns>The complete function type.</returns>
    public static InferredType BuildFunctionType(
        IReadOnlyList<SyntaxTypes.Node<SyntaxTypes.Pattern>> arguments,
        ImmutableDictionary<string, InferredType> parameterTypes,
        InferredType returnType)
    {
        if (arguments.Count is 0)
        {
            return returnType;
        }

        var result = returnType;

        // Build from right to left: paramN -> (paramN-1 -> (... -> returnType))
        for (var i = arguments.Count - 1; i >= 0; i--)
        {
            InferredType paramType;

            if (arguments[i].Value is SyntaxTypes.Pattern.VarPattern varPattern &&
                parameterTypes.TryGetValue(varPattern.Name, out var inferredParamType))
            {
                paramType = inferredParamType;
            }
            else
            {
                // Default to number type for parameters in numeric functions
                paramType = s_numberType;
            }

            result = new InferredType.FunctionType(paramType, result);
        }

        return result;
    }

    /// <summary>
    /// Builds a function type from a list of types where the last element is the return type.
    /// For example, [Int, Int, Int] becomes Int -> Int -> Int (which is Int -> (Int -> Int)).
    /// </summary>
    /// <param name="types">The types in the function signature where the last element is the return type.</param>
    /// <returns>The function type, or the single type if the list has only one element.</returns>
    private static InferredType BuildFunctionTypeFromList(IReadOnlyList<InferredType> types)
    {
        if (types.Count is 0)
        {
            return s_unknownType;
        }

        if (types.Count is 1)
        {
            return types[0];
        }

        // Build from right to left: t1 -> (t2 -> (... -> tN))
        var result = types[^1];
        for (var i = types.Count - 2; i >= 0; i--)
        {
            result = new InferredType.FunctionType(types[i], result);
        }

        return result;
    }

    /// <summary>
    /// Infers parameter types from how they are used in function applications.
    /// </summary>
    /// <param name="expression">The expression to analyze.</param>
    /// <param name="parameterNames">Map of parameter names to their indices.</param>
    /// <param name="functionSignatures">Map of known function signatures.</param>
    /// <returns>A dictionary mapping parameter names to their inferred types.</returns>
    public static ImmutableDictionary<string, InferredType> InferParameterTypesFromUsage(
        SyntaxTypes.Expression expression,
        IReadOnlyDictionary<string, int> parameterNames,
        IReadOnlyDictionary<string, InferredType> functionSignatures)
    {
        var parameterTypes = ImmutableDictionary<string, InferredType>.Empty;

        // Recursively scan expressions to find function applications
        parameterTypes = ScanExpressionForParameterTypes(expression, parameterNames, functionSignatures, parameterTypes);

        return parameterTypes;
    }

    /// <summary>
    /// Scans an expression recursively to infer parameter types from function applications.
    /// </summary>
    private static ImmutableDictionary<string, InferredType> ScanExpressionForParameterTypes(
        SyntaxTypes.Expression expression,
        IReadOnlyDictionary<string, int> parameterNames,
        IReadOnlyDictionary<string, InferredType> functionSignatures,
        ImmutableDictionary<string, InferredType> parameterTypes) =>
        ScanExpressionForParameterTypes(
            expression,
            parameterNames,
            functionSignatures,
            parameterTypes,
            localBindingDefinitions: []);

    /// <summary>
    /// Scans an expression recursively to infer parameter types from function applications,
    /// with support for tracking local binding definitions for constraint propagation.
    /// </summary>
    private static ImmutableDictionary<string, InferredType> ScanExpressionForParameterTypes(
        SyntaxTypes.Expression expression,
        IReadOnlyDictionary<string, int> parameterNames,
        IReadOnlyDictionary<string, InferredType> functionSignatures,
        ImmutableDictionary<string, InferredType> parameterTypes,
        ImmutableDictionary<string, SyntaxTypes.Expression> localBindingDefinitions)
    {
        switch (expression)
        {
            case SyntaxTypes.Expression.Application app when app.Arguments.Count >= 2:
                // Check if first argument is a function reference with known signature
                if (app.Arguments[0].Value is SyntaxTypes.Expression.FunctionOrValue funcRef)
                {
                    var qualifiedName =
                        funcRef.ModuleName.Count > 0
                        ? string.Join(".", funcRef.ModuleName) + "." + funcRef.Name
                        : funcRef.Name;

                    // Try both qualified and unqualified names
                    InferredType? funcType = null;
                    if (functionSignatures.TryGetValue(qualifiedName, out var ft))
                    {
                        funcType = ft;
                    }
                    else
                    {
                        // Try to find by scanning all keys
                        foreach (var kvp in functionSignatures)
                        {
                            if (kvp.Key.EndsWith("." + funcRef.Name))
                            {
                                funcType = kvp.Value;
                                break;
                            }
                        }
                    }

                    if (funcType is not null)
                    {
                        // Extract parameter types from function type
                        var argTypes = ExtractArgumentTypesFromFunctionType(funcType);

                        // Match application arguments with function parameter types
                        for (var i = 1; i < app.Arguments.Count && i - 1 < argTypes.Count; i++)
                        {
                            if (app.Arguments[i].Value is SyntaxTypes.Expression.FunctionOrValue argRef &&
                                argRef.ModuleName.Count is 0 &&
                                parameterNames.ContainsKey(argRef.Name) &&
                                !parameterTypes.ContainsKey(argRef.Name))
                            {
                                parameterTypes = parameterTypes.Add(argRef.Name, argTypes[i - 1]);
                            }
                        }
                    }
                }

                // Recurse into application arguments
                foreach (var arg in app.Arguments)
                {
                    parameterTypes = ScanExpressionForParameterTypes(arg.Value, parameterNames, functionSignatures, parameterTypes, localBindingDefinitions);
                }
                break;

            case SyntaxTypes.Expression.LetExpression letExpr:
                // Build a map of local binding definitions for constraint propagation
                var extendedLocalBindings = localBindingDefinitions;
                foreach (var decl in letExpr.Value.Declarations)
                {
                    if (decl.Value is SyntaxTypes.Expression.LetDeclaration.LetFunction letFunc)
                    {
                        var bindingName = letFunc.Function.Declaration.Value.Name.Value;
                        var bindingExpr = letFunc.Function.Declaration.Value.Expression.Value;
                        extendedLocalBindings = extendedLocalBindings.SetItem(bindingName, bindingExpr);
                        parameterTypes = ScanExpressionForParameterTypes(bindingExpr, parameterNames, functionSignatures, parameterTypes, extendedLocalBindings);
                    }
                    else if (decl.Value is SyntaxTypes.Expression.LetDeclaration.LetDestructuring letDestr)
                    {
                        parameterTypes = ScanExpressionForParameterTypes(letDestr.Expression.Value, parameterNames, functionSignatures, parameterTypes, extendedLocalBindings);
                    }
                }
                parameterTypes = ScanExpressionForParameterTypes(letExpr.Value.Expression.Value, parameterNames, functionSignatures, parameterTypes, extendedLocalBindings);
                break;

            case SyntaxTypes.Expression.IfBlock ifBlock:
                parameterTypes = ScanExpressionForParameterTypes(ifBlock.Condition.Value, parameterNames, functionSignatures, parameterTypes, localBindingDefinitions);
                parameterTypes = ScanExpressionForParameterTypes(ifBlock.ThenBlock.Value, parameterNames, functionSignatures, parameterTypes, localBindingDefinitions);
                parameterTypes = ScanExpressionForParameterTypes(ifBlock.ElseBlock.Value, parameterNames, functionSignatures, parameterTypes, localBindingDefinitions);
                break;

            case SyntaxTypes.Expression.CaseExpression caseExpr:
                parameterTypes = ScanExpressionForParameterTypes(caseExpr.CaseBlock.Expression.Value, parameterNames, functionSignatures, parameterTypes, localBindingDefinitions);
                foreach (var caseCase in caseExpr.CaseBlock.Cases)
                {
                    parameterTypes = ScanExpressionForParameterTypes(caseCase.Expression.Value, parameterNames, functionSignatures, parameterTypes, localBindingDefinitions);
                }
                break;

            case SyntaxTypes.Expression.OperatorApplication opApp:
                // Check if the operator forces specific operand types
                if (s_operatorConstraints.TryGetValue(opApp.Operator, out var constraints))
                {
                    // If the operator forces left operand type, constrain any parameter variables
                    if (constraints.LeftOperandType is not null)
                    {
                        parameterTypes = ExtractTypeConstraintFromExpression(
                            opApp.Left.Value,
                            constraints.LeftOperandType,
                            parameterNames,
                            parameterTypes,
                            localBindingDefinitions);
                    }

                    // If the operator forces right operand type, constrain any parameter variables
                    if (constraints.RightOperandType is not null)
                    {
                        parameterTypes = ExtractTypeConstraintFromExpression(
                            opApp.Right.Value,
                            constraints.RightOperandType,
                            parameterNames,
                            parameterTypes,
                            localBindingDefinitions);
                    }
                }

                // For arithmetic operators, check if one operand is a Float literal
                // If so, propagate Float constraint to the other operand
                if (opApp.Operator is "+" or "-" or "*" or "^" or "%")
                {
                    // Check if left operand is a Float literal
                    if (opApp.Left.Value is SyntaxTypes.Expression.Floatable)
                    {
                        parameterTypes = ExtractTypeConstraintFromExpression(
                            opApp.Right.Value,
                            s_floatType,
                            parameterNames,
                            parameterTypes,
                            localBindingDefinitions);
                    }

                    // Check if right operand is a Float literal
                    if (opApp.Right.Value is SyntaxTypes.Expression.Floatable)
                    {
                        parameterTypes = ExtractTypeConstraintFromExpression(
                            opApp.Left.Value,
                            s_floatType,
                            parameterNames,
                            parameterTypes,
                            localBindingDefinitions);
                    }
                }

                parameterTypes = ScanExpressionForParameterTypes(opApp.Left.Value, parameterNames, functionSignatures, parameterTypes, localBindingDefinitions);
                parameterTypes = ScanExpressionForParameterTypes(opApp.Right.Value, parameterNames, functionSignatures, parameterTypes, localBindingDefinitions);
                break;

            case SyntaxTypes.Expression.ParenthesizedExpression paren:
                parameterTypes = ScanExpressionForParameterTypes(paren.Expression.Value, parameterNames, functionSignatures, parameterTypes, localBindingDefinitions);
                break;

            case SyntaxTypes.Expression.LambdaExpression lambda:
                parameterTypes = ScanExpressionForParameterTypes(lambda.Lambda.Expression.Value, parameterNames, functionSignatures, parameterTypes, localBindingDefinitions);
                break;

            case SyntaxTypes.Expression.TupledExpression tuple:
                foreach (var elem in tuple.Elements)
                {
                    parameterTypes = ScanExpressionForParameterTypes(elem.Value, parameterNames, functionSignatures, parameterTypes, localBindingDefinitions);
                }
                break;

            case SyntaxTypes.Expression.ListExpr list:
                foreach (var elem in list.Elements)
                {
                    parameterTypes = ScanExpressionForParameterTypes(elem.Value, parameterNames, functionSignatures, parameterTypes, localBindingDefinitions);
                }
                break;

            case SyntaxTypes.Expression.RecordExpr record:
                foreach (var field in record.Fields)
                {
                    parameterTypes = ScanExpressionForParameterTypes(field.Value.valueExpr.Value, parameterNames, functionSignatures, parameterTypes, localBindingDefinitions);
                }
                break;
        }

        return parameterTypes;
    }

    /// <summary>
    /// Extracts type constraints from an expression for parameter variables.
    /// When an expression like 'a + b' is known to be Int (due to operator constraints),
    /// this method extracts constraints for parameter variables like 'a' and 'b'.
    /// </summary>
    private static ImmutableDictionary<string, InferredType> ExtractTypeConstraintFromExpression(
        SyntaxTypes.Expression expression,
        InferredType targetType,
        IReadOnlyDictionary<string, int> parameterNames,
        ImmutableDictionary<string, InferredType> parameterTypes) =>
        ExtractTypeConstraintFromExpression(
            expression,
            targetType,
            parameterNames,
            parameterTypes,
            localBindingDefinitions: []);

    /// <summary>
    /// Extracts type constraints from an expression for parameter variables,
    /// with support for tracking local binding definitions for constraint propagation.
    /// When an expression like 'a + b' is known to be Int (due to operator constraints),
    /// this method extracts constraints for parameter variables like 'a' and 'b'.
    /// </summary>
    private static ImmutableDictionary<string, InferredType> ExtractTypeConstraintFromExpression(
        SyntaxTypes.Expression expression,
        InferredType targetType,
        IReadOnlyDictionary<string, int> parameterNames,
        ImmutableDictionary<string, InferredType> parameterTypes,
        ImmutableDictionary<string, SyntaxTypes.Expression> localBindingDefinitions)
    {
        switch (expression)
        {
            case SyntaxTypes.Expression.FunctionOrValue varRef:
                // If this is a parameter variable, constrain its type
                if (varRef.ModuleName.Count is 0 &&
                    parameterNames.ContainsKey(varRef.Name) &&
                    !parameterTypes.ContainsKey(varRef.Name))
                {
                    parameterTypes = parameterTypes.Add(varRef.Name, targetType);
                }
                else if (varRef.ModuleName.Count is 0 &&
                    parameterNames.ContainsKey(varRef.Name) &&
                    parameterTypes.TryGetValue(varRef.Name, out var existingType))
                {
                    // Unify with existing constraint if present
                    var unifiedType = UnifyTypes(existingType, targetType);
                    parameterTypes = parameterTypes.SetItem(varRef.Name, unifiedType);
                }
                // If this is a local binding, look up its definition and propagate the constraint
                else if (varRef.ModuleName.Count is 0 &&
                    localBindingDefinitions.TryGetValue(varRef.Name, out var bindingExpr))
                {
                    parameterTypes = ExtractTypeConstraintFromExpression(
                        bindingExpr, targetType, parameterNames, parameterTypes, localBindingDefinitions);
                }
                return parameterTypes;

            case SyntaxTypes.Expression.ParenthesizedExpression paren:
                return ExtractTypeConstraintFromExpression(paren.Expression.Value, targetType, parameterNames, parameterTypes, localBindingDefinitions);

            case SyntaxTypes.Expression.OperatorApplication nestedOpApp:
                // For arithmetic operators that preserve numeric type, propagate constraints to operands
                if (nestedOpApp.Operator is "+" or "-" or "*" or "/" or "//" or "%" or "^")
                {
                    parameterTypes = ExtractTypeConstraintFromExpression(nestedOpApp.Left.Value, targetType, parameterNames, parameterTypes, localBindingDefinitions);
                    parameterTypes = ExtractTypeConstraintFromExpression(nestedOpApp.Right.Value, targetType, parameterNames, parameterTypes, localBindingDefinitions);
                }
                return parameterTypes;

            default:
                return parameterTypes;
        }
    }

    /// <summary>
    /// Extracts argument types from a function type.
    /// </summary>
    /// <param name="funcType">The function type to extract from.</param>
    /// <returns>A list of argument types in order.</returns>
    public static IReadOnlyList<InferredType> ExtractArgumentTypesFromFunctionType(InferredType funcType)
    {
        var argTypes = new List<InferredType>();
        var current = funcType;

        while (current is InferredType.FunctionType ft)
        {
            argTypes.Add(ft.ArgumentType);
            current = ft.ReturnType;
        }

        return argTypes;
    }
}
