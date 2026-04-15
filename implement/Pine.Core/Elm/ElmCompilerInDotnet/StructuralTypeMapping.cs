using Pine.Core.Elm.ElmSyntax.SyntaxModel;
using System.Collections.Generic;
using System.Collections.Immutable;

namespace Pine.Core.Elm.ElmCompilerInDotnet;

/// <summary>
/// Maps from the <see cref="TypeInference.InferredType"/> model used in earlier
/// compilation stages to the <see cref="StructuralType"/> model used in later stages.
/// <para>
/// The key difference between the two models is that <see cref="TypeInference.InferredType"/>
/// references choice (union) types by name (e.g., <c>Maybe a</c>), while
/// <see cref="StructuralType"/> identifies them structurally by their tag set.
/// A <see cref="TypeInference.ChoiceTypeDefinition"/> lookup table is therefore required
/// to expand named choice types into their structural representation.
/// </para>
/// </summary>
public static class StructuralTypeMapping
{
    /// <summary>
    /// Maps an <see cref="TypeInference.InferredType"/> to the corresponding
    /// <see cref="StructuralType"/>.
    /// </summary>
    /// <param name="inferredType">The inferred type to map.</param>
    /// <param name="choiceTypeDefinitions">
    /// Lookup table for resolving named choice types to their constructor definitions.
    /// Built by <see cref="TypeInference.BuildChoiceTypeDefinitions"/>.
    /// </param>
    /// <returns>
    /// The structural type, or <c>null</c> if the type cannot be mapped
    /// (e.g., <see cref="TypeInference.InferredType.UnknownType"/>).
    /// </returns>
    public static StructuralType? MapToStructuralType(
        TypeInference.InferredType inferredType,
        IReadOnlyDictionary<QualifiedNameRef, TypeInference.ChoiceTypeDefinition> choiceTypeDefinitions)
    {
        return
            MapToStructuralType(
                inferredType,
                choiceTypeDefinitions,
                resolving: [],
                nameToIndex: []);
    }

    private static StructuralType? MapToStructuralType(
        TypeInference.InferredType inferredType,
        IReadOnlyDictionary<QualifiedNameRef, TypeInference.ChoiceTypeDefinition> choiceTypeDefinitions,
        ImmutableHashSet<QualifiedNameRef> resolving,
        Dictionary<string, int> nameToIndex)
    {
        switch (inferredType)
        {
            case TypeInference.InferredType.IntType:
                return StructuralType.IntType.Instance;

            case TypeInference.InferredType.FloatType:
                return StructuralType.FloatType.Instance;

            case TypeInference.InferredType.StringType:
                return StructuralType.StringType.Instance;

            case TypeInference.InferredType.CharType:
                return StructuralType.CharType.Instance;

            case TypeInference.InferredType.BoolType:
                return StructuralType.BoolType.Instance;

            case TypeInference.InferredType.NumberType:
                return
                    new StructuralType.ConstrainedVariable(
                        GetOrAssignIndex("number", nameToIndex),
                        StructuralType.TypeConstraint.Number);

            case TypeInference.InferredType.TypeVariable tv:
                return
                    new StructuralType.TypeVariable(
                        GetOrAssignIndex(tv.Name, nameToIndex));

            case TypeInference.InferredType.FunctionType func:
                return MapFunctionType(func, choiceTypeDefinitions, resolving, nameToIndex);

            case TypeInference.InferredType.ListType list:
                return MapListType(list, choiceTypeDefinitions, resolving, nameToIndex);

            case TypeInference.InferredType.TupleType tuple:
                return MapTupleType(tuple, choiceTypeDefinitions, resolving, nameToIndex);

            case TypeInference.InferredType.RecordType record:
                return MapRecordType(record, choiceTypeDefinitions, resolving, nameToIndex);

            case TypeInference.InferredType.ChoiceType choice:
                return MapChoiceType(choice, choiceTypeDefinitions, resolving, nameToIndex);

            case TypeInference.InferredType.UnknownType:
                return null;

            default:
                return null;
        }
    }

    private static int GetOrAssignIndex(string name, Dictionary<string, int> nameToIndex)
    {
        if (nameToIndex.TryGetValue(name, out var existing))
            return existing;

        var index = nameToIndex.Count;
        nameToIndex[name] = index;
        return index;
    }

    private static StructuralType? MapFunctionType(
        TypeInference.InferredType.FunctionType func,
        IReadOnlyDictionary<QualifiedNameRef, TypeInference.ChoiceTypeDefinition> choiceTypeDefinitions,
        ImmutableHashSet<QualifiedNameRef> resolving,
        Dictionary<string, int> nameToIndex)
    {
        var argType = MapToStructuralType(func.ArgumentType, choiceTypeDefinitions, resolving, nameToIndex);
        var returnType = MapToStructuralType(func.ReturnType, choiceTypeDefinitions, resolving, nameToIndex);

        if (argType is null || returnType is null)
            return null;

        return new StructuralType.FunctionType(argType, returnType);
    }

    private static StructuralType? MapListType(
        TypeInference.InferredType.ListType list,
        IReadOnlyDictionary<QualifiedNameRef, TypeInference.ChoiceTypeDefinition> choiceTypeDefinitions,
        ImmutableHashSet<QualifiedNameRef> resolving,
        Dictionary<string, int> nameToIndex)
    {
        var elementType = MapToStructuralType(list.ElementType, choiceTypeDefinitions, resolving, nameToIndex);

        if (elementType is null)
            return null;

        return new StructuralType.ListType(elementType);
    }

    private static StructuralType? MapTupleType(
        TypeInference.InferredType.TupleType tuple,
        IReadOnlyDictionary<QualifiedNameRef, TypeInference.ChoiceTypeDefinition> choiceTypeDefinitions,
        ImmutableHashSet<QualifiedNameRef> resolving,
        Dictionary<string, int> nameToIndex)
    {
        var builder = ImmutableList.CreateBuilder<StructuralType>();

        for (var i = 0; i < tuple.ElementTypes.Count; i++)
        {
            var elementType = MapToStructuralType(tuple.ElementTypes[i], choiceTypeDefinitions, resolving, nameToIndex);

            if (elementType is null)
                return null;

            builder.Add(elementType);
        }

        return new StructuralType.TupleType(builder.ToImmutable());
    }

    private static StructuralType? MapRecordType(
        TypeInference.InferredType.RecordType record,
        IReadOnlyDictionary<QualifiedNameRef, TypeInference.ChoiceTypeDefinition> choiceTypeDefinitions,
        ImmutableHashSet<QualifiedNameRef> resolving,
        Dictionary<string, int> nameToIndex)
    {
        var builder = ImmutableDictionary.CreateBuilder<string, StructuralType>();

        for (var i = 0; i < record.Fields.Count; i++)
        {
            var (fieldName, fieldInferredType) = record.Fields[i];
            var fieldType = MapToStructuralType(fieldInferredType, choiceTypeDefinitions, resolving, nameToIndex);

            if (fieldType is null)
                return null;

            builder[fieldName] = fieldType;
        }

        return new StructuralType.ClosedRecord(builder.ToImmutable());
    }

    private static StructuralType? MapChoiceType(
        TypeInference.InferredType.ChoiceType choice,
        IReadOnlyDictionary<QualifiedNameRef, TypeInference.ChoiceTypeDefinition> choiceTypeDefinitions,
        ImmutableHashSet<QualifiedNameRef> resolving,
        Dictionary<string, int> nameToIndex)
    {
        var qualifiedName =
            QualifiedNameHelper.ToQualifiedNameRef(choice.ModuleName, choice.TypeName);

        if (!choiceTypeDefinitions.TryGetValue(qualifiedName, out var definition))
            return null;

        // Detect recursive types: if we are already resolving this type, return Self.
        if (resolving.Contains(qualifiedName))
            return StructuralType.Self.Instance;

        var innerResolving = resolving.Add(qualifiedName);

        // Build type variable substitution map from the definition's type parameters
        // to the concrete type arguments at this usage site.
        var substitutions =
            BuildTypeArgumentSubstitutions(
                choice,
                definition,
                choiceTypeDefinitions,
                innerResolving,
                nameToIndex);

        if (substitutions is null)
            return null;

        var tagBuilder = ImmutableDictionary.CreateBuilder<string, ImmutableList<StructuralType>>();

        for (var i = 0; i < definition.Constructors.Count; i++)
        {
            var constructor = definition.Constructors[i];
            var fieldBuilder = ImmutableList.CreateBuilder<StructuralType>();

            for (var j = 0; j < constructor.ArgumentTypes.Count; j++)
            {
                var argType =
                    MapToStructuralType(
                        constructor.ArgumentTypes[j],
                        choiceTypeDefinitions,
                        innerResolving,
                        nameToIndex);

                if (argType is null)
                    return null;

                // Apply type argument substitutions (e.g., replacing variable 0 with Int in Maybe Int).
                if (substitutions.Count > 0)
                    argType = StructuralType.Substitute(argType, substitutions);

                fieldBuilder.Add(argType);
            }

            tagBuilder[constructor.TagName] = fieldBuilder.ToImmutable();
        }

        return new StructuralType.ChoiceType(tagBuilder.ToImmutable());
    }

    /// <summary>
    /// Builds a substitution map from the type parameters in the choice type definition's
    /// constructor argument types (type variables) to the concrete type arguments at the usage site.
    /// </summary>
    private static IReadOnlyDictionary<int, StructuralType>? BuildTypeArgumentSubstitutions(
        TypeInference.InferredType.ChoiceType choice,
        TypeInference.ChoiceTypeDefinition definition,
        IReadOnlyDictionary<QualifiedNameRef, TypeInference.ChoiceTypeDefinition> choiceTypeDefinitions,
        ImmutableHashSet<QualifiedNameRef> resolving,
        Dictionary<string, int> nameToIndex)
    {
        if (choice.TypeArguments.Count is 0)
            return ImmutableDictionary<int, StructuralType>.Empty;

        // Collect type parameter names from the constructor argument types.
        // In Elm, type parameters are the generic type variables referenced in constructor args.
        // We collect them in order from the constructors to match with type arguments.
        var typeParamNames = CollectTypeParameterNames(definition);

        if (typeParamNames.Count != choice.TypeArguments.Count)
        {
            // Mismatch between type parameter count and argument count — cannot substitute.
            return ImmutableDictionary<int, StructuralType>.Empty;
        }

        var substitutions = new Dictionary<int, StructuralType>();

        for (var i = 0; i < typeParamNames.Count; i++)
        {
            var mapped =
                MapToStructuralType(
                    choice.TypeArguments[i],
                    choiceTypeDefinitions,
                    resolving,
                    nameToIndex);

            if (mapped is null)
                return null;

            var paramIndex = GetOrAssignIndex(typeParamNames[i], nameToIndex);
            substitutions[paramIndex] = mapped;
        }

        return substitutions;
    }

    /// <summary>
    /// Collects the distinct type parameter names (type variables) referenced across
    /// all constructors of a choice type definition, in the order they first appear.
    /// </summary>
    private static IReadOnlyList<string> CollectTypeParameterNames(
        TypeInference.ChoiceTypeDefinition definition)
    {
        var seen = new HashSet<string>();
        var result = new List<string>();

        for (var i = 0; i < definition.Constructors.Count; i++)
        {
            var constructor = definition.Constructors[i];

            for (var j = 0; j < constructor.ArgumentTypes.Count; j++)
            {
                CollectTypeVariables(constructor.ArgumentTypes[j], seen, result);
            }
        }

        return result;
    }

    private static void CollectTypeVariables(
        TypeInference.InferredType type,
        HashSet<string> seen,
        List<string> result)
    {
        switch (type)
        {
            case TypeInference.InferredType.TypeVariable tv:
                if (seen.Add(tv.Name))
                    result.Add(tv.Name);

                break;

            case TypeInference.InferredType.FunctionType func:
                CollectTypeVariables(func.ArgumentType, seen, result);
                CollectTypeVariables(func.ReturnType, seen, result);
                break;

            case TypeInference.InferredType.ListType list:
                CollectTypeVariables(list.ElementType, seen, result);
                break;

            case TypeInference.InferredType.TupleType tuple:
                for (var i = 0; i < tuple.ElementTypes.Count; i++)
                    CollectTypeVariables(tuple.ElementTypes[i], seen, result);

                break;

            case TypeInference.InferredType.RecordType record:
                for (var i = 0; i < record.Fields.Count; i++)
                    CollectTypeVariables(record.Fields[i].FieldType, seen, result);

                break;

            case TypeInference.InferredType.ChoiceType choice:
                for (var i = 0; i < choice.TypeArguments.Count; i++)
                    CollectTypeVariables(choice.TypeArguments[i], seen, result);

                break;
        }
    }
}
