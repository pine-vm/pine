using Pine.Core.CodeAnalysis;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text;

using SyntaxTypes = Pine.Core.Elm.ElmSyntax.ElmSyntaxAbstract;
using ModuleName = System.Collections.Generic.IReadOnlyList<string>;

namespace Pine.Core.Elm.ElmCompilerInDotnet;

/// <summary>
/// Renders optimization opportunities and related type information for display.
/// </summary>
public static class OptimizationOpportunityRenderer
{
    /// <summary>
    /// Returns the stable display name for an opportunity category.
    /// </summary>
    public static string ToDisplayName(OpportunityCategory category) =>
        category switch
        {
            OpportunityCategory.RecordAccess => "record-access",
            OpportunityCategory.RecordUpdate => "record-update",
            OpportunityCategory.BasicsArithmetic => "Basics.arithmetic",
            OpportunityCategory.BasicsCompare => "Basics.compare",
            OpportunityCategory.BasicsEq => "Basics.eq",
            OpportunityCategory.BasicsAppend => "Basics.append",
            OpportunityCategory.PartialApplication => "partial-application",
            OpportunityCategory.HigherOrderParameter_Direct => "higher-order-parameter-direct",
            OpportunityCategory.HigherOrderParameter_Indirect => "higher-order-parameter-indirect",
            OpportunityCategory.RootLevelChoiceTagWrapper => "root-level-choice-tag-wrapper",

            _ =>
            throw new System.NotImplementedException(
                "OptimizationOpportunityRenderer.ToDisplayName does not handle category: " +
                category),
        };

    /// <summary>
    /// Renders an unordered set of <see cref="Opportunity"/> values as a
    /// deterministic, line-oriented string suitable for snapshot assertions.
    /// </summary>
    public static string RenderOpportunities(IEnumerable<Opportunity> opportunities)
    {
        var sorted =
            opportunities
            .Distinct()
            .OrderBy(opportunity => opportunity)
            .ToList();

        var sb = new StringBuilder();

        for (var i = 0; i < sorted.Count; i++)
        {
            if (i > 0)
                sb.Append('\n');

            sb.Append(sorted[i].ContainingDecl.FullName);
            sb.Append(": ");
            sb.Append(ToDisplayName(sorted[i].Category));
            sb.Append(": ");
            sb.Append(sorted[i].Description);
        }

        return sb.ToString();
    }

    /// <summary>
    /// Renders opportunities grouped by category, omitting empty categories.
    /// </summary>
    public static string RenderOpportunitiesByCategory(IEnumerable<Opportunity> opportunities)
    {
        var distinct =
            opportunities.Distinct().ToList();

        var sb = new StringBuilder();
        var firstGroup = true;

        foreach (OpportunityCategory category in System.Enum.GetValues(typeof(OpportunityCategory)))
        {
            var inCategory =
                distinct
                .Where(opportunity => opportunity.Category == category)
                .OrderBy(opportunity => opportunity.ContainingDecl)
                .ThenBy(opportunity => opportunity.Description, System.StringComparer.Ordinal)
                .ToList();

            if (inCategory.Count is 0)
                continue;

            if (!firstGroup)
                sb.Append("\n\n");

            firstGroup = false;

            sb.Append(ToDisplayName(category));
            sb.Append(':');

            foreach (var entry in inCategory)
            {
                sb.Append('\n');
                sb.Append("  ");
                sb.Append(entry.ContainingDecl.FullName);
                sb.Append(": ");
                sb.Append(entry.Description);
            }
        }

        return sb.ToString();
    }

    internal static string RenderRootLevelWrapperParameterDescription(
        int parameterIndex,
        string? parameterName,
        OptimizationOpportunityFinder.SingleTagShapeInfo shape,
        IReadOnlyList<SyntaxTypes.TypeAnnotation>? substitutedTypes) =>
        "parameter[" + parameterIndex + "] " + (parameterName ?? "_") + ": " +
        shape.ConstructorName.FullName + " -> " +
        ParenIfTopLevelArrow(RenderUnwrappedTypes(shape, substitutedTypes));

    internal static string RenderRootLevelWrapperReturnDescription(
        OptimizationOpportunityFinder.SingleTagShapeInfo shape,
        IReadOnlyList<SyntaxTypes.TypeAnnotation>? substitutedTypes) =>
        "return: " + shape.ConstructorName.FullName + " -> " +
        ParenIfTopLevelArrow(RenderUnwrappedTypes(shape, substitutedTypes));

    private static string RenderUnwrappedTypes(
        OptimizationOpportunityFinder.SingleTagShapeInfo shape,
        IReadOnlyList<SyntaxTypes.TypeAnnotation>? substitutedTypes) =>
        RenderUnwrappedTypeAnnotations(substitutedTypes ?? shape.ConstructorArgumentTypes);

    private static string RenderUnwrappedTypeAnnotations(
        IReadOnlyList<SyntaxTypes.TypeAnnotation> annotations)
    {
        if (annotations.Count is 0)
            return "()";

        if (annotations.Count is 1)
            return RenderTypeAnnotation(annotations[0]);

        var sb = new StringBuilder();
        sb.Append('(');

        for (var i = 0; i < annotations.Count; i++)
        {
            if (i > 0)
                sb.Append(", ");

            sb.Append(RenderTypeAnnotation(annotations[i]));
        }

        sb.Append(')');
        return sb.ToString();
    }

    private static string ParenIfTopLevelArrow(string rendered)
    {
        var depth = 0;

        for (var i = 0; i < rendered.Length; i++)
        {
            var character = rendered[i];

            if (character is '(' or '{')
            {
                depth++;
            }
            else if (character is ')' or '}')
            {
                depth--;
            }
            else if (depth is 0 &&
                character is '-' &&
                i + 1 < rendered.Length &&
                rendered[i + 1] is '>')
            {
                return "(" + rendered + ")";
            }
        }

        return rendered;
    }

    private static string RenderTypeAnnotation(SyntaxTypes.TypeAnnotation annotation)
    {
        switch (annotation)
        {
            case SyntaxTypes.TypeAnnotation.GenericType generic:
                return generic.Name;

            case SyntaxTypes.TypeAnnotation.Unit:
                return "()";

            case SyntaxTypes.TypeAnnotation.Typed typed:
                {
                    var nameBuilder = new StringBuilder();

                    foreach (var namespacePart in typed.ModuleName)
                    {
                        nameBuilder.Append(namespacePart);
                        nameBuilder.Append('.');
                    }

                    nameBuilder.Append(typed.Name);

                    if (typed.TypeArguments.Count is 0)
                        return nameBuilder.ToString();

                    var builder = new StringBuilder();
                    builder.Append(nameBuilder);

                    foreach (var argument in typed.TypeArguments)
                    {
                        builder.Append(' ');
                        builder.Append(RenderTypeAnnotationParenIfComposite(argument));
                    }

                    return builder.ToString();
                }

            case SyntaxTypes.TypeAnnotation.Tupled tupled:
                {
                    if (tupled.TypeAnnotations.Count is 1)
                        return RenderTypeAnnotation(tupled.TypeAnnotations[0]);

                    var builder = new StringBuilder();
                    builder.Append('(');

                    for (var i = 0; i < tupled.TypeAnnotations.Count; i++)
                    {
                        if (i > 0)
                            builder.Append(", ");

                        builder.Append(RenderTypeAnnotation(tupled.TypeAnnotations[i]));
                    }

                    builder.Append(')');
                    return builder.ToString();
                }

            case SyntaxTypes.TypeAnnotation.Record record:
                return RenderRecordDefinition(record.RecordDefinition);

            case SyntaxTypes.TypeAnnotation.GenericRecord genericRecord:
                {
                    var builder = new StringBuilder();
                    builder.Append("{ ");
                    builder.Append(genericRecord.GenericName);
                    builder.Append(" | ");
                    AppendRecordFields(builder, genericRecord.RecordDefinition);
                    builder.Append(" }");
                    return builder.ToString();
                }

            case SyntaxTypes.TypeAnnotation.FunctionTypeAnnotation function:
                {
                    var left = RenderTypeAnnotation(function.ArgumentType);

                    if (function.ArgumentType is SyntaxTypes.TypeAnnotation.FunctionTypeAnnotation)
                        left = "(" + left + ")";

                    var right = RenderTypeAnnotation(function.ReturnType);

                    return left + " -> " + right;
                }

            default:
                throw new System.NotImplementedException(
                    "RenderTypeAnnotation does not handle TypeAnnotation variant: " +
                    annotation.GetType().Name);
        }
    }

    private static string RenderRecordDefinition(SyntaxTypes.RecordDefinition definition)
    {
        if (definition.Fields.Count is 0)
            return "{}";

        var builder = new StringBuilder();
        builder.Append("{ ");
        AppendRecordFields(builder, definition);
        builder.Append(" }");
        return builder.ToString();
    }

    private static void AppendRecordFields(
        StringBuilder builder,
        SyntaxTypes.RecordDefinition definition)
    {
        for (var i = 0; i < definition.Fields.Count; i++)
        {
            if (i > 0)
                builder.Append(", ");

            builder.Append(definition.Fields[i].FieldName);
            builder.Append(" : ");
            builder.Append(RenderTypeAnnotation(definition.Fields[i].FieldType));
        }
    }

    private static string RenderTypeAnnotationParenIfComposite(
        SyntaxTypes.TypeAnnotation annotation) =>
        annotation switch
        {
            SyntaxTypes.TypeAnnotation.GenericType or
            SyntaxTypes.TypeAnnotation.Unit or
            SyntaxTypes.TypeAnnotation.Tupled or
            SyntaxTypes.TypeAnnotation.Record =>
            RenderTypeAnnotation(annotation),

            SyntaxTypes.TypeAnnotation.Typed typed =>
            typed.TypeArguments.Count is 0
            ?
            RenderTypeAnnotation(typed)
            :
            "(" + RenderTypeAnnotation(typed) + ")",

            SyntaxTypes.TypeAnnotation.GenericRecord or
            SyntaxTypes.TypeAnnotation.FunctionTypeAnnotation =>
            "(" + RenderTypeAnnotation(annotation) + ")",

            _ =>
            throw new System.NotImplementedException(
                "RenderTypeAnnotationParenIfComposite does not handle TypeAnnotation variant: " +
                annotation.GetType().Name),
        };

    /// <summary>
    /// Renders a function signature after replacing root-level single-tag
    /// wrappers with their unwrapped types.
    /// </summary>
    public static string? TryRenderTransformedSignature(
        IReadOnlyDictionary<DeclQualifiedName, SyntaxTypes.Declaration> declarations,
        DeclQualifiedName functionName)
    {
        if (!declarations.TryGetValue(functionName, out var declaration))
            return null;

        if (declaration is not SyntaxTypes.Declaration.FunctionDeclaration functionDeclaration)
            return null;

        if (functionDeclaration.Function.Signature is not { } signature)
            return null;

        var singleTagRegistry =
            OptimizationOpportunityFinder.BuildSingleTagRegistry(declarations);

        if (singleTagRegistry.IsEmpty)
            return RenderTypeAnnotation(signature.TypeAnnotation);

        var ownModule = functionName.Namespaces;
        var implementation = functionDeclaration.Function.Declaration;
        var parameterCount = implementation.Arguments.Count;

        var signatureParameterTypes = new List<SyntaxTypes.TypeAnnotation?>();

        OptimizationOpportunityFinder.DecomposeFunctionSignature(
            signature.TypeAnnotation,
            parameterCount,
            signatureParameterTypes,
            out var signatureReturnType);

        var transformedParts = new List<string>();

        for (var i = 0; i < parameterCount; i++)
        {
            var parameterType =
                i < signatureParameterTypes.Count
                ?
                signatureParameterTypes[i]
                :
                null;

            transformedParts.Add(
                parameterType is null
                ?
                "?"
                :
                RenderTransformedTypeAnnotationAtRoot(
                    parameterType,
                    singleTagRegistry,
                    ownModule));
        }

        var renderedReturn =
            signatureReturnType is null
            ?
            "?"
            :
            RenderTransformedTypeAnnotationAtRoot(
                signatureReturnType,
                singleTagRegistry,
                ownModule);

        var builder = new StringBuilder();

        foreach (var transformedPart in transformedParts)
        {
            builder.Append(ParenIfTopLevelArrow(transformedPart));
            builder.Append(" -> ");
        }

        builder.Append(renderedReturn);
        return builder.ToString();
    }

    private static string RenderTransformedTypeAnnotationAtRoot(
        SyntaxTypes.TypeAnnotation annotation,
        ImmutableDictionary<DeclQualifiedName, OptimizationOpportunityFinder.SingleTagShapeInfo> singleTagRegistry,
        ModuleName ownModule)
    {
        var (info, unwrappedTypes) =
            OptimizationOpportunityFinder.TryResolveSingleTagWrap(
                annotation,
                singleTagRegistry,
                ownModule);

        if (info is not null && unwrappedTypes is not null)
            return RenderUnwrappedTypeAnnotations(unwrappedTypes);

        return RenderTypeAnnotation(annotation);
    }
}
