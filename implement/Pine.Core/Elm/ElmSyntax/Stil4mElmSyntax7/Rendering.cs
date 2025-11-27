using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;

public class Rendering
{
    /// <summary>
    /// Represents a line with an indent count (in spaces) and content.
    /// The indent is stored separately to allow efficient string building at the root.
    /// </summary>
    public readonly record struct IndentedLine(
        int IndentSpaces,
        string Content)
    {
        public static readonly IndentedLine Empty =
            new(0, "");

        public IndentedLine IndentFurther(int additionalSpaces) =>
            new(IndentSpaces + additionalSpaces, Content);
    }

    /// <summary>
    /// Centralized enum for expression complexity classification.
    /// Determines whether expressions need multi-line rendering.
    /// </summary>
    public enum ExpressionComplexity
    {
        /// <summary>Expression can be rendered on a single line.</summary>
        Simple,

        /// <summary>Expression requires multi-line rendering.</summary>
        Complex
    }

    public record Config(
        LineBreakingConfig LineBreaking,
        Func<QualifiedNameRef, QualifiedNameRef>? MapQualifiedName);

    public abstract record LineBreakingConfig()
    {
        public static LineBreakingConfig SnapshotTestsDefault =>
            new CanonicalBasedOnComplexity();

        /// <summary>
        /// Insert line breaks based on the complexity of expressions.
        /// </summary>
        public sealed record CanonicalBasedOnComplexity
            : LineBreakingConfig;
    }

    /// <summary>
    /// Do not use any location information from the given nodes, but normalize
    /// all locations for a consistent layout.
    /// 
    /// <para>
    /// The output should be stable with the use of elm-format.
    /// </para>
    /// </summary>
    public static Config ConfigNormalizeAllLocations(
        LineBreakingConfig lineBreaking,
        Func<QualifiedNameRef, QualifiedNameRef>? mapQualifiedName = null) =>
        new(
            LineBreaking: lineBreaking,
            MapQualifiedName: mapQualifiedName);

    /// <summary>
    /// Do not use any location information from the given nodes, but normalize
    /// all locations for a consistent layout.
    /// 
    /// <para>
    /// The output should be stable with the use of elm-format.
    /// </para>
    /// </summary>
    public static Config ConfigNormalizeAllLocations(
        LineBreakingConfig lineBreaking,
        IReadOnlyDictionary<QualifiedNameRef, QualifiedNameRef> mapQualifiedName) =>
        new(
            LineBreaking: lineBreaking,
            MapQualifiedName: qn =>
            mapQualifiedName.TryGetValue(qn, out var mapped) ? mapped : qn);

    public static string ToString(
        File file,
        Config config)
    {
        var indentedLines = ToIndentedLines(file, config);

        var sb = new StringBuilder();
        var isFirst = true;

        foreach (var line in indentedLines)
        {
            if (!isFirst)
            {
                sb.Append('\n');
            }

            isFirst = false;

            // Append indent spaces and content in one go
            if (line.IndentSpaces > 0)
            {
                sb.Append(' ', line.IndentSpaces);
            }

            sb.Append(line.Content);
        }

        return sb.ToString();
    }

    public static IEnumerable<IndentedLine> ToIndentedLines(
        File file,
        Config config)
    {
        // Render module definition
        foreach (var line in RenderModuleLines(file.ModuleDefinition.Value))
        {
            yield return line;
        }

        // Render imports
        if (file.Imports.Count > 0)
        {
            yield return IndentedLine.Empty;
            foreach (var import in file.Imports)
            {
                yield return new IndentedLine(0, RenderImport(import.Value));
            }
        }

        // Render declarations with comments
        Declaration? previousDeclaration = null;
        foreach (var declaration in file.Declarations)
        {
            // Determine spacing before this declaration
            if (previousDeclaration is Declaration.InfixDeclaration &&
                declaration.Value is Declaration.InfixDeclaration)
            {
                // Consecutive infix declarations get no blank lines between them
            }
            else
            {
                // All other declarations get two blank lines before them
                yield return IndentedLine.Empty;
                yield return IndentedLine.Empty;
            }

            // Get comments that appear between this declaration start and have a row before next declaration
            var declarationComments = GetCommentsForDeclaration(declaration, file.Comments);

            foreach (var line in RenderDeclarationLinesWithComments(declaration.Value, declarationComments, config))
            {
                yield return line;
            }

            previousDeclaration = declaration.Value;
        }
    }

    private static IReadOnlyList<Node<string>> GetCommentsForDeclaration(
        Node<Declaration> declaration,
        IReadOnlyList<Node<string>> allComments)
    {
        var declRange = declaration.Range;

        // Find comments that start after the declaration starts (inline comments)
        return
            [.. allComments
            .Where(c => c.Range.Start.Row >= declRange.Start.Row && c.Range.Start.Row <= declRange.End.Row)
            .OrderBy(c => c.Range.Start.Row)
            .ThenBy(c => c.Range.Start.Column)];
    }

    private static IEnumerable<IndentedLine> RenderModuleLines(Module module)
    {
        string moduleKeyword;
        Node<IReadOnlyList<string>> moduleName;
        Node<Exposing> exposingList;

        switch (module)
        {
            case Module.NormalModule normalModule:
                moduleKeyword = "module";
                moduleName = normalModule.ModuleData.ModuleName;
                exposingList = normalModule.ModuleData.ExposingList;
                break;
            case Module.PortModule portModule:
                moduleKeyword = "port module";
                moduleName = portModule.ModuleData.ModuleName;
                exposingList = portModule.ModuleData.ExposingList;
                break;
            case Module.EffectModule effectModule:
                moduleKeyword = "effect module";
                moduleName = effectModule.ModuleData.ModuleName;
                exposingList = effectModule.ModuleData.ExposingList;
                break;
            default:
                throw new NotImplementedException($"Unknown module type: {module.GetType().Name}");
        }

        var moduleNameStr = RenderModuleName(moduleName.Value);

        foreach (var line in RenderModuleWithExposing(moduleKeyword, moduleNameStr, exposingList.Value))
        {
            yield return line;
        }
    }

    private static IEnumerable<IndentedLine> RenderModuleWithExposing(string moduleKeyword, string moduleName, Exposing exposing)
    {
        if (exposing is Exposing.All)
        {
            yield return new IndentedLine(0, $"{moduleKeyword} {moduleName} exposing (..)");
            yield break;
        }

        if (exposing is Exposing.Explicit explicit_)
        {
            // For explicit exposing with multiple items, use multi-line format
            if (explicit_.Nodes.Count > 1)
            {
                yield return new IndentedLine(0, $"{moduleKeyword} {moduleName} exposing");
                for (var i = 0; i < explicit_.Nodes.Count; i++)
                {
                    var prefix =
                        i is 0
                        ?
                        "( " :
                        ", ";

                    yield return new IndentedLine(4, prefix + RenderTopLevelExpose(explicit_.Nodes[i].Value));
                }
                yield return new IndentedLine(4, ")");
            }
            else
            {
                yield return new IndentedLine(0, $"{moduleKeyword} {moduleName} exposing ({string.Join(", ", explicit_.Nodes.Select(n => RenderTopLevelExpose(n.Value)))})");
            }
            yield break;
        }

        throw new NotImplementedException($"Unknown exposing type: {exposing.GetType().Name}");
    }

    private static string RenderModuleName(IReadOnlyList<string> moduleName) =>
        string.Join(".", moduleName);

    private static string RenderExposing(Exposing exposing)
    {
        return exposing switch
        {
            Exposing.All => "(..)",

            Exposing.Explicit explicit_ =>
                "(" + string.Join(", ", explicit_.Nodes.Select(n => RenderTopLevelExpose(n.Value))) + ")",

            _ => throw new NotImplementedException($"Unknown exposing type: {exposing.GetType().Name}")
        };
    }

    private static string RenderTopLevelExpose(TopLevelExpose expose)
    {
        return expose switch
        {
            TopLevelExpose.InfixExpose infix => $"({infix.Name})",
            TopLevelExpose.FunctionExpose func => func.Name,
            TopLevelExpose.TypeOrAliasExpose typeOrAlias => typeOrAlias.Name,
            TopLevelExpose.TypeExpose typeExpose =>
                typeExpose.ExposedType.Open is not null
                    ? $"{typeExpose.ExposedType.Name}(..)"
                    : typeExpose.ExposedType.Name,
            _ => throw new NotImplementedException($"Unknown expose type: {expose.GetType().Name}")
        };
    }

    private static string RenderImport(Import import)
    {
        var sb = new StringBuilder();
        sb.Append("import ");
        sb.Append(RenderModuleName(import.ModuleName.Value));

        if (import.ModuleAlias is not null)
        {
            sb.Append(" as ");
            sb.Append(RenderModuleName(import.ModuleAlias.Value));
        }

        if (import.ExposingList is not null)
        {
            sb.Append(" exposing ");
            sb.Append(RenderExposing(import.ExposingList.Value));
        }

        return sb.ToString();
    }

    private static IEnumerable<IndentedLine> RenderDeclarationLinesWithComments(
        Declaration declaration,
        IReadOnlyList<Node<string>> comments,
        Config config)
    {
        switch (declaration)
        {
            case Declaration.CustomTypeDeclaration customType:
                foreach (var line in RenderCustomTypeLines(customType.TypeDeclaration, config, comments))
                {
                    yield return line;
                }
                break;

            default:
                foreach (var line in RenderDeclarationLines(declaration, config))
                {
                    yield return line;
                }
                break;
        }
    }

    private static IEnumerable<IndentedLine> RenderDeclarationLines(Declaration declaration, Config config)
    {
        return declaration switch
        {
            Declaration.FunctionDeclaration funcDecl =>
                RenderFunctionLines(funcDecl.Function, config),

            Declaration.CustomTypeDeclaration customType =>
                RenderCustomTypeLines(customType.TypeDeclaration, config),

            Declaration.AliasDeclaration aliasDecl =>
                RenderAliasLines(aliasDecl.TypeAlias, config),

            Declaration.PortDeclaration portDecl =>
                RenderPortLines(portDecl.Signature, config),

            Declaration.InfixDeclaration infixDecl =>
                [new IndentedLine(0, RenderInfix(infixDecl.Infix))],

            _ =>
            throw new NotImplementedException(
                $"Unknown declaration type: {declaration.GetType().Name}")
        };
    }

    private static IEnumerable<IndentedLine> RenderFunctionLines(FunctionStruct function, Config config)
    {
        if (function.Documentation is { } documentation)
        {
            yield return new IndentedLine(0, documentation.Value);
        }

        if (function.Signature is { } signature)
        {
            yield return new IndentedLine(0, RenderSignature(signature.Value, config));
        }

        var impl = function.Declaration.Value;
        var header = impl.Name.Value;

        if (impl.Arguments.Count > 0)
        {
            header += " " + string.Join(" ", impl.Arguments.Select(a => RenderPattern(a.Value)));
        }

        header += " =";

        yield return new IndentedLine(0, header);

        foreach (var line in RenderExpressionLines(impl.Expression.Value, config, indent: 1))
        {
            yield return line;
        }
    }

    private static string RenderSignature(
        Signature signature,
        Config config) =>
        $"{signature.Name.Value} : {RenderTypeAnnotation(signature.TypeAnnotation.Value, config)}";

    private static string RenderTypeAnnotation(
        TypeAnnotation typeAnnotation,
        Config config)
    {
        return typeAnnotation switch
        {
            TypeAnnotation.GenericType generic => generic.Name,

            TypeAnnotation.Typed typed =>
            RenderTypedAnnotation(typed, config),

            TypeAnnotation.Unit => "()",

            TypeAnnotation.Tupled tupled =>
            "( " + string.Join(", ", tupled.TypeAnnotations.Select(t => RenderTypeAnnotation(t.Value, config))) + " )",

            TypeAnnotation.Record record =>
            RenderRecordDefinition(record.RecordDefinition, config),

            TypeAnnotation.GenericRecord genericRecord =>
            "{ " + genericRecord.GenericName.Value +
            " | " +
            RenderRecordFields(genericRecord.RecordDefinition.Value, config) + " }",

            TypeAnnotation.FunctionTypeAnnotation funcType =>
            RenderTypeAnnotationParenthesizedForFunction(funcType.ArgumentType.Value, config) +
            " -> " +
            RenderTypeAnnotation(funcType.ReturnType.Value, config),

            _ =>
            throw new NotImplementedException(
                $"Unknown type annotation: {typeAnnotation.GetType().Name}")
        };
    }

    private static string RenderTypedAnnotation(
        TypeAnnotation.Typed typed,
        Config config)
    {
        var originalQualifiedName =
            new QualifiedNameRef(
                ModuleName: typed.TypeName.Value.ModuleName,
                Name: typed.TypeName.Value.Name);

        var mappedQualifiedName =
            config.MapQualifiedName is { } mapQualifiedName
            ?
            mapQualifiedName(originalQualifiedName)
            :
            originalQualifiedName;

        var typeName =
            mappedQualifiedName.ModuleName.Count > 0
            ?
            RenderModuleName(mappedQualifiedName.ModuleName) + "." + mappedQualifiedName.Name
            :
            mappedQualifiedName.Name;

        if (typed.TypeArguments.Count is 0)
            return typeName;

        return typeName + " " + string.Join(" ", typed.TypeArguments.Select(a => RenderTypeAnnotationParenthesized(a.Value, config)));
    }

    private static string RenderTypeAnnotationParenthesized(
        TypeAnnotation typeAnnotation,
        Config config)
    {
        // Wrap complex types in parentheses
        return typeAnnotation switch
        {
            TypeAnnotation.FunctionTypeAnnotation =>
            "(" + RenderTypeAnnotation(typeAnnotation, config) + ")",

            TypeAnnotation.Typed typed when typed.TypeArguments.Count > 0 =>
            "(" + RenderTypeAnnotation(typeAnnotation, config) + ")",

            _ =>
            RenderTypeAnnotation(typeAnnotation, config)
        };
    }

    private static string RenderTypeAnnotationParenthesizedForFunction(
        TypeAnnotation typeAnnotation,
        Config config)
    {
        // Only wrap function types in parentheses when they appear as arguments to a function type
        return typeAnnotation switch
        {
            TypeAnnotation.FunctionTypeAnnotation =>
            "(" + RenderTypeAnnotation(typeAnnotation, config) + ")",

            _ =>
            RenderTypeAnnotation(typeAnnotation, config)
        };
    }

    private static string RenderRecordDefinition(
        RecordDefinition recordDefinition,
        Config config)
    {
        if (recordDefinition.Fields.Count is 0)
            return "{}";

        return "{ " + RenderRecordFields(recordDefinition, config) + " }";
    }

    private static string RenderRecordFields(
        RecordDefinition recordDefinition,
        Config config) =>
        string.Join(", ", recordDefinition.Fields.Select(f =>
            f.Value.FieldName.Value + " : " + RenderTypeAnnotation(f.Value.FieldType.Value, config)));

    /// <summary>
    /// Renders a custom type declaration with optional comments between constructors.
    /// </summary>
    private static IEnumerable<IndentedLine> RenderCustomTypeLines(
        TypeStruct typeStruct,
        Config config,
        IReadOnlyList<Node<string>>? comments = null)
    {
        if (typeStruct.Documentation is { } documentation)
        {
            yield return new IndentedLine(0, documentation.Value);
        }

        var header = "type " + typeStruct.Name.Value;

        if (typeStruct.Generics.Count > 0)
        {
            header += " " + string.Join(" ", typeStruct.Generics.Select(g => g.Value));
        }

        if (typeStruct.Constructors.Count is 0)
        {
            yield return new IndentedLine(0, header);
            yield break;
        }

        yield return new IndentedLine(0, header);

        for (var i = 0; i < typeStruct.Constructors.Count; i++)
        {
            var constructor = typeStruct.Constructors[i];
            var prefix = i is 0 ? "= " : "| ";
            var constructorStr = constructor.Value.Name.Value;

            if (constructor.Value.Arguments.Count > 0)
            {
                constructorStr += " " + string.Join(" ", constructor.Value.Arguments.Select(a => RenderTypeAnnotationParenthesized(a.Value, config)));
            }

            yield return new IndentedLine(4, prefix + constructorStr);

            // Render comments between constructors if provided
            if (comments is not null)
            {
                var nextConstructorRow = i + 1 < typeStruct.Constructors.Count
                    ? typeStruct.Constructors[i + 1].Range.Start.Row
                    : int.MaxValue;

                var constructorComments = comments
                    .Where(c =>
                        c.Range.Start.Row > constructor.Range.Start.Row &&
                        c.Range.Start.Row < nextConstructorRow)
                    .OrderBy(c => c.Range.Start.Row);

                foreach (var comment in constructorComments)
                {
                    yield return new IndentedLine(8, comment.Value);
                }
            }
        }
    }

    private static IEnumerable<IndentedLine> RenderAliasLines(
        TypeAlias typeAlias,
        Config config)
    {
        if (typeAlias.Documentation is not null)
        {
            yield return new IndentedLine(0, typeAlias.Documentation.Value);
        }

        var header = "type alias " + typeAlias.Name.Value;
        if (typeAlias.Generics.Count > 0)
        {
            header += " " + string.Join(" ", typeAlias.Generics.Select(g => g.Value));
        }

        header += " =";

        yield return new IndentedLine(0, header);
        yield return new IndentedLine(4, RenderTypeAnnotation(typeAlias.TypeAnnotation.Value, config));
    }

    private static IEnumerable<IndentedLine> RenderPortLines(
        Signature signature,
        Config config)
    {
        yield return new IndentedLine(0, "port " + RenderSignature(signature, config));
    }

    private static string RenderInfix(Infix infix)
    {
        var direction = infix.Direction.Value switch
        {
            InfixDirection.Left =>
            "left ",  // Pad with space to align with "right"

            InfixDirection.Right =>
            "right",

            InfixDirection.Non =>
            "non  ",   // Pad with spaces to align with "right"

            _ =>
            throw new NotImplementedException(
                $"Unknown infix direction: {infix.Direction.Value}")
        };

        return $"infix {direction} {infix.Precedence.Value} ({infix.Operator.Value}) = {infix.FunctionName.Value}";
    }

    private static IEnumerable<IndentedLine> RenderExpressionLines(Expression expression, Config config, int indent)
    {
        // Use centralized complexity check
        if (GetExpressionComplexity(expression, config) is ExpressionComplexity.Complex)
        {
            foreach (var line in RenderMultiLine(expression, config, indent))
            {
                yield return line;
            }
        }
        else
        {
            yield return new IndentedLine(indent * 4, RenderExpression(expression));
        }
    }

    /// <summary>
    /// Centralized complexity evaluation for all expression types.
    /// Returns Complex if the expression requires multi-line rendering.
    /// </summary>
    private static ExpressionComplexity GetExpressionComplexity(Expression expression, Config config)
    {
        if (config.LineBreaking is not LineBreakingConfig.CanonicalBasedOnComplexity)
            return ExpressionComplexity.Simple;

        return expression switch
        {
            Expression.Application app when ApplicationNeedsMultiLine(app) =>
                ExpressionComplexity.Complex,

            Expression.ListExpr listExpr when ListNeedsMultiLine(listExpr) =>
                ExpressionComplexity.Complex,

            Expression.RecordExpr recordExpr when recordExpr.Fields.Count > 0 =>
                ExpressionComplexity.Complex,

            Expression.CaseExpression =>
                ExpressionComplexity.Complex,

            Expression.IfBlock =>
                ExpressionComplexity.Complex,

            Expression.LetExpression =>
                ExpressionComplexity.Complex,

            Expression.OperatorApplication opApp when OperatorApplicationNeedsMultiLine(opApp) =>
                ExpressionComplexity.Complex,

            Expression.LambdaExpression lambda when LambdaBodyNeedsMultiLine(lambda.Lambda.Expression.Value) =>
                ExpressionComplexity.Complex,

            _ =>
                ExpressionComplexity.Simple
        };
    }

    /// <summary>
    /// Unwraps any parenthesized expressions to get to the inner expression.
    /// </summary>
    private static Expression UnwrapParentheses(Expression expr)
    {
        while (expr is Expression.ParenthesizedExpression paren)
        {
            expr = paren.Expression.Value;
        }
        return expr;
    }

    /// <summary>
    /// Determines if an operator application needs multi-line rendering.
    /// </summary>
    private static bool OperatorApplicationNeedsMultiLine(Expression.OperatorApplication opApp) =>
        opApp.Left.Value is Expression.ListExpr listExpr && ListNeedsMultiLine(listExpr);

    /// <summary>
    /// Determines if an application expression needs multi-line rendering.
    /// Function applications are always laid out over multiple lines, so that each argument comes with a line break.
    /// </summary>
    private static bool ApplicationNeedsMultiLine(Expression.Application app) =>
        // Multi-line when there are actual arguments (Arguments[0] is the function, Arguments[1..] are actual arguments)
        app.Arguments.Count > 1;

    /// <summary>
    /// Determines if a lambda body needs multi-line rendering.
    /// </summary>
    private static bool LambdaBodyNeedsMultiLine(Expression body) =>
        body switch
        {
            Expression.ListExpr listExpr => ListNeedsMultiLine(listExpr),
            Expression.RecordExpr recordExpr => RecordNeedsMultiLine(recordExpr),
            Expression.CaseExpression => true,
            Expression.LetExpression => true,
            _ => false
        };

    /// <summary>
    /// Determines if an expression is simple (can stay on one line as an argument).
    /// </summary>
    private static bool IsSimpleExpression(Expression expr) =>
        expr switch
        {
            Expression.Integer => true,
            Expression.UnitExpr => true,
            Expression.FunctionOrValue => true,
            _ => false
        };

    /// <summary>
    /// Determines if a list expression needs multi-line rendering.
    /// </summary>
    private static bool ListNeedsMultiLine(Expression.ListExpr listExpr) =>
        listExpr.Elements.Any(e =>
            e.Value is Expression.Application ||
            e.Value is Expression.ListExpr ||
            e.Value is Expression.OperatorApplication ||
            e.Value is Expression.RecordExpr);

    /// <summary>
    /// Determines if a record expression needs multi-line rendering.
    /// </summary>
    private static bool RecordNeedsMultiLine(Expression.RecordExpr recordExpr)
    {
        if (recordExpr.Fields.Count is 0)
            return false;

        // More than one field always means multi-line
        if (recordExpr.Fields.Count > 1)
            return true;

        // Single field: multi-line if the value is complex
        return recordExpr.Fields.Any(f =>
            f.Value.valueExpr.Value is Expression.ListExpr listExpr && listExpr.Elements.Count > 0 ||
            f.Value.valueExpr.Value is Expression.RecordExpr nestedRecord && RecordNeedsMultiLine(nestedRecord) ||
            f.Value.valueExpr.Value is Expression.Application ||
            f.Value.valueExpr.Value is Expression.OperatorApplication);
    }

    /// <summary>
    /// Checks if a list contains any expression that requires the entire list to use complex multi-line format.
    /// </summary>
    private static bool ListContainsComplexExpression(Expression.ListExpr listExpr) =>
        listExpr.Elements.Any(e =>
            e.Value is Expression.Application app && ApplicationHasListArgument(app));

    /// <summary>
    /// Checks if an application has any list argument.
    /// </summary>
    private static bool ApplicationHasListArgument(Expression.Application app)
    {
        for (var i = 1; i < app.Arguments.Count; i++)
        {
            if (app.Arguments[i].Value is Expression.ListExpr)
                return true;
        }
        return false;
    }

    /// <summary>
    /// Unified multi-line rendering method using pattern matching for all expression types.
    /// This is the single entry point for complex expression rendering.
    /// </summary>
    private static IEnumerable<IndentedLine> RenderMultiLine(Expression expression, Config config, int indent)
    {
        var indentSpaces = indent * 4;

        return expression switch
        {
            Expression.Application app => RenderMultiLineApplication(app, config, indent),
            Expression.ListExpr listExpr => RenderMultiLineList(listExpr, config, indent),
            Expression.RecordExpr recordExpr => RenderMultiLineRecord(recordExpr, config, indent),
            Expression.CaseExpression caseExpr => RenderMultiLineCaseExpression(caseExpr.CaseBlock, config, indent),
            Expression.OperatorApplication opApp => RenderMultiLineOperatorApplication(opApp, config, indent),
            Expression.IfBlock ifBlock => RenderMultiLineIfBlock(ifBlock, config, indent),
            Expression.LetExpression letExpr => RenderMultiLineLetExpression(letExpr.Value, config, indent),
            _ => [new IndentedLine(indentSpaces, RenderExpression(expression))]
        };
    }

    /// <summary>
    /// Renders an application expression in multi-line format.
    /// </summary>
    private static IEnumerable<IndentedLine> RenderMultiLineApplication(Expression.Application app, Config config, int indent)
    {
        var indentSpaces = indent * 4;

        // Check if function is a lambda with multi-line body (possibly wrapped in parentheses)
        var funcExpr = UnwrapParentheses(app.Arguments[0].Value);

        if (funcExpr is Expression.LambdaExpression lambda && LambdaBodyNeedsMultiLine(lambda.Lambda.Expression.Value))
        {
            // Render lambda with multi-line body
            var lambdaHeader = "(\\" + string.Join(" ", lambda.Lambda.Arguments.Select(a => RenderPattern(a.Value))) + " ->";
            yield return new IndentedLine(indentSpaces, lambdaHeader);

            // Render lambda body multi-line
            foreach (var line in RenderMultiLine(lambda.Lambda.Expression.Value, config, indent + 1))
            {
                yield return line;
            }

            // Closing paren on its own line
            yield return new IndentedLine(indentSpaces, ")");

            // Render arguments on subsequent lines
            for (var i = 1; i < app.Arguments.Count; i++)
            {
                yield return new IndentedLine(indentSpaces + 4, RenderExpressionParenthesizedIfNeeded(app.Arguments[i].Value));
            }
        }
        else
        {
            // Render function on first line
            yield return new IndentedLine(indentSpaces, RenderExpression(app.Arguments[0].Value));
            // Render arguments on subsequent lines with extra indentation
            for (var i = 1; i < app.Arguments.Count; i++)
            {
                var arg = app.Arguments[i].Value;
                if (arg is Expression.ListExpr listExpr && ListNeedsMultiLine(listExpr))
                {
                    // Render list multi-line
                    foreach (var line in RenderMultiLineList(listExpr, config, indent + 1))
                    {
                        yield return line;
                    }
                }
                else
                {
                    yield return new IndentedLine(indentSpaces + 4, RenderExpressionParenthesizedIfNeeded(arg));
                }
            }
        }
    }

    /// <summary>
    /// Renders a single list element, handling both simple and complex cases.
    /// This unifies the logic for rendering list items across different contexts.
    /// </summary>
    private static IEnumerable<IndentedLine> RenderListElement(
        Expression element,
        Config config,
        int indent,
        string prefix)
    {
        var indentSpaces = indent * 4;

        switch (element)
        {
            case Expression.ListExpr nestedList when ListContainsComplexExpression(nestedList):
                // Nested list with complex expressions - render with nested multi-line format
                foreach (var line in RenderNestedMultiLineList(nestedList, indent, prefix))
                {
                    yield return line;
                }
                break;

            case Expression.ListExpr nestedList:
                // Simple nested list - render inline
                yield return new IndentedLine(indentSpaces, prefix + RenderExpression(nestedList));
                break;

            case Expression.Application app:
                // Applications with actual arguments are rendered multi-line with each argument on its own line
                yield return new IndentedLine(indentSpaces, prefix + RenderExpression(app.Arguments[0].Value));
                for (var j = 1; j < app.Arguments.Count; j++)
                {
                    var arg = app.Arguments[j].Value;
                    if (arg is Expression.ListExpr argListExpr)
                    {
                        if (ListNeedsMultiLine(argListExpr))
                        {
                            foreach (var line in RenderMultiLineList(argListExpr, config, indent + 1))
                            {
                                yield return line;
                            }
                        }
                        else
                        {
                            yield return new IndentedLine(indentSpaces + 4, RenderExpression(argListExpr));
                        }
                    }
                    else
                    {
                        yield return new IndentedLine(indentSpaces + 4, RenderExpressionParenthesizedIfNeeded(arg));
                    }
                }
                break;

            case Expression.RecordExpr recordExpr when RecordNeedsMultiLine(recordExpr):
                // Record that needs multi-line rendering
                foreach (var line in RenderRecordInList(recordExpr, config, indent, prefix))
                {
                    yield return line;
                }
                break;

            default:
                // Simple element - render inline
                yield return new IndentedLine(indentSpaces, prefix + RenderExpression(element));
                break;
        }
    }

    /// <summary>
    /// Renders a list expression in multi-line format.
    /// </summary>
    private static IEnumerable<IndentedLine> RenderMultiLineList(Expression.ListExpr listExpr, Config config, int indent)
    {
        var indentSpaces = indent * 4;

        for (var i = 0; i < listExpr.Elements.Count; i++)
        {
            var item = listExpr.Elements[i].Value;
            var prefix = i is 0 ? "[ " : ", ";

            foreach (var line in RenderListElement(item, config, indent, prefix))
            {
                yield return line;
            }
        }

        yield return new IndentedLine(indentSpaces, "]");
    }

    private static IEnumerable<IndentedLine> RenderMultiLineIfBlock(Expression.IfBlock ifBlock, Config config, int indent)
    {
        var indentSpaces = indent * 4;

        // Render "if condition then"
        yield return new IndentedLine(indentSpaces, "if " + RenderExpression(ifBlock.Condition.Value) + " then");

        // Render then branch with extra indent
        foreach (var line in RenderExpressionLines(ifBlock.ThenBlock.Value, config, indent + 1))
        {
            yield return line;
        }

        // Check if else branch is another if block (else if chain)
        if (ifBlock.ElseBlock.Value is Expression.IfBlock elseIfBlock)
        {
            // Blank line before else if
            yield return IndentedLine.Empty;

            // Render "else if condition then"
            yield return new IndentedLine(indentSpaces, "else if " + RenderExpression(elseIfBlock.Condition.Value) + " then");

            // Render then branch with extra indent
            foreach (var line in RenderExpressionLines(elseIfBlock.ThenBlock.Value, config, indent + 1))
            {
                yield return line;
            }

            // Continue with the else block from this else-if
            var currentElse = elseIfBlock.ElseBlock.Value;
            while (currentElse is Expression.IfBlock nextElseIf)
            {
                // Blank line before else if
                yield return IndentedLine.Empty;

                yield return new IndentedLine(indentSpaces, "else if " + RenderExpression(nextElseIf.Condition.Value) + " then");

                foreach (var line in RenderExpressionLines(nextElseIf.ThenBlock.Value, config, indent + 1))
                {
                    yield return line;
                }

                currentElse = nextElseIf.ElseBlock.Value;
            }

            // Final else branch
            yield return IndentedLine.Empty;
            yield return new IndentedLine(indentSpaces, "else");
            foreach (var line in RenderExpressionLines(currentElse, config, indent + 1))
            {
                yield return line;
            }
        }
        else
        {
            // Simple else branch
            yield return IndentedLine.Empty;
            yield return new IndentedLine(indentSpaces, "else");
            foreach (var line in RenderExpressionLines(ifBlock.ElseBlock.Value, config, indent + 1))
            {
                yield return line;
            }
        }
    }

    private static IEnumerable<IndentedLine> RenderMultiLineOperatorApplication(Expression.OperatorApplication opApp, Config config, int indent)
    {
        // Render the left side (which should be multi-line if we got here)
        foreach (var line in RenderExpressionLines(opApp.Left.Value, config, indent))
        {
            yield return line;
        }

        // Render operator and right side with extra indentation
        yield return new IndentedLine(indent * 4 + 4, opApp.Operator + " " + RenderExpression(opApp.Right.Value));
    }

    private static IEnumerable<IndentedLine> RenderMultiLineRecord(Expression.RecordExpr recordExpr, Config config, int indent)
    {
        var indentSpaces = indent * 4;

        for (var i = 0; i < recordExpr.Fields.Count; i++)
        {
            var field = recordExpr.Fields[i].Value;
            var prefix = i is 0 ? "{ " : ", ";
            var fieldValue = field.valueExpr.Value;

            // Check if the field value needs multi-line rendering
            if (GetExpressionComplexity(fieldValue, config) is ExpressionComplexity.Complex)
            {
                yield return new IndentedLine(indentSpaces, prefix + field.fieldName.Value + " =");
                foreach (var line in RenderExpressionLines(fieldValue, config, indent + 1))
                {
                    yield return line;
                }
            }
            else
            {
                yield return new IndentedLine(indentSpaces, prefix + field.fieldName.Value + " = " + RenderExpression(fieldValue));
            }
        }

        yield return new IndentedLine(indentSpaces, "}");
    }

    private static IEnumerable<IndentedLine> RenderMultiLineCaseExpression(CaseBlock caseBlock, Config config, int indent)
    {
        var indentSpaces = indent * 4;

        yield return new IndentedLine(indentSpaces, "case " + RenderExpression(caseBlock.Expression.Value) + " of");

        for (var i = 0; i < caseBlock.Cases.Count; i++)
        {
            var caseItem = caseBlock.Cases[i];

            // Add blank line between cases (except before the first one)
            if (i > 0)
            {
                yield return IndentedLine.Empty;
            }

            yield return new IndentedLine(indentSpaces + 4, RenderPattern(caseItem.Pattern.Value) + " ->");

            // Render the case expression body
            foreach (var line in RenderExpressionLines(caseItem.Expression.Value, config, indent + 2))
            {
                yield return line;
            }
        }
    }

    private static IEnumerable<IndentedLine> RenderMultiLineLetExpression(Expression.LetBlock letBlock, Config config, int indent)
    {
        var indentSpaces = indent * 4;

        // Render "let"
        yield return new IndentedLine(indentSpaces, "let");

        // Render declarations
        for (var i = 0; i < letBlock.Declarations.Count; i++)
        {
            var decl = letBlock.Declarations[i].Value;

            // Add blank line between declarations (except before the first one)
            if (i > 0)
            {
                yield return IndentedLine.Empty;
            }

            foreach (var line in RenderLetDeclarationLines(decl, config, indent + 1))
            {
                yield return line;
            }
        }

        // Render "in"
        yield return new IndentedLine(indentSpaces, "in");

        // Render the body expression
        foreach (var line in RenderExpressionLines(letBlock.Expression.Value, config, indent))
        {
            yield return line;
        }
    }

    private static IEnumerable<IndentedLine> RenderLetDeclarationLines(Expression.LetDeclaration letDecl, Config config, int indent)
    {
        var indentSpaces = indent * 4;

        switch (letDecl)
        {
            case Expression.LetDeclaration.LetFunction letFunc:
                {
                    // Render signature if present
                    if (letFunc.Function.Signature is { } signature)
                    {
                        yield return new IndentedLine(indentSpaces, RenderSignature(signature.Value, config));
                    }

                    var impl = letFunc.Function.Declaration.Value;
                    var header = impl.Name.Value;

                    if (impl.Arguments.Count > 0)
                    {
                        header += " " + string.Join(" ", impl.Arguments.Select(a => RenderPattern(a.Value)));
                    }

                    header += " =";

                    yield return new IndentedLine(indentSpaces, header);

                    // Render the expression body
                    foreach (var line in RenderExpressionLines(impl.Expression.Value, config, indent + 1))
                    {
                        yield return line;
                    }
                }
                break;

            case Expression.LetDeclaration.LetDestructuring letDestr:
                {
                    var header = RenderPattern(letDestr.Pattern.Value) + " =";

                    yield return new IndentedLine(indentSpaces, header);

                    // Render the expression body
                    foreach (var line in RenderExpressionLines(letDestr.Expression.Value, config, indent + 1))
                    {
                        yield return line;
                    }
                }
                break;

            default:
                throw new NotImplementedException($"Unknown let declaration type: {letDecl.GetType().Name}");
        }
    }

    /// <summary>
    /// Renders a record expression inside a list with proper prefix handling.
    /// </summary>
    private static IEnumerable<IndentedLine> RenderRecordInList(Expression.RecordExpr recordExpr, Config config, int indent, string prefix)
    {
        var indentSpaces = indent * 4;

        for (var i = 0; i < recordExpr.Fields.Count; i++)
        {
            var field = recordExpr.Fields[i].Value;
            var fieldPrefix = i is 0 ? prefix + "{ " : "  , ";
            var fieldValue = field.valueExpr.Value;

            // Check if the field value needs multi-line rendering
            if (GetExpressionComplexity(fieldValue, config) is ExpressionComplexity.Complex)
            {
                yield return new IndentedLine(indentSpaces, fieldPrefix + field.fieldName.Value + " =");
                foreach (var line in RenderExpressionLines(fieldValue, config, indent + 2))
                {
                    yield return line;
                }
            }
            else
            {
                yield return new IndentedLine(indentSpaces, fieldPrefix + field.fieldName.Value + " = " + RenderExpression(fieldValue));
            }
        }

        yield return new IndentedLine(indentSpaces, "  }");
    }

    /// <summary>
    /// Renders a nested list with complex expressions in multi-line format.
    /// </summary>
    private static IEnumerable<IndentedLine> RenderNestedMultiLineList(Expression.ListExpr nestedList, int outerIndent, string outerPrefix)
    {
        var outerIndentSpaces = outerIndent * 4;

        for (var i = 0; i < nestedList.Elements.Count; i++)
        {
            var item = nestedList.Elements[i].Value;
            var innerPrefix = i is 0 ? "[ " : ", ";

            if (i is 0)
            {
                // First item of nested list
                if (item is Expression.Application app && ApplicationHasListArgument(app))
                {
                    yield return new IndentedLine(outerIndentSpaces, outerPrefix + innerPrefix + RenderExpression(app.Arguments[0].Value));
                    // Application arguments get extra indent
                    for (var j = 1; j < app.Arguments.Count; j++)
                    {
                        yield return new IndentedLine(outerIndentSpaces + outerPrefix.Length + innerPrefix.Length + 4, RenderExpressionParenthesizedIfNeeded(app.Arguments[j].Value));
                    }
                }
                else
                {
                    yield return new IndentedLine(outerIndentSpaces, outerPrefix + innerPrefix + RenderExpression(item));
                }
            }
            else
            {
                // Subsequent items of nested list (indented 2 more spaces)
                var nestedIndent = outerIndentSpaces + 2;
                if (item is Expression.Application app && ApplicationHasListArgument(app))
                {
                    yield return new IndentedLine(nestedIndent, innerPrefix + RenderExpression(app.Arguments[0].Value));
                    for (var j = 1; j < app.Arguments.Count; j++)
                    {
                        yield return new IndentedLine(nestedIndent + innerPrefix.Length + 4, RenderExpressionParenthesizedIfNeeded(app.Arguments[j].Value));
                    }
                }
                else
                {
                    yield return new IndentedLine(nestedIndent, innerPrefix + RenderExpression(item));
                }
            }
        }

        // Close nested list (indented 2 more spaces from outer)
        yield return new IndentedLine(outerIndentSpaces + 2, "]");
    }

    private static string RenderExpression(Expression expression)
    {
        return expression switch
        {
            Expression.UnitExpr => "()",

            Expression.Literal literal =>
            RenderStringLiteral(literal.Value),

            Expression.CharLiteral charLiteral =>
            RenderCharLiteral(charLiteral.Value),

            Expression.Integer integer =>
            integer.Value.ToString(),

            Expression.Hex hex =>
            "0x" + hex.Value.ToString("X"),

            Expression.Floatable floatable =>
            FormatFloatForElm(floatable.Value),

            Expression.Negation negation =>
            "-" + RenderExpressionParenthesizedIfNeeded(negation.Expression.Value),

            Expression.ListExpr listExpr =>
            listExpr.Elements.Count is 0
            ?
            "[]"
            :
            "[ " + string.Join(", ", listExpr.Elements.Select(e => RenderExpression(e.Value))) + " ]",

            Expression.FunctionOrValue funcOrVal =>
            funcOrVal.ModuleName.Count > 0
            ?
            RenderModuleName(funcOrVal.ModuleName) + "." + funcOrVal.Name
            :
            funcOrVal.Name,

            Expression.IfBlock ifBlock =>
            "if " + RenderExpression(ifBlock.Condition.Value) +
            " then " + RenderExpression(ifBlock.ThenBlock.Value) +
            " else " + RenderExpression(ifBlock.ElseBlock.Value),

            Expression.PrefixOperator prefixOp => "(" + prefixOp.Operator + ")",

            Expression.ParenthesizedExpression paren =>
                "(" + RenderExpression(paren.Expression.Value) + ")",

            Expression.Application app =>
                string.Join(" ", app.Arguments.Select(a => RenderExpressionParenthesizedIfNeeded(a.Value))),

            Expression.OperatorApplication opApp =>
                RenderExpression(opApp.Left.Value) + " " + opApp.Operator + " " + RenderExpression(opApp.Right.Value),

            Expression.TupledExpression tupled =>
                "( " + string.Join(", ", tupled.Elements.Select(e => RenderExpression(e.Value))) + " )",

            Expression.LambdaExpression lambda =>
                "\\" + string.Join(" ", lambda.Lambda.Arguments.Select(a => RenderPattern(a.Value))) +
                " -> " + RenderExpression(lambda.Lambda.Expression.Value),

            Expression.CaseExpression caseExpr =>
                RenderCaseExpression(caseExpr.CaseBlock),

            Expression.LetExpression letExpr =>
                RenderLetExpression(letExpr.Value),

            Expression.RecordExpr recordExpr =>
                RenderRecordExpr(recordExpr),

            Expression.RecordAccess recordAccess =>
                RenderExpressionParenthesizedIfNeeded(recordAccess.Record.Value) + "." + recordAccess.FieldName.Value,

            Expression.RecordAccessFunction accessFunc => accessFunc.FunctionName,

            Expression.RecordUpdateExpression recordUpdate =>
                "{ " + recordUpdate.RecordName.Value + " | " +
                string.Join(", ", recordUpdate.Fields.Select(f =>
                    f.Value.fieldName.Value + " = " + RenderExpression(f.Value.valueExpr.Value))) + " }",

            _ =>
            throw new NotImplementedException(
                $"Unknown expression type: {expression.GetType().Name}")
        };
    }

    private static string RenderExpressionParenthesizedIfNeeded(Expression expression)
    {
        // Wrap complex expressions in parentheses when they appear as arguments
        return expression switch
        {
            Expression.Application =>
            "(" + RenderExpression(expression) + ")",

            Expression.OperatorApplication =>
            "(" + RenderExpression(expression) + ")",

            Expression.IfBlock =>
            "(" + RenderExpression(expression) + ")",

            Expression.CaseExpression =>
            "(" + RenderExpression(expression) + ")",

            Expression.LetExpression =>
            "(" + RenderExpression(expression) + ")",

            Expression.LambdaExpression =>
            "(" + RenderExpression(expression) + ")",
            Expression.Negation => "(" + RenderExpression(expression) + ")",

            _ =>
            RenderExpression(expression)
        };
    }

    private static string RenderStringLiteral(string value)
    {
        // Check if multi-line string
        if (value.Contains('\n'))
        {
            return "\"\"\"" + value + "\"\"\"";
        }

        // Escape special characters
        var escaped = value
            .Replace("\\", "\\\\")
            .Replace("\"", "\\\"")
            .Replace("\n", "\\n")
            .Replace("\r", "\\r")
            .Replace("\t", "\\t");

        return "\"" + escaped + "\"";
    }

    private static string RenderCharLiteral(int value)
    {
        var c = char.ConvertFromUtf32(value);

        if (c is "'")
            return "'\\''";

        if (c is "\\")
            return "'\\\\'";

        if (c is "\n")
            return "'\\n'";

        if (c is "\r")
            return "'\\r'";

        if (c is "\t")
            return "'\\t'";

        return "'" + c + "'";
    }

    private static string FormatFloatForElm(double value)
    {
        // Use "R" (round-trip) format which produces the shortest representation
        // that parses back to the same value
        var str = value.ToString("R", System.Globalization.CultureInfo.InvariantCulture);

        if (str.Contains('E') || str.Contains('e'))
        {
            // Convert scientific notation to fixed-point decimal
            // The format "F17" uses fixed-point with up to 17 digits
            str = value.ToString("F17", System.Globalization.CultureInfo.InvariantCulture).TrimEnd('0');

            // Make sure we don't end with just a decimal point
            if (str.EndsWith('.'))
            {
                str += "0";
            }
        }

        // Elm requires at least one digit after the decimal point
        if (!str.Contains('.'))
        {
            str += ".0";
        }

        return str;
    }

    private static string RenderCaseExpression(CaseBlock caseBlock)
    {
        var sb = new StringBuilder();
        sb.Append("case ");
        sb.Append(RenderExpression(caseBlock.Expression.Value));
        sb.Append(" of ");

        for (var i = 0; i < caseBlock.Cases.Count; i++)
        {
            var case_ = caseBlock.Cases[i];
            sb.Append(RenderPattern(case_.Pattern.Value));
            sb.Append(" -> ");
            sb.Append(RenderExpression(case_.Expression.Value));

            if (i < caseBlock.Cases.Count - 1)
            {
                sb.Append(' ');
            }
        }

        return sb.ToString();
    }

    private static string RenderLetExpression(Expression.LetBlock letBlock)
    {
        var sb = new StringBuilder();
        sb.Append("let ");

        foreach (var decl in letBlock.Declarations)
        {
            sb.Append(RenderLetDeclaration(decl.Value));
            sb.Append(' ');
        }

        sb.Append("in ");
        sb.Append(RenderExpression(letBlock.Expression.Value));

        return sb.ToString();
    }

    private static string RenderLetDeclaration(Expression.LetDeclaration letDecl)
    {
        return letDecl switch
        {
            Expression.LetDeclaration.LetFunction letFunc =>
                RenderLetFunction(letFunc),

            Expression.LetDeclaration.LetDestructuring letDestr =>
                RenderPattern(letDestr.Pattern.Value) + " = " + RenderExpression(letDestr.Expression.Value),

            _ => throw new NotImplementedException($"Unknown let declaration type: {letDecl.GetType().Name}")
        };
    }

    private static string RenderLetFunction(Expression.LetDeclaration.LetFunction letFunc)
    {
        var impl = letFunc.Function.Declaration.Value;
        var header = impl.Name.Value;
        if (impl.Arguments.Count > 0)
        {
            header += " " + string.Join(" ", impl.Arguments.Select(a => RenderPattern(a.Value)));
        }
        return header + " = " + RenderExpression(impl.Expression.Value);
    }

    private static string RenderRecordExpr(Expression.RecordExpr recordExpr)
    {
        if (recordExpr.Fields.Count is 0)
            return "{}";

        return "{ " + string.Join(", ", recordExpr.Fields.Select(f =>
            f.Value.fieldName.Value + " = " + RenderExpression(f.Value.valueExpr.Value))) + " }";
    }

    private static string RenderPattern(Pattern pattern)
    {
        return pattern switch
        {
            Pattern.AllPattern => "_",

            Pattern.VarPattern varPat => varPat.Name,

            Pattern.UnitPattern => "()",

            Pattern.CharPattern charPat => RenderCharLiteral(charPat.Value),

            Pattern.StringPattern stringPat => RenderStringLiteral(stringPat.Value),

            Pattern.IntPattern intPat => intPat.Value.ToString(),

            Pattern.HexPattern hexPat => "0x" + hexPat.Value.ToString("X"),

            Pattern.FloatPattern floatPat => floatPat.Value.ToString(System.Globalization.CultureInfo.InvariantCulture),

            Pattern.TuplePattern tuplePat =>
                "( " + string.Join(", ", tuplePat.Elements.Select(e => RenderPattern(e.Value))) + " )",

            Pattern.RecordPattern recordPat =>
                "{ " + string.Join(", ", recordPat.Fields.Select(f => f.Value)) + " }",

            Pattern.UnConsPattern unconsPat =>
                RenderPatternParenthesizedIfNeeded(unconsPat.Head.Value) + " :: " + RenderPattern(unconsPat.Tail.Value),

            Pattern.ListPattern listPat =>
                listPat.Elements.Count is 0
                    ? "[]"
                    : "[ " + string.Join(", ", listPat.Elements.Select(e => RenderPattern(e.Value))) + " ]",

            Pattern.NamedPattern namedPat =>
                RenderNamedPattern(namedPat),

            Pattern.AsPattern asPat =>
                RenderPatternParenthesizedIfNeeded(asPat.Pattern.Value) + " as " + asPat.Name.Value,

            Pattern.ParenthesizedPattern parenPat =>
                "(" + RenderPattern(parenPat.Pattern.Value) + ")",

            _ => throw new NotImplementedException($"Unknown pattern type: {pattern.GetType().Name}")
        };
    }

    private static string RenderNamedPattern(Pattern.NamedPattern namedPat)
    {
        var name = namedPat.Name.ModuleName.Count > 0
            ? RenderModuleName(namedPat.Name.ModuleName) + "." + namedPat.Name.Name
            : namedPat.Name.Name;

        if (namedPat.Arguments.Count is 0)
            return name;

        return name + " " + string.Join(" ", namedPat.Arguments.Select(a => RenderPatternParenthesizedIfNeeded(a.Value)));
    }

    private static string RenderPatternParenthesizedIfNeeded(Pattern pattern)
    {
        return pattern switch
        {
            Pattern.NamedPattern namedPat when namedPat.Arguments.Count > 0 =>
                "(" + RenderPattern(pattern) + ")",
            Pattern.AsPattern => "(" + RenderPattern(pattern) + ")",
            Pattern.UnConsPattern => "(" + RenderPattern(pattern) + ")",
            _ => RenderPattern(pattern)
        };
    }
}
