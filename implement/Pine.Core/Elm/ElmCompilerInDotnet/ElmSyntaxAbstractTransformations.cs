using System;
using System.Collections.Generic;
using System.Linq;

using SyntaxTypes = Pine.Core.Elm.ElmSyntax.ElmSyntaxAbstract;

namespace Pine.Core.Elm.ElmCompilerInDotnet;

/// <summary>
/// Syntax transformations that operate on the <see cref="ElmSyntax.ElmSyntaxAbstract"/> model.
/// </summary>
internal static class ElmSyntaxAbstractTransformations
{
    /// <summary>
    /// Rebuilds an expression by applying <paramref name="mapChild"/> to all immediate child
    /// expression nodes. This centralizes the expression-variant reconstruction pattern for
    /// tree-mapping operations (substitution, rewriting, flattening). Leaf expressions
    /// (<see cref="SyntaxTypes.Expression.Identifier"/>, literals, etc.) are returned
    /// unchanged.
    /// </summary>
    public static SyntaxTypes.Expression MapChildExpressions(
        SyntaxTypes.Expression expr,
        Func<SyntaxTypes.Expression, SyntaxTypes.Expression> mapChild)
    {
        switch (expr)
        {
            case SyntaxTypes.Expression.Application app:
                return
                    new SyntaxTypes.Expression.Application(
                        mapChild(app.Function),
                        [.. app.Arguments.Select(mapChild)]);

            case SyntaxTypes.Expression.IfBlock ifBlock:
                return
                    new SyntaxTypes.Expression.IfBlock(
                        mapChild(ifBlock.Condition),
                        mapChild(ifBlock.ThenBlock),
                        mapChild(ifBlock.ElseBlock));

            case SyntaxTypes.Expression.CaseExpression caseExpr:
                return
                    new SyntaxTypes.Expression.CaseExpression(
                        mapChild(caseExpr.Expression),
                        [
                        .. caseExpr.Cases.Select(
                            c => new SyntaxTypes.Case(c.Pattern, mapChild(c.Expression)))
                        ]);

            case SyntaxTypes.Expression.LetExpression letExpr:
                return
                    new SyntaxTypes.Expression.LetExpression(
                        [
                        .. letExpr.Declarations.Select(
                            d => d switch
                            {
                                SyntaxTypes.LetDeclaration.LetFunction letFunc =>
                                (SyntaxTypes.LetDeclaration)new SyntaxTypes.LetDeclaration.LetFunction(
                                    letFunc.Function with
                                    {
                                        Declaration =
                                        new SyntaxTypes.FunctionImplementation(
                                            letFunc.Function.Declaration.Name,
                                            letFunc.Function.Declaration.Arguments,
                                            mapChild(letFunc.Function.Declaration.Expression))
                                    }),

                                SyntaxTypes.LetDeclaration.LetDestructuring letDestr =>
                                new SyntaxTypes.LetDeclaration.LetDestructuring(
                                    letDestr.Pattern,
                                    mapChild(letDestr.Expression)),

                                _ =>
                                throw new NotImplementedException(
                                    "MapChildExpressions does not handle let declaration variant: " + d.GetType().Name)
                            })
                        ],
                        mapChild(letExpr.Expression));

            case SyntaxTypes.Expression.LambdaExpression lambda:
                return
                    new SyntaxTypes.Expression.LambdaExpression(
                        lambda.Arguments,
                        mapChild(lambda.Expression));

            case SyntaxTypes.Expression.ListExpr listExpr:
                return
                    new SyntaxTypes.Expression.ListExpr(
                        [.. listExpr.Elements.Select(mapChild)]);

            case SyntaxTypes.Expression.TupledExpression tupled:
                return
                    new SyntaxTypes.Expression.TupledExpression(
                        [.. tupled.Elements.Select(mapChild)]);

            case SyntaxTypes.Expression.RecordExpr recordExpr:
                return
                    new SyntaxTypes.Expression.RecordExpr(
                        [
                        .. recordExpr.Fields.Select(
                            f => f with { Value = mapChild(f.Value) })
                        ]);

            case SyntaxTypes.Expression.RecordUpdateExpression recordUpdate:
                return
                    new SyntaxTypes.Expression.RecordUpdateExpression(
                        recordUpdate.RecordName,
                        [
                        .. recordUpdate.Fields.Select(
                            f => f with { Value = mapChild(f.Value) })
                        ]);

            case SyntaxTypes.Expression.RecordAccess recordAccess:
                return recordAccess with { Record = mapChild(recordAccess.Record) };

            case SyntaxTypes.Expression.Negation negation:
                return new SyntaxTypes.Expression.Negation(mapChild(negation.Expression));

            case SyntaxTypes.Expression.OperatorApplication opApp:
                return
                    new SyntaxTypes.Expression.OperatorApplication(
                        opApp.Operator,
                        opApp.Direction,
                        mapChild(opApp.Left),
                        mapChild(opApp.Right));

            // Leaf expression variants have no child expressions to map; return them
            // unchanged. They are listed explicitly so that the throwing default below never
            // fires for a valid expression value.
            case SyntaxTypes.Expression.UnitExpr:
            case SyntaxTypes.Expression.StringLiteral:
            case SyntaxTypes.Expression.CharLiteral:
            case SyntaxTypes.Expression.IntegerLiteral:
            case SyntaxTypes.Expression.FloatLiteral:
            case SyntaxTypes.Expression.Identifier:
            case SyntaxTypes.Expression.PrefixOperator:
            case SyntaxTypes.Expression.RecordAccessFunction:
            case SyntaxTypes.Expression.GLSLExpression:
                return expr;

            default:
                throw new NotImplementedException(
                    "MapChildExpressions does not handle expression variant: " + expr.GetType().Name);
        }
    }

    /// <summary>
    /// Recursively walks the expression tree and replaces every
    /// <see cref="SyntaxTypes.Expression.Application"/> whose head
    /// (<see cref="SyntaxTypes.Expression.Application.Function"/>) is itself an
    /// <see cref="SyntaxTypes.Expression.Application"/> with the equivalent flat form.
    /// <para>
    /// Flattening lets
    /// <see cref="ExpressionCompiler.CompileApplication"/> take the direct-call fast path
    /// instead of allocating a closure for a partial application.
    /// </para>
    /// </summary>
    public static SyntaxTypes.Expression FlattenAllNestedApplicationHeads(
        SyntaxTypes.Expression expr)
    {
        var withChildrenFlattened = MapChildExpressions(expr, FlattenAllNestedApplicationHeads);

        if (withChildrenFlattened is SyntaxTypes.Expression.Application app &&
            app.Function is SyntaxTypes.Expression.Application innerApp)
        {
            // The child pass already flattened innerApp, so innerApp.Function is guaranteed
            // not to be an Application. A single splice therefore yields the flat form.
            var combinedArguments =
                new List<SyntaxTypes.Expression>(innerApp.Arguments.Count + app.Arguments.Count);

            combinedArguments.AddRange(innerApp.Arguments);
            combinedArguments.AddRange(app.Arguments);

            return new SyntaxTypes.Expression.Application(innerApp.Function, combinedArguments);
        }

        return withChildrenFlattened;
    }

    /// <summary>
    /// Generates the first available suffixed name without changing <paramref name="usedNames"/>.
    /// </summary>
    internal static string GenerateUniqueLocalName(string baseName, ISet<string> usedNames)
    {
        for (var suffix = 0; ; suffix++)
        {
            var candidate = baseName + "_" + suffix;

            if (!usedNames.Contains(candidate))
                return candidate;
        }
    }

    /// <summary>
    /// Renames local bindings and their references to avoid names already visible to the caller.
    /// Qualified references and record field labels are not variable references.
    /// </summary>
    internal static SyntaxTypes.FunctionImplementation RenameBindingsAvoidingCapture(
        SyntaxTypes.FunctionImplementation implementation,
        IReadOnlySet<string> namesAlreadyInScope)
    {
        var scope = new HashSet<string>(namesAlreadyInScope, StringComparer.Ordinal);
        var reserved = CollectReservedNames(implementation.Expression, namesAlreadyInScope);
        reserved.UnionWith(SyntaxTypes.SyntaxAnalysis.CollectNamesBoundByPatterns(implementation.Arguments));
        var renames = new Dictionary<string, string>(StringComparer.Ordinal);
        var extractedBindings = new List<SyntaxTypes.LetDeclaration>();

        var arguments =
            implementation.Arguments.Select(
                argument => RenamePatternBindings(argument, scope, reserved, renames, extractedBindings)).ToArray();

        return
            new SyntaxTypes.FunctionImplementation(
                implementation.Name,
                arguments,
                WrapExtractedBindings(
                    RenameExpressionBindings(implementation.Expression, renames, scope, reserved),
                    extractedBindings));
    }

    /// <summary>
    /// Renames bindings within an expression without changing its free variable references.
    /// </summary>
    internal static SyntaxTypes.Expression RenameBindingsAvoidingCapture(
        SyntaxTypes.Expression expression,
        IReadOnlySet<string> namesAlreadyInScope) =>
        RenameExpressionBindings(
            expression,
            new Dictionary<string, string>(StringComparer.Ordinal),
            namesAlreadyInScope,
            CollectReservedNames(expression, namesAlreadyInScope));

    private static HashSet<string> CollectReservedNames(
        SyntaxTypes.Expression expression,
        IReadOnlySet<string> namesAlreadyInScope)
    {
        var names = new HashSet<string>(namesAlreadyInScope, StringComparer.Ordinal);

        SyntaxTypes.Expression Collect(SyntaxTypes.Expression current)
        {
            if (current is SyntaxTypes.Expression.Identifier identifier &&
                identifier.QualifiedName.Namespaces.Count is 0)
            {
                names.Add(identifier.QualifiedName.DeclName);
            }

            if (current is SyntaxTypes.Expression.RecordUpdateExpression recordUpdate)
                names.Add(recordUpdate.RecordName);

            if (current is SyntaxTypes.Expression.LambdaExpression lambda)
                names.UnionWith(SyntaxTypes.SyntaxAnalysis.CollectNamesBoundByPatterns(lambda.Arguments));

            if (current is SyntaxTypes.Expression.CaseExpression caseExpression)
            {
                foreach (var branch in caseExpression.Cases)
                    names.UnionWith(SyntaxTypes.SyntaxAnalysis.CollectNamesBoundByPattern(branch.Pattern));
            }

            if (current is SyntaxTypes.Expression.LetExpression letExpression)
            {
                foreach (var declaration in letExpression.Declarations)
                {
                    switch (declaration)
                    {
                        case SyntaxTypes.LetDeclaration.LetFunction function:
                            names.Add(function.Function.Declaration.Name);

                            names.UnionWith(
                                SyntaxTypes.SyntaxAnalysis.CollectNamesBoundByPatterns(
                                    function.Function.Declaration.Arguments));

                            break;

                        case SyntaxTypes.LetDeclaration.LetDestructuring destructuring:
                            names.UnionWith(
                                SyntaxTypes.SyntaxAnalysis.CollectNamesBoundByPattern(destructuring.Pattern));

                            break;

                        default:
                            throw new NotImplementedException(
                                "CollectReservedNames does not handle let declaration variant: " +
                                declaration.GetType().Name);
                    }
                }
            }

            return MapChildExpressions(current, Collect);
        }

        Collect(expression);
        return names;
    }

    private static string RenameBinding(
        string name,
        HashSet<string> scope,
        HashSet<string> reserved,
        Dictionary<string, string> renames)
    {
        var chosenName = scope.Contains(name) ? GenerateUniqueLocalName(name, reserved) : name;
        scope.Add(chosenName);
        reserved.Add(chosenName);
        renames[name] = chosenName;
        return chosenName;
    }

    private static SyntaxTypes.Pattern RenamePatternBindings(
        SyntaxTypes.Pattern pattern,
        HashSet<string> scope,
        HashSet<string> reserved,
        Dictionary<string, string> renames,
        List<SyntaxTypes.LetDeclaration> extractedBindings)
    {
        SyntaxTypes.Pattern Recurse(SyntaxTypes.Pattern child) =>
            RenamePatternBindings(child, scope, reserved, renames, extractedBindings);

        switch (pattern)
        {
            case SyntaxTypes.Pattern.VarPattern variable:
                return
                    new SyntaxTypes.Pattern.VarPattern(
                        RenameBinding(variable.Name, scope, reserved, renames));

            case SyntaxTypes.Pattern.TuplePattern tuple:
                return new SyntaxTypes.Pattern.TuplePattern([.. tuple.Elements.Select(Recurse)]);

            case SyntaxTypes.Pattern.RecordPattern record:
                {
                    if (!record.Fields.Any(field => scope.Contains(field.FieldName)))
                    {
                        foreach (var field in record.Fields)
                            RenameBinding(field.FieldName, scope, reserved, renames);

                        return record;
                    }

                    // Record patterns conflate field labels and binders. Extract the fields
                    // explicitly instead of renaming the labels and selecting different fields.
                    var recordName = GenerateUniqueLocalName("recordPattern", reserved);
                    reserved.Add(recordName);
                    scope.Add(recordName);

                    foreach (var field in record.Fields)
                    {
                        var fieldName = RenameBinding(field.FieldName, scope, reserved, renames);

                        extractedBindings.Add(
                            new SyntaxTypes.LetDeclaration.LetFunction(
                                new SyntaxTypes.FunctionStruct(
                                    null,
                                    new SyntaxTypes.FunctionImplementation(
                                        fieldName,
                                        [],
                                        new SyntaxTypes.Expression.RecordAccess(
                                            SyntaxTypes.Expression.Identifier.Create([], recordName),
                                            field.FieldName,
                                            field.FieldNameValue)))));
                    }

                    return new SyntaxTypes.Pattern.VarPattern(recordName);
                }

            case SyntaxTypes.Pattern.UnConsPattern uncons:
                return new SyntaxTypes.Pattern.UnConsPattern(Recurse(uncons.Head), Recurse(uncons.Tail));

            case SyntaxTypes.Pattern.ListPattern list:
                return new SyntaxTypes.Pattern.ListPattern([.. list.Elements.Select(Recurse)]);

            case SyntaxTypes.Pattern.NamedPattern named:
                return new SyntaxTypes.Pattern.NamedPattern(named.Name, [.. named.Arguments.Select(Recurse)]);

            case SyntaxTypes.Pattern.AsPattern alias:
                return
                    new SyntaxTypes.Pattern.AsPattern(
                        Recurse(alias.Pattern),
                        RenameBinding(alias.Name, scope, reserved, renames));

            case SyntaxTypes.Pattern.AllPattern:
            case SyntaxTypes.Pattern.UnitPattern:
            case SyntaxTypes.Pattern.CharPattern:
            case SyntaxTypes.Pattern.StringPattern:
            case SyntaxTypes.Pattern.IntPattern:
            case SyntaxTypes.Pattern.FloatPattern:
                return pattern;

            default:
                throw new NotImplementedException(
                    "RenamePatternBindings does not handle pattern variant: " + pattern.GetType().Name);
        }
    }

    private static SyntaxTypes.Expression WrapExtractedBindings(
        SyntaxTypes.Expression expression,
        IReadOnlyList<SyntaxTypes.LetDeclaration> extractedBindings) =>
        extractedBindings.Count is 0
        ?
        expression
        :
        new SyntaxTypes.Expression.LetExpression(extractedBindings, expression);

    private static SyntaxTypes.Expression RenameExpressionBindings(
        SyntaxTypes.Expression expression,
        IReadOnlyDictionary<string, string> renames,
        IReadOnlySet<string> scope,
        HashSet<string> reserved)
    {
        SyntaxTypes.Expression Recurse(SyntaxTypes.Expression child) =>
            RenameExpressionBindings(child, renames, scope, reserved);

        switch (expression)
        {
            case SyntaxTypes.Expression.Identifier identifier:
                return
                    identifier.QualifiedName.Namespaces.Count is 0 &&
                    renames.TryGetValue(identifier.QualifiedName.DeclName, out var renamed)
                    ?
                    SyntaxTypes.Expression.Identifier.Create([], renamed)
                    :
                    expression;

            case SyntaxTypes.Expression.LambdaExpression lambda:
                {
                    var innerScope = new HashSet<string>(scope, StringComparer.Ordinal);
                    var innerRenames = new Dictionary<string, string>(renames, StringComparer.Ordinal);
                    var extractedBindings = new List<SyntaxTypes.LetDeclaration>();

                    var arguments =
                        lambda.Arguments.Select(
                            argument => RenamePatternBindings(
                                argument,
                                innerScope,
                                reserved,
                                innerRenames,
                                extractedBindings)).ToArray();

                    return
                        new SyntaxTypes.Expression.LambdaExpression(
                            arguments,
                            WrapExtractedBindings(
                                RenameExpressionBindings(lambda.Expression, innerRenames, innerScope, reserved),
                                extractedBindings));
                }

            case SyntaxTypes.Expression.CaseExpression caseExpression:
                return
                    new SyntaxTypes.Expression.CaseExpression(
                        Recurse(caseExpression.Expression),
                        [
                        .. caseExpression.Cases.Select(
                            branch =>
                            {
                                var innerScope = new HashSet<string>(scope, StringComparer.Ordinal);
                                var innerRenames = new Dictionary<string, string>(renames, StringComparer.Ordinal);
                                var extractedBindings = new List<SyntaxTypes.LetDeclaration>();

                                var pattern =
                                    RenamePatternBindings(
                                        branch.Pattern,
                                        innerScope,
                                        reserved,
                                        innerRenames,
                                        extractedBindings);

                                return
                                    new SyntaxTypes.Case(
                                        pattern,
                                        WrapExtractedBindings(
                                            RenameExpressionBindings(branch.Expression, innerRenames, innerScope, reserved),
                                            extractedBindings));
                            })
                        ]);

            case SyntaxTypes.Expression.LetExpression letExpression:
                {
                    var innerScope = new HashSet<string>(scope, StringComparer.Ordinal);
                    var innerRenames = new Dictionary<string, string>(renames, StringComparer.Ordinal);
                    var declarations = new List<SyntaxTypes.LetDeclaration>();
                    var extractedBindings = new List<SyntaxTypes.LetDeclaration>();

                    // All let names are visible in every declaration, including mutual recursion.
                    foreach (var declaration in letExpression.Declarations)
                    {
                        declarations.Add(
                            declaration switch
                            {
                                SyntaxTypes.LetDeclaration.LetFunction function =>
                                new SyntaxTypes.LetDeclaration.LetFunction(
                                    new SyntaxTypes.FunctionStruct(
                                        function.Function.Signature,
                                        new SyntaxTypes.FunctionImplementation(
                                            RenameBinding(
                                                function.Function.Declaration.Name,
                                                innerScope,
                                                reserved,
                                                innerRenames),
                                            function.Function.Declaration.Arguments,
                                            function.Function.Declaration.Expression))),

                                SyntaxTypes.LetDeclaration.LetDestructuring destructuring =>
                                new SyntaxTypes.LetDeclaration.LetDestructuring(
                                    RenamePatternBindings(
                                        destructuring.Pattern,
                                        innerScope,
                                        reserved,
                                        innerRenames,
                                        extractedBindings),
                                    destructuring.Expression),

                                _ =>
                                throw new NotImplementedException(
                                    "RenameExpressionBindings does not handle let declaration variant: " +
                                    declaration.GetType().Name)
                            });
                    }

                    declarations.AddRange(extractedBindings);

                    var freshDeclarations =
                        declarations.Select(
                            declaration =>
                            {
                                switch (declaration)
                                {
                                    case SyntaxTypes.LetDeclaration.LetFunction function:
                                        var implementation = function.Function.Declaration;
                                        var functionScope = new HashSet<string>(innerScope, StringComparer.Ordinal);

                                        var functionRenames =
                                            new Dictionary<string, string>(innerRenames, StringComparer.Ordinal);

                                        var functionExtractedBindings = new List<SyntaxTypes.LetDeclaration>();

                                        var arguments =
                                            implementation.Arguments.Select(
                                                argument => RenamePatternBindings(
                                                    argument,
                                                    functionScope,
                                                    reserved,
                                                    functionRenames,
                                                    functionExtractedBindings)).ToArray();

                                        return
                                            (SyntaxTypes.LetDeclaration)new SyntaxTypes.LetDeclaration.LetFunction(
                                                new SyntaxTypes.FunctionStruct(
                                                    function.Function.Signature is { } signature
                                                    ?
                                                    signature with { Name = implementation.Name }
                                                    :
                                                    null,
                                                    new SyntaxTypes.FunctionImplementation(
                                                        implementation.Name,
                                                        arguments,
                                                        WrapExtractedBindings(
                                                            RenameExpressionBindings(
                                                                implementation.Expression,
                                                                functionRenames,
                                                                functionScope,
                                                                reserved),
                                                            functionExtractedBindings))));

                                    case SyntaxTypes.LetDeclaration.LetDestructuring destructuring:
                                        return
                                            new SyntaxTypes.LetDeclaration.LetDestructuring(
                                                destructuring.Pattern,
                                                RenameExpressionBindings(
                                                    destructuring.Expression,
                                                    innerRenames,
                                                    innerScope,
                                                    reserved));

                                    default:
                                        throw new NotImplementedException(
                                            "RenameExpressionBindings does not handle let declaration variant: " +
                                            declaration.GetType().Name);
                                }
                            }).ToArray();

                    return
                        new SyntaxTypes.Expression.LetExpression(
                            freshDeclarations,
                            RenameExpressionBindings(letExpression.Expression, innerRenames, innerScope, reserved));
                }

            case SyntaxTypes.Expression.RecordUpdateExpression recordUpdate:
                return
                    new SyntaxTypes.Expression.RecordUpdateExpression(
                        renames.TryGetValue(recordUpdate.RecordName, out var recordName)
                        ?
                        recordName
                        :
                        recordUpdate.RecordName,
                        [.. recordUpdate.Fields.Select(field => field with { Value = Recurse(field.Value) })]);

            case SyntaxTypes.Expression.UnitExpr:
            case SyntaxTypes.Expression.StringLiteral:
            case SyntaxTypes.Expression.CharLiteral:
            case SyntaxTypes.Expression.IntegerLiteral:
            case SyntaxTypes.Expression.FloatLiteral:
            case SyntaxTypes.Expression.Application:
            case SyntaxTypes.Expression.IfBlock:
            case SyntaxTypes.Expression.ListExpr:
            case SyntaxTypes.Expression.TupledExpression:
            case SyntaxTypes.Expression.RecordExpr:
            case SyntaxTypes.Expression.RecordAccess:
            case SyntaxTypes.Expression.Negation:
            case SyntaxTypes.Expression.OperatorApplication:
            case SyntaxTypes.Expression.PrefixOperator:
            case SyntaxTypes.Expression.RecordAccessFunction:
            case SyntaxTypes.Expression.GLSLExpression:
                return MapChildExpressions(expression, Recurse);

            default:
                throw new NotImplementedException(
                    "RenameExpressionBindings does not handle expression variant: " + expression.GetType().Name);
        }
    }
}
