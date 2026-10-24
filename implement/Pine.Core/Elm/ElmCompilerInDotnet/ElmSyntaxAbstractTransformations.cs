using Pine.Core.Elm.Syntax.Abstract;
using System;
using System.Collections.Generic;
using System.Linq;

namespace Pine.Core.Elm.ElmCompilerInDotnet;

/// <summary>
/// Syntax transformations that operate on the <see cref="Syntax.Abstract"/> model.
/// </summary>
internal static class ElmSyntaxAbstractTransformations
{
    /// <summary>
    /// Rebuilds an expression by applying <paramref name="mapChild"/> to all immediate child
    /// expression nodes. This centralizes the expression-variant reconstruction pattern for
    /// tree-mapping operations (substitution, rewriting, flattening). Leaf expressions
    /// (<see cref="Expression.Identifier"/>, literals, etc.) are returned
    /// unchanged.
    /// </summary>
    public static Expression MapChildExpressions(
        Expression expr,
        Func<Expression, Expression> mapChild)
    {
        switch (expr)
        {
            case Expression.Application app:
                return
                    new Expression.Application(
                        mapChild(app.Function),
                        [.. app.Arguments.Select(mapChild)]);

            case Expression.IfBlock ifBlock:
                return
                    new Expression.IfBlock(
                        mapChild(ifBlock.Condition),
                        mapChild(ifBlock.ThenBlock),
                        mapChild(ifBlock.ElseBlock));

            case Expression.CaseExpression caseExpr:
                return
                    new Expression.CaseExpression(
                        mapChild(caseExpr.Expression),
                        [
                        .. caseExpr.Cases.Select(
                            c => new Case(c.Pattern, mapChild(c.Expression)))
                        ]);

            case Expression.LetExpression letExpr:
                return
                    new Expression.LetExpression(
                        [
                        .. letExpr.Declarations.Select(
                            d => d switch
                            {
                                LetDeclaration.LetFunction letFunc =>
                                (LetDeclaration)new LetDeclaration.LetFunction(
                                    letFunc.Function with
                                    {
                                        Declaration =
                                        new FunctionImplementation(
                                            letFunc.Function.Declaration.Name,
                                            letFunc.Function.Declaration.Arguments,
                                            mapChild(letFunc.Function.Declaration.Expression))
                                    }),

                                LetDeclaration.LetDestructuring letDestr =>
                                new LetDeclaration.LetDestructuring(
                                    letDestr.Pattern,
                                    mapChild(letDestr.Expression)),

                                _ =>
                                throw new NotImplementedException(
                                    "MapChildExpressions does not handle let declaration variant: " + d.GetType().Name)
                            })
                        ],
                        mapChild(letExpr.Expression));

            case Expression.LambdaExpression lambda:
                return
                    new Expression.LambdaExpression(
                        lambda.Arguments,
                        mapChild(lambda.Expression));

            case Expression.ListExpr listExpr:
                return
                    new Expression.ListExpr(
                        [.. listExpr.Elements.Select(mapChild)]);

            case Expression.TupledExpression tupled:
                return
                    new Expression.TupledExpression(
                        [.. tupled.Elements.Select(mapChild)]);

            case Expression.RecordExpr recordExpr:
                return
                    new Expression.RecordExpr(
                        [
                        .. recordExpr.Fields.Select(
                            f => f with { Value = mapChild(f.Value) })
                        ]);

            case Expression.RecordUpdateExpression recordUpdate:
                return
                    new Expression.RecordUpdateExpression(
                        recordUpdate.RecordName,
                        [
                        .. recordUpdate.Fields.Select(
                            f => f with { Value = mapChild(f.Value) })
                        ]);

            case Expression.RecordAccess recordAccess:
                return recordAccess with { Record = mapChild(recordAccess.Record) };

            case Expression.Negation negation:
                return new Expression.Negation(mapChild(negation.Expression));

            case Expression.OperatorApplication opApp:
                return
                    new Expression.OperatorApplication(
                        opApp.Operator,
                        opApp.Direction,
                        mapChild(opApp.Left),
                        mapChild(opApp.Right));

            // Leaf expression variants have no child expressions to map; return them
            // unchanged. They are listed explicitly so that the throwing default below never
            // fires for a valid expression value.
            case Expression.UnitExpr:
            case Expression.StringLiteral:
            case Expression.CharLiteral:
            case Expression.IntegerLiteral:
            case Expression.FloatLiteral:
            case Expression.Identifier:
            case Expression.PrefixOperator:
            case Expression.RecordAccessFunction:
            case Expression.GLSLExpression:
                return expr;

            default:
                throw new NotImplementedException(
                    "MapChildExpressions does not handle expression variant: " + expr.GetType().Name);
        }
    }

    /// <summary>
    /// Recursively walks the expression tree and replaces every
    /// <see cref="Expression.Application"/> whose head
    /// (<see cref="Expression.Application.Function"/>) is itself an
    /// <see cref="Expression.Application"/> with the equivalent flat form.
    /// <para>
    /// Flattening lets
    /// <see cref="ExpressionCompiler.CompileApplication"/> take the direct-call fast path
    /// instead of allocating a closure for a partial application.
    /// </para>
    /// </summary>
    public static Expression FlattenAllNestedApplicationHeads(
        Expression expr)
    {
        var withChildrenFlattened = MapChildExpressions(expr, FlattenAllNestedApplicationHeads);

        if (withChildrenFlattened is Expression.Application app &&
            app.Function is Expression.Application innerApp)
        {
            // The child pass already flattened innerApp, so innerApp.Function is guaranteed
            // not to be an Application. A single splice therefore yields the flat form.
            var combinedArguments =
                new List<Expression>(innerApp.Arguments.Count + app.Arguments.Count);

            combinedArguments.AddRange(innerApp.Arguments);
            combinedArguments.AddRange(app.Arguments);

            return new Expression.Application(innerApp.Function, combinedArguments);
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
    internal static FunctionImplementation RenameBindingsAvoidingCapture(
        FunctionImplementation implementation,
        IReadOnlySet<string> namesAlreadyInScope)
    {
        var scope = new HashSet<string>(namesAlreadyInScope, StringComparer.Ordinal);
        var reserved = CollectReservedNames(implementation.Expression, namesAlreadyInScope);
        reserved.UnionWith(SyntaxAnalysis.CollectNamesBoundByPatterns(implementation.Arguments));
        var renames = new Dictionary<string, string>(StringComparer.Ordinal);
        var extractedBindings = new List<LetDeclaration>();

        var arguments =
            implementation.Arguments.Select(
                argument => RenamePatternBindings(argument, scope, reserved, renames, extractedBindings)).ToArray();

        return
            new FunctionImplementation(
                implementation.Name,
                arguments,
                WrapExtractedBindings(
                    RenameExpressionBindings(implementation.Expression, renames, scope, reserved),
                    extractedBindings));
    }

    /// <summary>
    /// Renames bindings within an expression without changing its free variable references.
    /// </summary>
    internal static Expression RenameBindingsAvoidingCapture(
        Expression expression,
        IReadOnlySet<string> namesAlreadyInScope) =>
        RenameExpressionBindings(
            expression,
            new Dictionary<string, string>(StringComparer.Ordinal),
            namesAlreadyInScope,
            CollectReservedNames(expression, namesAlreadyInScope));

    private static HashSet<string> CollectReservedNames(
        Expression expression,
        IReadOnlySet<string> namesAlreadyInScope)
    {
        var names = new HashSet<string>(namesAlreadyInScope, StringComparer.Ordinal);

        Expression Collect(Expression current)
        {
            if (current is Expression.Identifier identifier &&
                identifier.QualifiedName.Namespaces.Count is 0)
            {
                names.Add(identifier.QualifiedName.DeclName);
            }

            if (current is Expression.RecordUpdateExpression recordUpdate)
                names.Add(recordUpdate.RecordName);

            if (current is Expression.LambdaExpression lambda)
                names.UnionWith(SyntaxAnalysis.CollectNamesBoundByPatterns(lambda.Arguments));

            if (current is Expression.CaseExpression caseExpression)
            {
                foreach (var branch in caseExpression.Cases)
                    names.UnionWith(SyntaxAnalysis.CollectNamesBoundByPattern(branch.Pattern));
            }

            if (current is Expression.LetExpression letExpression)
            {
                foreach (var declaration in letExpression.Declarations)
                {
                    switch (declaration)
                    {
                        case LetDeclaration.LetFunction function:
                            names.Add(function.Function.Declaration.Name);

                            names.UnionWith(
                                SyntaxAnalysis.CollectNamesBoundByPatterns(
                                    function.Function.Declaration.Arguments));

                            break;

                        case LetDeclaration.LetDestructuring destructuring:
                            names.UnionWith(
                                SyntaxAnalysis.CollectNamesBoundByPattern(destructuring.Pattern));

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

    private static Pattern RenamePatternBindings(
        Pattern pattern,
        HashSet<string> scope,
        HashSet<string> reserved,
        Dictionary<string, string> renames,
        List<LetDeclaration> extractedBindings)
    {
        Pattern Recurse(Pattern child) =>
            RenamePatternBindings(child, scope, reserved, renames, extractedBindings);

        switch (pattern)
        {
            case Pattern.VarPattern variable:
                return
                    new Pattern.VarPattern(
                        RenameBinding(variable.Name, scope, reserved, renames));

            case Pattern.TuplePattern tuple:
                return new Pattern.TuplePattern([.. tuple.Elements.Select(Recurse)]);

            case Pattern.RecordPattern record:
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
                            new LetDeclaration.LetFunction(
                                new FunctionStruct(
                                    null,
                                    new FunctionImplementation(
                                        fieldName,
                                        [],
                                        new Expression.RecordAccess(
                                            Expression.Identifier.Create([], recordName),
                                            field.FieldName,
                                            field.FieldNameValue)))));
                    }

                    return new Pattern.VarPattern(recordName);
                }

            case Pattern.UnConsPattern uncons:
                return new Pattern.UnConsPattern(Recurse(uncons.Head), Recurse(uncons.Tail));

            case Pattern.ListPattern list:
                return new Pattern.ListPattern([.. list.Elements.Select(Recurse)]);

            case Pattern.NamedPattern named:
                return new Pattern.NamedPattern(named.Name, [.. named.Arguments.Select(Recurse)]);

            case Pattern.AsPattern alias:
                return
                    new Pattern.AsPattern(
                        Recurse(alias.Pattern),
                        RenameBinding(alias.Name, scope, reserved, renames));

            case Pattern.AllPattern:
            case Pattern.UnitPattern:
            case Pattern.CharPattern:
            case Pattern.StringPattern:
            case Pattern.IntPattern:
            case Pattern.FloatPattern:
                return pattern;

            default:
                throw new NotImplementedException(
                    "RenamePatternBindings does not handle pattern variant: " + pattern.GetType().Name);
        }
    }

    private static Expression WrapExtractedBindings(
        Expression expression,
        IReadOnlyList<LetDeclaration> extractedBindings) =>
        extractedBindings.Count is 0
        ?
        expression
        :
        new Expression.LetExpression(extractedBindings, expression);

    private static Expression RenameExpressionBindings(
        Expression expression,
        IReadOnlyDictionary<string, string> renames,
        IReadOnlySet<string> scope,
        HashSet<string> reserved)
    {
        Expression Recurse(Expression child) =>
            RenameExpressionBindings(child, renames, scope, reserved);

        switch (expression)
        {
            case Expression.Identifier identifier:
                return
                    identifier.QualifiedName.Namespaces.Count is 0 &&
                    renames.TryGetValue(identifier.QualifiedName.DeclName, out var renamed)
                    ?
                    Expression.Identifier.Create([], renamed)
                    :
                    expression;

            case Expression.LambdaExpression lambda:
                {
                    var innerScope = new HashSet<string>(scope, StringComparer.Ordinal);
                    var innerRenames = new Dictionary<string, string>(renames, StringComparer.Ordinal);
                    var extractedBindings = new List<LetDeclaration>();

                    var arguments =
                        lambda.Arguments.Select(
                            argument => RenamePatternBindings(
                                argument,
                                innerScope,
                                reserved,
                                innerRenames,
                                extractedBindings)).ToArray();

                    return
                        new Expression.LambdaExpression(
                            arguments,
                            WrapExtractedBindings(
                                RenameExpressionBindings(lambda.Expression, innerRenames, innerScope, reserved),
                                extractedBindings));
                }

            case Expression.CaseExpression caseExpression:
                return
                    new Expression.CaseExpression(
                        Recurse(caseExpression.Expression),
                        [
                        .. caseExpression.Cases.Select(
                            branch =>
                            {
                                var innerScope = new HashSet<string>(scope, StringComparer.Ordinal);
                                var innerRenames = new Dictionary<string, string>(renames, StringComparer.Ordinal);
                                var extractedBindings = new List<LetDeclaration>();

                                var pattern =
                                    RenamePatternBindings(
                                        branch.Pattern,
                                        innerScope,
                                        reserved,
                                        innerRenames,
                                        extractedBindings);

                                return
                                    new Case(
                                        pattern,
                                        WrapExtractedBindings(
                                            RenameExpressionBindings(branch.Expression, innerRenames, innerScope, reserved),
                                            extractedBindings));
                            })
                        ]);

            case Expression.LetExpression letExpression:
                {
                    var innerScope = new HashSet<string>(scope, StringComparer.Ordinal);
                    var innerRenames = new Dictionary<string, string>(renames, StringComparer.Ordinal);
                    var declarations = new List<LetDeclaration>();
                    var extractedBindings = new List<LetDeclaration>();

                    // All let names are visible in every declaration, including mutual recursion.
                    foreach (var declaration in letExpression.Declarations)
                    {
                        declarations.Add(
                            declaration switch
                            {
                                LetDeclaration.LetFunction function =>
                                new LetDeclaration.LetFunction(
                                    new FunctionStruct(
                                        function.Function.Signature,
                                        new FunctionImplementation(
                                            RenameBinding(
                                                function.Function.Declaration.Name,
                                                innerScope,
                                                reserved,
                                                innerRenames),
                                            function.Function.Declaration.Arguments,
                                            function.Function.Declaration.Expression))),

                                LetDeclaration.LetDestructuring destructuring =>
                                new LetDeclaration.LetDestructuring(
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
                                    case LetDeclaration.LetFunction function:
                                        var implementation = function.Function.Declaration;
                                        var functionScope = new HashSet<string>(innerScope, StringComparer.Ordinal);

                                        var functionRenames =
                                            new Dictionary<string, string>(innerRenames, StringComparer.Ordinal);

                                        var functionExtractedBindings = new List<LetDeclaration>();

                                        var arguments =
                                            implementation.Arguments.Select(
                                                argument => RenamePatternBindings(
                                                    argument,
                                                    functionScope,
                                                    reserved,
                                                    functionRenames,
                                                    functionExtractedBindings)).ToArray();

                                        return
                                            (LetDeclaration)new LetDeclaration.LetFunction(
                                                new FunctionStruct(
                                                    function.Function.Signature is { } signature
                                                    ?
                                                    signature with { Name = implementation.Name }
                                                    :
                                                    null,
                                                    new FunctionImplementation(
                                                        implementation.Name,
                                                        arguments,
                                                        WrapExtractedBindings(
                                                            RenameExpressionBindings(
                                                                implementation.Expression,
                                                                functionRenames,
                                                                functionScope,
                                                                reserved),
                                                            functionExtractedBindings))));

                                    case LetDeclaration.LetDestructuring destructuring:
                                        return
                                            new LetDeclaration.LetDestructuring(
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
                        new Expression.LetExpression(
                            freshDeclarations,
                            RenameExpressionBindings(letExpression.Expression, innerRenames, innerScope, reserved));
                }

            case Expression.RecordUpdateExpression recordUpdate:
                return
                    new Expression.RecordUpdateExpression(
                        renames.TryGetValue(recordUpdate.RecordName, out var recordName)
                        ?
                        recordName
                        :
                        recordUpdate.RecordName,
                        [.. recordUpdate.Fields.Select(field => field with { Value = Recurse(field.Value) })]);

            case Expression.UnitExpr:
            case Expression.StringLiteral:
            case Expression.CharLiteral:
            case Expression.IntegerLiteral:
            case Expression.FloatLiteral:
            case Expression.Application:
            case Expression.IfBlock:
            case Expression.ListExpr:
            case Expression.TupledExpression:
            case Expression.RecordExpr:
            case Expression.RecordAccess:
            case Expression.Negation:
            case Expression.OperatorApplication:
            case Expression.PrefixOperator:
            case Expression.RecordAccessFunction:
            case Expression.GLSLExpression:
                return MapChildExpressions(expression, Recurse);

            default:
                throw new NotImplementedException(
                    "RenameExpressionBindings does not handle expression variant: " + expression.GetType().Name);
        }
    }
}
