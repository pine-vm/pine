using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using System;
using System.Collections.Generic;
using System.Linq;

namespace Pine.Core.DotNet;

internal static class PineCSharpSourceSimplification
{
    public static string Format(string source, IReadOnlySet<string> retainedLocals, string? literalsTypeName = null)
    {
        var tree = SyntaxFactory.ParseSyntaxTree(source);
        var root = new MethodRewriter(retainedLocals).Visit(tree.GetRoot())!;
        root = new ReferenceRewriter().Visit(root)!;
        var unit = (CompilationUnitSyntax)root;

        if (literalsTypeName is not null &&
            unit.DescendantNodes().OfType<MemberAccessExpressionSyntax>()
            .Any(member => member.Expression.ToString() == "global::" + literalsTypeName))
        {
            var nodes = unit.DescendantNodes().ToArray();

            var declared =
                nodes.OfType<MethodDeclarationSyntax>().Select(method => method.Identifier.ValueText)
                .Concat(nodes.OfType<BaseTypeDeclarationSyntax>().Select(type => type.Identifier.ValueText))
                .Concat(nodes.OfType<ParameterSyntax>().Select(parameter => parameter.Identifier.ValueText))
                .Concat(nodes.OfType<VariableDeclaratorSyntax>().Select(variable => variable.Identifier.ValueText))
                .Concat(
                    nodes.OfType<SingleVariableDesignationSyntax>().Select(variable => variable.Identifier.ValueText))
                .Concat(
                    unit.Usings.Where(item => item.Alias is not null)
                    .Select(item => item.Alias!.Name.Identifier.ValueText)).ToHashSet();

            var moduleName = literalsTypeName.Split('.')[^1];

            if (!literalsTypeName.Contains('.'))
                declared.Add(moduleName);

            foreach (var space in nodes.OfType<BaseNamespaceDeclarationSyntax>())
                declared.Add(space.Name.ToString().Split('.')[0]);

            var alias = moduleName;

            while (declared.Contains(alias))
                alias += "Module";

            unit = (CompilationUnitSyntax)new LiteralReferenceRewriter(literalsTypeName, alias).Visit(unit)!;
            unit = unit.WithLeadingTrivia(default(SyntaxTriviaList));

            unit =
                unit.AddUsings(
                    SyntaxFactory.UsingDirective(SyntaxFactory.ParseName("global::" + literalsTypeName))
                    .WithAlias(SyntaxFactory.NameEquals(alias)).NormalizeWhitespace()
                    .WithTrailingTrivia(SyntaxFactory.LineFeed));
        }

        var used =
            unit.DescendantNodes().OfType<IdentifierNameSyntax>()
            .Where(identifier => !identifier.Ancestors().OfType<UsingDirectiveSyntax>().Any())
            .Select(identifier => identifier.Identifier.ValueText).ToHashSet();

        root =
            unit.WithUsings(
                SyntaxFactory.List(
                    unit.Usings.Where(
                        item =>
                        item.Alias is not { } alias || used.Contains(alias.Name.Identifier.ValueText))));

        root = new UnusedAliasRewriter(used).Visit(root)!;
        root = root.WithLeadingTrivia(tree.GetRoot().GetLeadingTrivia());

        return
            FormatCSharpFile.FormatSyntaxTree(tree.WithRootAndOptions(root, tree.Options))
            .Extract(error => throw new InvalidOperationException("C# formatting did not converge: " + error))
            .GetRoot().ToFullString();
    }

    private sealed class LiteralReferenceRewriter(string literalsTypeName, string alias) : CSharpSyntaxRewriter
    {
        public override SyntaxNode? VisitMemberAccessExpression(MemberAccessExpressionSyntax node) =>
            node.Expression.ToString() == "global::" + literalsTypeName
            ?
            node.WithExpression(SyntaxFactory.IdentifierName(alias))
            :
            base.VisitMemberAccessExpression(node);
    }

    private sealed class UnusedAliasRewriter(IReadOnlySet<string> used) : CSharpSyntaxRewriter
    {
        public override SyntaxNode? VisitUsingDirective(UsingDirectiveSyntax node) =>
            node.Alias is { } alias && !used.Contains(alias.Name.Identifier.ValueText) ||
            node.Alias is null && node.Name?.ToString() == "global::System.Numerics" && !used.Contains("BigInteger")
            ?
            null
            :
            node;
    }

    private sealed class MethodRewriter(IReadOnlySet<string> retainedLocals) : CSharpSyntaxRewriter
    {
        public override SyntaxNode? VisitMethodDeclaration(MethodDeclarationSyntax node)
        {
            if (node.Body is not { } body)
                return node;

            body = SimplifyFlow(body);

            var types =
                body.DescendantNodes().OfType<VariableDeclarationSyntax>()
                .SelectMany(
                    declaration => declaration.Variables.Select(
                        variable =>
                        (Name: variable.Identifier.ValueText, Type: declaration.Type.ToString())))
                .Concat(
                    node.ParameterList.Parameters.Where(parameter => parameter.Type is not null).Select(
                        parameter =>
                        (Name: parameter.Identifier.ValueText, Type: parameter.Type!.ToString())))
                .GroupBy(item => item.Name)
                .Where(group => group.Select(item => item.Type).Distinct().Count() == 1)
                .ToDictionary(group => group.Key, group => group.First().Type);

            body = (BlockSyntax)new CopyRewriter(types).Visit(body)!;

            var declarations =
                body.Statements.OfType<LocalDeclarationStatementSyntax>()
                .Where(
                    statement => statement.Declaration.Type is IdentifierNameSyntax
                    { Identifier.ValueText: "PineValueInProcess" or "NativeValue" } ||
                        statement.Declaration.Type is PredefinedTypeSyntax { Keyword.RawKind: (int)SyntaxKind.LongKeyword })
                .ToDictionary(statement => statement.Declaration.Variables.Single().Identifier.ValueText);

            var scopes = new Dictionary<string, BlockSyntax>();

            foreach (var identifier in body.DescendantNodes().OfType<IdentifierNameSyntax>())
            {
                var name = identifier.Identifier.ValueText;

                if (!declarations.ContainsKey(name) ||
                    identifier.Parent is QualifiedNameSyntax or AliasQualifiedNameSyntax ||
                    identifier.Parent is MemberAccessExpressionSyntax member && member.Name == identifier)
                {
                    continue;
                }

                var scope = identifier.Ancestors().OfType<BlockSyntax>().First();

                if (scopes.TryGetValue(name, out var previous))
                {
                    var ancestors = scope.AncestorsAndSelf().OfType<BlockSyntax>().ToHashSet();
                    scope = previous.AncestorsAndSelf().OfType<BlockSyntax>().First(ancestors.Contains);
                }

                scopes[name] = scope;
            }

            var removed = new HashSet<string>();
            var moved = new Dictionary<BlockSyntax, List<LocalDeclarationStatementSyntax>>();

            var writes =
                body.DescendantNodes().OfType<IdentifierNameSyntax>().Where(IsWrite)
                .Select(identifier => identifier.Identifier.ValueText).ToHashSet();

            foreach (var (name, declaration) in declarations)
            {
                if (retainedLocals.Contains(name))
                    continue;

                var initial = declaration.Declaration.Variables.Single().Initializer?.Value;

                if (initial is not null &&
                    (initial is not DefaultExpressionSyntax and not LiteralExpressionSyntax && !IsCopy(initial) ||
                    initial.DescendantNodesAndSelf().OfType<IdentifierNameSyntax>()
                    .Any(identifier => writes.Contains(identifier.Identifier.ValueText))))
                {
                    continue;
                }

                if (!scopes.TryGetValue(name, out var scope))
                    removed.Add(name);

                else if (scope != body)
                {
                    removed.Add(name);

                    if (!moved.TryGetValue(scope, out var items))
                        moved.Add(scope, items = []);

                    items.Add(declaration);
                }
            }

            var scopeRewriter = new BlockRewriter(body, removed, moved, retainedLocals);
            var rewritten = (BlockSyntax)scopeRewriter.Visit(body)!;
            rewritten = (BlockSyntax)new DeadCopyRewriter(scopeRewriter.DeadCopies).Visit(rewritten)!;
            rewritten = StructureLoops(SimplifyFlow(rewritten));
            return node.WithBody(rewritten);
        }
    }

    private sealed class ReferenceRewriter : CSharpSyntaxRewriter
    {
        private readonly Dictionary<MethodDeclarationSyntax, HashSet<string>> _locals = [];

        private readonly Dictionary<ClassDeclarationSyntax, string> _owners = [];

        public override SyntaxNode? VisitMemberAccessExpression(MemberAccessExpressionSyntax node)
        {
            var owner = node.Expression.ToString();
            var name = node.Name.Identifier.ValueText;
            var containingClass = node.Ancestors().OfType<ClassDeclarationSyntax>().LastOrDefault();

            if (containingClass is null)
                return base.VisitMemberAccessExpression(node);

            if (!_owners.TryGetValue(containingClass, out var qualifiedClass))
            {
                var namespaceName =
                    node.Ancestors().OfType<BaseNamespaceDeclarationSyntax>()
                    .Select(item => item.Name.ToString()).Reverse().ToArray();

                _owners[containingClass] =
                    qualifiedClass =
                        "global::" +
                        string.Join(".", namespaceName.Append(containingClass.Identifier.ValueText));
            }

            var shadows = false;

            if (node.Ancestors().OfType<MethodDeclarationSyntax>().FirstOrDefault() is { } method)
            {
                if (!_locals.TryGetValue(method, out var locals))
                {
                    _locals[method] =
                        locals =
                            method.ParameterList
                            .Parameters.Select(parameter => parameter.Identifier.ValueText)
                            .Concat(
                                method.DescendantNodes().OfType<VariableDeclaratorSyntax>().Select(
                                    variable => variable.Identifier.ValueText))
                            .Concat(
                                method.DescendantNodes().OfType<SingleVariableDesignationSyntax>().Select(
                                    variable => variable.Identifier.ValueText))
                            .ToHashSet();
                }

                shadows = locals.Contains(name);
            }

            return
                owner == qualifiedClass && !shadows
                ?
                node.Name.WithTriviaFrom(node)
                :
                base.VisitMemberAccessExpression(node);
        }
    }

    private sealed class BlockRewriter(
        BlockSyntax methodBody,
        IReadOnlySet<string> removed,
        IReadOnlyDictionary<BlockSyntax, List<LocalDeclarationStatementSyntax>> moved,
        IReadOnlySet<string> retained) : CSharpSyntaxRewriter
    {
        public HashSet<string> DeadCopies { get; } = [];

        public override SyntaxNode? VisitBlock(BlockSyntax node)
        {
            var rewritten = (BlockSyntax)base.VisitBlock(node)!;
            var statements = rewritten.Statements;

            if (node == methodBody)
            {
                statements =
                    SyntaxFactory.List(
                        statements.Where(
                            statement =>
                            statement is not LocalDeclarationStatementSyntax declaration ||
                            !removed.Contains(declaration.Declaration.Variables.Single().Identifier.ValueText)));
            }

            if (moved.TryGetValue(node, out var declarations))
                statements = statements.InsertRange(0, declarations);

            foreach (var declarationName in statements.OfType<LocalDeclarationStatementSyntax>()
                .Select(item => item.Declaration.Variables.Single().Identifier.ValueText).ToArray())
            {
                var declaration =
                    statements.OfType<LocalDeclarationStatementSyntax>()
                    .Single(item => item.Declaration.Variables.Single().Identifier.ValueText == declarationName);

                var variable = declaration.Declaration.Variables.Single();

                if (variable.Initializer is null || retained.Contains(variable.Identifier.ValueText))
                    continue;

                var initial = variable.Initializer.Value;

                if (initial is not DefaultExpressionSyntax && !IsCopy(initial) &&
                    !(initial is LiteralExpressionSyntax literal && literal.Token.ValueText == "0"))
                {
                    continue;
                }

                var name = variable.Identifier.ValueText;

                var reads =
                    statements.Where(statement => statement != declaration)
                    .SelectMany(statement => statement.DescendantNodesAndSelf().OfType<IdentifierNameSyntax>())
                    .Where(identifier => identifier.Identifier.ValueText == name && !IsWrite(identifier)).ToArray();

                if (reads.Length == 0)
                {
                    DeadCopies.Add(name);

                    statements =
                        SyntaxFactory.List(
                            statements.Where(
                                statement => statement != declaration &&
                                    !(statement is ExpressionStatementSyntax { Expression: AssignmentExpressionSyntax assignment } &&
                          assignment.Left.ToString() == name && IsCopy(assignment.Right))));

                    continue;
                }

                var first =
                    statements.First(
                        statement => statement != declaration &&
                            statement.DescendantNodesAndSelf().OfType<IdentifierNameSyntax>()
                            .Any(identifier => identifier.Identifier.ValueText == name));

                if (first is ExpressionStatementSyntax { Expression: AssignmentExpressionSyntax definition } &&
                    definition.Left.ToString() == name && !statements.OfType<LabeledStatementSyntax>().Any())
                {
                    var initialized =
                        declaration.WithDeclaration(
                            declaration.Declaration.WithVariables(
                                SyntaxFactory.SingletonSeparatedList(
                                    variable.WithInitializer(SyntaxFactory.EqualsValueClause(definition.Right)))));

                    statements =
                        SyntaxFactory.List(
                            statements.Where(item => item != declaration)
                            .Select(item => item == first ? initialized : item));
                }
            }

            for (var index = statements.Count - 2; index >= 0; --index)
            {
                if (statements[index + 1] is not LabeledStatementSyntax destination)
                    continue;

                var statement = RemoveTrailingGoto(statements[index], destination.Identifier.ValueText);

                statements =
                    statement is null
                    ?
                    statements.RemoveAt(index)
                    :
                    statements.Replace(statements[index], statement);
            }

            return rewritten.WithStatements(statements);
        }
    }

    private sealed class DeadCopyRewriter(IReadOnlySet<string> removed) : CSharpSyntaxRewriter
    {
        public override SyntaxNode? VisitExpressionStatement(ExpressionStatementSyntax node) =>
            node.Expression is AssignmentExpressionSyntax { Left: IdentifierNameSyntax target } assignment &&
            removed.Contains(target.Identifier.ValueText) &&
            IsCopy(assignment.Right)
            ?
            null
            :
            base.VisitExpressionStatement(node);

        public override SyntaxNode? VisitIdentifierName(IdentifierNameSyntax node) =>
            removed.Contains(node.Identifier.ValueText) && IsWrite(node)
            ?
            SyntaxFactory.IdentifierName("_").WithTriviaFrom(node)
            :
            node;
    }

    private static bool IsWrite(IdentifierNameSyntax identifier) =>
        identifier.Ancestors().TakeWhile(node => node is not StatementSyntax).OfType<AssignmentExpressionSyntax>()
        .Any(assignment => assignment.Left.Span.Contains(identifier.Span)) ||
        identifier.Parent is PrefixUnaryExpressionSyntax prefix &&
        prefix.Kind() is SyntaxKind.PreIncrementExpression or SyntaxKind.PreDecrementExpression ||
        identifier.Parent is PostfixUnaryExpressionSyntax postfix &&
        postfix.Kind() is SyntaxKind.PostIncrementExpression or SyntaxKind.PostDecrementExpression ||
        identifier.Parent is ArgumentSyntax argument &&
        argument.RefKindKeyword.Kind() is SyntaxKind.RefKeyword or SyntaxKind.OutKeyword;

    private static bool IsCopy(ExpressionSyntax expression) =>
        expression is IdentifierNameSyntax || expression.IsKind(SyntaxKind.DefaultLiteralExpression) ||
        expression.ToString() == "PineValueInProcess.EmptyList";

    private sealed class Substitution(IReadOnlyDictionary<string, ExpressionSyntax> values) : CSharpSyntaxRewriter
    {
        public override SyntaxNode? VisitIdentifierName(IdentifierNameSyntax node) =>
            !IsWrite(node) && node.Parent is not QualifiedNameSyntax and not AliasQualifiedNameSyntax &&
            !(node.Parent is MemberAccessExpressionSyntax member && member.Name == node) &&
            values.TryGetValue(node.Identifier.ValueText, out var value)
            ?
            value.WithTriviaFrom(node)
            :
            node;
    }

    private sealed class CopyRewriter(IReadOnlyDictionary<string, string> types) : CSharpSyntaxRewriter
    {
        public override SyntaxNode? VisitBlock(BlockSyntax node)
        {
            var aliases = new Dictionary<string, ExpressionSyntax>();
            var derived = new Dictionary<(string Type, string Expression), (string Name, HashSet<string> Inputs)>();
            var statements = new List<StatementSyntax>();

            var writtenAnywhere =
                node.DescendantNodes().OfType<IdentifierNameSyntax>().Where(IsWrite)
                .Select(identifier => identifier.Identifier.ValueText).ToHashSet();

            foreach (var original in node.Statements)
            {
                if (original is LabeledStatementSyntax)
                {
                    aliases.Clear();
                    derived.Clear();
                }

                var writes =
                    original.DescendantNodesAndSelf().OfType<IdentifierNameSyntax>().Where(IsWrite)
                    .Select(identifier => identifier.Identifier.ValueText).ToHashSet();

                if (original is not ExpressionStatementSyntax { Expression: AssignmentExpressionSyntax })
                    Invalidate(writes);

                if (original.DescendantNodesAndSelf().OfType<InvocationExpressionSyntax>().Any(call => !IsPureCall(call)))
                    derived.Clear();

                var statement = (StatementSyntax)new Substitution(aliases).Visit(original)!;
                statement = (StatementSyntax)base.Visit(statement)!;

                if (statement is LocalDeclarationStatementSyntax declaration)
                {
                    statement =
                        declaration.WithDeclaration(
                            declaration.Declaration.WithVariables(
                                SyntaxFactory.SeparatedList(
                                    declaration.Declaration.Variables.Select(
                                        variable =>
                                        variable.Initializer is not { } initializer
                                        ?
                                        variable
                                        :
                                        variable.WithInitializer(
                                            initializer.WithValue(
                                                Bind(
                                                    variable.Identifier.ValueText,
                                                    declaration.Declaration.Type.ToString(),
                                                    initializer.Value,
                                                    !writtenAnywhere.Contains(variable.Identifier.ValueText))))))));
                }

                if (statement is ExpressionStatementSyntax { Expression: AssignmentExpressionSyntax assignment })
                {
                    Invalidate(writes);

                    if (assignment.IsKind(SyntaxKind.SimpleAssignmentExpression) &&
                        assignment.Left is IdentifierNameSyntax target)
                    {
                        var name = target.Identifier.ValueText;
                        var right = Bind(name, types.GetValueOrDefault(name), assignment.Right, true);
                        statement = ((ExpressionStatementSyntax)statement).WithExpression(assignment.WithRight(right));
                    }
                }

                statements.Add(statement);
            }

            return node.WithStatements(SyntaxFactory.List(statements));

            ExpressionSyntax Bind(string name, string? type, ExpressionSyntax expression, bool alias)
            {
                var inputs =
                    expression.DescendantNodesAndSelf().OfType<IdentifierNameSyntax>()
                    .Select(identifier => identifier.Identifier.ValueText).ToHashSet();

                if (inputs.Contains(name))
                    return expression;

                var key = (type ?? "", expression.WithoutTrivia().NormalizeWhitespace().ToString());

                if (type is not null and not "var" && IsDerived(expression))
                {
                    if (derived.TryGetValue(key, out var previous))
                        expression = SyntaxFactory.IdentifierName(previous.Name);

                    else
                        derived[key] = (name, inputs);
                }

                if (alias && expression is IdentifierNameSyntax source && source.Identifier.ValueText != name &&
                    types.TryGetValue(source.Identifier.ValueText, out var sourceType) &&
                    (type == sourceType || type == "var" && !writtenAnywhere.Contains(name)))
                {
                    aliases[name] = expression;
                }

                return expression;
            }

            void Invalidate(IReadOnlySet<string> written)
            {
                foreach (var name in aliases.Keys.ToArray())
                {
                    if (written.Contains(name) ||
                        aliases[name].DescendantNodesAndSelf().OfType<IdentifierNameSyntax>()
                        .Any(identifier => written.Contains(identifier.Identifier.ValueText)))
                    {
                        aliases.Remove(name);
                    }
                }

                foreach (var key in derived.Keys.ToArray())
                {
                    if (written.Contains(derived[key].Name) || derived[key].Inputs.Overlaps(written))
                        derived.Remove(key);
                }
            }
        }

        private bool IsDerived(ExpressionSyntax expression) =>
            expression is not IdentifierNameSyntax and not LiteralExpressionSyntax &&
            (expression is not MemberAccessExpressionSyntax member ||
            member.Name.Identifier.ValueText is "Bytes" or "Items" or "Span" or "Length" or "Value" or "HasValue") &&
            IsPure(expression);

        private bool IsPure(ExpressionSyntax expression) =>
            expression switch
            {
                IdentifierNameSyntax or LiteralExpressionSyntax => true,
                ParenthesizedExpressionSyntax parenthesized => IsPure(parenthesized.Expression),
                CastExpressionSyntax cast => IsPure(cast.Expression),

                MemberAccessExpressionSyntax member =>
                member.Name.Identifier.ValueText is "Bytes" or "Items" or "Span" or "Length" or "Value" or "HasValue" &&
                IsPure(member.Expression) ||
                member.Expression.ToString() == "PineValueInProcess" &&
                member.Name.Identifier.ValueText is "EmptyList" or "KernelTrueValue" or "KernelFalseValue" ||
                member.Name.Identifier.ValueText.StartsWith("Value_", StringComparison.Ordinal),

                InvocationExpressionSyntax call =>
                IsPureCall(call) &&
                ((MemberAccessExpressionSyntax)call.Expression).Expression is { } receiver &&
                (receiver.ToString() is "NativeValue" or "PineValueInProcess" or "BigInteger" || IsPure(receiver)) &&
                call.ArgumentList.Arguments.All(
                    argument => argument.RefKindKeyword.IsKind(SyntaxKind.None) && IsPure(argument.Expression)),

                ElementAccessExpressionSyntax element =>
                IsPure(element.Expression) &&
                element.ArgumentList.Arguments.All(argument => IsPure(argument.Expression)),

                BinaryExpressionSyntax binary => IsPure(binary.Left) && IsPure(binary.Right),

                PrefixUnaryExpressionSyntax prefix =>
                prefix.Kind() is
                SyntaxKind.UnaryPlusExpression or SyntaxKind.UnaryMinusExpression or
                SyntaxKind.LogicalNotExpression or SyntaxKind.BitwiseNotExpression && IsPure(prefix.Operand),

                ConditionalExpressionSyntax conditional =>
                IsPure(conditional.Condition) &&
                IsPure(conditional.WhenTrue) && IsPure(conditional.WhenFalse),

                ObjectCreationExpressionSyntax creation =>
                creation.Type.ToString() == "BigInteger" &&
                creation.ArgumentList is { } arguments &&
                arguments.Arguments.All(argument => IsPure(argument.Expression)),

                _ =>
                false
            };

        private bool IsPureCall(InvocationExpressionSyntax call)
        {
            if (call.Expression is not MemberAccessExpressionSyntax member)
                return false;

            var receiver = member.Expression.ToString();

            if (receiver == "NativeValue")
            {
                return
                    member.Name.Identifier.ValueText is "FromValue" or "FromInt64" or "FromInteger" or "FromBoolean" or
                    "Add" or "Multiply" or "MultiplyAdd" or "LessThan" or "LessThanOrEqual";
            }

            if (receiver == "PineValueInProcess")
                return member.Name.Identifier.ValueText is "AreEqual" or "CreateBool";

            if (receiver == "BigInteger")
                return member.Name.Identifier.ValueText == "Abs";

            return
                member.Name.Identifier.ValueText is "AsInteger" or "ToPineValue" or "GetElementAt" or
                "GetLength" or "LengthEqual" or "Evaluate" or "EvaluateIfBlob" or "EvaluateIfList" or
                "IsBlob" or "IsList" or "IsTrue" or "Equals" &&
                member.Expression.DescendantNodesAndSelf().OfType<IdentifierNameSyntax>().Any(
                    identifier =>
                    types.TryGetValue(identifier.Identifier.ValueText, out var type) &&
                    type is "PineValueInProcess" or "NativeValue" or "PineValue" or
                        "PineValue.BlobValue" or "PineValue.ListValue" or "BigInteger" or "BigInteger?");
        }
    }

    private static BlockSyntax SimplifyFlow(BlockSyntax body)
    {
        while (true)
        {
            var previous = body;
            var exits = new Dictionary<string, StatementSyntax>();

            foreach (var label in body.Statements.OfType<LabeledStatementSyntax>())
            {
                if (TryExit(label.Statement) is { } exit)
                    exits[label.Identifier.ValueText] = exit;
            }

            body = (BlockSyntax)new ExitRewriter(exits).Visit(body)!;

            var labels =
                body.Statements.Select((statement, index) => (statement, index))
                .Where(item => item.statement is LabeledStatementSyntax)
                .ToDictionary(
                    item => ((LabeledStatementSyntax)item.statement).Identifier.ValueText,
                    item => item.index);

            var reachable = new HashSet<int>();
            var pending = new Stack<int>();

            if (body.Statements.Count > 0)
                pending.Push(0);

            while (pending.TryPop(out var index))
            {
                if (!reachable.Add(index))
                    continue;

                var statement = body.Statements[index];

                foreach (var jump in ActiveGotos(statement))
                {
                    if (labels.TryGetValue(jump, out var target))
                        pending.Push(target);
                }

                if (FallsThrough(statement) && index + 1 < body.Statements.Count)
                    pending.Push(index + 1);
            }

            var statements = body.Statements.Where((_, index) => reachable.Contains(index)).ToArray();
            var targeted = statements.SelectMany(ActiveGotos).ToHashSet();

            body =
                body.WithStatements(
                    SyntaxFactory.List(
                        statements.SelectMany(
                            statement =>
                            statement is LabeledStatementSyntax label && !targeted.Contains(label.Identifier.ValueText)
                            ?
                            label.Statement is BlockSyntax block ? block.Statements.AsEnumerable() : [label.Statement]
                            :
                            [statement])));

            if (body.IsEquivalentTo(previous))
                return body;
        }
    }

    private static IEnumerable<string> ActiveGotos(StatementSyntax statement)
    {
        if (statement is BlockSyntax block)
        {
            foreach (var item in block.Statements)
            {
                foreach (var jump in ActiveGotos(item))
                    yield return jump;

                if (!FallsThrough(item))
                    break;
            }
        }
        else
        {
            foreach (var jump in statement.DescendantNodesAndSelf().OfType<GotoStatementSyntax>())
            {
                if (jump.Expression is IdentifierNameSyntax identifier)
                    yield return identifier.Identifier.ValueText;
            }
        }
    }

    private static bool FallsThrough(StatementSyntax statement)
    {
        if (statement is ReturnStatementSyntax or ThrowStatementSyntax or GotoStatementSyntax or ContinueStatementSyntax)
            return false;

        if (statement is LabeledStatementSyntax label)
            return FallsThrough(label.Statement);

        if (statement is BlockSyntax block)
            return block.Statements.All(FallsThrough);

        if (statement is IfStatementSyntax branch)
            return FallsThrough(branch.Statement) || branch.Else is null || FallsThrough(branch.Else.Statement);

        if (statement is SwitchStatementSyntax choice)
        {
            return
                !choice.Sections.Any(section => section.Labels.Any(label => label is DefaultSwitchLabelSyntax)) ||
                choice.Sections.Any(section => section.Statements.All(FallsThrough));
        }

        return true;
    }

    private static StatementSyntax? TryExit(StatementSyntax statement)
    {
        if (statement is ReturnStatementSyntax or ThrowStatementSyntax)
            return statement;

        if (statement is not BlockSyntax block)
            return null;

        var values = new Dictionary<string, ExpressionSyntax>();

        foreach (var item in block.Statements)
        {
            if (item is LocalDeclarationStatementSyntax declaration &&
                declaration.Declaration.Variables.All(
                    variable => variable.Initializer?.Value is { } initial &&
                        (IsCopy(initial) || initial is DefaultExpressionSyntax)))
            {
                foreach (var variable in declaration.Declaration.Variables)
                {
                    values[variable.Identifier.ValueText] =
                        variable.Initializer!.Value.IsKind(SyntaxKind.DefaultLiteralExpression)
                        ?
                        SyntaxFactory.DefaultExpression(declaration.Declaration.Type)
                        :
                        variable.Initializer.Value;
                }

                continue;
            }

            if (item is ExpressionStatementSyntax
                { Expression: AssignmentExpressionSyntax { Left: IdentifierNameSyntax target, Right: IdentifierNameSyntax source } })
            {
                values[target.Identifier.ValueText] = values.GetValueOrDefault(source.Identifier.ValueText, source);
                continue;
            }

            if (item is ReturnStatementSyntax or ThrowStatementSyntax)
                return (StatementSyntax)new Substitution(values).Visit(item)!;

            return null;
        }

        return null;
    }

    private sealed class ExitRewriter(IReadOnlyDictionary<string, StatementSyntax> exits) : CSharpSyntaxRewriter
    {
        public override SyntaxNode? VisitGotoStatement(GotoStatementSyntax node) =>
            node.IsKind(SyntaxKind.GotoStatement) && node.Expression is IdentifierNameSyntax target &&
            exits.TryGetValue(target.Identifier.ValueText, out var exit)
            ?
            exit.WithTriviaFrom(node)
            :
            node;
    }

    private static BlockSyntax StructureLoops(BlockSyntax body)
    {
        var statements = body.Statements;

        for (var start = 0; start < statements.Count; start++)
        {
            if (statements[start] is not LabeledStatementSyntax header)
                continue;

            var name = header.Identifier.ValueText;
            var end = -1;

            for (var index = start; index < statements.Count; index++)
            {
                if (ActiveGotos(statements[index]).Contains(name))
                    end = index;
            }

            if (end < start)
                continue;

            var region = statements.Skip(start).Take(end - start + 1).ToArray();

            var interiors =
                region.Skip(1).OfType<LabeledStatementSyntax>()
                .Select(label => label.Identifier.ValueText).ToHashSet();

            var outside = statements.Take(start).Concat(statements.Skip(end + 1)).SelectMany(ActiveGotos).ToHashSet();

            if (outside.Overlaps(interiors) ||
                region.SelectMany(
                    statement => statement.DescendantNodes()
                    .OfType<GotoStatementSyntax>()).Any(
                    jump => jump.Expression?.ToString() == name &&
                        jump.Ancestors().Any(ancestor => ancestor is WhileStatementSyntax or ForStatementSyntax)))
            {
                continue;
            }

            var loopBody =
                SyntaxFactory.Block(
                    region.Select(
                        statement =>
                        (StatementSyntax)new ContinueRewriter(name).Visit(statement)!));

            if (!outside.Contains(name))
            {
                loopBody =
                    loopBody.WithStatements(
                        SyntaxFactory.List(
                            loopBody.Statements.SelectMany(
                                statement =>
                                statement is LabeledStatementSyntax label && label.Identifier.ValueText == name &&
                                label.Statement is BlockSyntax block
                                ?
                                block.Statements.AsEnumerable()
                                :
                                [statement])));
            }
            else
            {
                loopBody =
                    loopBody.WithStatements(loopBody.Statements.Replace(loopBody.Statements[0], header.Statement));
            }

            StatementSyntax loop =
                SyntaxFactory.WhileStatement(
                    SyntaxFactory.LiteralExpression(SyntaxKind.TrueLiteralExpression),
                    loopBody);

            if (outside.Contains(name))
                loop = SyntaxFactory.LabeledStatement(header.Identifier, loop);

            statements = SyntaxFactory.List(statements.Take(start).Append(loop).Concat(statements.Skip(end + 1)));
        }

        return body.WithStatements(statements);
    }

    private sealed class ContinueRewriter(string target) : CSharpSyntaxRewriter
    {
        public override SyntaxNode? VisitGotoStatement(GotoStatementSyntax node) =>
            node.Expression?.ToString() == target ? SyntaxFactory.ContinueStatement().WithTriviaFrom(node) : node;
    }

    private static StatementSyntax? RemoveTrailingGoto(StatementSyntax statement, string destination)
    {
        if (statement is GotoStatementSyntax { Expression: IdentifierNameSyntax identifier })
            return identifier.Identifier.ValueText == destination ? null : statement;

        if (statement is BlockSyntax block && block.Statements.Count > 0)
        {
            var last = block.Statements[^1];
            var replacement = RemoveTrailingGoto(last, destination);

            return
                block.WithStatements(
                    replacement is null
                    ?
                    block.Statements.RemoveAt(block.Statements.Count - 1)
                    :
                    block.Statements.Replace(last, replacement));
        }

        if (statement is LabeledStatementSyntax label)
            return label.WithStatement(RemoveTrailingGoto(label.Statement, destination) ?? SyntaxFactory.EmptyStatement());

        if (statement is IfStatementSyntax branch)
        {
            var rewritten =
                branch.WithStatement(
                    RemoveTrailingGoto(branch.Statement, destination) ?? SyntaxFactory.EmptyStatement());

            return
                branch.Else is { } otherwise
                ?
                rewritten.WithElse(
                    otherwise.WithStatement(
                        RemoveTrailingGoto(otherwise.Statement, destination) ?? SyntaxFactory.EmptyStatement()))
                :
                rewritten;
        }

        return statement;
    }
}
