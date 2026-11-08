using Pine.Core.Elm.Syntax.Concrete;
using System;
using System.Collections.Generic;
using System.Linq;

using Abstract = Pine.Core.Elm.Syntax.Abstract;
using Range = Pine.Core.Elm.Syntax.Concrete.Range;

namespace Pine.Core.Elm.ElmCompilerInDotnet.DeclarationUnits;

/// <summary>Reifies a reusable result using only the current caller's syntax and resolution path.</summary>
public static class ConcreteProjection
{
    /// <summary>This presentation function is intentionally not memoized with semantic artifacts.</summary>
    public static Node<Declaration> Reify(
        Abstract.Declaration syntax,
        CanonicalDeclaration transformation,
        Node<Declaration> source,
        bool preservePrefixOperators = false)
    {
        var projection =
            new Projection(
                source,
                transformation.Projections.ToDictionary(item => item.Output, item => item.Input),
                preservePrefixOperators);

        return
            projection.RewriteDeclaration(
                new(source.Range, Abstract.ConvertToConcrete.ToDeclaration(syntax)),
                SyntaxPath.Root);
    }

    private sealed class Projection(Node<Declaration> source, Dictionary<SyntaxPath, SyntaxPath> paths, bool preservePrefixOperators)
    {
        private Range Range(SyntaxPath output, bool name = false)
        {
            var input = paths.GetValueOrDefault(output, output);

            return
                SourceLookup.RangeAtPath(source, input, name) ??
                throw new InvalidOperationException("Canonical syntax projection has no source node at " + input);
        }

        private Node<T> Node<T>(T value, SyntaxPath path) => new(Range(path), value);

        private Node<string> Name(Node<string> name, SyntaxPath path) => new(Range(path, true), name.Value);

        private object? OriginalNode(SyntaxPath path, bool unwrapParentheses = true) =>
            SourceLookup.NodeAtPath(source, paths.GetValueOrDefault(path, path), unwrapParentheses);

        private T? Original<T>(SyntaxPath path) where T : class =>
            OriginalNode(path) switch
            {
                Node<Declaration> node => node.Value as T,
                Node<Expression> node => node.Value as T,
                Node<Pattern> node => node.Value as T,
                Node<TypeAnnotation> node => node.Value as T,
                Node<Signature> node => node.Value as T,
                Node<FunctionImplementation> node => node.Value as T,
                Node<ValueConstructor> node => node.Value as T,
                Node<RecordField> node => node.Value as T,

                _ =>
                OriginalNode(path) as T
            };

        public Node<Declaration> RewriteDeclaration(Node<Declaration> node, SyntaxPath path)
        {
            var constructorIndex = 0;

            var value =
                node.Value switch
                {
                    Declaration.FunctionDeclaration function =>
                    (Declaration)new Declaration.FunctionDeclaration(Function(function.Function, path)),

                    Declaration.AliasDeclaration alias =>
                    new Declaration.AliasDeclaration(
                        (Original<Declaration.AliasDeclaration>(path)?.TypeAlias ?? alias.TypeAlias) with
                        {
                            Name = Name(alias.TypeAlias.Name, path),
                            TypeAnnotation =
                            Type(alias.TypeAlias.TypeAnnotation, path.Append(SyntaxStepKind.TypeAnnotation)),
                            Documentation =
                            source.Value is Declaration.AliasDeclaration original
                            ?
                            original.TypeAlias.Documentation
                            :
                            null
                        }),

                    Declaration.ChoiceTypeDeclaration choice =>
                    new Declaration.ChoiceTypeDeclaration(
                        (Original<Declaration.ChoiceTypeDeclaration>(path)?.TypeDeclaration ?? choice.TypeDeclaration) with
                        {
                            Name = Name(choice.TypeDeclaration.Name, path),
                            Constructors =
                            Map(
                                choice.TypeDeclaration.Constructors,
                                (constructor, index) =>
                                {
                                    var child = path.Append(SyntaxStepKind.Constructor, constructorIndex++);

                                    return
                                        Node(
                                            (Original<ValueConstructor>(child) ?? constructor.Value) with
                                            {
                                                Name = Name(constructor.Value.Name, child),
                                                Arguments =
                                                [
                                                .. constructor.Value.Arguments.Select(
                                                    (argument, argumentIndex) =>
                                                    Type(argument, child.Append(SyntaxStepKind.Argument, argumentIndex)))
                                                ]
                                            },
                                            child);
                                },
                                Original<Declaration.ChoiceTypeDeclaration>(path)?.TypeDeclaration.Constructors),
                            Documentation =
                            source.Value is Declaration.ChoiceTypeDeclaration original
                            ?
                            original.TypeDeclaration.Documentation
                            :
                            null
                        }),

                    Declaration.PortDeclaration port =>
                    new Declaration.PortDeclaration(
                        Original<Declaration.PortDeclaration>(path)?.PortTokenLocation ?? port.PortTokenLocation,
                        Signature(port.Signature, path.Append(SyntaxStepKind.Signature))),

                    Declaration.InfixDeclaration infix =>
                    new Declaration.InfixDeclaration(
                        (Original<Declaration.InfixDeclaration>(path)?.Infix ?? infix.Infix) with
                        {
                            FunctionName =
                            Name(
                                preservePrefixOperators
                                ?
                                Original<Declaration.InfixDeclaration>(path)?.Infix.FunctionName ??
                                infix.Infix.FunctionName
                                :
                                infix.Infix.FunctionName,
                                path)
                        }),

                    _ =>
                    throw new NotImplementedException(
                        nameof(Declaration) + " does not handle declaration: " + node.Value.GetType().Name)
                };

            return Node(value, path);
        }

        private Signature Signature(Signature signature, SyntaxPath path) =>
            (Original<Signature>(path) ?? signature) with
            {
                Name = Name(signature.Name, path),
                TypeAnnotation = Type(signature.TypeAnnotation, path.Append(SyntaxStepKind.TypeAnnotation))
            };

        private FunctionStruct Function(FunctionStruct function, SyntaxPath path)
        {
            var implementation = path.Append(SyntaxStepKind.Implementation);

            return
                function with
                {
                    Documentation =
                    path.Steps.Count is 0 && source.Value is Declaration.FunctionDeclaration original
                    ?
                    original.Function.Documentation
                    :
                    null,
                    Signature =
                    function.Signature is null
                    ?
                    null
                    :
                    Node(
                        Signature(function.Signature.Value, path.Append(SyntaxStepKind.Signature)),
                        path.Append(SyntaxStepKind.Signature)),
                    Declaration =
                    Node(
                        (Original<FunctionImplementation>(implementation) ?? function.Declaration.Value) with
                        {
                            Name = Name(function.Declaration.Value.Name, implementation),
                            Arguments =
                            [
                            .. function.Declaration.Value.Arguments.Select(
                                (pattern, index) =>
                                RewritePattern(pattern, implementation.Append(SyntaxStepKind.Argument, index)))
                            ],
                            Expression =
                            RewriteExpression(
                                function.Declaration.Value.Expression,
                                implementation.Append(SyntaxStepKind.Body))
                        },
                        implementation)
                };
        }

        private Node<TypeAnnotation> Type(Node<TypeAnnotation> node, SyntaxPath path)
        {
            var value =
                node.Value switch
                {
                    TypeAnnotation.Typed typed =>
                    (TypeAnnotation)new TypeAnnotation.Typed(
                        new(Range(path, true), typed.TypeName.Value),
                        [
                            .. typed.TypeArguments
                            .Select(
                                (argument, index) => Type(argument, path.Append(SyntaxStepKind.Argument, index)))
                        ]),

                    TypeAnnotation.Tupled tuple =>
                    new TypeAnnotation.Tupled(
                        Map(
                            tuple.TypeAnnotations,
                            (child, index) => Type(child, path.Append(SyntaxStepKind.Child, index)),
                            Original<TypeAnnotation.Tupled>(path)?.TypeAnnotations)),

                    TypeAnnotation.FunctionTypeAnnotation function =>
                    (Original<TypeAnnotation.FunctionTypeAnnotation>(path) ?? function) with
                    {
                        ArgumentType = Type(function.ArgumentType, path.Append(SyntaxStepKind.Child)),
                        ReturnType = Type(function.ReturnType, path.Append(SyntaxStepKind.Child, 1))
                    },

                    TypeAnnotation.Record record =>
                    new TypeAnnotation.Record(RecordType(record.RecordDefinition, path)),

                    TypeAnnotation.GenericRecord record =>
                    (Original<TypeAnnotation.GenericRecord>(path) ?? record) with
                    {
                        RecordDefinition = new(Range(path), RecordType(record.RecordDefinition.Value, path))
                    },

                    TypeAnnotation.GenericType => node.Value,
                    TypeAnnotation.Unit => node.Value,

                    _ =>
                    throw new NotImplementedException(nameof(Type) + " does not handle type: " + node.Value.GetType().Name)
                };

            return Node(value, path);
        }

        private RecordDefinition RecordType(RecordDefinition record, SyntaxPath path)
        {
            var original =
                Original<TypeAnnotation.Record>(path)?.RecordDefinition ??
                Original<TypeAnnotation.GenericRecord>(path)?.RecordDefinition.Value ?? record;

            var values =
                record.Fields.GroupBy(field => field.Value.FieldName.Value)
                .ToDictionary(group => group.Key, group => new Queue<Node<RecordField>>(group));

            var occurrences = new Dictionary<string, int>();

            return
                new(
                    original.Fields.Map(
                        originalField =>
                        {
                            var field = values[originalField.Value.FieldName.Value].Dequeue();
                            var name = field.Value.FieldName.Value;
                            var occurrence = occurrences.GetValueOrDefault(name);
                            occurrences[name] = occurrence + 1;
                            var child = path.Append(SyntaxStepKind.RecordField, name: name, occurrence: occurrence);

                            return
                                Node(
                                    originalField.Value with
                                    {
                                        FieldName = Name(field.Value.FieldName, child),
                                        FieldType =
                                        Type(field.Value.FieldType, child.Append(SyntaxStepKind.TypeAnnotation))
                                    },
                                    child);
                        }));
        }

        private Node<Pattern> RewritePattern(Node<Pattern> node, SyntaxPath path)
        {
            var value =
                node.Value switch
                {
                    Pattern.NamedPattern named =>
                    (Pattern)new Pattern.NamedPattern(
                        named.Name,
                        [
                            .. named.Arguments
                            .Select(
                                (argument, index) => RewritePattern(argument, path.Append(SyntaxStepKind.Argument, index)))
                        ]),

                    Pattern.TuplePattern tuple =>
                    new Pattern.TuplePattern(
                        Map(
                            tuple.Elements,
                            (child, index) => RewritePattern(child, path.Append(SyntaxStepKind.Child, index)),
                            Original<Pattern.TuplePattern>(path)?.Elements)),

                    Pattern.ListPattern list =>
                    new Pattern.ListPattern(
                        Map(
                            list.Elements,
                            (child, index) => RewritePattern(child, path.Append(SyntaxStepKind.Child, index)),
                            Original<Pattern.ListPattern>(path)?.Elements)),

                    Pattern.UnConsPattern cons =>
                    (Original<Pattern.UnConsPattern>(path) ?? cons) with
                    {
                        Head = RewritePattern(cons.Head, path.Append(SyntaxStepKind.Child)),
                        Tail = RewritePattern(cons.Tail, path.Append(SyntaxStepKind.Child, 1))
                    },

                    Pattern.AsPattern alias =>
                    (Original<Pattern.AsPattern>(path) ?? alias) with
                    {
                        Pattern = RewritePattern(alias.Pattern, path.Append(SyntaxStepKind.Child)),
                        Name = Name(alias.Name, path)
                    },

                    Pattern.ParenthesizedPattern parentheses => RewritePattern(parentheses.Pattern, path).Value,
                    Pattern.AllPattern => node.Value,
                    Pattern.VarPattern => node.Value,
                    Pattern.UnitPattern => node.Value,
                    Pattern.CharPattern => node.Value,
                    Pattern.StringPattern => node.Value,
                    Pattern.IntPattern => node.Value,
                    Pattern.HexPattern => Original<Pattern.HexPattern>(path) ?? node.Value,
                    Pattern.FloatPattern => node.Value,
                    Pattern.RecordPattern => Original<Pattern.RecordPattern>(path) ?? node.Value,

                    _ =>
                    throw new NotImplementedException(
                        nameof(Pattern) + " does not handle pattern: " + node.Value.GetType().Name)
                };

            return Node(value, path);
        }

        private SeparatedSyntaxList<RecordExprField> Fields(
            SeparatedSyntaxList<RecordExprField> fields,
            SyntaxPath path)
        {
            var original =
                Original<Expression.RecordExpr>(path)?.Fields ??
                Original<Expression.RecordUpdateExpression>(path)?.Fields ?? fields;

            var values =
                fields.GroupBy(field => field.FieldName.Value)
                .ToDictionary(group => group.Key, group => new Queue<RecordExprField>(group));

            var occurrences = new Dictionary<string, int>();

            return
                original.Map(
                    originalField =>
                    {
                        var field = values[originalField.FieldName.Value].Dequeue();
                        var name = field.FieldName.Value;
                        var occurrence = occurrences.GetValueOrDefault(name);
                        occurrences[name] = occurrence + 1;
                        var child = path.Append(SyntaxStepKind.RecordField, name: name, occurrence: occurrence);

                        return
                            originalField with
                            {
                                FieldName = Name(field.FieldName, child),
                                ValueExpr = RewriteExpression(field.ValueExpr, child.Append(SyntaxStepKind.Child))
                            };
                    });
        }

        private Node<Expression> RewriteExpression(Node<Expression> node, SyntaxPath path)
        {
            if (node.Value is Expression.Parenthesized parentheses)
                return RewriteExpression(parentheses.Expression, path);

            Node<Expression> Child(Node<Expression> child, int index) =>
                RewriteExpression(child, path.Append(SyntaxStepKind.Child, index));

            var value =
                node.Value switch
                {
                    Expression.Application application =>
                    (Expression)new Expression.Application(
                        Child(application.Function, 0),
                        [.. application.Arguments.Select((argument, index) => Child(argument, index + 1))]),

                    Expression.OperatorApplication operation =>
                    operation with
                    {
                        Operator = Name(operation.Operator, path),
                        Left = Child(operation.Left, 0),
                        Right = Child(operation.Right, 1)
                    },

                    Expression.IfBlock conditional =>
                    (Original<Expression.IfBlock>(path) ?? conditional) with
                    {
                        Condition = Child(conditional.Condition, 0),
                        ThenBlock = Child(conditional.ThenBlock, 1),
                        ElseBlock = Child(conditional.ElseBlock, 2)
                    },

                    Expression.Negation negation => new Expression.Negation(Child(negation.Expression, 0)),

                    Expression.ListExpr list =>
                    new Expression.ListExpr(
                        Map(
                            list.Elements,
                            Child,
                            Original<Expression.ListExpr>(path)?.Elements)),

                    Expression.TupledExpression tuple =>
                    new Expression.TupledExpression(
                        Map(
                            tuple.Elements,
                            Child,
                            Original<Expression.TupledExpression>(path)?.Elements)),

                    Expression.RecordExpr record => new Expression.RecordExpr(Fields(record.Fields, path)),

                    Expression.RecordUpdateExpression update =>
                    (Original<Expression.RecordUpdateExpression>(path) ?? update) with
                    {
                        RecordName = Name(update.RecordName, path),
                        Fields = Fields(update.Fields, path)
                    },

                    Expression.RecordAccess access =>
                    (Original<Expression.RecordAccess>(path) ?? access) with { Record = Child(access.Record, 0) },

                    Expression.LambdaExpression lambda =>
                    new Expression.LambdaExpression(
                        (Original<Expression.LambdaExpression>(path)?.Lambda ?? lambda.Lambda) with
                        {
                            Arguments =
                            [
                            .. lambda.Lambda
                            .Arguments
                            .Select(
                                (pattern, index) => RewritePattern(pattern, path.Append(SyntaxStepKind.Argument, index)))
                            ],
                            Expression = RewriteExpression(lambda.Lambda.Expression, path.Append(SyntaxStepKind.Body))
                        }),

                    Expression.CaseExpression cases =>
                    new Expression.CaseExpression(
                        (Original<Expression.CaseExpression>(path)?.CaseBlock ?? cases.CaseBlock) with
                        {
                            Expression = Child(cases.CaseBlock.Expression, 0),
                            Cases =
                            [
                            .. cases.CaseBlock.Cases.Select(
                                (branch, index) =>
                                {
                                    var child = path.Append(SyntaxStepKind.CaseBranch, index);

                                    return
                                        (Original<Case>(child) ?? branch) with
                                        {
                                            Pattern =
                                            RewritePattern(branch.Pattern, child.Append(SyntaxStepKind.Pattern)),
                                            Expression =
                                            RewriteExpression(branch.Expression, child.Append(SyntaxStepKind.Body))
                                        };
                                })
                            ]
                        }),

                    Expression.LetExpression let =>
                    new Expression.LetExpression(
                        (Original<Expression.LetExpression>(path)?.Value ?? let.Value) with
                        {
                            Declarations =
                            [
                            .. let.Value.Declarations.Select(
                                (local, index) =>
                                {
                                    var child = path.Append(SyntaxStepKind.LetDeclaration, index);

                                    var declaration =
                                        local.Value switch
                                        {
                                            Expression.LetDeclaration.LetFunction function =>
                                            (Expression.LetDeclaration)new Expression.LetDeclaration.LetFunction(
                                                Function(function.Function, child)),

                                            Expression.LetDeclaration.LetDestructuring destructuring =>
                                            destructuring with
                                            {
                                                Pattern =
                                                RewritePattern(destructuring.Pattern, child.Append(SyntaxStepKind.Pattern)),
                                                Expression =
                                                RewriteExpression(destructuring.Expression, child.Append(SyntaxStepKind.Body))
                                            },

                                            _ =>
                                            throw new NotImplementedException(
                                                nameof(Expression) + " does not handle let: " +
                                                local.Value.GetType().Name)
                                        };

                                    return Node(declaration, child);
                                })
                            ],
                            Expression = RewriteExpression(let.Value.Expression, path.Append(SyntaxStepKind.Body))
                        }),

                    Expression.Parenthesized =>
                    throw new InvalidOperationException("Synthetic parentheses must be unwrapped before reification."),

                    Expression.UnitExpr => node.Value,

                    Expression.StringLiteral =>
                    Original<Expression.StringLiteral>(path) ??
                    (Expression?)Original<Expression.MultilineStringLiteral>(path) ?? node.Value,

                    Expression.MultilineStringLiteral =>
                    Original<Expression.MultilineStringLiteral>(path) ?? node.Value,

                    Expression.CharLiteral => node.Value,
                    Expression.IntegerLiteral => Original<Expression.IntegerLiteral>(path) ?? node.Value,
                    Expression.FloatLiteral => Original<Expression.FloatLiteral>(path) ?? node.Value,

                    Expression.Identifier =>
                    preservePrefixOperators && Original<Expression.PrefixOperator>(path) is { } prefix
                    ?
                    prefix
                    :
                    node.Value,

                    Expression.PrefixOperator => node.Value,
                    Expression.RecordAccessFunction => node.Value,
                    Expression.GLSLExpression => node.Value,

                    _ =>
                    throw new NotImplementedException(
                        nameof(Expression) + " does not handle expression: " + node.Value.GetType().Name)
                };

            var original = Original<Expression>(path);
            var rewritten = Node(value, path);

            var correspondsToExpression =
                original?.GetType() == value.GetType() ||
                original is Expression.OperatorApplication && value is Expression.Application ||
                original is Expression.PrefixOperator && value is Expression.Identifier ||
                original is Expression.RecordUpdateExpression && value is Expression.LetExpression ||
                original is Expression.MultilineStringLiteral && value is Expression.StringLiteral;

            return
                correspondsToExpression
                ?
                RestoreParentheses(OriginalNode(path, false) as Node<Expression>, rewritten)
                :
                rewritten;
        }

        private static Node<Expression> RestoreParentheses(Node<Expression>? original, Node<Expression> rewritten) =>
            original?.Value is Expression.Parenthesized parentheses
            ?
            new(
                original.Range,
                new Expression.Parenthesized(RestoreParentheses(parentheses.Expression, rewritten)))
            :
            rewritten;

        private static SeparatedSyntaxList<T> Map<T>(
            SeparatedSyntaxList<T> list, Func<T, int, T> map, SeparatedSyntaxList<T>? original = null)
        {
            var values = list.ToArray();
            var index = 0;

            return
                (original ?? list).Map(
                    item =>
                    {
                        var position = index++;
                        return map(values[position], position);
                    });
        }
    }
}
