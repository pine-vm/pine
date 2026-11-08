using Pine.Core.Elm.Syntax.Concrete;
using System;
using System.Collections.Generic;
using System.Linq;

using Range = Pine.Core.Elm.Syntax.Concrete.Range;

namespace Pine.Core.Elm.ElmCompilerInDotnet.DeclarationUnits;

/// <summary>Presentation-only projection of structural paths against the caller's concrete declaration.</summary>
public static class SourceLookup
{
    /// <summary>Returns the addressed range, never an invented location for an invalid path.</summary>
    public static Range? RangeAtPath(Node<Declaration> declaration, SyntaxPath path, bool selectName = false)
    {
        var current = NodeAtPath(declaration, path);
        return selectName ? NameRange(current) ?? WholeRange(current) : WholeRange(current);
    }

    internal static object? NodeAtPath(Node<Declaration> declaration, SyntaxPath path, bool unwrapParentheses = true)
    {
        object? current = declaration;

        foreach (var step in path.Steps)
        {
            current = Child(Unwrap(current), step);

            if (current is null)
                return null;
        }

        return unwrapParentheses ? UnwrapParentheses(current) : current;
    }

    private static object? Unwrap(object? node) =>
        UnwrapParentheses(node) switch
        {
            Node<Declaration> item => item.Value,
            Node<Expression> item => item.Value,
            Node<Pattern> item => item.Value,
            Node<TypeAnnotation> item => item.Value,
            Node<Signature> item => item.Value,
            Node<FunctionImplementation> item => item.Value,
            Node<ValueConstructor> item => item.Value,
            Node<Expression.LetDeclaration> item => item.Value,
            Node<RecordField> item => item.Value,

            _ =>
            node
        };

    private static object? UnwrapParentheses(object? node)
    {
        while (true)
        {
            if (node is Node<Expression> { Value: Expression.Parenthesized parentheses })
                node = parentheses.Expression;

            else if (node is Node<Pattern> { Value: Pattern.ParenthesizedPattern parenthesesPattern })
                node = parenthesesPattern.Pattern;

            else
                return node;
        }
    }

    private static object? Item<T>(IEnumerable<T> items, int index)
    {
        if (index < 0)
            return null;

        if (items is IReadOnlyList<T> list)
            return index < list.Count ? list[index] : null;

        if (items is SeparatedSyntaxList<T> separated)
        {
            return separated switch
            {
                SeparatedSyntaxList<T>.Empty => null,

                SeparatedSyntaxList<T>.NonEmpty nonempty =>
                index is 0
                ?
                nonempty.First
                :
                index <= nonempty.Rest.Count ? nonempty.Rest[index - 1].Node : null,

                _ =>
                throw new NotImplementedException(nameof(Item) + " does not handle syntax list: " + separated.GetType().Name)
            };
        }

        return items.Skip(index).Cast<object>().FirstOrDefault();
    }

    private static object? Child(object? node, SyntaxStep step)
    {
        switch (node)
        {
            case Declaration.FunctionDeclaration function:
                return step.Kind switch
                {
                    SyntaxStepKind.Signature => function.Function.Signature,
                    SyntaxStepKind.Implementation => function.Function.Declaration,

                    _ =>
                    null
                };

            case Declaration.AliasDeclaration alias:
                return step.Kind is SyntaxStepKind.TypeAnnotation ? alias.TypeAlias.TypeAnnotation : null;

            case Declaration.ChoiceTypeDeclaration choice:
                return
                    step.Kind is SyntaxStepKind.Constructor
                    ?
                    Item(choice.TypeDeclaration.Constructors, step.Index)
                    :
                    null;

            case Declaration.PortDeclaration port:
                return step.Kind is SyntaxStepKind.Signature ? port.Signature : null;

            case Declaration.InfixDeclaration:
                return null;

            case Signature signature:
                return step.Kind is SyntaxStepKind.TypeAnnotation ? signature.TypeAnnotation : null;

            case FunctionImplementation function:
                return step.Kind switch
                {
                    SyntaxStepKind.Argument => Item(function.Arguments, step.Index),
                    SyntaxStepKind.Body => function.Expression,

                    _ =>
                    null
                };

            case ValueConstructor constructor:
                return step.Kind is SyntaxStepKind.Argument ? Item(constructor.Arguments, step.Index) : null;

            case Expression.LetDeclaration.LetFunction function:
                return step.Kind switch
                {
                    SyntaxStepKind.Signature => function.Function.Signature,
                    SyntaxStepKind.Implementation => function.Function.Declaration,

                    _ =>
                    null
                };

            case Expression.LetDeclaration.LetDestructuring destructuring:
                return step.Kind switch
                {
                    SyntaxStepKind.Pattern => destructuring.Pattern,
                    SyntaxStepKind.Body => destructuring.Expression,

                    _ =>
                    null
                };

            case Case branch:
                return step.Kind switch
                {
                    SyntaxStepKind.Pattern => branch.Pattern,
                    SyntaxStepKind.Body => branch.Expression,

                    _ =>
                    null
                };

            case Expression.Application application:
                return
                    step.Kind is not SyntaxStepKind.Child
                    ?
                    null
                    :
                    step.Index is 0 ? application.Function : Item(application.Arguments, step.Index - 1);

            case Expression.OperatorApplication operation:
                return
                    step.Kind is not SyntaxStepKind.Child
                    ?
                    null
                    :
                    step.Index is 0 ? operation.Left : step.Index is 1 ? operation.Right : null;

            case Expression.IfBlock conditional:
                return
                    step.Kind is not SyntaxStepKind.Child
                    ?
                    null
                    :
                    step.Index switch
                    {
                        0 => conditional.Condition,
                        1 => conditional.ThenBlock,
                        2 => conditional.ElseBlock,

                        _ =>
                        null
                    };

            case Expression.Negation negation:
                return step.Kind is SyntaxStepKind.Child && step.Index is 0 ? negation.Expression : null;

            case Expression.ListExpr list:
                return step.Kind is SyntaxStepKind.Child ? Item(list.Elements, step.Index) : null;

            case Expression.TupledExpression tuple:
                return step.Kind is SyntaxStepKind.Child ? Item(tuple.Elements, step.Index) : null;

            case Expression.RecordAccess access:
                return step.Kind is SyntaxStepKind.Child && step.Index is 0 ? access.Record : null;

            case Expression.LambdaExpression lambda:
                return step.Kind switch
                {
                    SyntaxStepKind.Argument => Item(lambda.Lambda.Arguments, step.Index),
                    SyntaxStepKind.Body => lambda.Lambda.Expression,

                    _ =>
                    null
                };

            case Expression.CaseExpression cases:
                return step.Kind switch
                {
                    SyntaxStepKind.Child when step.Index is 0 => cases.CaseBlock.Expression,
                    SyntaxStepKind.CaseBranch => Item(cases.CaseBlock.Cases, step.Index),

                    _ =>
                    null
                };

            case Expression.LetExpression let:
                return step.Kind switch
                {
                    SyntaxStepKind.LetDeclaration => Item(let.Value.Declarations, step.Index),
                    SyntaxStepKind.Body => let.Value.Expression,

                    _ =>
                    null
                };

            case Expression.RecordExpr record:
                return Field(record.Fields, step);

            case Expression.RecordUpdateExpression update:
                return Field(update.Fields, step);

            case RecordExprField field:
                return step.Kind is SyntaxStepKind.Child && step.Index is 0 ? field.ValueExpr : null;

            case TypeAnnotation.Typed typed:
                return step.Kind is SyntaxStepKind.Argument ? Item(typed.TypeArguments, step.Index) : null;

            case TypeAnnotation.Tupled tuple:
                return step.Kind is SyntaxStepKind.Child ? Item(tuple.TypeAnnotations, step.Index) : null;

            case TypeAnnotation.FunctionTypeAnnotation function:
                return
                    step.Kind is not SyntaxStepKind.Child
                    ?
                    null
                    :
                    step.Index is 0 ? function.ArgumentType : step.Index is 1 ? function.ReturnType : null;

            case TypeAnnotation.Record record:
                return TypeField(record.RecordDefinition, step);

            case TypeAnnotation.GenericRecord record:
                return TypeField(record.RecordDefinition.Value, step);

            case RecordField field:
                return step.Kind is SyntaxStepKind.TypeAnnotation ? field.FieldType : null;

            case Pattern.NamedPattern named:
                return step.Kind is SyntaxStepKind.Argument ? Item(named.Arguments, step.Index) : null;

            case Pattern.TuplePattern tuple:
                return step.Kind is SyntaxStepKind.Child ? Item(tuple.Elements, step.Index) : null;

            case Pattern.ListPattern list:
                return step.Kind is SyntaxStepKind.Child ? Item(list.Elements, step.Index) : null;

            case Pattern.UnConsPattern cons:
                return
                    step.Kind is not SyntaxStepKind.Child
                    ?
                    null
                    :
                    step.Index is 0 ? cons.Head : step.Index is 1 ? cons.Tail : null;

            case Pattern.AsPattern alias:
                return step.Kind is SyntaxStepKind.Child && step.Index is 0 ? alias.Pattern : null;

            case Pattern.RecordPattern record:
                return step.Kind is SyntaxStepKind.Child ? Item(record.Fields, step.Index) : null;

            case Expression.UnitExpr:
            case Expression.StringLiteral:
            case Expression.MultilineStringLiteral:
            case Expression.CharLiteral:
            case Expression.IntegerLiteral:
            case Expression.FloatLiteral:
            case Expression.PrefixOperator:
            case Expression.RecordAccessFunction:
            case Expression.GLSLExpression:
            case Pattern.AllPattern:
            case Pattern.VarPattern:
            case Pattern.UnitPattern:
            case Pattern.CharPattern:
            case Pattern.StringPattern:
            case Pattern.IntPattern:
            case Pattern.HexPattern:
            case Pattern.FloatPattern:
            case TypeAnnotation.GenericType:
            case TypeAnnotation.Unit:
            case Node<string>:
                return null;

            case Expression.Parenthesized:
            case Pattern.ParenthesizedPattern:
                throw new InvalidOperationException("Parentheses must be unwrapped before path lookup.");

            default:
                throw new NotImplementedException(nameof(Child) + " does not handle syntax: " + node?.GetType().Name);
        }
    }

    private static RecordExprField? Field(IEnumerable<RecordExprField> fields, SyntaxStep step) =>
        step.Kind is SyntaxStepKind.RecordField && step.Occurrence >= 0
        ?
        fields.Where(field => field.FieldName.Value == step.Name).Skip(step.Occurrence).FirstOrDefault()
        :
        null;

    private static Node<RecordField>? TypeField(RecordDefinition record, SyntaxStep step) =>
        step.Kind is SyntaxStepKind.RecordField && step.Occurrence >= 0
        ?
        record.Fields.Where(field => field.Value.FieldName.Value == step.Name).Skip(step.Occurrence).FirstOrDefault()
        :
        null;

    private static Range? WholeRange(object? node) =>
        node switch
        {
            Node<Declaration> item => item.Range,
            Node<Expression> item => item.Range,
            Node<Pattern> item => item.Range,
            Node<TypeAnnotation> item => item.Range,
            Node<Signature> item => item.Range,
            Node<FunctionImplementation> item => item.Range,
            Node<ValueConstructor> item => item.Range,
            Node<Expression.LetDeclaration> item => item.Range,
            Node<RecordField> item => item.Range,
            Node<string> item => item.Range,
            Signature item => new(item.Name.Range.Start, item.TypeAnnotation.Range.End),
            Case item => new(item.Pattern.Range.Start, item.Expression.Range.End),
            RecordExprField item => new(item.FieldName.Range.Start, item.ValueExpr.Range.End),

            _ =>
            null
        };

    private static Range? NameRange(object? node) =>
        Unwrap(node) switch
        {
            Declaration.FunctionDeclaration item => item.Function.Declaration.Value.Name.Range,
            Declaration.AliasDeclaration item => item.TypeAlias.Name.Range,
            Declaration.ChoiceTypeDeclaration item => item.TypeDeclaration.Name.Range,
            Declaration.PortDeclaration item => item.Signature.Name.Range,
            Declaration.InfixDeclaration item => item.Infix.FunctionName.Range,
            FunctionImplementation item => item.Name.Range,
            Signature item => item.Name.Range,
            ValueConstructor item => item.Name.Range,
            Expression.OperatorApplication item => item.Operator.Range,
            Expression.RecordUpdateExpression item => item.RecordName.Range,
            RecordExprField item => item.FieldName.Range,
            RecordField item => item.FieldName.Range,
            TypeAnnotation.Typed item => item.TypeName.Range,
            Pattern.AsPattern item => item.Name.Range,

            _ =>
            WholeRange(node)
        };
}
