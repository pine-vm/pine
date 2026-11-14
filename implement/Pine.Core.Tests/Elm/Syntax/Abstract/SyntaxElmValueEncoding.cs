using Pine.Core.CodeAnalysis;
using Pine.Core.Elm;
using Pine.Core.Elm.Syntax.Abstract;
using System;
using System.Collections;
using System.Linq;
using System.Numerics;

using InfixDirection = Pine.Core.Elm.Syntax.Concrete.InfixDirection;

namespace Pine.Core.Tests.Elm.Syntax.Abstract;

/// <summary>The Elm abstract model, independently of its binary representation and cached Pine payloads.</summary>
internal static class SyntaxElmValueEncoding
{
    private static ElmValue Tag(string name, params object[] fields) =>
        ElmValue.TagInstance(name, [.. fields.Select(Encode)]);

    private static ElmValue Record(params (string Name, object Value)[] fields) =>
        ElmValue.RecordInstance(
            [
            .. fields.OrderBy(field => field.Name, StringComparer.Ordinal).Select(
                field => (field.Name, Encode(field.Value)))
            ]);

    private static ElmValue Float(BigInteger numerator, BigInteger denominator) =>
        ElmValue.ElmFloat.Normalized(numerator, denominator);

    public static ElmValue Encode(object value) =>
        value switch
        {
            ElmValue elm => elm,
            string text => ElmValue.StringInstance(text),
            int integer => ElmValue.Integer(integer),
            BigInteger integer => ElmValue.Integer(integer),
            InfixDirection.Left => Tag("Left"),
            InfixDirection.Right => Tag("Right"),
            InfixDirection.Non => Tag("Non"),
            DeclQualifiedName name => Record(("moduleName", name.Namespaces), ("name", name.DeclName)),
            QualifiedNameRef name => Record(("moduleName", name.ModuleName), ("name", name.Name)),

            FunctionStruct function =>
            Record(
                ("signature", function.Signature is { } signature ? Tag("Just", signature) : Tag("Nothing")),
                ("declaration", function.Declaration)),

            FunctionImplementation function =>
            Record(("name", function.Name), ("arguments", function.Arguments), ("expression", function.Expression)),

            Signature signature => Record(("name", signature.Name), ("typeAnnotation", signature.TypeAnnotation)),

            ChoiceTypeStruct choice =>
            Record(("name", choice.Name), ("generics", choice.Generics), ("constructors", choice.Constructors)),

            TypeAlias alias =>
            Record(("name", alias.Name), ("generics", alias.Generics), ("typeAnnotation", alias.TypeAnnotation)),

            Infix infix =>
            Record(
                ("direction", infix.Direction),
                ("precedence", infix.Precedence),
                ("operator", infix.Operator),
                ("functionName", infix.FunctionName)),

            ValueConstructor constructor => Record(("name", constructor.Name), ("arguments", constructor.Arguments)),
            RecordDefinition record => Encode(record.Fields),
            RecordField field => Record(("fieldName", field.FieldName), ("fieldType", field.FieldType)),
            RecordSetter setter => Record(("fieldName", setter.FieldName), ("value", setter.Value)),
            Case branch => Record(("pattern", branch.Pattern), ("expression", branch.Expression)),
            LetDeclaration.LetFunction function => Tag("LetFunction", function.Function),

            LetDeclaration.LetDestructuring destructuring =>
            Tag("LetDestructuring", destructuring.Pattern, destructuring.Expression),

            Declaration.FunctionDeclaration function => Tag("FunctionDeclaration", function.Function),
            Declaration.ChoiceTypeDeclaration choice => Tag("ChoiceTypeDeclaration", choice.TypeDeclaration),
            Declaration.AliasDeclaration alias => Tag("AliasDeclaration", alias.TypeAlias),
            Declaration.PortDeclaration port => Tag("PortDeclaration", port.Signature),
            Declaration.InfixDeclaration infix => Tag("InfixDeclaration", infix.Infix),
            TypeAnnotation.GenericType generic => Tag("GenericType", generic.Name),
            TypeAnnotation.Typed typed => Tag("Typed", typed.ModuleName, typed.Name, typed.TypeArguments),
            TypeAnnotation.Unit => Tag("Unit"),
            TypeAnnotation.Tupled tuple => Tag("Tupled", tuple.TypeAnnotations),
            TypeAnnotation.Record record => Tag("Record", record.RecordDefinition),
            TypeAnnotation.GenericRecord record => Tag("GenericRecord", record.GenericName, record.RecordDefinition),

            TypeAnnotation.FunctionTypeAnnotation function =>
            Tag("FunctionTypeAnnotation", function.ArgumentType, function.ReturnType),

            Pattern.AllPattern => Tag("AllPattern"),
            Pattern.VarPattern variable => Tag("VarPattern", variable.Name),
            Pattern.UnitPattern => Tag("UnitPattern"),
            Pattern.CharPattern literal => Tag("CharPattern", literal.Value),
            Pattern.StringPattern literal => Tag("StringPattern", literal.Value),
            Pattern.IntPattern literal => Tag("IntPattern", literal.Value),
            Pattern.FloatPattern literal => Tag("FloatPattern", Float(literal.Numerator, literal.Denominator)),
            Pattern.TuplePattern tuple => Tag("TuplePattern", tuple.Elements),

            Pattern.RecordPattern record =>
            Tag("RecordPattern", (object)record.Fields.Select(field => field.FieldName).ToArray()),

            Pattern.UnConsPattern cons => Tag("UnConsPattern", cons.Head, cons.Tail),
            Pattern.ListPattern list => Tag("ListPattern", list.Elements),
            Pattern.NamedPattern named => Tag("NamedPattern", named.Name, named.Arguments),
            Pattern.AsPattern alias => Tag("AsPattern", alias.Pattern, alias.Name),
            Expression.UnitExpr => Tag("UnitExpr"),
            Expression.StringLiteral literal => Tag("StringLiteral", literal.Value),
            Expression.CharLiteral literal => Tag("CharLiteral", literal.Value),
            Expression.IntegerLiteral literal => Tag("IntegerLiteral", literal.Value),
            Expression.FloatLiteral literal => Tag("FloatLiteral", Float(literal.Numerator, literal.Denominator)),
            Expression.Negation negation => Tag("Negation", negation.Expression),
            Expression.ListExpr list => Tag("ListExpr", list.Elements),

            Expression.Identifier identifier =>
            Tag("Identifier", identifier.QualifiedName.Namespaces, identifier.QualifiedName.DeclName),

            Expression.IfBlock conditional =>
            Tag("IfBlock", conditional.Condition, conditional.ThenBlock, conditional.ElseBlock),

            Expression.PrefixOperator prefix => Tag("PrefixOperator", prefix.Operator),
            Expression.Application application => Tag("Application", application.Function, application.Arguments),

            Expression.OperatorApplication operation =>
            Tag("OperatorApplication", operation.Operator, operation.Direction, operation.Left, operation.Right),

            Expression.TupledExpression tuple => Tag("TupledExpression", tuple.Elements),
            Expression.LambdaExpression lambda => Tag("LambdaExpression", lambda.Arguments, lambda.Expression),
            Expression.CaseExpression cases => Tag("CaseExpression", cases.Expression, cases.Cases),
            Expression.LetExpression let => Tag("LetExpression", let.Declarations, let.Expression),
            Expression.RecordExpr record => Tag("RecordExpr", record.Fields),
            Expression.RecordAccess access => Tag("RecordAccess", access.Record, access.FieldName),
            Expression.RecordAccessFunction access => Tag("RecordAccessFunction", access.FieldName),
            Expression.RecordUpdateExpression update => Tag("RecordUpdateExpression", update.RecordName, update.Fields),
            Expression.GLSLExpression glsl => Tag("GLSLExpression", glsl.ShaderCode),
            IEnumerable items => new ElmValue.ElmList([.. items.Cast<object>().Select(Encode)]),

            _ =>
            throw new NotImplementedException(
                nameof(Encode) + " does not handle syntax variant: " + value.GetType().Name)
        };
}
