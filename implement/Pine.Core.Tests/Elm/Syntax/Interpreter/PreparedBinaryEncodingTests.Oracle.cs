using Pine.Core.Elm;
using Pine.Core.Encodings;
using Pine.Core.Tests.Elm.Syntax.Abstract;
using System;
using System.Linq;

using Engine = Pine.Core.Elm.Syntax.Interpreter.ElmSyntaxInterpreter;
using Expr = Pine.Core.Elm.Syntax.Interpreter.ElmSyntaxInterpreter.PreparedExpression;
using InfixDirection = Pine.Core.Elm.Syntax.Concrete.InfixDirection;
using PineValue = Pine.Core.PineLanguage.PineValue;

namespace Pine.Core.Tests.Elm.Syntax.Interpreter;

public partial class PreparedBinaryEncodingTests
{
    private static PineValue Node(string name, params PineValue[] fields) =>
        ElmValueEncoding.TagAsPineValue("Prepared" + name, [PineValue.List(fields)]);

    private static PineValue Text(string text) => ElmValueEncoding.ElmValueAsPineValue(ElmValue.StringInstance(text));

    private static PineValue Project(object value) =>
        value switch
        {
            Engine.Prepared app =>
            Node(
                "App",
                [
                    .. app.Declarations.OrderBy(entry => entry.Key)
                    .SelectMany(entry => new[] { Project(entry.Key), Project(entry.Value) })
                ]),

            Core.Elm.Syntax.Interpreter.PreparedSourceDeclaration source =>
            Node("SourceDeclaration", Project(source.Syntax), Project(source.Prepared)),

            Engine.PreparedDeclaration.FunctionDeclaration function =>
            Node("FunctionDeclaration", Project(function.Function)),

            Engine.PreparedDeclaration.ChoiceTypeDeclaration choice =>
            Node("ChoiceTypeDeclaration", Project(choice.TypeDeclaration)),

            Engine.PreparedDeclaration.AliasDeclaration alias => Node("AliasDeclaration", Project(alias.TypeAlias)),
            Engine.PreparedDeclaration.PortDeclaration port => Node("PortDeclaration", Project(port.Signature)),
            Engine.PreparedDeclaration.InfixDeclaration infix => Node("InfixDeclaration", Project(infix.Infix)),

            Engine.PreparedFunctionStruct function =>
            Node(
                "Function",
                function.Signature is { } signature
                ?
                ElmValueEncoding.TagAsPineValue("Just", [Project(signature)])
                :
                ElmValueEncoding.TagAsPineValue("Nothing", []),
                Project(function.Declaration)),

            Engine.PreparedFunctionImplementation implementation =>
            Node(
                "Implementation",
                [
                Text(implementation.Name),
                Project(implementation.Expression),
                .. implementation.Arguments.Select(Project)
                ]),

            Expr.ValueLiteral literal => Node("ValueLiteral", literal.Value.Evaluate()),
            Expr.Negation negation => Node("Negation", Project(negation.Expression)),
            Expr.ListExpr list => Node("ListExpr", [.. list.Elements.Select(Project)]),
            Expr.Identifier identifier => Node("Identifier", Project(identifier.QualifiedName)),

            Expr.IfBlock conditional =>
            Node(
                "IfBlock",
                Project(conditional.Condition),
                Project(conditional.ThenBlock),
                Project(conditional.ElseBlock)),

            Expr.PrefixOperator prefix => Node("PrefixOperator", Text(prefix.Operator)),

            Expr.Application application =>
            Node("Application", [Project(application.Function), .. application.Arguments.Select(Project)]),

            Expr.OperatorApplication operation =>
            Node(
                "OperatorApplication",
                Text(operation.Operator),
                Direction(operation.Direction),
                Project(operation.Left),
                Project(operation.Right)),

            Expr.TupledExpression tuple => Node("TupledExpression", [.. tuple.Elements.Select(Project)]),

            Expr.LambdaExpression lambda =>
            Node("LambdaExpression", [Project(lambda.Expression), .. lambda.Arguments.Select(Project)]),

            Expr.CaseExpression cases => Node("CaseExpression", Project(cases.Expression), Project(cases.Dispatch)),

            Expr.LetExpression let =>
            Node("LetExpression", [Project(let.Expression), .. let.Declarations.Select(Project)]),

            Expr.RecordExpr record => Node("RecordExpr", [.. record.Fields.Select(Project)]),

            Expr.RecordAccess access =>
            Node("RecordAccess", Project(access.Record), Text(access.FieldName), access.FieldNameValue),

            Expr.RecordAccessFunction access =>
            Node("RecordAccessFunction", Text(access.FieldName), access.FieldNameValue),

            Expr.RecordUpdateExpression update =>
            Node("RecordUpdateExpression", [Text(update.RecordName), .. update.Fields.Select(Project)]),

            Expr.GLSLExpression glsl => Node("GLSLExpression", Text(glsl.Code)),
            Engine.PreparedLetDeclaration.LetFunction function => Node("LetFunction", Project(function.Function)),

            Engine.PreparedLetDeclaration.LetDestructuring destructuring =>
            Node("LetDestructuring", Project(destructuring.Pattern), Project(destructuring.Expression)),

            Engine.PreparedRecordSetter setter =>
            Node("RecordSetter", Text(setter.FieldName), setter.FieldNameValue, Project(setter.Value)),

            Engine.PreparedCaseDispatch dispatch => Node("CaseDispatch", [.. dispatch.Steps.Select(Project)]),

            Engine.PreparedCaseDispatchSegment.ConstantValuePatterns constants =>
            Node("ConstantValuePatterns", [.. constants.Cases.Select(Project)]),

            Engine.PreparedCaseDispatchSegment.PatternCase branch =>
            Node("PatternCase", Project(branch.Pattern), Project(branch.Expression)),

            Engine.PreparedCaseDispatchSegment.DiscardCase discard => Node("DiscardCase", Project(discard.Expression)),

            Engine.PreparedConstantCase branch =>
            Node("ConstantCase", Project(branch.Pattern), branch.Value, Project(branch.Expression)),

            Core.Elm.Syntax.Abstract.Pattern.FloatPattern pattern =>
            ElmValueEncoding.TagAsPineValue(
                "FloatPattern",
                [
                ElmValueEncoding.TagAsPineValue(
                    ElmValue.ElmFloatTypeTagName,
                    [
                    IntegerEncoding.EncodeSignedInteger(pattern.Numerator),
                    IntegerEncoding.EncodeSignedInteger(pattern.Denominator)
                    ])
                ]),

            _ =>
            ElmValueEncoding.ElmValueAsPineValue(SyntaxElmValueEncoding.Encode(value))
        };

    private static PineValue Direction(InfixDirection direction) =>
        IntegerEncoding.EncodeSignedInteger(
            direction switch
            {
                InfixDirection.Left => 0,
                InfixDirection.Right => 1,
                InfixDirection.Non => 2,

                _ =>
                throw new NotImplementedException("Unknown direction: " + direction)
            });
}
