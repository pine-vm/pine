using Pine.Core.PineLanguage;
using System;
using System.Linq;

namespace Pine.Core.Elm.ElmCompilerInDotnet.CompilerModel;

/// <summary>Readable compiler-model output. This is not a source-language round trip or an identity encoding.</summary>
public static class Rendering
{
    /// <summary>Renders the actual program with separately allocated output names.</summary>
    public static string Render(Program program, DisplayNames? names = null)
    {
        names ??= DisplayNames.Create(program);

        return
            string.Join(
                "\n\n",
                program.Functions.OrderBy(entry => entry.Key, IdentityComparer.Instance)
                .Select(
                    entry => names.Functions[entry.Key] + "(" +
                        string.Join(", ", entry.Value.Parameters.Select(pattern => RenderPattern(pattern, names))) +
                        ") = " + Body(entry.Value.Body, names)));
    }

    private static string RenderPattern(Pattern pattern, DisplayNames names) =>
        pattern switch
        {
            Pattern.VarPattern variable => names.Bindings[variable.Binding],
            Pattern.AsPattern alias => RenderPattern(alias.Pattern, names) + " as " + names.Bindings[alias.Binding!],

            Pattern.TuplePattern tuple =>
            "(" + string.Join(", ", tuple.Elements.Select(item => RenderPattern(item, names))) + ")",

            Pattern.ListPattern list =>
            "[" + string.Join(", ", list.Elements.Select(item => RenderPattern(item, names))) + "]",

            Pattern.UnConsPattern cons => RenderPattern(cons.Head, names) + " :: " + RenderPattern(cons.Tail, names),

            Pattern.NamedPattern named =>
            named.Name.FullName +
            "(" + string.Join(", ", named.Arguments.Select(item => RenderPattern(item, names))) + ")",

            Pattern.AllPattern => "_",
            Pattern.UnitPattern => "()",
            Pattern.CharPattern character => Literal(character.ValueAsPineValue),
            Pattern.StringPattern text => Literal(text.ValueAsPineValue),
            Pattern.IntPattern integer => Literal(integer.ValueAsPineValue),
            Pattern.FloatPattern floating => floating.Value.ToString(System.Globalization.CultureInfo.InvariantCulture),

            _ =>
            throw new NotImplementedException($"{nameof(Pattern)} does not handle pattern variant: {pattern.GetType().Name}")
        };

    private static string Body(Expression expression, DisplayNames names) =>
        expression switch
        {
            Expression.Literal literal => Literal(literal.Value),

            Expression.ReferenceExpression reference =>
            reference.Reference switch
            {
                Reference.Function function => names.Functions[function.Id],
                Reference.Local local => names.Bindings[local.Binding],
                Reference.Constructor constructor => constructor.Name.FullName,
                Reference.Native native => native.Name.FullName,

                _ =>
                throw new NotImplementedException(
                    $"{nameof(Body)} does not handle reference variant: {reference.Reference.GetType().Name}")
            },

            Expression.Application application =>
            Body(application.Function, names) + "(" +
            string.Join(", ", application.Arguments.Select(item => Body(item, names))) + ")",

            Expression.List list => "[" + string.Join(", ", list.Items.Select(item => Body(item, names))) + "]",

            Expression.Conditional conditional =>
            "if " + Body(conditional.Condition, names) + " then " + Body(conditional.Then, names) + " else " +
            Body(conditional.Else, names),

            Expression.Let let =>
            "let " +
            string.Join(
                "; ",
                let.Bindings.Select(
                    binding =>
                    RenderPattern(binding.Pattern, names) + " = " + Body(binding.Value, names))) +
            " in " +
            Body(let.Body, names),

            Expression.Match match =>
            "case " + Body(match.Value, names) + " of " +
            string.Join(
                "; ",
                match.Branches.Select(
                    branch => RenderPattern(branch.Pattern, names) + " -> " + Body(branch.Body, names))),

            Expression.Record record =>
            "{ " + string.Join(", ", record.Fields.Select(field => field.Name + " = " + Body(field.Value, names))) +
            " }",

            Expression.RecordUpdate update =>
            "{ " + Body(update.Receiver, names) + " | " +
            string.Join(", ", update.Fields.Select(field => field.Name + " = " + Body(field.Value, names))) + " }",

            Expression.RecordAccess access => Body(access.Value, names) + "." + access.FieldName,
            Expression.RecordAccessor accessor => "." + accessor.FieldName,
            Expression.Project project => "project(" + Body(project.Value, names) + ", " + project.Index + ")",

            Expression.Operator operation =>
            "(" + Body(operation.Left, names) + " " + operation.Name + " " + Body(operation.Right, names) + ")",

            Expression.PrefixOperator prefix => "(" + prefix.Name + ")",
            Expression.Negate negate => "-(" + Body(negate.Value, names) + ")",
            Expression.Shader shader => "shader(" + shader.Code + ")",

            Expression.Lambda =>
            throw new InvalidOperationException("Lambdas must be lifted before rendering."),

            _ =>
            throw new NotImplementedException(
                $"{nameof(Body)} does not handle expression variant: {expression.GetType().Name}")
        };

    private static string Literal(PineValue value) =>
        value switch
        {
            PineValue.BlobValue blob => "0x" + Convert.ToHexString(blob.Bytes.Span),
            PineValue.ListValue list => "[" + string.Join(", ", list.Items.ToArray().Select(Literal)) + "]",

            _ =>
            throw new NotImplementedException($"{nameof(Literal)} does not handle value variant: {value.GetType().Name}")
        };
}
