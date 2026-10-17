using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Globalization;
using System.Text;

using Abstract = Pine.Core.Elm.ElmSyntax.ElmSyntaxAbstract;
using ModuleName = System.Collections.Generic.IReadOnlyList<string>;

namespace Pine.Core.Elm.ElmCompilerInDotnet;

/// <summary>
/// Computes deterministic structural fingerprints for comparing function declarations.
/// Declaration names and type signatures are ignored; the abstract syntax model already
/// omits documentation, source positions, and redundant parentheses.
/// </summary>
public static class DeclarationDeduplication
{
    /// <summary>
    /// Serializes the abstract parameter patterns and body directly, without constructing
    /// another syntax tree or formatting source code. Unqualified non-local references
    /// resolve against <paramref name="currentModuleName"/>.
    /// </summary>
    /// <remarks>
    /// Explicit variant tags, delimited lists, escaped strings, and invariant numbers make
    /// the fingerprint unambiguous and stable across processes and cultures. Precomputed
    /// runtime values do not affect the fingerprint. The format is internal to the compiler;
    /// consumers use ordinal equality or ordering, not an Elm source representation.
    /// </remarks>
    public static string GetStructuralFingerprint(
        Abstract.Declaration.FunctionDeclaration funcDecl,
        ModuleName currentModuleName)
    {
        var writer = new FingerprintWriter(currentModuleName);
        writer.WriteFunctionImplementation(funcDecl.Function.Declaration, []);
        return writer.ToString();
    }

    private sealed class FingerprintWriter(ModuleName currentModuleName)
    {
        private readonly StringBuilder _builder = new();

        public override string ToString() => _builder.ToString();

        public void WriteFunctionImplementation(
            Abstract.FunctionImplementation implementation,
            ImmutableHashSet<string> outerLocals)
        {
            _builder.Append("function(");
            WritePatterns(implementation.Arguments);
            _builder.Append(',');

            WriteExpression(
                implementation.Expression,
                outerLocals.Union(
                    Abstract.SyntaxAnalysis.CollectNamesBoundByPatterns(implementation.Arguments)));

            _builder.Append(')');
        }

        private void WriteExpression(
            Abstract.Expression expression,
            ImmutableHashSet<string> locals)
        {
            switch (expression)
            {
                case Abstract.Expression.UnitExpr:
                    _builder.Append("unit");
                    break;

                case Abstract.Expression.StringLiteral literal:
                    _builder.Append("string(");
                    WriteString(literal.Value);
                    _builder.Append(')');
                    break;

                case Abstract.Expression.CharLiteral literal:
                    _builder.Append("char(").Append(literal.Value.ToString(CultureInfo.InvariantCulture)).Append(')');
                    break;

                case Abstract.Expression.IntegerLiteral literal:
                    _builder.Append("int(").Append(literal.Value.ToString(CultureInfo.InvariantCulture)).Append(')');
                    break;

                case Abstract.Expression.FloatLiteral literal:
                    _builder.Append("float(")
                        .Append(literal.Numerator.ToString(CultureInfo.InvariantCulture))
                        .Append(',')
                        .Append(literal.Denominator.ToString(CultureInfo.InvariantCulture))
                        .Append(')');

                    break;

                case Abstract.Expression.Identifier identifier:
                    _builder.Append("identifier(");
                    var name = identifier.QualifiedName;

                    WriteQualifiedName(
                        name.Namespaces.Count is 0 && !locals.Contains(name.DeclName)
                        ?
                        currentModuleName
                        :
                        name.Namespaces,
                        name.DeclName);

                    _builder.Append(')');
                    break;

                case Abstract.Expression.Negation negation:
                    _builder.Append("negate(");
                    WriteExpression(negation.Expression, locals);
                    _builder.Append(')');
                    break;

                case Abstract.Expression.ListExpr list:
                    _builder.Append("list(");
                    WriteExpressions(list.Elements, locals);
                    _builder.Append(')');
                    break;

                case Abstract.Expression.TupledExpression tuple:
                    _builder.Append("tuple(");
                    WriteExpressions(tuple.Elements, locals);
                    _builder.Append(')');
                    break;

                case Abstract.Expression.IfBlock conditional:
                    _builder.Append("if(");
                    WriteExpression(conditional.Condition, locals);
                    _builder.Append(',');
                    WriteExpression(conditional.ThenBlock, locals);
                    _builder.Append(',');
                    WriteExpression(conditional.ElseBlock, locals);
                    _builder.Append(')');
                    break;

                case Abstract.Expression.PrefixOperator prefix:
                    _builder.Append("prefix(");
                    WriteString(prefix.Operator);
                    _builder.Append(')');
                    break;

                case Abstract.Expression.Application application:
                    _builder.Append("apply(");
                    WriteExpression(application.Function, locals);
                    _builder.Append(',');
                    WriteExpressions(application.Arguments, locals);
                    _builder.Append(')');
                    break;

                case Abstract.Expression.OperatorApplication application:
                    _builder.Append("operator(");
                    WriteString(application.Operator);

                    _builder.Append(',')
                        .Append(((int)application.Direction).ToString(CultureInfo.InvariantCulture))
                        .Append(',');

                    WriteExpression(application.Left, locals);
                    _builder.Append(',');
                    WriteExpression(application.Right, locals);
                    _builder.Append(')');
                    break;

                case Abstract.Expression.LambdaExpression lambda:
                    _builder.Append("lambda(");
                    WritePatterns(lambda.Arguments);
                    _builder.Append(',');

                    WriteExpression(
                        lambda.Expression,
                        locals.Union(Abstract.SyntaxAnalysis.CollectNamesBoundByPatterns(lambda.Arguments)));

                    _builder.Append(')');
                    break;

                case Abstract.Expression.CaseExpression caseExpression:
                    _builder.Append("case(");
                    WriteExpression(caseExpression.Expression, locals);
                    _builder.Append(",[");

                    foreach (var branch in caseExpression.Cases)
                    {
                        WritePattern(branch.Pattern);
                        _builder.Append(',');

                        WriteExpression(
                            branch.Expression,
                            locals.Union(Abstract.SyntaxAnalysis.CollectNamesBoundByPattern(branch.Pattern)));

                        _builder.Append(';');
                    }

                    _builder.Append("])");
                    break;

                case Abstract.Expression.LetExpression letExpression:
                    WriteLetExpression(letExpression, locals);
                    break;

                case Abstract.Expression.RecordExpr record:
                    _builder.Append("record(");
                    WriteFields(record.Fields, locals);
                    _builder.Append(')');
                    break;

                case Abstract.Expression.RecordAccess access:
                    _builder.Append("access(");
                    WriteExpression(access.Record, locals);
                    _builder.Append(',');
                    WriteString(access.FieldName);
                    _builder.Append(')');
                    break;

                case Abstract.Expression.RecordAccessFunction access:
                    _builder.Append("getter(");
                    WriteString(access.FieldName);
                    _builder.Append(')');
                    break;

                case Abstract.Expression.RecordUpdateExpression update:
                    _builder.Append("update(");
                    WriteString(update.RecordName);
                    _builder.Append(',');
                    WriteFields(update.Fields, locals);
                    _builder.Append(')');
                    break;

                case Abstract.Expression.GLSLExpression shader:
                    _builder.Append("glsl(");
                    WriteString(shader.ShaderCode);
                    _builder.Append(')');
                    break;

                default:
                    throw new NotImplementedException(
                        "WriteExpression does not handle expression variant: " + expression.GetType().Name);
            }
        }

        private void WriteLetExpression(
            Abstract.Expression.LetExpression expression,
            ImmutableHashSet<string> locals)
        {
            foreach (var declaration in expression.Declarations)
            {
                switch (declaration)
                {
                    case Abstract.LetDeclaration.LetFunction function:
                        locals = locals.Add(function.Function.Declaration.Name);
                        break;

                    case Abstract.LetDeclaration.LetDestructuring destructuring:
                        locals =
                            locals.Union(
                                Abstract.SyntaxAnalysis.CollectNamesBoundByPattern(destructuring.Pattern));

                        break;

                    default:
                        throw new NotImplementedException(
                            "WriteLetExpression does not handle let declaration variant: " +
                            declaration.GetType().Name);
                }
            }

            _builder.Append("let([");

            foreach (var declaration in expression.Declarations)
            {
                switch (declaration)
                {
                    case Abstract.LetDeclaration.LetFunction function:
                        _builder.Append("binding(");
                        WriteString(function.Function.Declaration.Name);
                        _builder.Append(',');

                        if (function.Function.Signature is { } signature)
                        {
                            _builder.Append("signature(");
                            WriteString(signature.Name);
                            _builder.Append(',');
                            WriteTypeAnnotation(signature.TypeAnnotation);
                            _builder.Append(')');
                        }
                        else
                        {
                            _builder.Append("none");
                        }

                        _builder.Append(',');
                        WriteFunctionImplementation(function.Function.Declaration, locals);
                        _builder.Append(')');
                        break;

                    case Abstract.LetDeclaration.LetDestructuring destructuring:
                        _builder.Append("destructure(");
                        WritePattern(destructuring.Pattern);
                        _builder.Append(',');
                        WriteExpression(destructuring.Expression, locals);
                        _builder.Append(')');
                        break;

                    default:
                        throw new NotImplementedException(
                            "WriteLetExpression does not handle let declaration variant: " +
                            declaration.GetType().Name);
                }

                _builder.Append(';');
            }

            _builder.Append("],");
            WriteExpression(expression.Expression, locals);
            _builder.Append(')');
        }

        private void WriteExpressions(
            IReadOnlyList<Abstract.Expression> expressions,
            ImmutableHashSet<string> locals)
        {
            _builder.Append('[');

            foreach (var expression in expressions)
            {
                WriteExpression(expression, locals);
                _builder.Append(';');
            }

            _builder.Append(']');
        }

        private void WriteFields(
            IReadOnlyList<Abstract.RecordSetter> fields,
            ImmutableHashSet<string> locals)
        {
            _builder.Append('[');

            foreach (var field in fields)
            {
                WriteString(field.FieldName);
                _builder.Append(',');
                WriteExpression(field.Value, locals);
                _builder.Append(';');
            }

            _builder.Append(']');
        }

        private void WritePatterns(IReadOnlyList<Abstract.Pattern> patterns)
        {
            _builder.Append('[');

            foreach (var pattern in patterns)
            {
                WritePattern(pattern);
                _builder.Append(';');
            }

            _builder.Append(']');
        }

        private void WritePattern(Abstract.Pattern pattern)
        {
            switch (pattern)
            {
                case Abstract.Pattern.AllPattern:
                    _builder.Append("all");
                    break;

                case Abstract.Pattern.VarPattern variable:
                    _builder.Append("var(");
                    WriteString(variable.Name);
                    _builder.Append(')');
                    break;

                case Abstract.Pattern.UnitPattern:
                    _builder.Append("unit");
                    break;

                case Abstract.Pattern.CharPattern literal:
                    _builder.Append("char(").Append(literal.Value.ToString(CultureInfo.InvariantCulture)).Append(')');
                    break;

                case Abstract.Pattern.StringPattern literal:
                    _builder.Append("string(");
                    WriteString(literal.Value);
                    _builder.Append(')');
                    break;

                case Abstract.Pattern.IntPattern literal:
                    _builder.Append("int(").Append(literal.Value.ToString(CultureInfo.InvariantCulture)).Append(')');
                    break;

                case Abstract.Pattern.FloatPattern literal:
                    _builder.Append("float(").Append(literal.Value.ToString("R", CultureInfo.InvariantCulture)).Append(
                        ')');

                    break;

                case Abstract.Pattern.TuplePattern tuple:
                    _builder.Append("tuple(");
                    WritePatterns(tuple.Elements);
                    _builder.Append(')');
                    break;

                case Abstract.Pattern.RecordPattern record:
                    _builder.Append("record([");

                    foreach (var field in record.Fields)
                    {
                        WriteString(field.FieldName);
                        _builder.Append(';');
                    }

                    _builder.Append("])");
                    break;

                case Abstract.Pattern.UnConsPattern cons:
                    _builder.Append("cons(");
                    WritePattern(cons.Head);
                    _builder.Append(',');
                    WritePattern(cons.Tail);
                    _builder.Append(')');
                    break;

                case Abstract.Pattern.ListPattern list:
                    _builder.Append("list(");
                    WritePatterns(list.Elements);
                    _builder.Append(')');
                    break;

                case Abstract.Pattern.NamedPattern named:
                    _builder.Append("named(");
                    WriteQualifiedName(named.Name.ModuleName, named.Name.Name);
                    _builder.Append(',');
                    WritePatterns(named.Arguments);
                    _builder.Append(')');
                    break;

                case Abstract.Pattern.AsPattern alias:
                    _builder.Append("as(");
                    WritePattern(alias.Pattern);
                    _builder.Append(',');
                    WriteString(alias.Name);
                    _builder.Append(')');
                    break;

                default:
                    throw new NotImplementedException(
                        "WritePattern does not handle pattern variant: " + pattern.GetType().Name);
            }
        }

        private void WriteTypeAnnotation(Abstract.TypeAnnotation annotation)
        {
            switch (annotation)
            {
                case Abstract.TypeAnnotation.GenericType generic:
                    _builder.Append("generic(");
                    WriteString(generic.Name);
                    _builder.Append(')');
                    break;

                case Abstract.TypeAnnotation.Typed typed:
                    _builder.Append("typed(");
                    WriteQualifiedName(typed.ModuleName, typed.Name);
                    _builder.Append(",[");

                    foreach (var argument in typed.TypeArguments)
                    {
                        WriteTypeAnnotation(argument);
                        _builder.Append(';');
                    }

                    _builder.Append("])");
                    break;

                case Abstract.TypeAnnotation.Unit:
                    _builder.Append("unit");
                    break;

                case Abstract.TypeAnnotation.Tupled tuple:
                    _builder.Append("tuple([");

                    foreach (var element in tuple.TypeAnnotations)
                    {
                        WriteTypeAnnotation(element);
                        _builder.Append(';');
                    }

                    _builder.Append("])");
                    break;

                case Abstract.TypeAnnotation.Record record:
                    _builder.Append("record(");
                    WriteRecordDefinition(record.RecordDefinition);
                    _builder.Append(')');
                    break;

                case Abstract.TypeAnnotation.GenericRecord record:
                    _builder.Append("extend(");
                    WriteString(record.GenericName);
                    _builder.Append(',');
                    WriteRecordDefinition(record.RecordDefinition);
                    _builder.Append(')');
                    break;

                case Abstract.TypeAnnotation.FunctionTypeAnnotation function:
                    _builder.Append("arrow(");
                    WriteTypeAnnotation(function.ArgumentType);
                    _builder.Append(',');
                    WriteTypeAnnotation(function.ReturnType);
                    _builder.Append(')');
                    break;

                default:
                    throw new NotImplementedException(
                        "WriteTypeAnnotation does not handle type annotation variant: " +
                        annotation.GetType().Name);
            }
        }

        private void WriteRecordDefinition(Abstract.RecordDefinition record)
        {
            _builder.Append('[');

            foreach (var field in record.Fields)
            {
                WriteString(field.FieldName);
                _builder.Append(',');
                WriteTypeAnnotation(field.FieldType);
                _builder.Append(';');
            }

            _builder.Append(']');
        }

        private void WriteQualifiedName(ModuleName moduleName, string name)
        {
            _builder.Append('[');

            foreach (var segment in moduleName)
            {
                WriteString(segment);
                _builder.Append(';');
            }

            _builder.Append("],");
            WriteString(name);
        }

        private void WriteString(string value)
        {
            _builder.Append('"');

            foreach (var character in value)
            {
                switch (character)
                {
                    case '"':
                        _builder.Append("\\\"");
                        break;

                    case '\\':
                        _builder.Append("\\\\");
                        break;

                    default:
                        if (character < ' ')
                        {
                            _builder.Append("\\u")
                                .Append(((int)character).ToString("x4", CultureInfo.InvariantCulture));
                        }
                        else
                        {
                            _builder.Append(character);
                        }

                        break;
                }
            }

            _builder.Append('"');
        }
    }
}
