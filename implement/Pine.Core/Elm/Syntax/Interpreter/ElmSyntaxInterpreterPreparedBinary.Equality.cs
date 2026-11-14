using System;
using System.Collections.Generic;
using System.Runtime.CompilerServices;

using Engine = Pine.Core.Elm.Syntax.Interpreter.ElmSyntaxInterpreter;
using Expr = Pine.Core.Elm.Syntax.Interpreter.ElmSyntaxInterpreter.PreparedExpression;

namespace Pine.Core.Elm.Syntax.Interpreter;

public static partial class ElmSyntaxInterpreterPreparedBinary
{
    // Prepared records use reference equality for list fields; wire reuse must compare their contents.
    private sealed class PreparedExpressionComparer : IEqualityComparer<object>
    {
        internal static readonly PreparedExpressionComparer Instance = new();

        private sealed record ExpressionHash(int Value);

        // Avoid rehashing every descendant at each level, without retaining prepared trees.
        private readonly ConditionalWeakTable<Expr, ExpressionHash> _expressionHashes = new();

        public new bool Equals(object? x, object? y)
        {
            if (ReferenceEquals(x, y))
                return true;

            if (x is null || y is null || x.GetType() != y.GetType())
                return false;

            return x switch
            {
                Expr expression =>
                EqualExpression(expression, (Expr)y),

                Engine.PreparedFunctionStruct function =>
                function.Signature == ((Engine.PreparedFunctionStruct)y).Signature &&
                Equals(function.Declaration, ((Engine.PreparedFunctionStruct)y).Declaration),

                Engine.PreparedFunctionImplementation implementation =>
                implementation.Name == ((Engine.PreparedFunctionImplementation)y).Name &&
                Sequence(implementation.Arguments, ((Engine.PreparedFunctionImplementation)y).Arguments) &&
                Equals(implementation.Expression, ((Engine.PreparedFunctionImplementation)y).Expression),

                Engine.PreparedRecordSetter setter =>
                setter.FieldName == ((Engine.PreparedRecordSetter)y).FieldName &&
                setter.FieldNameValue == ((Engine.PreparedRecordSetter)y).FieldNameValue &&
                Equals(setter.Value, ((Engine.PreparedRecordSetter)y).Value),

                Engine.PreparedCaseDispatch dispatch =>
                Sequence(dispatch.Steps, ((Engine.PreparedCaseDispatch)y).Steps),

                Engine.PreparedConstantCase branch =>
                branch.Pattern == ((Engine.PreparedConstantCase)y).Pattern &&
                branch.Value == ((Engine.PreparedConstantCase)y).Value &&
                Equals(branch.Expression, ((Engine.PreparedConstantCase)y).Expression),

                Engine.PreparedLetDeclaration declaration =>
                EqualLet(declaration, (Engine.PreparedLetDeclaration)y),

                Engine.PreparedCaseDispatchSegment segment =>
                EqualSegment(segment, (Engine.PreparedCaseDispatchSegment)y),

                _ =>
                x.Equals(y)
            };
        }

        private bool Sequence<T>(IReadOnlyList<T> a, IReadOnlyList<T> b) where T : class
        {
            if (a.Count != b.Count)
                return false;

            for (var index = 0; index < a.Count; ++index)
            {
                if (!Equals(a[index], b[index]))
                    return false;
            }

            return true;
        }

        private bool EqualExpression(Expr x, Expr y) =>
            x switch
            {
                Expr.ValueLiteral literal =>
                literal.Value.Evaluate().Equals(((Expr.ValueLiteral)y).Value.Evaluate()),

                Expr.Negation negation =>
                Equals(negation.Expression, ((Expr.Negation)y).Expression),

                Expr.ListExpr list =>
                Sequence(list.Elements, ((Expr.ListExpr)y).Elements),

                Expr.Identifier identifier =>
                identifier.QualifiedName == ((Expr.Identifier)y).QualifiedName,

                Expr.IfBlock conditional =>
                Equals(conditional.Condition, ((Expr.IfBlock)y).Condition) &&
                Equals(conditional.ThenBlock, ((Expr.IfBlock)y).ThenBlock) &&
                Equals(conditional.ElseBlock, ((Expr.IfBlock)y).ElseBlock),

                Expr.PrefixOperator prefix =>
                prefix.Operator == ((Expr.PrefixOperator)y).Operator,

                Expr.Application application =>
                Equals(application.Function, ((Expr.Application)y).Function) &&
                Sequence(application.Arguments, ((Expr.Application)y).Arguments),

                Expr.OperatorApplication operation =>
                operation.Operator == ((Expr.OperatorApplication)y).Operator &&
                operation.Direction == ((Expr.OperatorApplication)y).Direction &&
                Equals(operation.Left, ((Expr.OperatorApplication)y).Left) &&
                Equals(operation.Right, ((Expr.OperatorApplication)y).Right),

                Expr.TupledExpression tuple =>
                Sequence(tuple.Elements, ((Expr.TupledExpression)y).Elements),

                Expr.LambdaExpression lambda =>
                Sequence(lambda.Arguments, ((Expr.LambdaExpression)y).Arguments) &&
                Equals(lambda.Expression, ((Expr.LambdaExpression)y).Expression),

                Expr.CaseExpression cases =>
                Equals(cases.Expression, ((Expr.CaseExpression)y).Expression) &&
                Equals(cases.Dispatch, ((Expr.CaseExpression)y).Dispatch),

                Expr.LetExpression let =>
                Sequence(let.Declarations, ((Expr.LetExpression)y).Declarations) &&
                Equals(let.Expression, ((Expr.LetExpression)y).Expression),

                Expr.RecordExpr record =>
                Sequence(record.Fields, ((Expr.RecordExpr)y).Fields),

                Expr.RecordAccess access =>
                access.FieldName == ((Expr.RecordAccess)y).FieldName &&
                access.FieldNameValue == ((Expr.RecordAccess)y).FieldNameValue &&
                Equals(access.Record, ((Expr.RecordAccess)y).Record),

                Expr.RecordAccessFunction access =>
                access.FieldName == ((Expr.RecordAccessFunction)y).FieldName &&
                access.FieldNameValue == ((Expr.RecordAccessFunction)y).FieldNameValue,

                Expr.RecordUpdateExpression update =>
                update.RecordName == ((Expr.RecordUpdateExpression)y).RecordName &&
                Sequence(update.Fields, ((Expr.RecordUpdateExpression)y).Fields),

                Expr.GLSLExpression glsl =>
                glsl.Code == ((Expr.GLSLExpression)y).Code,

                _ =>
                throw new NotImplementedException(
                    nameof(EqualExpression) + " does not handle expression variant: " + x.GetType().Name)
            };

        private bool EqualLet(Engine.PreparedLetDeclaration x, Engine.PreparedLetDeclaration y) =>
            x switch
            {
                Engine.PreparedLetDeclaration.LetFunction function =>
                Equals(function.Function, ((Engine.PreparedLetDeclaration.LetFunction)y).Function),

                Engine.PreparedLetDeclaration.LetDestructuring destructuring =>
                destructuring.Pattern == ((Engine.PreparedLetDeclaration.LetDestructuring)y).Pattern &&
                Equals(destructuring.Expression, ((Engine.PreparedLetDeclaration.LetDestructuring)y).Expression),

                _ =>
                throw new NotImplementedException(
                    nameof(EqualLet) + " does not handle let variant: " + x.GetType().Name)
            };

        private bool EqualSegment(Engine.PreparedCaseDispatchSegment x, Engine.PreparedCaseDispatchSegment y) =>
            x switch
            {
                Engine.PreparedCaseDispatchSegment.ConstantValuePatterns constants =>
                Sequence(constants.Cases, ((Engine.PreparedCaseDispatchSegment.ConstantValuePatterns)y).Cases),

                Engine.PreparedCaseDispatchSegment.PatternCase branch =>
                branch.Pattern == ((Engine.PreparedCaseDispatchSegment.PatternCase)y).Pattern &&
                Equals(branch.Expression, ((Engine.PreparedCaseDispatchSegment.PatternCase)y).Expression),

                Engine.PreparedCaseDispatchSegment.DiscardCase discard =>
                Equals(discard.Expression, ((Engine.PreparedCaseDispatchSegment.DiscardCase)y).Expression),

                _ =>
                throw new NotImplementedException(
                    nameof(EqualSegment) + " does not handle dispatch variant: " + x.GetType().Name)
            };

        public int GetHashCode(object value) =>
            value switch
            {
                Expr expression =>
                _expressionHashes.GetValue(
                    expression,
                    static expression => new(Instance.HashExpression(expression))).Value,

                Engine.PreparedFunctionStruct function =>
                HashCode.Combine(function.Signature, GetHashCode(function.Declaration)),

                Engine.PreparedFunctionImplementation implementation =>
                HashCode.Combine(
                    implementation.Name,
                    HashSequence(implementation.Arguments),
                    GetHashCode(implementation.Expression)),

                Engine.PreparedRecordSetter setter =>
                HashCode.Combine(setter.FieldName, setter.FieldNameValue, GetHashCode(setter.Value)),

                Engine.PreparedCaseDispatch dispatch => HashSequence(dispatch.Steps),

                Engine.PreparedConstantCase branch =>
                HashCode.Combine(branch.Pattern, branch.Value, GetHashCode(branch.Expression)),

                Engine.PreparedLetDeclaration declaration => HashLet(declaration),
                Engine.PreparedCaseDispatchSegment segment => HashSegment(segment),

                _ =>
                value.GetHashCode()
            };

        private int HashSequence<T>(IReadOnlyList<T> values) where T : class
        {
            var hash = new HashCode();
            hash.Add(values.Count);

            for (var index = 0; index < values.Count; ++index)
                hash.Add(GetHashCode(values[index]));

            return hash.ToHashCode();
        }

        private int HashExpression(Expr expression) =>
            HashCode.Combine(
                expression.GetType(),
                expression switch
                {
                    Expr.ValueLiteral literal => literal.GetHashCode(),
                    Expr.Negation negation => GetHashCode(negation.Expression),
                    Expr.ListExpr list => HashSequence(list.Elements),
                    Expr.Identifier identifier => identifier.QualifiedName.GetHashCode(),

                    Expr.IfBlock conditional =>
                    HashCode.Combine(
                        GetHashCode(conditional.Condition),
                        GetHashCode(conditional.ThenBlock),
                        GetHashCode(conditional.ElseBlock)),

                    Expr.PrefixOperator prefix => StringComparer.Ordinal.GetHashCode(prefix.Operator),

                    Expr.Application application =>
                    HashCode.Combine(GetHashCode(application.Function), HashSequence(application.Arguments)),

                    Expr.OperatorApplication operation =>
                    HashCode.Combine(
                        operation.Operator,
                        operation.Direction,
                        GetHashCode(operation.Left),
                        GetHashCode(operation.Right)),

                    Expr.TupledExpression tuple => HashSequence(tuple.Elements),

                    Expr.LambdaExpression lambda =>
                    HashCode.Combine(HashSequence(lambda.Arguments), GetHashCode(lambda.Expression)),

                    Expr.CaseExpression cases =>
                    HashCode.Combine(GetHashCode(cases.Expression), GetHashCode(cases.Dispatch)),

                    Expr.LetExpression let =>
                    HashCode.Combine(HashSequence(let.Declarations), GetHashCode(let.Expression)),

                    Expr.RecordExpr record => HashSequence(record.Fields),

                    Expr.RecordAccess access =>
                    HashCode.Combine(GetHashCode(access.Record), access.FieldName, access.FieldNameValue),

                    Expr.RecordAccessFunction access => HashCode.Combine(access.FieldName, access.FieldNameValue),

                    Expr.RecordUpdateExpression update =>
                    HashCode.Combine(update.RecordName, HashSequence(update.Fields)),

                    Expr.GLSLExpression glsl => StringComparer.Ordinal.GetHashCode(glsl.Code),

                    _ =>
                    throw new NotImplementedException(
                        nameof(HashExpression) + " does not handle expression variant: " + expression.GetType().Name)
                });

        private int HashLet(Engine.PreparedLetDeclaration declaration) =>
            declaration switch
            {
                Engine.PreparedLetDeclaration.LetFunction function =>
                GetHashCode(function.Function),

                Engine.PreparedLetDeclaration.LetDestructuring destructuring =>
                HashCode.Combine(destructuring.Pattern, GetHashCode(destructuring.Expression)),

                _ =>
                throw new NotImplementedException(
                    nameof(HashLet) + " does not handle let variant: " + declaration.GetType().Name)
            };

        private int HashSegment(Engine.PreparedCaseDispatchSegment segment) =>
            segment switch
            {
                Engine.PreparedCaseDispatchSegment.ConstantValuePatterns constants =>
                HashSequence(constants.Cases),

                Engine.PreparedCaseDispatchSegment.PatternCase branch =>
                HashCode.Combine(branch.Pattern, GetHashCode(branch.Expression)),

                Engine.PreparedCaseDispatchSegment.DiscardCase discard =>
                GetHashCode(discard.Expression),

                _ =>
                throw new NotImplementedException(
                    nameof(HashSegment) + " does not handle dispatch variant: " + segment.GetType().Name)
            };
    }
}
