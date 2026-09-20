using Pine.Core.CodeAnalysis;
using Pine.Core.Elm.ElmSyntax.ElmSyntaxAbstract;
using Pine.Core.Internal;
using Pine.Core.Json;
using System.Collections.Generic;
using System.Linq;
using System.Text.Json.Serialization;

using ModuleName = System.Collections.Generic.IReadOnlyList<string>;
using AbstractCase = Pine.Core.Elm.ElmSyntax.ElmSyntaxAbstract.Case;
using AbstractDeclaration = Pine.Core.Elm.ElmSyntax.ElmSyntaxAbstract.Declaration;
using AbstractExpression = Pine.Core.Elm.ElmSyntax.ElmSyntaxAbstract.Expression;
using AbstractFunctionImplementation = Pine.Core.Elm.ElmSyntax.ElmSyntaxAbstract.FunctionImplementation;
using AbstractFunctionStruct = Pine.Core.Elm.ElmSyntax.ElmSyntaxAbstract.FunctionStruct;
using AbstractLetDeclaration = Pine.Core.Elm.ElmSyntax.ElmSyntaxAbstract.LetDeclaration;
using AbstractPattern = Pine.Core.Elm.ElmSyntax.ElmSyntaxAbstract.Pattern;
using AbstractRecordSetter = Pine.Core.Elm.ElmSyntax.ElmSyntaxAbstract.RecordSetter;

namespace Pine.Core.Elm.ElmSyntax;

#pragma warning disable CS1591

public partial class ElmSyntaxInterpreter
{
    [JsonConverter(typeof(JsonConverterForChoiceType))]
    public abstract record PreparedDeclaration
    {
        public sealed record FunctionDeclaration(
            PreparedFunctionStruct Function)
            : PreparedDeclaration;

        public sealed record ChoiceTypeDeclaration(
            ChoiceTypeStruct TypeDeclaration)
            : PreparedDeclaration;

        public sealed record AliasDeclaration(
            TypeAlias TypeAlias)
            : PreparedDeclaration;

        public sealed record PortDeclaration(
            Signature Signature)
            : PreparedDeclaration;

        public sealed record InfixDeclaration(
            Infix Infix)
            : PreparedDeclaration;
    }

    public record PreparedFunctionStruct(
        Signature? Signature,
        PreparedFunctionImplementation Declaration);

    public record PreparedFunctionImplementation(
        string Name,
        IReadOnlyList<AbstractPattern> Arguments,
        PreparedExpression Expression);

    [JsonConverter(typeof(JsonConverterForChoiceType))]
    public abstract record PreparedExpression
    {
        public sealed record ValueLiteral(
            PineValueInProcess Value)
            : PreparedExpression
        {
            public bool Equals(ValueLiteral? other) =>
                ReferenceEquals(this, other) ||
                other is not null &&
                PineValueInProcess.AreEqual(Value, other.Value);

            public override int GetHashCode() =>
                Value.Evaluate().GetHashCode();
        }

        public sealed record Negation(
            PreparedExpression Expression)
            : PreparedExpression;

        public sealed record ListExpr(
            IReadOnlyList<PreparedExpression> Elements)
            : PreparedExpression;

        public sealed record Identifier(
            DeclQualifiedName QualifiedName)
            : PreparedExpression
        {
            public static Identifier Create(
                ModuleName moduleName,
                string name) =>
                new(DeclQualifiedName.Create(moduleName, name));
        }

        public sealed record IfBlock(
            PreparedExpression Condition,
            PreparedExpression ThenBlock,
            PreparedExpression ElseBlock)
            : PreparedExpression;

        public sealed record PrefixOperator(
            string Operator)
            : PreparedExpression;

        public sealed record Application(
            PreparedExpression Function,
            IReadOnlyList<PreparedExpression> Arguments)
            : PreparedExpression;

        public sealed record OperatorApplication(
            string Operator,
            SyntaxModel.InfixDirection Direction,
            PreparedExpression Left,
            PreparedExpression Right)
            : PreparedExpression;

        public sealed record TupledExpression(
            IReadOnlyList<PreparedExpression> Elements)
            : PreparedExpression;

        public sealed record LambdaExpression(
            IReadOnlyList<AbstractPattern> Arguments,
            PreparedExpression Expression)
            : PreparedExpression;

        public sealed record CaseExpression(
            PreparedExpression Expression,
            PreparedCaseDispatch Dispatch)
            : PreparedExpression;

        public sealed record LetExpression(
            IReadOnlyList<PreparedLetDeclaration> Declarations,
            PreparedExpression Expression)
            : PreparedExpression;

        public sealed record RecordExpr(
            IReadOnlyList<PreparedRecordSetter> Fields)
            : PreparedExpression;

        public sealed record RecordAccess(
            PreparedExpression Record,
            string FieldName,
            PineValue FieldNameValue)
            : PreparedExpression;

        public sealed record RecordAccessFunction(
            string FieldName,
            PineValue FieldNameValue)
            : PreparedExpression;

        public sealed record RecordUpdateExpression(
            string RecordName,
            IReadOnlyList<PreparedRecordSetter> Fields)
            : PreparedExpression;

        public sealed record GLSLExpression(
            string Code)
            : PreparedExpression;
    }

    [JsonConverter(typeof(JsonConverterForChoiceType))]
    public abstract record PreparedLetDeclaration
    {
        public sealed record LetFunction(
            PreparedFunctionStruct Function)
            : PreparedLetDeclaration;

        public sealed record LetDestructuring(
            AbstractPattern Pattern,
            PreparedExpression Expression)
            : PreparedLetDeclaration;
    }

    public record PreparedRecordSetter(
        string FieldName,
        PineValue FieldNameValue,
        PreparedExpression Value);

    public record PreparedCaseDispatch(
        IReadOnlyList<PreparedCaseDispatchSegment> Steps);

    [JsonConverter(typeof(JsonConverterForChoiceType))]
    public abstract record PreparedCaseDispatchSegment
    {
        public sealed record ConstantValuePatterns(
            IReadOnlyList<PreparedConstantCase> Cases)
            : PreparedCaseDispatchSegment;

        public sealed record PatternCase(
            AbstractPattern Pattern,
            PreparedExpression Expression)
            : PreparedCaseDispatchSegment;

        public sealed record DiscardCase(
            PreparedExpression Expression)
            : PreparedCaseDispatchSegment;
    }

    public record PreparedConstantCase(
        AbstractPattern Pattern,
        PineValue Value,
        PreparedExpression Expression);

    internal static PreparedDeclaration PrepareDeclaration(
        AbstractDeclaration declaration) =>
        PrepareDeclaration(declaration, closedExpressionReducer: null);

    private static PreparedDeclaration PrepareDeclaration(
        AbstractDeclaration declaration,
        ClosedExpressionReducer? closedExpressionReducer) =>
        declaration switch
        {
            AbstractDeclaration.FunctionDeclaration functionDeclaration =>
            new PreparedDeclaration.FunctionDeclaration(
                PrepareFunctionStruct(functionDeclaration.Function, closedExpressionReducer)),

            AbstractDeclaration.ChoiceTypeDeclaration choiceTypeDeclaration =>
            new PreparedDeclaration.ChoiceTypeDeclaration(choiceTypeDeclaration.TypeDeclaration),

            AbstractDeclaration.AliasDeclaration aliasDeclaration =>
            new PreparedDeclaration.AliasDeclaration(aliasDeclaration.TypeAlias),

            AbstractDeclaration.PortDeclaration portDeclaration =>
            new PreparedDeclaration.PortDeclaration(portDeclaration.Signature),

            AbstractDeclaration.InfixDeclaration infixDeclaration =>
            new PreparedDeclaration.InfixDeclaration(infixDeclaration.Infix),

            _ =>
            throw new System.NotImplementedException(
                "PrepareDeclaration does not handle declaration variant: " + declaration.GetType().Name),
        };

    internal static PreparedFunctionStruct PrepareFunctionStruct(
        AbstractFunctionStruct functionStruct) =>
        PrepareFunctionStruct(functionStruct, closedExpressionReducer: null);

    private static PreparedFunctionStruct PrepareFunctionStruct(
        AbstractFunctionStruct functionStruct,
        ClosedExpressionReducer? closedExpressionReducer) =>
        new(
            functionStruct.Signature,
            PrepareFunctionImplementation(functionStruct.Declaration, closedExpressionReducer));

    internal static PreparedFunctionImplementation PrepareFunctionImplementation(
        AbstractFunctionImplementation functionImplementation) =>
        PrepareFunctionImplementation(functionImplementation, closedExpressionReducer: null);

    private static PreparedFunctionImplementation PrepareFunctionImplementation(
        AbstractFunctionImplementation functionImplementation,
        ClosedExpressionReducer? closedExpressionReducer) =>
        new(
            functionImplementation.Name,
            functionImplementation.Arguments,
            PrepareExpression(functionImplementation.Expression, closedExpressionReducer));

    internal static PreparedExpression PrepareExpression(
        AbstractExpression expression) =>
        PrepareExpression(expression, closedExpressionReducer: null);

    private static PreparedExpression PrepareExpression(
        AbstractExpression expression,
        ClosedExpressionReducer? closedExpressionReducer)
    {
        PreparedExpression preparedExpression =
            expression switch
            {
                AbstractExpression.UnitExpr =>
                PrepareValueLiteral(PineValue.EmptyList),

                AbstractExpression.StringLiteral stringLiteral =>
                PrepareValueLiteral(stringLiteral.ValueAsPineValue),

                AbstractExpression.CharLiteral charLiteral =>
                PrepareValueLiteral(charLiteral.ValueAsPineValue),

                AbstractExpression.IntegerLiteral integerLiteral =>
                PrepareValueLiteral(integerLiteral.ValueAsPineValue),

                AbstractExpression.FloatLiteral floatLiteral =>
                PrepareValueLiteral(
                    ElmValueEncoding.ElmValueAsPineValue(
                        ElmValue.ElmFloat.Normalized(
                            floatLiteral.Numerator,
                            floatLiteral.Denominator))),

                AbstractExpression.Negation negation =>
                new PreparedExpression.Negation(PrepareExpression(negation.Expression, closedExpressionReducer)),

                AbstractExpression.ListExpr listExpr =>
                new PreparedExpression.ListExpr(
                    [.. listExpr.Elements.Select(element => PrepareExpression(element, closedExpressionReducer))]),

                AbstractExpression.Identifier identifier =>
                new PreparedExpression.Identifier(identifier.QualifiedName),

                AbstractExpression.IfBlock ifBlock =>
                new PreparedExpression.IfBlock(
                    PrepareExpression(ifBlock.Condition, closedExpressionReducer),
                    PrepareExpression(ifBlock.ThenBlock, closedExpressionReducer),
                    PrepareExpression(ifBlock.ElseBlock, closedExpressionReducer)),

                AbstractExpression.PrefixOperator prefixOperator =>
                new PreparedExpression.PrefixOperator(prefixOperator.Operator),

                AbstractExpression.Application application =>
                new PreparedExpression.Application(
                    PrepareExpression(application.Function, closedExpressionReducer),
                    [
                    .. application.Arguments.Select(argument => PrepareExpression(argument, closedExpressionReducer))
                    ]),

                AbstractExpression.OperatorApplication operatorApplication =>
                new PreparedExpression.OperatorApplication(
                    operatorApplication.Operator,
                    operatorApplication.Direction,
                    PrepareExpression(operatorApplication.Left, closedExpressionReducer),
                    PrepareExpression(operatorApplication.Right, closedExpressionReducer)),

                AbstractExpression.TupledExpression tupledExpression =>
                new PreparedExpression.TupledExpression(
                    [
                    .. tupledExpression.Elements.Select(element => PrepareExpression(element, closedExpressionReducer))
                    ]),

                AbstractExpression.LambdaExpression lambdaExpression =>
                new PreparedExpression.LambdaExpression(
                    lambdaExpression.Arguments,
                    PrepareExpression(lambdaExpression.Expression, closedExpressionReducer)),

                AbstractExpression.CaseExpression caseExpression =>
                new PreparedExpression.CaseExpression(
                    PrepareExpression(caseExpression.Expression, closedExpressionReducer),
                    PrepareCaseDispatch(caseExpression.Cases, closedExpressionReducer)),

                AbstractExpression.LetExpression letExpression =>
                new PreparedExpression.LetExpression(
                    [
                    .. letExpression.Declarations.Select(
                        declaration => PrepareLetDeclaration(declaration, closedExpressionReducer))
                    ],
                    PrepareExpression(letExpression.Expression, closedExpressionReducer)),

                AbstractExpression.RecordExpr recordExpr =>
                new PreparedExpression.RecordExpr(
                    [.. recordExpr.Fields.Select(field => PrepareRecordSetter(field, closedExpressionReducer))]),

                AbstractExpression.RecordAccess recordAccess =>
                new PreparedExpression.RecordAccess(
                    PrepareExpression(recordAccess.Record, closedExpressionReducer),
                    recordAccess.FieldName,
                    recordAccess.FieldNameValue),

                AbstractExpression.RecordAccessFunction recordAccessFunction =>
                new PreparedExpression.RecordAccessFunction(
                    recordAccessFunction.FieldName,
                    recordAccessFunction.FieldNameValue),

                AbstractExpression.RecordUpdateExpression recordUpdateExpression =>
                new PreparedExpression.RecordUpdateExpression(
                    recordUpdateExpression.RecordName,
                    [
                    .. recordUpdateExpression.Fields.Select(field => PrepareRecordSetter(field, closedExpressionReducer))
                    ]),

                AbstractExpression.GLSLExpression glslExpression =>
                new PreparedExpression.GLSLExpression(glslExpression.ShaderCode),

                _ =>
                throw new System.NotImplementedException(
                    "PrepareExpression does not handle expression variant: " + expression.GetType().Name),
            };

        return closedExpressionReducer?.Reduce(expression, preparedExpression) ?? preparedExpression;
    }

    private static PreparedExpression.ValueLiteral PrepareValueLiteral(PineValue value) =>
        new(PineValueInProcess.CreateFullyRepresented(value));

    private static PreparedLetDeclaration PrepareLetDeclaration(
        AbstractLetDeclaration declaration,
        ClosedExpressionReducer? closedExpressionReducer) =>
        declaration switch
        {
            AbstractLetDeclaration.LetFunction letFunction =>
            new PreparedLetDeclaration.LetFunction(
                PrepareFunctionStruct(letFunction.Function, closedExpressionReducer)),

            AbstractLetDeclaration.LetDestructuring letDestructuring =>
            new PreparedLetDeclaration.LetDestructuring(
                letDestructuring.Pattern,
                PrepareExpression(letDestructuring.Expression, closedExpressionReducer)),

            _ =>
            throw new System.NotImplementedException(
                "PrepareLetDeclaration does not handle let declaration variant: " + declaration.GetType().Name),
        };

    private static PreparedRecordSetter PrepareRecordSetter(
        AbstractRecordSetter recordSetter,
        ClosedExpressionReducer? closedExpressionReducer) =>
        new(
            recordSetter.FieldName,
            recordSetter.FieldNameValue,
            PrepareExpression(recordSetter.Value, closedExpressionReducer));

    private static PreparedCaseDispatch PrepareCaseDispatch(
        IReadOnlyList<AbstractCase> cases,
        ClosedExpressionReducer? closedExpressionReducer)
    {
        var steps = new List<PreparedCaseDispatchSegment>();
        List<PreparedConstantCase>? pendingConstantCases = null;

        void FlushConstantCases()
        {
            if (pendingConstantCases is null || pendingConstantCases.Count is 0)
                return;

            steps.Add(new PreparedCaseDispatchSegment.ConstantValuePatterns(pendingConstantCases));
            pendingConstantCases = null;
        }

        foreach (var caseNode in cases)
        {
            var preparedExpression = PrepareExpression(caseNode.Expression, closedExpressionReducer);

            if (caseNode.Pattern is AbstractPattern.AllPattern)
            {
                FlushConstantCases();
                steps.Add(new PreparedCaseDispatchSegment.DiscardCase(preparedExpression));
                continue;
            }

            if (TryPrepareConstantPatternValue(caseNode.Pattern, out var constantValue))
            {
                pendingConstantCases ??= [];
                pendingConstantCases.Add(new PreparedConstantCase(caseNode.Pattern, constantValue, preparedExpression));
                continue;
            }

            FlushConstantCases();
            steps.Add(new PreparedCaseDispatchSegment.PatternCase(caseNode.Pattern, preparedExpression));
        }

        FlushConstantCases();

        return new PreparedCaseDispatch(steps);
    }

    private static bool TryPrepareConstantPatternValue(
        AbstractPattern pattern,
        out PineValue constantValue)
    {
        switch (pattern)
        {
            case AbstractPattern.UnitPattern:
                constantValue = PineValue.EmptyList;
                return true;

            case AbstractPattern.CharPattern charPattern:
                constantValue = charPattern.ValueAsPineValue;
                return true;

            case AbstractPattern.StringPattern stringPattern:
                constantValue = stringPattern.ValueAsPineValue;
                return true;

            case AbstractPattern.IntPattern intPattern:
                constantValue = intPattern.ValueAsPineValue;
                return true;

            case AbstractPattern.FloatPattern floatPattern:
                constantValue =
                    ElmValueEncoding.ElmValueAsPineValue(
                        ElmValue.ElmFloat.Convert(floatPattern.Value));

                return true;

            case AbstractPattern.TuplePattern tuplePattern:
                if (TryPrepareConstantSequenceValue(tuplePattern.Elements, out var tupleValue))
                {
                    constantValue = tupleValue;
                    return true;
                }

                constantValue = PineValue.EmptyList;
                return false;

            case AbstractPattern.RecordPattern:
                constantValue = PineValue.EmptyList;
                return false;

            case AbstractPattern.UnConsPattern unConsPattern:
                if (TryPrepareConstantListValue(unConsPattern, out var unConsValue))
                {
                    constantValue = unConsValue;
                    return true;
                }

                constantValue = PineValue.EmptyList;
                return false;

            case AbstractPattern.ListPattern listPattern:
                if (TryPrepareConstantSequenceValue(listPattern.Elements, out var listValue))
                {
                    constantValue = listValue;
                    return true;
                }

                constantValue = PineValue.EmptyList;
                return false;

            case AbstractPattern.NamedPattern namedPattern:
                if (TryPrepareConstantNamedPatternValue(namedPattern, out var namedPatternValue))
                {
                    constantValue = namedPatternValue;
                    return true;
                }

                constantValue = PineValue.EmptyList;
                return false;

            case AbstractPattern.AllPattern:
            case AbstractPattern.VarPattern:
            case AbstractPattern.AsPattern:
                constantValue = PineValue.EmptyList;
                return false;

            default:
                throw new System.NotImplementedException(
                    "TryPrepareConstantPatternValue does not handle pattern variant: " + pattern.GetType().Name);
        }
    }

    private static bool TryPrepareConstantNamedPatternValue(
        AbstractPattern.NamedPattern namedPattern,
        out PineValue constantValue)
    {
        if (namedPattern.Arguments.Count is 0)
        {
            if (namedPattern.Name.Name is "True")
            {
                constantValue = Pine.Core.PineVM.PineKernelValues.TrueValue;
                return true;
            }

            if (namedPattern.Name.Name is "False")
            {
                constantValue = Pine.Core.PineVM.PineKernelValues.FalseValue;
                return true;
            }
        }

        var argumentValues = new PineValue[namedPattern.Arguments.Count];

        for (var i = 0; i < namedPattern.Arguments.Count; i++)
        {
            if (!TryPrepareConstantPatternValue(namedPattern.Arguments[i], out var argumentValue))
            {
                constantValue = PineValue.EmptyList;
                return false;
            }

            argumentValues[i] = argumentValue;
        }

        constantValue =
            PineValue.List(
                [
                ElmValue.ElmChoiceTypeTagNameAsValue,
                namedPattern.TagNameAsPineValue,
                .. argumentValues
                ]);

        return true;
    }

    private static bool TryPrepareConstantListValue(
        AbstractPattern pattern,
        out PineValue.ListValue constantValue)
    {
        switch (pattern)
        {
            case AbstractPattern.AllPattern:
            case AbstractPattern.VarPattern:
            case AbstractPattern.UnitPattern:
            case AbstractPattern.CharPattern:
            case AbstractPattern.StringPattern:
            case AbstractPattern.IntPattern:
            case AbstractPattern.FloatPattern:
            case AbstractPattern.TuplePattern:
            case AbstractPattern.RecordPattern:
            case AbstractPattern.NamedPattern:
            case AbstractPattern.AsPattern:
                constantValue = PineValue.EmptyList;
                return false;

            case AbstractPattern.UnConsPattern unConsPattern:
                {
                    if (!TryPrepareConstantPatternValue(unConsPattern.Head, out var headValue) ||
                        !TryPrepareConstantListValue(unConsPattern.Tail, out var tailValue))
                    {
                        constantValue = PineValue.EmptyList;
                        return false;
                    }

                    var items = new PineValue[tailValue.Items.Length + 1];
                    items[0] = headValue;

                    for (var i = 0; i < tailValue.Items.Length; i++)
                        items[i + 1] = tailValue.Items.Span[i];

                    constantValue = PineValue.List(items);
                    return true;
                }

            case AbstractPattern.ListPattern listPattern:
                return TryPrepareConstantSequenceValue(listPattern.Elements, out constantValue);

            default:
                throw new System.NotImplementedException(
                    "TryPrepareConstantListValue does not handle pattern variant: " + pattern.GetType().Name);
        }
    }

    private static bool TryPrepareConstantSequenceValue(
        IReadOnlyList<AbstractPattern> elementPatterns,
        out PineValue.ListValue constantValue)
    {
        var elementValues = new PineValue[elementPatterns.Count];

        for (var i = 0; i < elementPatterns.Count; i++)
        {
            if (!TryPrepareConstantPatternValue(elementPatterns[i], out var elementValue))
            {
                constantValue = PineValue.EmptyList;
                return false;
            }

            elementValues[i] = elementValue;
        }

        constantValue = PineValue.List(elementValues);
        return true;
    }

#pragma warning restore CS1591
}
