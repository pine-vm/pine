using Pine.Core.Encodings;
using Pine.Core.PineLanguage;
using System.Linq;
using System.Numerics;

namespace Pine.Core.Elm.Syntax.Abstract;

/// <summary>
/// Converts the concrete Elm syntax model (namespace <see cref="Concrete"/>) into the abstract
/// model defined in this namespace.
/// <para>
/// The conversion drops source locations and trivia, unwraps redundant parentheses, normalizes
/// literals to their numeric/decoded form, and precomputes the <see cref="PineValue"/> instances
/// described on the abstract model types.
/// </para>
/// </summary>
public static class ConvertFromConcrete
{
    /// <summary>
    /// Converts a concrete <see cref="Concrete.File"/> into an abstract <see cref="File"/>.
    /// Comments and incomplete declarations are dropped.
    /// </summary>
    public static File FromFile(Concrete.File file) =>
        new(
            FromModule(file.ModuleDefinition.Value),
            [.. file.Imports.Select(import => FromImport(import.Value))],
            [.. file.Declarations.Select(declaration => FromDeclaration(declaration.Value))]);

    /// <summary>
    /// Converts a concrete <see cref="Concrete.Import"/> into an abstract <see cref="Import"/>.
    /// </summary>
    public static Import FromImport(Concrete.Import import) =>
        new(
            import.ModuleName.Value,
            import.ModuleAlias?.Alias.Value,
            import.ExposingList is { } exposingList
            ?
            FromExposing(exposingList.ExposingList.Value)
            :
            null);

    /// <summary>
    /// Converts a concrete <see cref="Concrete.Module"/> into an abstract <see cref="Module"/>.
    /// </summary>
    public static Module FromModule(Concrete.Module module) =>
        module switch
        {
            Concrete.Module.NormalModule normalModule =>
            new Module.NormalModule(FromDefaultModuleData(normalModule.ModuleData)),

            Concrete.Module.PortModule portModule =>
            new Module.PortModule(FromDefaultModuleData(portModule.ModuleData)),

            Concrete.Module.EffectModule effectModule =>
            new Module.EffectModule(FromEffectModuleData(effectModule.ModuleData)),

            _ =>
            throw new System.NotImplementedException(
                "Unexpected module type: " + module.GetType().FullName)
        };

    private static DefaultModuleData FromDefaultModuleData(Concrete.DefaultModuleData moduleData) =>
        new(
            moduleData.ModuleName.Value,
            FromExposing(moduleData.ExposingList.Value));

    private static EffectModuleData FromEffectModuleData(Concrete.EffectModuleData moduleData) =>
        new(
            moduleData.ModuleName.Value,
            FromExposing(moduleData.ExposingList.Value),
            moduleData.Command?.Value,
            moduleData.Subscription?.Value);

    /// <summary>
    /// Converts a concrete <see cref="Concrete.Exposing"/> into an abstract <see cref="Exposing"/>.
    /// </summary>
    public static Exposing FromExposing(Concrete.Exposing exposing) =>
        exposing switch
        {
            Concrete.Exposing.All =>
            new Exposing.All(),

            Concrete.Exposing.Explicit explicitExposing =>
            new Exposing.Explicit(
                [.. explicitExposing.Nodes.Nodes.Select(node => FromTopLevelExpose(node.Value))]),

            _ =>
            throw new System.NotImplementedException(
                "Unexpected exposing type: " + exposing.GetType().FullName)
        };

    private static TopLevelExpose FromTopLevelExpose(Concrete.TopLevelExpose topLevelExpose) =>
        topLevelExpose switch
        {
            Concrete.TopLevelExpose.InfixExpose infixExpose =>
            new TopLevelExpose.InfixExpose(infixExpose.Name),

            Concrete.TopLevelExpose.FunctionExpose functionExpose =>
            new TopLevelExpose.FunctionExpose(functionExpose.Name),

            Concrete.TopLevelExpose.TypeOrAliasExpose typeOrAliasExpose =>
            new TopLevelExpose.TypeOrAliasExpose(typeOrAliasExpose.Name),

            Concrete.TopLevelExpose.TypeExpose typeExpose =>
            new TopLevelExpose.TypeExpose(
                new ExposedType(
                    typeExpose.ExposedType.Name,
                    typeExpose.ExposedType.Open is not null)),

            _ =>
            throw new System.NotImplementedException(
                "Unexpected top-level expose type: " + topLevelExpose.GetType().FullName)
        };

    /// <summary>
    /// Converts a concrete <see cref="Concrete.Declaration"/> into an abstract <see cref="Declaration"/>.
    /// </summary>
    public static Declaration FromDeclaration(Concrete.Declaration declaration) =>
        declaration switch
        {
            Concrete.Declaration.FunctionDeclaration functionDeclaration =>
            new Declaration.FunctionDeclaration(FromFunctionStruct(functionDeclaration.Function)),

            Concrete.Declaration.ChoiceTypeDeclaration choiceTypeDeclaration =>
            new Declaration.ChoiceTypeDeclaration(FromTypeStruct(choiceTypeDeclaration.TypeDeclaration)),

            Concrete.Declaration.AliasDeclaration aliasDeclaration =>
            new Declaration.AliasDeclaration(FromTypeAlias(aliasDeclaration.TypeAlias)),

            Concrete.Declaration.PortDeclaration portDeclaration =>
            new Declaration.PortDeclaration(FromSignature(portDeclaration.Signature)),

            Concrete.Declaration.InfixDeclaration infixDeclaration =>
            new Declaration.InfixDeclaration(FromInfix(infixDeclaration.Infix)),

            _ =>
            throw new System.NotImplementedException(
                "Unexpected declaration type: " + declaration.GetType().FullName)
        };

    private static Infix FromInfix(Concrete.Infix infix) =>
        new(
            infix.Direction.Value,
            infix.Precedence.Value,
            infix.Operator.Value,
            infix.FunctionName.Value);

    private static TypeAlias FromTypeAlias(Concrete.TypeAlias typeAlias) =>
        new(
            typeAlias.Name.Value,
            [.. typeAlias.Generics.Select(generic => generic.Value)],
            FromTypeAnnotation(typeAlias.TypeAnnotation.Value));

    private static ChoiceTypeStruct FromTypeStruct(Concrete.ChoiceTypeStruct typeStruct) =>
        new(
            typeStruct.Name.Value,
            [.. typeStruct.Generics.Select(generic => generic.Value)],
            [.. typeStruct.Constructors.Select(constructor => FromValueConstructor(constructor.Value))]);

    private static ValueConstructor FromValueConstructor(Concrete.ValueConstructor valueConstructor) =>
        ValueConstructor.Create(
            valueConstructor.Name.Value,
            [.. valueConstructor.Arguments.Select(argument => FromTypeAnnotation(argument.Value))]);

    /// <summary>
    /// Converts a concrete <see cref="Concrete.TypeAnnotation"/> into an abstract <see cref="TypeAnnotation"/>.
    /// </summary>
    public static TypeAnnotation FromTypeAnnotation(Concrete.TypeAnnotation typeAnnotation) =>
        typeAnnotation switch
        {
            Concrete.TypeAnnotation.GenericType genericType =>
            new TypeAnnotation.GenericType(genericType.Name),

            Concrete.TypeAnnotation.Typed typed =>
            new TypeAnnotation.Typed(
                typed.TypeName.Value.ModuleName,
                typed.TypeName.Value.Name,
                [.. typed.TypeArguments.Select(argument => FromTypeAnnotation(argument.Value))]),

            Concrete.TypeAnnotation.Unit =>
            new TypeAnnotation.Unit(),

            Concrete.TypeAnnotation.Tupled tupled =>
            new TypeAnnotation.Tupled(
                [.. tupled.TypeAnnotations.Nodes.Select(node => FromTypeAnnotation(node.Value))]),

            Concrete.TypeAnnotation.Record record =>
            new TypeAnnotation.Record(FromRecordDefinition(record.RecordDefinition)),

            Concrete.TypeAnnotation.GenericRecord genericRecord =>
            new TypeAnnotation.GenericRecord(
                genericRecord.GenericName.Value,
                FromRecordDefinition(genericRecord.RecordDefinition.Value)),

            Concrete.TypeAnnotation.FunctionTypeAnnotation functionTypeAnnotation =>
            new TypeAnnotation.FunctionTypeAnnotation(
                FromTypeAnnotation(functionTypeAnnotation.ArgumentType.Value),
                FromTypeAnnotation(functionTypeAnnotation.ReturnType.Value)),

            _ =>
            throw new System.NotImplementedException(
                "Unexpected type annotation type: " + typeAnnotation.GetType().FullName)
        };

    private static RecordDefinition FromRecordDefinition(Concrete.RecordDefinition recordDefinition) =>
        new(
            [.. recordDefinition.Fields.Nodes.Select(node => FromRecordField(node.Value))]);

    private static RecordField FromRecordField(Concrete.RecordField recordField) =>
        RecordField.Create(
            recordField.FieldName.Value,
            FromTypeAnnotation(recordField.FieldType.Value));

    /// <summary>
    /// Converts a parsed function struct into the abstract representation, including its optional signature and implementation body.
    /// </summary>
    public static FunctionStruct FromFunctionStruct(Concrete.FunctionStruct functionStruct) =>
        new(
            functionStruct.Signature is { } signature
            ?
            FromSignature(signature.Value)
            :
            null,
            FromFunctionImplementation(functionStruct.Declaration.Value));

    private static FunctionImplementation FromFunctionImplementation(
        Concrete.FunctionImplementation functionImplementation) =>
        new(
            functionImplementation.Name.Value,
            [.. functionImplementation.Arguments.Select(argument => FromPattern(argument.Value))],
            FromExpression(functionImplementation.Expression.Value));

    private static Signature FromSignature(Concrete.Signature signature) =>
        new(
            signature.Name.Value,
            FromTypeAnnotation(signature.TypeAnnotation.Value));

    /// <summary>
    /// Converts a concrete <see cref="Concrete.Pattern"/> into an abstract <see cref="Pattern"/>.
    /// Parenthesized patterns are unwrapped and hex/decimal integer patterns are normalized.
    /// </summary>
    public static Pattern FromPattern(Concrete.Pattern pattern) =>
        pattern switch
        {
            Concrete.Pattern.AllPattern =>
            new Pattern.AllPattern(),

            Concrete.Pattern.VarPattern varPattern =>
            new Pattern.VarPattern(varPattern.Name),

            Concrete.Pattern.UnitPattern =>
            new Pattern.UnitPattern(),

            Concrete.Pattern.CharPattern charPattern =>
            new Pattern.CharPattern(charPattern.Value),

            Concrete.Pattern.StringPattern stringPattern =>
            new Pattern.StringPattern(stringPattern.Value),

            Concrete.Pattern.IntPattern intPattern =>
            new Pattern.IntPattern(intPattern.Value),

            Concrete.Pattern.HexPattern hexPattern =>
            new Pattern.IntPattern(hexPattern.Value),

            Concrete.Pattern.FloatPattern floatPattern =>
            new Pattern.FloatPattern(floatPattern.Value),

            Concrete.Pattern.TuplePattern tuplePattern =>
            new Pattern.TuplePattern(
                [.. tuplePattern.Elements.Nodes.Select(node => FromPattern(node.Value))]),

            Concrete.Pattern.RecordPattern recordPattern =>
            Pattern.RecordPattern.Create(
                [.. recordPattern.Fields.Nodes.Select(node => node.Value)]),

            Concrete.Pattern.UnConsPattern unConsPattern =>
            new Pattern.UnConsPattern(
                FromPattern(unConsPattern.Head.Value),
                FromPattern(unConsPattern.Tail.Value)),

            Concrete.Pattern.ListPattern listPattern =>
            new Pattern.ListPattern(
                [.. listPattern.Elements.Nodes.Select(node => FromPattern(node.Value))]),

            Concrete.Pattern.NamedPattern namedPattern =>
            new Pattern.NamedPattern(
                FromQualifiedNameRef(namedPattern.Name),
                [.. namedPattern.Arguments.Select(argument => FromPattern(argument.Value))]),

            Concrete.Pattern.AsPattern asPattern =>
            new Pattern.AsPattern(
                FromPattern(asPattern.Pattern.Value),
                asPattern.Name.Value),

            Concrete.Pattern.ParenthesizedPattern parenthesizedPattern =>
            FromPattern(parenthesizedPattern.Pattern.Value),

            _ =>
            throw new System.NotImplementedException(
                "Unexpected pattern type: " + pattern.GetType().FullName)
        };

    private static QualifiedNameRef FromQualifiedNameRef(Concrete.QualifiedNameRef qualifiedNameRef) =>
        new(
            qualifiedNameRef.ModuleName,
            qualifiedNameRef.Name);

    /// <summary>
    /// Converts a concrete <see cref="Concrete.Expression"/> into an abstract <see cref="Expression"/>.
    /// Redundant parentheses are unwrapped, literals are normalized, and the precomputed
    /// <see cref="PineValue"/> instances described on the abstract model are populated.
    /// </summary>
    public static Expression FromExpression(Concrete.Expression expression) =>
        expression switch
        {
            Concrete.Expression.UnitExpr =>
            Expression.UnitExpr.Instance,

            Concrete.Expression.StringLiteral literal =>
            Expression.StringLiteral.Create(literal.Value),

            Concrete.Expression.MultilineStringLiteral multiline =>
            Expression.StringLiteral.Create(multiline.Value),

            Concrete.Expression.CharLiteral charLiteral =>
            Expression.CharLiteral.Create(charLiteral.Value),

            Concrete.Expression.IntegerLiteral integer =>
            MakeInteger(ParseIntegerLiteral(integer.LiteralText)),

            Concrete.Expression.FloatLiteral floatable =>
            MakeFloatable(floatable.LiteralText),

            Concrete.Expression.Negation negation =>
            new Expression.Negation(FromExpression(negation.Expression.Value)),

            Concrete.Expression.ListExpr listExpr =>
            new Expression.ListExpr(
                [.. listExpr.Elements.Nodes.Select(node => FromExpression(node.Value))]),

            Concrete.Expression.Identifier functionOrValue =>
            Expression.Identifier.Create(functionOrValue.ModuleName, functionOrValue.Name),

            Concrete.Expression.IfBlock ifBlock =>
            new Expression.IfBlock(
                FromExpression(ifBlock.Condition.Value),
                FromExpression(ifBlock.ThenBlock.Value),
                FromExpression(ifBlock.ElseBlock.Value)),

            Concrete.Expression.PrefixOperator prefixOperator =>
            new Expression.PrefixOperator(prefixOperator.Operator),

            Concrete.Expression.Parenthesized parenthesizedExpression =>
            FromExpression(parenthesizedExpression.Expression.Value),

            Concrete.Expression.Application application =>
            new Expression.Application(
                FromExpression(application.Function.Value),
                [.. application.Arguments.Select(argument => FromExpression(argument.Value))]),

            Concrete.Expression.OperatorApplication operatorApplication =>
            new Expression.OperatorApplication(
                operatorApplication.Operator.Value,
                operatorApplication.Direction,
                FromExpression(operatorApplication.Left.Value),
                FromExpression(operatorApplication.Right.Value)),

            Concrete.Expression.TupledExpression tupledExpression =>
            new Expression.TupledExpression(
                [.. tupledExpression.Elements.Nodes.Select(node => FromExpression(node.Value))]),

            Concrete.Expression.LambdaExpression lambdaExpression =>
            new Expression.LambdaExpression(
                [.. lambdaExpression.Lambda.Arguments.Select(argument => FromPattern(argument.Value))],
                FromExpression(lambdaExpression.Lambda.Expression.Value)),

            Concrete.Expression.CaseExpression caseExpression =>
            new Expression.CaseExpression(
                FromExpression(caseExpression.CaseBlock.Expression.Value),
                [.. caseExpression.CaseBlock.Cases.Select(FromCase)]),

            Concrete.Expression.LetExpression letExpression =>
            new Expression.LetExpression(
                [.. letExpression.Value.Declarations.Select(declaration => FromLetDeclaration(declaration.Value))],
                FromExpression(letExpression.Value.Expression.Value)),

            Concrete.Expression.RecordExpr recordExpr =>
            Expression.RecordExpr.CreateSorted(
                [
                .. recordExpr.Fields.Nodes.Select(field => (field.FieldName.Value, FromExpression(field.ValueExpr.Value)))
                ]),

            Concrete.Expression.RecordAccess recordAccess =>
            new Expression.RecordAccess(
                FromExpression(recordAccess.Record.Value),
                recordAccess.FieldName.Value,
                StringEncoding.ValueFromString(recordAccess.FieldName.Value)),

            Concrete.Expression.RecordAccessFunction recordAccessFunction =>
            MakeRecordAccessFunction(recordAccessFunction.FunctionName),

            Concrete.Expression.RecordUpdateExpression recordUpdateExpression =>
            Expression.RecordUpdateExpression.CreateSorted(
                recordUpdateExpression.RecordName.Value,
                [
                .. recordUpdateExpression.Fields.Nodes.Select(
                    field => (field.FieldName.Value, FromExpression(field.ValueExpr.Value)))
                ]),

            Concrete.Expression.GLSLExpression glslExpression =>
            new Expression.GLSLExpression(glslExpression.ShaderCode),

            _ =>
            throw new System.NotImplementedException(
                "Unexpected expression type: " + expression.GetType().FullName)
        };

    private static Expression.IntegerLiteral MakeInteger(BigInteger value) =>
        new(value, IntegerEncoding.EncodeSignedInteger(value));

    private static Expression.FloatLiteral MakeFloatable(string literalText)
    {
        var elmFloat = FloatLiteralConversion.ToElmFloat(literalText);

        return new Expression.FloatLiteral(elmFloat.Numerator, elmFloat.Denominator);
    }

    private static Expression.RecordAccessFunction MakeRecordAccessFunction(string functionName)
    {
        // The concrete model includes the leading dot in the function name; strip it to obtain
        // the bare field name.
        var fieldName =
            functionName.Length > 0 && functionName[0] is '.'
            ?
            functionName[1..]
            :
            functionName;

        return
            new Expression.RecordAccessFunction(
                fieldName,
                StringEncoding.ValueFromString(fieldName));
    }

    private static Case FromCase(Concrete.Case caseItem) =>
        new(
            FromPattern(caseItem.Pattern.Value),
            FromExpression(caseItem.Expression.Value));

    private static LetDeclaration FromLetDeclaration(Concrete.Expression.LetDeclaration letDeclaration) =>
        letDeclaration switch
        {
            Concrete.Expression.LetDeclaration.LetFunction letFunction =>
            new LetDeclaration.LetFunction(FromFunctionStruct(letFunction.Function)),

            Concrete.Expression.LetDeclaration.LetDestructuring letDestructuring =>
            new LetDeclaration.LetDestructuring(
                FromPattern(letDestructuring.Pattern.Value),
                FromExpression(letDestructuring.Expression.Value)),

            _ =>
            throw new System.NotImplementedException(
                "Unexpected let declaration type: " + letDeclaration.GetType().FullName)
        };

    /// <summary>
    /// Parses an Elm integer literal (decimal or hexadecimal, optionally negative) into its numeric value.
    /// </summary>
    public static BigInteger ParseIntegerLiteral(string literalText)
    {
        var trimmed = literalText.Trim();

        var negative = trimmed.StartsWith('-');

        var absolute = negative ? trimmed[1..] : trimmed;

        BigInteger magnitude;

        if (absolute.StartsWith("0x", System.StringComparison.OrdinalIgnoreCase))
        {
            // Prepend '0' to the hex digits so BigInteger does not interpret a leading high-bit as sign.
            var hexDigits = "0" + absolute[2..];

            magnitude =
                BigInteger.Parse(
                    hexDigits,
                    System.Globalization.NumberStyles.HexNumber,
                    System.Globalization.CultureInfo.InvariantCulture);
        }
        else
        {
            magnitude =
                BigInteger.Parse(
                    absolute,
                    System.Globalization.NumberStyles.Integer,
                    System.Globalization.CultureInfo.InvariantCulture);
        }

        return negative ? -magnitude : magnitude;
    }
}
