using Pine.Core.PopularEncodings;
using System.Linq;
using System.Numerics;

namespace Pine.Core.Elm.ElmSyntax.ElmSyntaxAbstract;

/// <summary>
/// Converts the concrete Elm syntax model (namespace <see cref="SyntaxModel"/>) into the abstract
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
    /// Converts a concrete <see cref="SyntaxModel.File"/> into an abstract <see cref="File"/>.
    /// Comments and incomplete declarations are dropped.
    /// </summary>
    public static File FromFile(SyntaxModel.File file) =>
        new(
            FromModule(file.ModuleDefinition.Value),
            [.. file.Imports.Select(import => FromImport(import.Value))],
            [.. file.Declarations.Select(declaration => FromDeclaration(declaration.Value))]);

    /// <summary>
    /// Converts a concrete <see cref="SyntaxModel.Import"/> into an abstract <see cref="Import"/>.
    /// </summary>
    public static Import FromImport(SyntaxModel.Import import) =>
        new(
            import.ModuleName.Value,
            import.ModuleAlias?.Alias.Value,
            import.ExposingList is { } exposingList
            ?
            FromExposing(exposingList.ExposingList.Value)
            :
            null);

    /// <summary>
    /// Converts a concrete <see cref="SyntaxModel.Module"/> into an abstract <see cref="Module"/>.
    /// </summary>
    public static Module FromModule(SyntaxModel.Module module) =>
        module switch
        {
            SyntaxModel.Module.NormalModule normalModule =>
            new Module.NormalModule(FromDefaultModuleData(normalModule.ModuleData)),

            SyntaxModel.Module.PortModule portModule =>
            new Module.PortModule(FromDefaultModuleData(portModule.ModuleData)),

            SyntaxModel.Module.EffectModule effectModule =>
            new Module.EffectModule(FromEffectModuleData(effectModule.ModuleData)),

            _ =>
            throw new System.NotImplementedException(
                "Unexpected module type: " + module.GetType().FullName)
        };

    private static DefaultModuleData FromDefaultModuleData(SyntaxModel.DefaultModuleData moduleData) =>
        new(
            moduleData.ModuleName.Value,
            FromExposing(moduleData.ExposingList.Value));

    private static EffectModuleData FromEffectModuleData(SyntaxModel.EffectModuleData moduleData) =>
        new(
            moduleData.ModuleName.Value,
            FromExposing(moduleData.ExposingList.Value),
            moduleData.Command?.Value,
            moduleData.Subscription?.Value);

    /// <summary>
    /// Converts a concrete <see cref="SyntaxModel.Exposing"/> into an abstract <see cref="Exposing"/>.
    /// </summary>
    public static Exposing FromExposing(SyntaxModel.Exposing exposing) =>
        exposing switch
        {
            SyntaxModel.Exposing.All =>
            new Exposing.All(),

            SyntaxModel.Exposing.Explicit explicitExposing =>
            new Exposing.Explicit(
                [.. explicitExposing.Nodes.Nodes.Select(node => FromTopLevelExpose(node.Value))]),

            _ =>
            throw new System.NotImplementedException(
                "Unexpected exposing type: " + exposing.GetType().FullName)
        };

    private static TopLevelExpose FromTopLevelExpose(SyntaxModel.TopLevelExpose topLevelExpose) =>
        topLevelExpose switch
        {
            SyntaxModel.TopLevelExpose.InfixExpose infixExpose =>
            new TopLevelExpose.InfixExpose(infixExpose.Name),

            SyntaxModel.TopLevelExpose.FunctionExpose functionExpose =>
            new TopLevelExpose.FunctionExpose(functionExpose.Name),

            SyntaxModel.TopLevelExpose.TypeOrAliasExpose typeOrAliasExpose =>
            new TopLevelExpose.TypeOrAliasExpose(typeOrAliasExpose.Name),

            SyntaxModel.TopLevelExpose.TypeExpose typeExpose =>
            new TopLevelExpose.TypeExpose(
                new ExposedType(
                    typeExpose.ExposedType.Name,
                    typeExpose.ExposedType.Open is not null)),

            _ =>
            throw new System.NotImplementedException(
                "Unexpected top-level expose type: " + topLevelExpose.GetType().FullName)
        };

    /// <summary>
    /// Converts a concrete <see cref="SyntaxModel.Declaration"/> into an abstract <see cref="Declaration"/>.
    /// </summary>
    public static Declaration FromDeclaration(SyntaxModel.Declaration declaration) =>
        declaration switch
        {
            SyntaxModel.Declaration.FunctionDeclaration functionDeclaration =>
            new Declaration.FunctionDeclaration(FromFunctionStruct(functionDeclaration.Function)),

            SyntaxModel.Declaration.ChoiceTypeDeclaration choiceTypeDeclaration =>
            new Declaration.ChoiceTypeDeclaration(FromTypeStruct(choiceTypeDeclaration.TypeDeclaration)),

            SyntaxModel.Declaration.AliasDeclaration aliasDeclaration =>
            new Declaration.AliasDeclaration(FromTypeAlias(aliasDeclaration.TypeAlias)),

            SyntaxModel.Declaration.PortDeclaration portDeclaration =>
            new Declaration.PortDeclaration(FromSignature(portDeclaration.Signature)),

            SyntaxModel.Declaration.InfixDeclaration infixDeclaration =>
            new Declaration.InfixDeclaration(FromInfix(infixDeclaration.Infix)),

            _ =>
            throw new System.NotImplementedException(
                "Unexpected declaration type: " + declaration.GetType().FullName)
        };

    private static Infix FromInfix(SyntaxModel.Infix infix) =>
        new(
            infix.Direction.Value,
            infix.Precedence.Value,
            infix.Operator.Value,
            infix.FunctionName.Value);

    private static TypeAlias FromTypeAlias(SyntaxModel.TypeAlias typeAlias) =>
        new(
            typeAlias.Name.Value,
            [.. typeAlias.Generics.Select(generic => generic.Value)],
            FromTypeAnnotation(typeAlias.TypeAnnotation.Value));

    private static TypeStruct FromTypeStruct(SyntaxModel.TypeStruct typeStruct) =>
        new(
            typeStruct.Name.Value,
            [.. typeStruct.Generics.Select(generic => generic.Value)],
            [.. typeStruct.Constructors.Select(constructor => FromValueConstructor(constructor.Constructor.Value))]);

    private static ValueConstructor FromValueConstructor(SyntaxModel.ValueConstructor valueConstructor) =>
        new(
            valueConstructor.Name.Value,
            [.. valueConstructor.Arguments.Select(argument => FromTypeAnnotation(argument.Value))]);

    /// <summary>
    /// Converts a concrete <see cref="SyntaxModel.TypeAnnotation"/> into an abstract <see cref="TypeAnnotation"/>.
    /// </summary>
    public static TypeAnnotation FromTypeAnnotation(SyntaxModel.TypeAnnotation typeAnnotation) =>
        typeAnnotation switch
        {
            SyntaxModel.TypeAnnotation.GenericType genericType =>
            new TypeAnnotation.GenericType(genericType.Name),

            SyntaxModel.TypeAnnotation.Typed typed =>
            new TypeAnnotation.Typed(
                typed.TypeName.Value.ModuleName,
                typed.TypeName.Value.Name,
                [.. typed.TypeArguments.Select(argument => FromTypeAnnotation(argument.Value))]),

            SyntaxModel.TypeAnnotation.Unit =>
            new TypeAnnotation.Unit(),

            SyntaxModel.TypeAnnotation.Tupled tupled =>
            new TypeAnnotation.Tupled(
                [.. tupled.TypeAnnotations.Nodes.Select(node => FromTypeAnnotation(node.Value))]),

            SyntaxModel.TypeAnnotation.Record record =>
            new TypeAnnotation.Record(FromRecordDefinition(record.RecordDefinition)),

            SyntaxModel.TypeAnnotation.GenericRecord genericRecord =>
            new TypeAnnotation.GenericRecord(
                genericRecord.GenericName.Value,
                FromRecordDefinition(genericRecord.RecordDefinition.Value)),

            SyntaxModel.TypeAnnotation.FunctionTypeAnnotation functionTypeAnnotation =>
            new TypeAnnotation.FunctionTypeAnnotation(
                FromTypeAnnotation(functionTypeAnnotation.ArgumentType.Value),
                FromTypeAnnotation(functionTypeAnnotation.ReturnType.Value)),

            _ =>
            throw new System.NotImplementedException(
                "Unexpected type annotation type: " + typeAnnotation.GetType().FullName)
        };

    private static RecordDefinition FromRecordDefinition(SyntaxModel.RecordDefinition recordDefinition) =>
        new(
            [.. recordDefinition.Fields.Nodes.Select(node => FromRecordField(node.Value))]);

    private static RecordField FromRecordField(SyntaxModel.RecordField recordField) =>
        new(
            recordField.FieldName.Value,
            FromTypeAnnotation(recordField.FieldType.Value));

    private static FunctionStruct FromFunctionStruct(SyntaxModel.FunctionStruct functionStruct) =>
        new(
            functionStruct.Signature is { } signature
            ?
            FromSignature(signature.Value)
            :
            null,
            FromFunctionImplementation(functionStruct.Declaration.Value));

    private static FunctionImplementation FromFunctionImplementation(
        SyntaxModel.FunctionImplementation functionImplementation) =>
        new(
            functionImplementation.Name.Value,
            [.. functionImplementation.Arguments.Select(argument => FromPattern(argument.Value))],
            FromExpression(functionImplementation.Expression.Value));

    private static Signature FromSignature(SyntaxModel.Signature signature) =>
        new(
            signature.Name.Value,
            FromTypeAnnotation(signature.TypeAnnotation.Value));

    /// <summary>
    /// Converts a concrete <see cref="SyntaxModel.Pattern"/> into an abstract <see cref="Pattern"/>.
    /// Parenthesized patterns are unwrapped and hex/decimal integer patterns are normalized.
    /// </summary>
    public static Pattern FromPattern(SyntaxModel.Pattern pattern) =>
        pattern switch
        {
            SyntaxModel.Pattern.AllPattern =>
            new Pattern.AllPattern(),

            SyntaxModel.Pattern.VarPattern varPattern =>
            new Pattern.VarPattern(varPattern.Name),

            SyntaxModel.Pattern.UnitPattern =>
            new Pattern.UnitPattern(),

            SyntaxModel.Pattern.CharPattern charPattern =>
            new Pattern.CharPattern(charPattern.Value),

            SyntaxModel.Pattern.StringPattern stringPattern =>
            new Pattern.StringPattern(stringPattern.Value),

            SyntaxModel.Pattern.IntPattern intPattern =>
            new Pattern.IntPattern(intPattern.Value),

            SyntaxModel.Pattern.HexPattern hexPattern =>
            new Pattern.IntPattern(hexPattern.Value),

            SyntaxModel.Pattern.FloatPattern floatPattern =>
            new Pattern.FloatPattern(floatPattern.Value),

            SyntaxModel.Pattern.TuplePattern tuplePattern =>
            new Pattern.TuplePattern(
                [.. tuplePattern.Elements.Nodes.Select(node => FromPattern(node.Value))]),

            SyntaxModel.Pattern.RecordPattern recordPattern =>
            new Pattern.RecordPattern(
                [.. recordPattern.Fields.Nodes.Select(node => node.Value)]),

            SyntaxModel.Pattern.UnConsPattern unConsPattern =>
            new Pattern.UnConsPattern(
                FromPattern(unConsPattern.Head.Value),
                FromPattern(unConsPattern.Tail.Value)),

            SyntaxModel.Pattern.ListPattern listPattern =>
            new Pattern.ListPattern(
                [.. listPattern.Elements.Nodes.Select(node => FromPattern(node.Value))]),

            SyntaxModel.Pattern.NamedPattern namedPattern =>
            new Pattern.NamedPattern(
                FromQualifiedNameRef(namedPattern.Name),
                [.. namedPattern.Arguments.Select(argument => FromPattern(argument.Value))]),

            SyntaxModel.Pattern.AsPattern asPattern =>
            new Pattern.AsPattern(
                FromPattern(asPattern.Pattern.Value),
                asPattern.Name.Value),

            SyntaxModel.Pattern.ParenthesizedPattern parenthesizedPattern =>
            FromPattern(parenthesizedPattern.Pattern.Value),

            _ =>
            throw new System.NotImplementedException(
                "Unexpected pattern type: " + pattern.GetType().FullName)
        };

    private static QualifiedNameRef FromQualifiedNameRef(SyntaxModel.QualifiedNameRef qualifiedNameRef) =>
        new(
            qualifiedNameRef.ModuleName,
            qualifiedNameRef.Name);

    /// <summary>
    /// Converts a concrete <see cref="SyntaxModel.Expression"/> into an abstract <see cref="Expression"/>.
    /// Redundant parentheses are unwrapped, literals are normalized, and the precomputed
    /// <see cref="PineValue"/> instances described on the abstract model are populated.
    /// </summary>
    public static Expression FromExpression(SyntaxModel.Expression expression) =>
        expression switch
        {
            SyntaxModel.Expression.UnitExpr =>
            Expression.UnitExpr.Instance,

            SyntaxModel.Expression.Literal literal =>
            Expression.StringLiteral.Create(literal.Value),

            SyntaxModel.Expression.CharLiteral charLiteral =>
            Expression.CharLiteral.Create(charLiteral.Value),

            SyntaxModel.Expression.Integer integer =>
            MakeInteger(ParseIntegerLiteral(integer.LiteralText)),

            SyntaxModel.Expression.Floatable floatable =>
            MakeFloatable(floatable.LiteralText),

            SyntaxModel.Expression.Negation negation =>
            new Expression.Negation(FromExpression(negation.Expression.Value)),

            SyntaxModel.Expression.ListExpr listExpr =>
            new Expression.ListExpr(
                [.. listExpr.Elements.Nodes.Select(node => FromExpression(node.Value))]),

            SyntaxModel.Expression.FunctionOrValue functionOrValue =>
            new Expression.FunctionOrValue(functionOrValue.ModuleName, functionOrValue.Name),

            SyntaxModel.Expression.IfBlock ifBlock =>
            new Expression.IfBlock(
                FromExpression(ifBlock.Condition.Value),
                FromExpression(ifBlock.ThenBlock.Value),
                FromExpression(ifBlock.ElseBlock.Value)),

            SyntaxModel.Expression.PrefixOperator prefixOperator =>
            new Expression.PrefixOperator(prefixOperator.Operator),

            SyntaxModel.Expression.ParenthesizedExpression parenthesizedExpression =>
            FromExpression(parenthesizedExpression.Expression.Value),

            SyntaxModel.Expression.Application application =>
            new Expression.Application(
                FromExpression(application.Function.Value),
                [.. application.Arguments.Select(argument => FromExpression(argument.Value))]),

            SyntaxModel.Expression.OperatorApplication operatorApplication =>
            new Expression.OperatorApplication(
                operatorApplication.Operator.Value,
                operatorApplication.Direction,
                FromExpression(operatorApplication.Left.Value),
                FromExpression(operatorApplication.Right.Value)),

            SyntaxModel.Expression.TupledExpression tupledExpression =>
            new Expression.TupledExpression(
                [.. tupledExpression.Elements.Nodes.Select(node => FromExpression(node.Value))]),

            SyntaxModel.Expression.LambdaExpression lambdaExpression =>
            new Expression.LambdaExpression(
                [.. lambdaExpression.Lambda.Arguments.Select(argument => FromPattern(argument.Value))],
                FromExpression(lambdaExpression.Lambda.Expression.Value)),

            SyntaxModel.Expression.CaseExpression caseExpression =>
            new Expression.CaseExpression(
                FromExpression(caseExpression.CaseBlock.Expression.Value),
                [.. caseExpression.CaseBlock.Cases.Select(FromCase)]),

            SyntaxModel.Expression.LetExpression letExpression =>
            new Expression.LetExpression(
                [.. letExpression.Value.Declarations.Select(declaration => FromLetDeclaration(declaration.Value))],
                FromExpression(letExpression.Value.Expression.Value)),

            SyntaxModel.Expression.RecordExpr recordExpr =>
            new Expression.RecordExpr(
                [.. recordExpr.Fields.Nodes.Select(FromRecordSetter)]),

            SyntaxModel.Expression.RecordAccess recordAccess =>
            new Expression.RecordAccess(
                FromExpression(recordAccess.Record.Value),
                recordAccess.FieldName.Value,
                StringEncoding.ValueFromString(recordAccess.FieldName.Value)),

            SyntaxModel.Expression.RecordAccessFunction recordAccessFunction =>
            MakeRecordAccessFunction(recordAccessFunction.FunctionName),

            SyntaxModel.Expression.RecordUpdateExpression recordUpdateExpression =>
            new Expression.RecordUpdateExpression(
                recordUpdateExpression.RecordName.Value,
                [.. recordUpdateExpression.Fields.Nodes.Select(FromRecordSetter)]),

            SyntaxModel.Expression.GLSLExpression glslExpression =>
            new Expression.GLSLExpression(glslExpression.ShaderCode),

            _ =>
            throw new System.NotImplementedException(
                "Unexpected expression type: " + expression.GetType().FullName)
        };

    private static Expression.Integer MakeInteger(BigInteger value) =>
        new(value, IntegerEncoding.EncodeSignedInteger(value));

    private static Expression.Floatable MakeFloatable(string literalText)
    {
        var elmFloat = FloatLiteralConversion.ToElmFloat(literalText);

        return new Expression.Floatable(elmFloat.Numerator, elmFloat.Denominator);
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

    private static RecordSetter FromRecordSetter(SyntaxModel.RecordExprField field) =>
        new(
            field.FieldName.Value,
            StringEncoding.ValueFromString(field.FieldName.Value),
            FromExpression(field.ValueExpr.Value));

    private static Case FromCase(SyntaxModel.Case caseItem) =>
        new(
            FromPattern(caseItem.Pattern.Value),
            FromExpression(caseItem.Expression.Value));

    private static LetDeclaration FromLetDeclaration(SyntaxModel.Expression.LetDeclaration letDeclaration) =>
        letDeclaration switch
        {
            SyntaxModel.Expression.LetDeclaration.LetFunction letFunction =>
            new LetDeclaration.LetFunction(FromFunctionStruct(letFunction.Function)),

            SyntaxModel.Expression.LetDeclaration.LetDestructuring letDestructuring =>
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
