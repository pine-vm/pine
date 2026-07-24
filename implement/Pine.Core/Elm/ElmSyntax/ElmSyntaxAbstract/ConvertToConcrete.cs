using System.Collections.Generic;
using System.Linq;
using System.Numerics;

namespace Pine.Core.Elm.ElmSyntax.ElmSyntaxAbstract;

/// <summary>
/// Converts the abstract Elm syntax model defined in this namespace back into the concrete syntax
/// model (namespace <see cref="SyntaxModel"/>).
/// <para>
/// The abstract model carries no source locations or trivia, so the produced concrete nodes use a
/// placeholder zero range/location everywhere. The result is therefore not suitable for exact
/// source roundtripping, but it is structurally faithful: it can be rendered (for example via
/// <see cref="SnapshotTestFormat"/>) or re-parsed for use in snapshot tests of syntactic constructs
/// produced inside the Elm syntax optimization and lowering pipeline.
/// </para>
/// <para>
/// Literals that the abstract model stores in normalized form are re-serialized into the textual
/// form the concrete model expects (integers and floats to their literal text). The precomputed
/// <see cref="PineValue"/> instances carried by the abstract model are not represented in the
/// concrete model and are dropped.
/// </para>
/// For the inverse direction, see <see cref="ConvertFromConcrete"/>.
/// </summary>
public static class ConvertToConcrete
{
    private static readonly SyntaxModel.Location s_zeroLocation = new(0, 0);

    private static readonly SyntaxModel.Range s_zeroRange = new(s_zeroLocation, s_zeroLocation);

    /// <summary>
    /// Wraps a value in a concrete <see cref="SyntaxModel.Node{T}"/> using the placeholder zero range.
    /// </summary>
    private static SyntaxModel.Node<T> Node<T>(T value) =>
        new(s_zeroRange, value);

    /// <summary>
    /// Builds a concrete <see cref="SyntaxModel.SeparatedSyntaxList{TNode}"/> from a flat list, using the
    /// placeholder zero location for every separator.
    /// </summary>
    private static SyntaxModel.SeparatedSyntaxList<TNode> ToSeparatedList<TNode>(IReadOnlyList<TNode> items)
    {
        if (items.Count is 0)
            return new SyntaxModel.SeparatedSyntaxList<TNode>.Empty();

        return
            new SyntaxModel.SeparatedSyntaxList<TNode>.NonEmpty(
                items[0],
                [.. items.Skip(1).Select(item => (s_zeroLocation, item))]);
    }

    /// <summary>
    /// Converts an abstract <see cref="File"/> into a concrete <see cref="SyntaxModel.File"/>.
    /// </summary>
    public static SyntaxModel.File FromFile(File file) =>
        new(
            Node(ToModule(file.ModuleDefinition)),
            [.. file.Imports.Select(import => Node(ToImport(import)))],
            [.. file.Declarations.Select(declaration => Node(ToDeclaration(declaration)))],
            Comments: [],
            IncompleteDeclarations: []);

    /// <summary>
    /// Converts an abstract <see cref="Import"/> into a concrete <see cref="SyntaxModel.Import"/>.
    /// </summary>
    public static SyntaxModel.Import ToImport(Import import) =>
        new(
            s_zeroLocation,
            Node(import.ModuleName),
            import.ModuleAlias is { } alias
            ?
            (s_zeroLocation, Node(alias))
            :
            null,
            import.ExposingList is { } exposing
            ?
            (s_zeroLocation, Node(ToExposing(exposing)))
            :
            null);

    /// <summary>
    /// Converts an abstract <see cref="Module"/> into a concrete <see cref="SyntaxModel.Module"/>.
    /// </summary>
    public static SyntaxModel.Module ToModule(Module module) =>
        module switch
        {
            Module.NormalModule normalModule =>
            new SyntaxModel.Module.NormalModule(
                s_zeroLocation,
                ToDefaultModuleData(normalModule.ModuleData)),

            Module.PortModule portModule =>
            new SyntaxModel.Module.PortModule(
                s_zeroLocation,
                s_zeroLocation,
                ToDefaultModuleData(portModule.ModuleData)),

            Module.EffectModule effectModule =>
            new SyntaxModel.Module.EffectModule(
                s_zeroLocation,
                s_zeroLocation,
                ToEffectModuleData(effectModule.ModuleData)),

            _ =>
            throw new System.NotImplementedException(
                "Unexpected module type: " + module.GetType().FullName)
        };

    private static SyntaxModel.DefaultModuleData ToDefaultModuleData(DefaultModuleData moduleData) =>
        new(
            Node(moduleData.ModuleName),
            s_zeroLocation,
            Node(ToExposing(moduleData.ExposingList)));

    private static SyntaxModel.EffectModuleData ToEffectModuleData(EffectModuleData moduleData) =>
        new(
            Node(moduleData.ModuleName),
            s_zeroLocation,
            Node(ToExposing(moduleData.ExposingList)),
            moduleData.Command is { } command ? Node(command) : null,
            moduleData.Subscription is { } subscription ? Node(subscription) : null);

    /// <summary>
    /// Converts an abstract <see cref="Exposing"/> into a concrete <see cref="SyntaxModel.Exposing"/>.
    /// </summary>
    public static SyntaxModel.Exposing ToExposing(Exposing exposing) =>
        exposing switch
        {
            Exposing.All =>
            new SyntaxModel.Exposing.All(s_zeroRange),

            Exposing.Explicit explicitExposing =>
            new SyntaxModel.Exposing.Explicit(
                s_zeroLocation,
                ToSeparatedList(
                    [.. explicitExposing.Exposes.Select(expose => Node(ToTopLevelExpose(expose)))]),
                s_zeroLocation),

            _ =>
            throw new System.NotImplementedException(
                "Unexpected exposing type: " + exposing.GetType().FullName)
        };

    private static SyntaxModel.TopLevelExpose ToTopLevelExpose(TopLevelExpose topLevelExpose) =>
        topLevelExpose switch
        {
            TopLevelExpose.InfixExpose infixExpose =>
            new SyntaxModel.TopLevelExpose.InfixExpose(infixExpose.Name),

            TopLevelExpose.FunctionExpose functionExpose =>
            new SyntaxModel.TopLevelExpose.FunctionExpose(functionExpose.Name),

            TopLevelExpose.TypeOrAliasExpose typeOrAliasExpose =>
            new SyntaxModel.TopLevelExpose.TypeOrAliasExpose(typeOrAliasExpose.Name),

            TopLevelExpose.TypeExpose typeExpose =>
            new SyntaxModel.TopLevelExpose.TypeExpose(
                new SyntaxModel.ExposedType(
                    typeExpose.ExposedType.Name,
                    typeExpose.ExposedType.ExposesConstructors ? s_zeroRange : null)),

            _ =>
            throw new System.NotImplementedException(
                "Unexpected top-level expose type: " + topLevelExpose.GetType().FullName)
        };

    /// <summary>
    /// Converts an abstract <see cref="Declaration"/> into a concrete <see cref="SyntaxModel.Declaration"/>.
    /// </summary>
    public static SyntaxModel.Declaration ToDeclaration(Declaration declaration) =>
        declaration switch
        {
            Declaration.FunctionDeclaration functionDeclaration =>
            new SyntaxModel.Declaration.FunctionDeclaration(ToFunctionStruct(functionDeclaration.Function)),

            Declaration.ChoiceTypeDeclaration choiceTypeDeclaration =>
            new SyntaxModel.Declaration.ChoiceTypeDeclaration(ToTypeStruct(choiceTypeDeclaration.TypeDeclaration)),

            Declaration.AliasDeclaration aliasDeclaration =>
            new SyntaxModel.Declaration.AliasDeclaration(ToTypeAlias(aliasDeclaration.TypeAlias)),

            Declaration.PortDeclaration portDeclaration =>
            new SyntaxModel.Declaration.PortDeclaration(s_zeroLocation, ToSignature(portDeclaration.Signature)),

            Declaration.InfixDeclaration infixDeclaration =>
            new SyntaxModel.Declaration.InfixDeclaration(ToInfix(infixDeclaration.Infix)),

            _ =>
            throw new System.NotImplementedException(
                "Unexpected declaration type: " + declaration.GetType().FullName)
        };

    private static SyntaxModel.Infix ToInfix(Infix infix) =>
        new(
            s_zeroLocation,
            Node(infix.Direction),
            Node(infix.Precedence),
            Node(infix.Operator),
            s_zeroLocation,
            Node(infix.FunctionName));

    private static SyntaxModel.TypeAlias ToTypeAlias(TypeAlias typeAlias) =>
        new(
            Documentation: null,
            s_zeroLocation,
            s_zeroLocation,
            Node(typeAlias.Name),
            [.. typeAlias.Generics.Select(Node)],
            s_zeroLocation,
            Node(ToTypeAnnotation(typeAlias.TypeAnnotation)));

    private static SyntaxModel.TypeStruct ToTypeStruct(TypeStruct typeStruct) =>
        new(
            Documentation: null,
            s_zeroLocation,
            Node(typeStruct.Name),
            [.. typeStruct.Generics.Select(Node)],
            s_zeroLocation,
            [
            .. typeStruct.Constructors.Select(
                (constructor, index) =>
                ((SyntaxModel.Location?)(index is 0 ? null : s_zeroLocation),
                Node(ToValueConstructor(constructor))))
            ]);

    private static SyntaxModel.ValueConstructor ToValueConstructor(ValueConstructor valueConstructor) =>
        new(
            Node(valueConstructor.Name),
            [.. valueConstructor.Arguments.Select(argument => Node(ToTypeAnnotation(argument)))]);

    /// <summary>
    /// Converts an abstract <see cref="TypeAnnotation"/> into a concrete <see cref="SyntaxModel.TypeAnnotation"/>.
    /// </summary>
    public static SyntaxModel.TypeAnnotation ToTypeAnnotation(TypeAnnotation typeAnnotation) =>
        typeAnnotation switch
        {
            TypeAnnotation.GenericType genericType =>
            new SyntaxModel.TypeAnnotation.GenericType(genericType.Name),

            TypeAnnotation.Typed typed =>
            new SyntaxModel.TypeAnnotation.Typed(
                Node((typed.ModuleName, typed.Name)),
                [.. typed.TypeArguments.Select(argument => Node(ToTypeAnnotation(argument)))]),

            TypeAnnotation.Unit =>
            new SyntaxModel.TypeAnnotation.Unit(),

            TypeAnnotation.Tupled tupled =>
            new SyntaxModel.TypeAnnotation.Tupled(
                ToSeparatedList(
                    [.. tupled.TypeAnnotations.Select(annotation => Node(ToTypeAnnotation(annotation)))])),

            TypeAnnotation.Record record =>
            new SyntaxModel.TypeAnnotation.Record(ToRecordDefinition(record.RecordDefinition)),

            TypeAnnotation.GenericRecord genericRecord =>
            new SyntaxModel.TypeAnnotation.GenericRecord(
                Node(genericRecord.GenericName),
                s_zeroLocation,
                Node(ToRecordDefinition(genericRecord.RecordDefinition))),

            TypeAnnotation.FunctionTypeAnnotation functionTypeAnnotation =>
            new SyntaxModel.TypeAnnotation.FunctionTypeAnnotation(
                Node(ToTypeAnnotation(functionTypeAnnotation.ArgumentType)),
                s_zeroLocation,
                Node(ToTypeAnnotation(functionTypeAnnotation.ReturnType))),

            _ =>
            throw new System.NotImplementedException(
                "Unexpected type annotation type: " + typeAnnotation.GetType().FullName)
        };

    private static SyntaxModel.RecordDefinition ToRecordDefinition(RecordDefinition recordDefinition) =>
        new(
            ToSeparatedList(
                [.. recordDefinition.Fields.Select(field => Node(ToRecordField(field)))]));

    private static SyntaxModel.RecordField ToRecordField(RecordField recordField) =>
        new(
            Node(recordField.FieldName),
            s_zeroLocation,
            Node(ToTypeAnnotation(recordField.FieldType)));

    private static SyntaxModel.FunctionStruct ToFunctionStruct(FunctionStruct functionStruct) =>
        new(
            Documentation: null,
            functionStruct.Signature is { } signature
            ?
            Node(ToSignature(signature))
            :
            null,
            Node(ToFunctionImplementation(functionStruct.Declaration)));

    private static SyntaxModel.FunctionImplementation ToFunctionImplementation(
        FunctionImplementation functionImplementation) =>
        new(
            Node(functionImplementation.Name),
            [.. functionImplementation.Arguments.Select(argument => Node(ToPatternInArgumentPosition(argument)))],
            s_zeroLocation,
            Node(ToExpression(functionImplementation.Expression)));

    private static SyntaxModel.Signature ToSignature(Signature signature) =>
        new(
            Node(signature.Name),
            s_zeroLocation,
            Node(ToTypeAnnotation(signature.TypeAnnotation)));

    /// <summary>
    /// Converts an abstract <see cref="Pattern"/> into a concrete <see cref="SyntaxModel.Pattern"/>.
    /// The precomputed <see cref="PineValue"/> instances are dropped, and integer patterns are emitted as
    /// decimal integer patterns.
    /// </summary>
    public static SyntaxModel.Pattern ToPattern(Pattern pattern) =>
        pattern switch
        {
            Pattern.AllPattern =>
            new SyntaxModel.Pattern.AllPattern(),

            Pattern.VarPattern varPattern =>
            new SyntaxModel.Pattern.VarPattern(varPattern.Name),

            Pattern.UnitPattern =>
            new SyntaxModel.Pattern.UnitPattern(),

            Pattern.CharPattern charPattern =>
            new SyntaxModel.Pattern.CharPattern(charPattern.Value),

            Pattern.StringPattern stringPattern =>
            new SyntaxModel.Pattern.StringPattern(stringPattern.Value),

            Pattern.IntPattern intPattern =>
            new SyntaxModel.Pattern.IntPattern((long)intPattern.Value),

            Pattern.FloatPattern floatPattern =>
            new SyntaxModel.Pattern.FloatPattern((float)floatPattern.Value),

            Pattern.TuplePattern tuplePattern =>
            new SyntaxModel.Pattern.TuplePattern(
                ToSeparatedList(
                    [.. tuplePattern.Elements.Select(element => Node(ToPattern(element)))])),

            Pattern.RecordPattern recordPattern =>
            new SyntaxModel.Pattern.RecordPattern(
                ToSeparatedList(
                    [.. recordPattern.Fields.Select(field => Node(field.FieldName))])),

            Pattern.UnConsPattern unConsPattern =>
            new SyntaxModel.Pattern.UnConsPattern(
                Node(ToPatternInArgumentPosition(unConsPattern.Head)),
                s_zeroLocation,
                Node(ToPatternInArgumentPosition(unConsPattern.Tail))),

            Pattern.ListPattern listPattern =>
            new SyntaxModel.Pattern.ListPattern(
                ToSeparatedList(
                    [.. listPattern.Elements.Select(element => Node(ToPattern(element)))])),

            Pattern.NamedPattern namedPattern =>
            new SyntaxModel.Pattern.NamedPattern(
                ToQualifiedNameRef(namedPattern.Name),
                [.. namedPattern.Arguments.Select(argument => Node(ToPatternInArgumentPosition(argument)))]),

            Pattern.AsPattern asPattern =>
            new SyntaxModel.Pattern.AsPattern(
                Node(ToPatternInArgumentPosition(asPattern.Pattern)),
                s_zeroLocation,
                Node(asPattern.Name)),

            _ =>
            throw new System.NotImplementedException(
                "Unexpected pattern type: " + pattern.GetType().FullName)
        };

    private static SyntaxModel.Pattern ToPatternInArgumentPosition(Pattern pattern)
    {
        var converted = ToPattern(pattern);

        if (!NeedsParenthesesInArgumentPosition(pattern))
            return converted;

        return new SyntaxModel.Pattern.ParenthesizedPattern(Node(converted));
    }

    private static bool NeedsParenthesesInArgumentPosition(Pattern pattern) =>
        pattern switch
        {
            Pattern.NamedPattern namedPattern =>
            namedPattern.Arguments.Count is not 0,

            Pattern.UnConsPattern or
            Pattern.AsPattern =>
            true,

            Pattern.AllPattern or
            Pattern.VarPattern or
            Pattern.UnitPattern or
            Pattern.CharPattern or
            Pattern.StringPattern or
            Pattern.IntPattern or
            Pattern.FloatPattern or
            Pattern.TuplePattern or
            Pattern.RecordPattern or
            Pattern.ListPattern =>
            false,

            _ =>
            throw new System.NotImplementedException(
                "NeedsParenthesesInArgumentPosition does not handle pattern variant: "
                + pattern.GetType().Name)
        };

    private static SyntaxModel.QualifiedNameRef ToQualifiedNameRef(QualifiedNameRef qualifiedNameRef) =>
        new(
            qualifiedNameRef.ModuleName,
            qualifiedNameRef.Name);

    /// <summary>
    /// Converts an abstract <see cref="Expression"/> into a concrete <see cref="SyntaxModel.Expression"/>.
    /// Normalized literals are re-serialized into their textual form and precomputed <see cref="PineValue"/>
    /// instances are dropped.
    /// </summary>
    public static SyntaxModel.Expression ToExpression(Expression expression) =>
        expression switch
        {
            Expression.UnitExpr =>
            new SyntaxModel.Expression.UnitExpr(),

            Expression.StringLiteral stringLiteral =>
            new SyntaxModel.Expression.Literal(stringLiteral.Value),

            Expression.CharLiteral charLiteral =>
            new SyntaxModel.Expression.CharLiteral(charLiteral.Value),

            Expression.Integer integer =>
            new SyntaxModel.Expression.Integer(IntegerLiteralText(integer.Value)),

            Expression.FloatLiteral floatable =>
            new SyntaxModel.Expression.FloatLiteral(FloatLiteralText(floatable.Numerator, floatable.Denominator)),

            Expression.Negation negation =>
            new SyntaxModel.Expression.Negation(Node(ToExpressionInApplicationPosition(negation.Expression))),

            Expression.ListExpr listExpr =>
            new SyntaxModel.Expression.ListExpr(
                ToSeparatedList(
                    [.. listExpr.Elements.Select(element => Node(ToExpression(element)))])),

            Expression.FunctionOrValue functionOrValue =>
            new SyntaxModel.Expression.FunctionOrValue(
                functionOrValue.QualifiedName.Namespaces,
                functionOrValue.QualifiedName.DeclName),

            Expression.IfBlock ifBlock =>
            new SyntaxModel.Expression.IfBlock(
                s_zeroLocation,
                Node(ToExpression(ifBlock.Condition)),
                s_zeroLocation,
                Node(ToExpression(ifBlock.ThenBlock)),
                s_zeroLocation,
                Node(ToExpression(ifBlock.ElseBlock))),

            Expression.PrefixOperator prefixOperator =>
            new SyntaxModel.Expression.PrefixOperator(prefixOperator.Operator),

            Expression.Application application =>
            new SyntaxModel.Expression.Application(
                Node(ToExpressionInApplicationPosition(application.Function)),
                [.. application.Arguments.Select(argument => Node(ToExpressionInApplicationPosition(argument)))]),

            Expression.OperatorApplication operatorApplication =>
            new SyntaxModel.Expression.OperatorApplication(
                Node(operatorApplication.Operator),
                operatorApplication.Direction,
                Node(ToExpressionInOperatorOperandPosition(operatorApplication.Left)),
                Node(ToExpressionInOperatorOperandPosition(operatorApplication.Right))),

            Expression.TupledExpression tupledExpression =>
            new SyntaxModel.Expression.TupledExpression(
                ToSeparatedList(
                    [.. tupledExpression.Elements.Select(element => Node(ToExpression(element)))])),

            Expression.LambdaExpression lambdaExpression =>
            new SyntaxModel.Expression.LambdaExpression(
                new SyntaxModel.LambdaStruct(
                    s_zeroLocation,
                    [.. lambdaExpression.Arguments.Select(argument => Node(ToPatternInArgumentPosition(argument)))],
                    s_zeroLocation,
                    Node(ToExpression(lambdaExpression.Expression)))),

            Expression.CaseExpression caseExpression =>
            new SyntaxModel.Expression.CaseExpression(
                new SyntaxModel.CaseBlock(
                    s_zeroLocation,
                    Node(ToExpression(caseExpression.Expression)),
                    s_zeroLocation,
                    [.. caseExpression.Cases.Select(ToCase)])),

            Expression.LetExpression letExpression =>
            new SyntaxModel.Expression.LetExpression(
                new SyntaxModel.Expression.LetBlock(
                    s_zeroLocation,
                    [.. letExpression.Declarations.Select(declaration => Node(ToLetDeclaration(declaration)))],
                    s_zeroLocation,
                    Node(ToExpression(letExpression.Expression)))),

            Expression.RecordExpr recordExpr =>
            new SyntaxModel.Expression.RecordExpr(
                ToSeparatedList(
                    [.. recordExpr.Fields.Select(ToRecordExprField)])),

            Expression.RecordAccess recordAccess =>
            new SyntaxModel.Expression.RecordAccess(
                Node(ToExpressionInApplicationPosition(recordAccess.Record)),
                Node(recordAccess.FieldName)),

            Expression.RecordAccessFunction recordAccessFunction =>
            new SyntaxModel.Expression.RecordAccessFunction("." + recordAccessFunction.FieldName),

            Expression.RecordUpdateExpression recordUpdateExpression =>
            new SyntaxModel.Expression.RecordUpdateExpression(
                Node(recordUpdateExpression.RecordName),
                s_zeroLocation,
                ToSeparatedList(
                    [.. recordUpdateExpression.Fields.Select(ToRecordExprField)])),

            Expression.GLSLExpression glslExpression =>
            new SyntaxModel.Expression.GLSLExpression(glslExpression.ShaderCode),

            _ =>
            throw new System.NotImplementedException(
                "Unexpected expression type: " + expression.GetType().FullName)
        };

    private static SyntaxModel.Expression ToExpressionInApplicationPosition(Expression expression)
    {
        var converted = ToExpression(expression);

        if (!NeedsParenthesesInApplicationPosition(expression))
            return converted;

        return new SyntaxModel.Expression.ParenthesizedExpression(Node(converted));
    }

    private static SyntaxModel.Expression ToExpressionInOperatorOperandPosition(Expression expression) =>
        NeedsParenthesesInApplicationPosition(expression)
        ?
        new SyntaxModel.Expression.ParenthesizedExpression(Node(ToExpression(expression)))
        :
        ToExpression(expression);

    private static bool NeedsParenthesesInApplicationPosition(Expression expression) =>
        expression switch
        {
            Expression.Negation =>
            true,

            Expression.IfBlock =>
            true,

            Expression.Application =>
            true,

            Expression.OperatorApplication =>
            true,

            Expression.LambdaExpression =>
            true,

            Expression.CaseExpression =>
            true,

            Expression.LetExpression =>
            true,

            Expression.UnitExpr or
            Expression.StringLiteral or
            Expression.CharLiteral or
            Expression.Integer or
            Expression.FloatLiteral or
            Expression.ListExpr or
            Expression.FunctionOrValue or
            Expression.PrefixOperator or
            Expression.TupledExpression or
            Expression.RecordExpr or
            Expression.RecordAccess or
            Expression.RecordAccessFunction or
            Expression.RecordUpdateExpression or
            Expression.GLSLExpression =>
            false,

            _ =>
            throw new System.NotImplementedException(
                "NeedsParenthesesInApplicationPosition does not handle expression variant: "
                + expression.GetType().Name)
        };

    private static SyntaxModel.RecordExprField ToRecordExprField(RecordSetter setter) =>
        new(
            Node(setter.FieldName),
            s_zeroLocation,
            Node(ToExpression(setter.Value)));

    private static SyntaxModel.Case ToCase(Case caseItem) =>
        new(
            Node(ToPattern(caseItem.Pattern)),
            s_zeroLocation,
            Node(ToExpression(caseItem.Expression)));

    private static SyntaxModel.Expression.LetDeclaration ToLetDeclaration(LetDeclaration letDeclaration) =>
        letDeclaration switch
        {
            LetDeclaration.LetFunction letFunction =>
            new SyntaxModel.Expression.LetDeclaration.LetFunction(ToFunctionStruct(letFunction.Function)),

            LetDeclaration.LetDestructuring letDestructuring =>
            new SyntaxModel.Expression.LetDeclaration.LetDestructuring(
                Node(ToPatternInArgumentPosition(letDestructuring.Pattern)),
                s_zeroLocation,
                Node(ToExpression(letDestructuring.Expression))),

            _ =>
            throw new System.NotImplementedException(
                "Unexpected let declaration type: " + letDeclaration.GetType().FullName)
        };

    /// <summary>
    /// Serializes a normalized integer value into the decimal literal text expected by the concrete model.
    /// </summary>
    public static string IntegerLiteralText(BigInteger value) =>
        value.ToString(System.Globalization.CultureInfo.InvariantCulture);

    /// <summary>
    /// Serializes a normalized rational float value (numerator over denominator) into a decimal float literal
    /// text expected by the concrete model.
    /// </summary>
    /// <remarks>
    /// Elm float literals always originate from finite decimal source text, so after normalization the
    /// denominator divides a power of ten and the value has an exact finite decimal representation.
    /// </remarks>
    public static string FloatLiteralText(BigInteger numerator, BigInteger denominator)
    {
        if (denominator.IsZero)
            throw new System.ArgumentException("Denominator must not be zero", nameof(denominator));

        var negative = (numerator.Sign * denominator.Sign) < 0;

        var absNumerator = BigInteger.Abs(numerator);
        var absDenominator = BigInteger.Abs(denominator);

        // Find the smallest power of ten that is a multiple of the denominator, so the fraction can be
        // expressed exactly as a decimal with that many fractional digits.
        var fractionalDigits = 0;
        var powerOfTen = BigInteger.One;

        while (fractionalDigits < 1000 && powerOfTen % absDenominator != BigInteger.Zero)
        {
            fractionalDigits++;
            powerOfTen *= 10;
        }

        if (powerOfTen % absDenominator != BigInteger.Zero)
        {
            // The denominator is not of the form 2^a * 5^b; fall back to a double approximation.
            var approximation = (double)numerator / (double)denominator;

            return approximation.ToString("R", System.Globalization.CultureInfo.InvariantCulture);
        }

        var scaled = absNumerator * (powerOfTen / absDenominator);

        var digits = scaled.ToString(System.Globalization.CultureInfo.InvariantCulture);

        string magnitude;

        if (fractionalDigits is 0)
        {
            magnitude = digits + ".0";
        }
        else
        {
            if (digits.Length <= fractionalDigits)
                digits = new string('0', fractionalDigits - digits.Length + 1) + digits;

            var integerPart = digits[..^fractionalDigits];
            var fractionalPart = digits[^fractionalDigits..];

            magnitude = integerPart + "." + fractionalPart;
        }

        return negative ? "-" + magnitude : magnitude;
    }
}
