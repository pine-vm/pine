using System;
using System.Linq;

using Abstract = Pine.Core.Elm.ElmSyntax.ElmSyntaxAbstract;

namespace Pine.Core.Elm.ElmCompilerInDotnet.IntermediateRepresentation;

/// <summary>
/// Copies abstract Elm syntax into the compiler IR without lowering or changing value encodings.
/// Precomputed Pine values and qualified declaration names are preserved.
/// </summary>
public static class ConvertFromAbstractSyntax
{
    /// <summary>Maps a module definition.</summary>
    public static Module FromModule(Abstract.Module module) =>
        module switch
        {
            Abstract.Module.NormalModule normal =>
            new Module.NormalModule(FromDefaultModuleData(normal.ModuleData)),

            Abstract.Module.PortModule port =>
            new Module.PortModule(FromDefaultModuleData(port.ModuleData)),

            Abstract.Module.EffectModule effect =>
            new Module.EffectModule(
                new EffectModuleData(
                    effect.ModuleData.ModuleName,
                    FromExposing(effect.ModuleData.ExposingList),
                    effect.ModuleData.Command,
                    effect.ModuleData.Subscription)),

            _ =>
            throw new NotImplementedException(
                $"{nameof(FromModule)} does not handle module variant: {module.GetType().Name}")
        };

    /// <summary>Maps the shared data of normal and port modules.</summary>
    public static DefaultModuleData FromDefaultModuleData(Abstract.DefaultModuleData data) =>
        new(data.ModuleName, FromExposing(data.ExposingList));

    /// <summary>Maps an import, including optional alias and exposing list.</summary>
    public static Import FromImport(Abstract.Import import) =>
        new(
            import.ModuleName,
            import.ModuleAlias,
            import.ExposingList is null ? null : FromExposing(import.ExposingList));

    /// <summary>Maps a module's or import's exposing list.</summary>
    public static Exposing FromExposing(Abstract.Exposing exposing) =>
        exposing switch
        {
            Abstract.Exposing.All => new Exposing.All(),

            Abstract.Exposing.Explicit explicitExposing =>
            new Exposing.Explicit([.. explicitExposing.Exposes.Select(FromTopLevelExpose)]),

            _ =>
            throw new NotImplementedException(
                $"{nameof(FromExposing)} does not handle exposing variant: {exposing.GetType().Name}")
        };

    /// <summary>Maps a single exposed item.</summary>
    public static TopLevelExpose FromTopLevelExpose(Abstract.TopLevelExpose expose) =>
        expose switch
        {
            Abstract.TopLevelExpose.InfixExpose infix => new TopLevelExpose.InfixExpose(infix.Name),
            Abstract.TopLevelExpose.FunctionExpose function => new TopLevelExpose.FunctionExpose(function.Name),
            Abstract.TopLevelExpose.TypeOrAliasExpose type => new TopLevelExpose.TypeOrAliasExpose(type.Name),

            Abstract.TopLevelExpose.TypeExpose type =>
            new TopLevelExpose.TypeExpose(
                new ExposedType(type.ExposedType.Name, type.ExposedType.ExposesConstructors)),

            _ =>
            throw new NotImplementedException(
                $"{nameof(FromTopLevelExpose)} does not handle exposed-item variant: {expose.GetType().Name}")
        };

    /// <summary>Maps a top-level declaration.</summary>
    public static Declaration FromDeclaration(Abstract.Declaration declaration) =>
        declaration switch
        {
            Abstract.Declaration.FunctionDeclaration function =>
            new Declaration.FunctionDeclaration(FromFunction(function.Function)),

            Abstract.Declaration.ChoiceTypeDeclaration choice =>
            new Declaration.ChoiceTypeDeclaration(FromChoiceType(choice.TypeDeclaration)),

            Abstract.Declaration.AliasDeclaration alias =>
            new Declaration.AliasDeclaration(FromTypeAlias(alias.TypeAlias)),

            Abstract.Declaration.PortDeclaration port =>
            new Declaration.PortDeclaration(FromSignature(port.Signature)),

            Abstract.Declaration.InfixDeclaration infix =>
            new Declaration.InfixDeclaration(
                new Infix(
                    infix.Infix.Direction,
                    infix.Infix.Precedence,
                    infix.Infix.Operator,
                    infix.Infix.FunctionName)),

            _ =>
            throw new NotImplementedException(
                $"{nameof(FromDeclaration)} does not handle declaration variant: {declaration.GetType().Name}")
        };

    /// <summary>Maps a type alias.</summary>
    public static TypeAlias FromTypeAlias(Abstract.TypeAlias alias) =>
        new(alias.Name, alias.Generics, FromTypeAnnotation(alias.TypeAnnotation));

    /// <summary>Maps a custom type and all its constructors.</summary>
    public static ChoiceTypeStruct FromChoiceType(Abstract.ChoiceTypeStruct choice) =>
        new(choice.Name, choice.Generics, [.. choice.Constructors.Select(FromValueConstructor)]);

    /// <summary>Maps a constructor, preserving its precomputed name value.</summary>
    public static ValueConstructor FromValueConstructor(Abstract.ValueConstructor constructor) =>
        new(constructor.Name, constructor.NameValue, [.. constructor.Arguments.Select(FromTypeAnnotation)]);

    /// <summary>Maps a type annotation recursively.</summary>
    public static TypeAnnotation FromTypeAnnotation(Abstract.TypeAnnotation annotation) =>
        annotation switch
        {
            Abstract.TypeAnnotation.GenericType generic => new TypeAnnotation.GenericType(generic.Name),

            Abstract.TypeAnnotation.Typed typed =>
            new TypeAnnotation.Typed(
                typed.ModuleName,
                typed.Name,
                [.. typed.TypeArguments.Select(FromTypeAnnotation)]),

            Abstract.TypeAnnotation.Unit => new TypeAnnotation.Unit(),

            Abstract.TypeAnnotation.Tupled tuple =>
            new TypeAnnotation.Tupled([.. tuple.TypeAnnotations.Select(FromTypeAnnotation)]),

            Abstract.TypeAnnotation.Record record =>
            new TypeAnnotation.Record(FromRecordDefinition(record.RecordDefinition)),

            Abstract.TypeAnnotation.GenericRecord record =>
            new TypeAnnotation.GenericRecord(record.GenericName, FromRecordDefinition(record.RecordDefinition)),

            Abstract.TypeAnnotation.FunctionTypeAnnotation function =>
            new TypeAnnotation.FunctionTypeAnnotation(
                FromTypeAnnotation(function.ArgumentType),
                FromTypeAnnotation(function.ReturnType)),

            _ =>
            throw new NotImplementedException(
                $"{nameof(FromTypeAnnotation)} does not handle type annotation variant: {annotation.GetType().Name}")
        };

    /// <summary>Maps record field definitions in their existing order.</summary>
    public static RecordDefinition FromRecordDefinition(Abstract.RecordDefinition record) =>
        new(
            [
            .. record.Fields.Select(
                field => new RecordField(field.FieldName, field.FieldNameValue, FromTypeAnnotation(field.FieldType)))
            ]);

    /// <summary>Maps a function's optional signature and implementation.</summary>
    public static FunctionStruct FromFunction(Abstract.FunctionStruct function) =>
        new(
            function.Signature is null ? null : FromSignature(function.Signature),
            new FunctionImplementation(
                function.Declaration.Name,
                [.. function.Declaration.Arguments.Select(FromPattern)],
                FromExpression(function.Declaration.Expression)));

    /// <summary>Maps a signature.</summary>
    public static Signature FromSignature(Abstract.Signature signature) =>
        new(signature.Name, FromTypeAnnotation(signature.TypeAnnotation));

    /// <summary>Maps a qualified constructor reference.</summary>
    public static QualifiedNameRef FromQualifiedNameRef(Abstract.QualifiedNameRef name) =>
        new(name.ModuleName, name.Name);

    /// <summary>Maps a pattern recursively, preserving precomputed values.</summary>
    public static Pattern FromPattern(Abstract.Pattern pattern) =>
        pattern switch
        {
            Abstract.Pattern.AllPattern => new Pattern.AllPattern(),
            Abstract.Pattern.VarPattern variable => new Pattern.VarPattern(variable.Name),
            Abstract.Pattern.UnitPattern => new Pattern.UnitPattern(),

            Abstract.Pattern.CharPattern character =>
            new Pattern.CharPattern(character.Value, character.ValueAsPineValue),

            Abstract.Pattern.StringPattern text => new Pattern.StringPattern(text.Value, text.ValueAsPineValue),
            Abstract.Pattern.IntPattern integer => new Pattern.IntPattern(integer.Value, integer.ValueAsPineValue),
            Abstract.Pattern.FloatPattern floating => new Pattern.FloatPattern(floating.Value),
            Abstract.Pattern.TuplePattern tuple => new Pattern.TuplePattern([.. tuple.Elements.Select(FromPattern)]),
            Abstract.Pattern.RecordPattern record => new Pattern.RecordPattern(record.Fields),

            Abstract.Pattern.UnConsPattern cons =>
            new Pattern.UnConsPattern(FromPattern(cons.Head), FromPattern(cons.Tail)),

            Abstract.Pattern.ListPattern list => new Pattern.ListPattern([.. list.Elements.Select(FromPattern)]),

            Abstract.Pattern.NamedPattern named =>
            new Pattern.NamedPattern(
                FromQualifiedNameRef(named.Name),
                [.. named.Arguments.Select(FromPattern)],
                named.TagNameAsPineValue),

            Abstract.Pattern.AsPattern alias => new Pattern.AsPattern(FromPattern(alias.Pattern), alias.Name),

            _ =>
            throw new NotImplementedException(
                $"{nameof(FromPattern)} does not handle pattern variant: {pattern.GetType().Name}")
        };

    /// <summary>Maps an expression recursively. This mapping never introduces ProjectItem.</summary>
    public static Expression FromExpression(Abstract.Expression expression) =>
        expression switch
        {
            Abstract.Expression.UnitExpr => Expression.UnitExpr.Instance,
            Abstract.Expression.StringLiteral text => new Expression.StringLiteral(text.Value, text.ValueAsPineValue),

            Abstract.Expression.CharLiteral character =>
            new Expression.CharLiteral(character.Value, character.ValueAsPineValue),

            Abstract.Expression.IntegerLiteral integer =>
            new Expression.IntegerLiteral(integer.Value, integer.ValueAsPineValue),

            Abstract.Expression.FloatLiteral floating =>
            new Expression.FloatLiteral(floating.Numerator, floating.Denominator),

            Abstract.Expression.Negation negation => new Expression.Negation(FromExpression(negation.Expression)),
            Abstract.Expression.ListExpr list => new Expression.ListExpr([.. list.Elements.Select(FromExpression)]),
            Abstract.Expression.Identifier identifier => new Expression.Identifier(identifier.QualifiedName),

            Abstract.Expression.IfBlock conditional =>
            new Expression.IfBlock(
                FromExpression(conditional.Condition),
                FromExpression(conditional.ThenBlock),
                FromExpression(conditional.ElseBlock)),

            Abstract.Expression.PrefixOperator prefix => new Expression.PrefixOperator(prefix.Operator),

            Abstract.Expression.Application application =>
            new Expression.Application(
                FromExpression(application.Function),
                [.. application.Arguments.Select(FromExpression)]),

            Abstract.Expression.OperatorApplication operation =>
            new Expression.OperatorApplication(
                operation.Operator,
                operation.Direction,
                FromExpression(operation.Left),
                FromExpression(operation.Right)),

            Abstract.Expression.TupledExpression tuple =>
            new Expression.TupledExpression([.. tuple.Elements.Select(FromExpression)]),

            Abstract.Expression.LambdaExpression lambda =>
            new Expression.LambdaExpression(
                [
                .. lambda.Arguments.Select(FromPattern)
                ],
                FromExpression(lambda.Expression)),

            Abstract.Expression.CaseExpression cases =>
            new Expression.CaseExpression(FromExpression(cases.Expression), [.. cases.Cases.Select(FromCase)]),

            Abstract.Expression.LetExpression let =>
            new Expression.LetExpression(
                [
                .. let.Declarations.Select(FromLetDeclaration)
                ],
                FromExpression(let.Expression)),

            Abstract.Expression.RecordExpr record =>
            new Expression.RecordExpr([.. record.Fields.Select(FromRecordSetter)]),

            Abstract.Expression.RecordAccess access =>
            new Expression.RecordAccess(FromExpression(access.Record), access.FieldName, access.FieldNameValue),

            Abstract.Expression.RecordAccessFunction access =>
            new Expression.RecordAccessFunction(access.FieldName, access.FieldNameValue),

            Abstract.Expression.RecordUpdateExpression update =>
            new Expression.RecordUpdateExpression(update.RecordName, [.. update.Fields.Select(FromRecordSetter)]),

            Abstract.Expression.GLSLExpression shader => new Expression.GLSLExpression(shader.ShaderCode),

            _ =>
            throw new NotImplementedException(
                $"{nameof(FromExpression)} does not handle expression variant: {expression.GetType().Name}")
        };

    /// <summary>Maps a record setter, preserving its field name value.</summary>
    public static RecordSetter FromRecordSetter(Abstract.RecordSetter setter) =>
        new(setter.FieldName, setter.FieldNameValue, FromExpression(setter.Value));

    /// <summary>Maps a local declaration.</summary>
    public static LetDeclaration FromLetDeclaration(Abstract.LetDeclaration declaration) =>
        declaration switch
        {
            Abstract.LetDeclaration.LetFunction function =>
            new LetDeclaration.LetFunction(FromFunction(function.Function)),

            Abstract.LetDeclaration.LetDestructuring destructuring =>
            new LetDeclaration.LetDestructuring(
                FromPattern(destructuring.Pattern),
                FromExpression(destructuring.Expression)),

            _ =>
            throw new NotImplementedException(
                $"{nameof(FromLetDeclaration)} does not handle let declaration variant: {declaration.GetType().Name}")
        };

    /// <summary>Maps a case branch.</summary>
    public static Case FromCase(Abstract.Case branch) =>
        new(FromPattern(branch.Pattern), FromExpression(branch.Expression));
}
