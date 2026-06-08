namespace Pine.Core.Elm.ElmCompilerInDotnet;

using Abstract = ElmSyntax.ElmSyntaxAbstract;
using Concrete = ElmSyntax.Stil4mElmSyntax7;

internal static class ElmSyntaxAbstractConversion
{
    public static Abstract.File FromFile(Concrete.File file) =>
        Abstract.ConvertFromConcrete.FromFile(Concrete.ToFullSyntaxModel.Convert(file));

    public static Abstract.Expression FromExpression(Concrete.Expression expression) =>
        Abstract.ConvertFromConcrete.FromExpression(Concrete.ToFullSyntaxModel.Convert(expression));

    public static Abstract.Pattern FromPattern(Concrete.Pattern pattern) =>
        Abstract.ConvertFromConcrete.FromPattern(Concrete.ToFullSyntaxModel.Convert(pattern));

    public static Abstract.TypeAnnotation FromTypeAnnotation(Concrete.TypeAnnotation typeAnnotation) =>
        Abstract.ConvertFromConcrete.FromTypeAnnotation(Concrete.ToFullSyntaxModel.Convert(typeAnnotation));

    public static Abstract.Declaration FromDeclaration(Concrete.Declaration declaration) =>
        Abstract.ConvertFromConcrete.FromDeclaration(Concrete.ToFullSyntaxModel.Convert(declaration));

    public static Abstract.FunctionStruct FromFunctionStruct(Concrete.FunctionStruct functionStruct) =>
        Abstract.ConvertFromConcrete.FromFunctionStruct(Concrete.ToFullSyntaxModel.Convert(functionStruct));
}
