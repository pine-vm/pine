using System.Collections.Generic;
using System.Linq;
using Pine.Core.Elm.ElmSyntax.SyntaxModel;

using Abstract = Pine.Core.Elm.ElmSyntax.ElmSyntaxAbstract;
using Concrete = Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.ElmCompilerTests;

internal static class AbstractSyntaxTestConversion
{
    public static Abstract.File AbsFile(Concrete.File file) =>
        Abstract.ConvertFromConcrete.FromFile(Concrete.ToFullSyntaxModel.Convert(file));

    public static Abstract.Expression Abs(Concrete.Expression expression) =>
        Abstract.ConvertFromConcrete.FromExpression(Concrete.ToFullSyntaxModel.Convert(expression));

    public static Abstract.Pattern Abs(Concrete.Pattern pattern) =>
        Abstract.ConvertFromConcrete.FromPattern(Concrete.ToFullSyntaxModel.Convert(pattern));

    public static Abstract.TypeAnnotation Abs(Concrete.TypeAnnotation typeAnnotation) =>
        Abstract.ConvertFromConcrete.FromTypeAnnotation(Concrete.ToFullSyntaxModel.Convert(typeAnnotation));

    public static IReadOnlyList<Abstract.Pattern> AbsPatterns(IReadOnlyList<Node<Concrete.Pattern>> patterns) =>
        [.. patterns.Select(pattern => Abs(pattern.Value))];
}
