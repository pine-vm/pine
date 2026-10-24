using Pine.Core.Elm.Syntax.Abstract;
using Pine.Core.Elm.Syntax.Concrete;
using System.Collections.Generic;
using System.Linq;

using Abstract = Pine.Core.Elm.Syntax.Abstract;
using Stil4mElmSyntax7 = Pine.Core.Elm.Syntax.Compatibility.Stil4mElmSyntax7;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.ElmCompilerTests;

internal static class AbstractSyntaxTestConversion
{
    public static Abstract.File AbsFile(Stil4mElmSyntax7.File file) =>
        ConvertFromConcrete.FromFile(Stil4mElmSyntax7.ToFullSyntaxModel.Convert(file));

    public static Abstract.Expression Abs(Stil4mElmSyntax7.Expression expression) =>
        ConvertFromConcrete.FromExpression(Stil4mElmSyntax7.ToFullSyntaxModel.Convert(expression));

    public static Abstract.Pattern Abs(Stil4mElmSyntax7.Pattern pattern) =>
        ConvertFromConcrete.FromPattern(Stil4mElmSyntax7.ToFullSyntaxModel.Convert(pattern));

    public static Abstract.TypeAnnotation Abs(Stil4mElmSyntax7.TypeAnnotation typeAnnotation) =>
        ConvertFromConcrete.FromTypeAnnotation(Stil4mElmSyntax7.ToFullSyntaxModel.Convert(typeAnnotation));

    public static IReadOnlyList<Abstract.Pattern> AbsPatterns(IReadOnlyList<Node<Stil4mElmSyntax7.Pattern>> patterns) =>
        [.. patterns.Select(pattern => Abs(pattern.Value))];
}
