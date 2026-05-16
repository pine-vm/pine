using Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;
using Pine.Core.Elm.ElmSyntax.SyntaxModel;
using System.Collections.Generic;
using System.Collections.Immutable;

using SyntaxTypes = Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;

namespace Pine.Core.Elm.ElmCompilerInDotnet;

/// <summary>
/// Detects "higher-order" parameters of an Elm function declaration: parameters
/// whose value influences the function position of at least one application
/// inside the function body.
///
/// <para>
/// A parameter is higher-order in this sense if and only if at least one of the
/// names it introduces (via its pattern) flows — possibly through enclosing
/// <c>let</c>-bindings, lambdas, case branches, etc. — into an expression that
/// is used as the function part of an
/// <see cref="SyntaxTypes.Expression.Application"/> somewhere in the body.
/// </para>
///
/// <para>
/// The data-flow tracing is delegated to <see cref="SyntaxAnalysis"/>;
/// this module merely enumerates the names introduced by each parameter pattern
/// and intersects them with the set of names that flow into application
/// function positions in the body.
/// </para>
/// </summary>
internal static class HigherOrderParameterAnalysis
{
    /// <summary>
    /// Returns the indices of those parameters in <paramref name="parameters"/>
    /// that bind at least one name flowing into the function position of an
    /// Application in <paramref name="body"/>.
    /// </summary>
    public static IReadOnlyList<int> FindHigherOrderParameterIndices(
        IReadOnlyList<Node<SyntaxTypes.Pattern>> parameters,
        SyntaxTypes.Expression body)
    {
        var flowingIntoAppFunctions =
            SyntaxAnalysis.ComputeNamesFlowingIntoApplicationFunctions(body);

        if (flowingIntoAppFunctions.IsEmpty)
            return [];

        var result = new List<int>(parameters.Count);

        for (var i = 0; i < parameters.Count; i++)
        {
            var paramNames =
                SyntaxAnalysis.CollectNamesBoundByPattern(parameters[i].Value);

            foreach (var name in paramNames)
            {
                if (flowingIntoAppFunctions.Contains(name))
                {
                    result.Add(i);
                    break;
                }
            }
        }

        return result;
    }

    /// <summary>
    /// Convenience overload accepting a <see cref="SyntaxTypes.FunctionImplementation"/>.
    /// </summary>
    public static IReadOnlyList<int> FindHigherOrderParameterIndices(
        SyntaxTypes.FunctionImplementation funcImpl)
    {
        return FindHigherOrderParameterIndices(funcImpl.Arguments, funcImpl.Expression.Value);
    }

    /// <summary>
    /// Returns the set of names introduced by the given parameter that flow into
    /// the function position of any application in <paramref name="body"/>.
    /// Useful for diagnostic / explanatory output.
    /// </summary>
    public static ImmutableHashSet<string> FindHigherOrderNamesIntroducedByParameter(
        SyntaxTypes.Pattern parameter,
        SyntaxTypes.Expression body)
    {
        var flowing = SyntaxAnalysis.ComputeNamesFlowingIntoApplicationFunctions(body);
        var bound = SyntaxAnalysis.CollectNamesBoundByPattern(parameter);
        return bound.Intersect(flowing);
    }
}
