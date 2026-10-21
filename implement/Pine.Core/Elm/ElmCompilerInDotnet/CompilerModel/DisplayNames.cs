using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text;

namespace Pine.Core.Elm.ElmCompilerInDotnet.CompilerModel;

/// <summary>
/// Output-scoped names for diagnostics or a textual backend. They are never compiler identities,
/// are not parsed, and may change when the set of declarations being displayed changes.
/// </summary>
public sealed record DisplayNames(
    ImmutableDictionary<FunctionId, string> Functions,
    ImmutableDictionary<BindingId, string> Bindings)
{
    /// <summary>Assigns deterministic, unique ASCII identifiers, reserving function names before locals.</summary>
    public static DisplayNames Create(Program program)
    {
        var used = new HashSet<string>(StringComparer.Ordinal);

        string Allocate(string label)
        {
            var text = new StringBuilder();

            foreach (var character in label)
                text.Append(char.IsAsciiLetterOrDigit(character) || character is '_' ? character : '_');

            if (text.Length is 0 || !char.IsAsciiLetter(text[0]))
                text.Insert(0, "name_");

            var basis = text.ToString();
            var candidate = basis;

            for (var suffix = 2; !used.Add(candidate); suffix++)
                candidate = basis + "_" + suffix;

            return candidate;
        }

        var functions = ImmutableDictionary.CreateBuilder<FunctionId, string>();

        foreach (var id in program.Functions.Keys.OrderBy(id => id, IdentityComparer.Instance))
        {
            functions.Add(
                id,
                Allocate(
                    id switch
                    {
                        FunctionId.Source source => "function_" + source.Name.FullName,
                        FunctionId.Lifted => "lifted",
                        FunctionId.Specialized => "specialized",

                        _ =>
                        throw new NotImplementedException(
                            $"{nameof(Create)} does not handle function variant: {id.GetType().Name}")
                    }));
        }

        var allBindings = new HashSet<BindingId>();

        void Visit(Expression expression)
        {
            if (expression is Expression.ReferenceExpression { Reference: Reference.Local local })
                allBindings.Add(local.Binding);

            if (expression is Expression.Lambda lambda)
                allBindings.UnionWith(lambda.Parameters.SelectMany(ExpressionTransform.PatternBindings));

            if (expression is Expression.Let let)
            {
                foreach (var binding in let.Bindings)
                {
                    allBindings.UnionWith(ExpressionTransform.PatternBindings(binding.Pattern));
                }
            }

            if (expression is Expression.Match match)
            {
                foreach (var branch in match.Branches)
                    allBindings.UnionWith(ExpressionTransform.PatternBindings(branch.Pattern));
            }

            foreach (var child in ExpressionTransform.Children(expression))
                Visit(child);
        }

        foreach (var function in program.Functions.Values)
        {
            allBindings.UnionWith(function.Parameters.SelectMany(ExpressionTransform.PatternBindings));
            Visit(function.Body);
        }

        var bindings = ImmutableDictionary.CreateBuilder<BindingId, string>();

        foreach (var id in allBindings.OrderBy(id => id, IdentityComparer.Instance))
            bindings.Add(id, Allocate("binding_" + program.BindingNames.GetValueOrDefault(id, "temporary")));

        return new(functions.ToImmutable(), bindings.ToImmutable());
    }
}
