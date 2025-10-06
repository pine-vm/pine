using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.Core.DotNet;

/// <summary>
/// Represents the result of filtering local variable declarations to include only those that are used or required as
/// dependencies by referenced statements.
/// </summary>
/// <remarks>This struct is typically produced by analyzing a set of local declarations and subsequent statements
/// to eliminate unused variables while preserving those required for correct program execution. Dependencies are
/// determined by examining references in variable initializers, ensuring that all prerequisite declarations are
/// included.</remarks>
/// <param name="Declarations">The list of local variable declarations that are retained after filtering. Each declaration typically contains a
/// single variable.</param>
/// <param name="KeptLocalNames">The set of local variable names that are used directly or as dependencies and are therefore kept in the filtered
/// result.</param>
public readonly record struct FilteredLocalsResult(
    IReadOnlyList<LocalDeclarationStatementSyntax> Declarations,
    ImmutableHashSet<string> KeptLocalNames)
{
    /// <summary>
    /// Filters a set of local variable declarations to include only those that are referenced in the provided
    /// statements, including any declarations required as dependencies by initializers.
    /// </summary>
    /// <remarks>Dependencies between local variables are determined by analyzing references in initializer
    /// expressions. If a variable is used, all variables it depends on (transitively) are also retained. This method is
    /// useful for eliminating unused local declarations while ensuring that prerequisite declarations are preserved for
    /// correct program semantics.</remarks>
    /// <param name="declarations">The list of local variable declarations to analyze for usage and dependencies. Each declaration is expected to
    /// contain a single variable.</param>
    /// <param name="subsequentStatements">The statements that follow the declarations and may reference the declared local variables. Usage is determined
    /// based on references found in these statements.</param>
    /// <param name="candidateNames">The set of local variable names that are eligible for filtering. Only declarations with names in this set are
    /// considered.</param>
    /// <returns>A FilteredLocalsResult containing the filtered list of declarations that are used directly or as dependencies,
    /// preserving their original order, and the set of all used local names.</returns>
    public static FilteredLocalsResult FilterDeclarationsByUsage(
        IReadOnlyList<LocalDeclarationStatementSyntax> declarations,
        IReadOnlyList<StatementSyntax> subsequentStatements,
        ImmutableHashSet<string> candidateNames)
    {
        // Map local name -> dependencies (other local names) from its initializer
        var nameToDeps = new Dictionary<string, ImmutableHashSet<string>>();
        var nameToDecl = new Dictionary<string, LocalDeclarationStatementSyntax>();

        foreach (var decl in declarations)
        {
            // The code base creates declarations with a single variable per statement
            var variable = decl.Declaration.Variables.FirstOrDefault();
            if (variable is null)
            {
                continue;
            }

            var name =
                variable.Identifier.ValueText;

            nameToDecl[name] = decl;

            var initExpr = variable.Initializer?.Value;

            var deps =
                (initExpr is null)
                ?
                []
                :
                EnumerateReferencedIdentifiers(initExpr).Where(candidateNames.Contains).ToImmutableHashSet();

            nameToDeps[name] = deps;
        }

        // Collect initially used names from subsequent statements
        var used = new HashSet<string>();
        foreach (var stmt in subsequentStatements)
        {
            foreach (var id in EnumerateReferencedIdentifiers(stmt))
            {
                if (candidateNames.Contains(id))
                {
                    used.Add(id);
                }
            }
        }

        // Expand transitively to include dependencies
        var queue = new Queue<string>(used);
        while (queue.Count > 0)
        {
            var current = queue.Dequeue();
            if (!nameToDeps.TryGetValue(current, out var deps))
            {
                continue;
            }

            foreach (var dep in deps)
            {
                if (used.Add(dep))
                {
                    queue.Enqueue(dep);
                }
            }
        }

        // Keep declarations in original order if they are used
        var filteredDecls =
            declarations.Where(d =>
            {
                var variable = d.Declaration.Variables.FirstOrDefault();
                return variable is not null && used.Contains(variable.Identifier.ValueText);
            }).ToImmutableArray();

        return new FilteredLocalsResult(filteredDecls, [.. used]);
    }

    /// <summary>
    /// Enumerates all identifier names referenced in the given syntax node.
    /// This is used to determine which local variable declarations are actually used.
    /// </summary>
    private static IEnumerable<string> EnumerateReferencedIdentifiers(SyntaxNode syntaxNode)
    {
        return syntaxNode
            .DescendantNodesAndSelf()
            .OfType<IdentifierNameSyntax>()
            .Select(identifier => identifier.Identifier.ValueText);
    }
}
