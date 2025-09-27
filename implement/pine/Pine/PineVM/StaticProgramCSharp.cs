using Pine.Core.CodeAnalysis;
using System.Collections.Frozen;
using System.Collections.Generic;
using System.Linq;

namespace Pine.Pine.PineVM;

public record StaticProgramCSharp(
    IReadOnlyDictionary<DeclQualifiedName, StaticProgramCSharpClass> Classes)
{
    public static StaticProgramCSharp FromStaticProgram(
        StaticProgram staticProgram)
    {
        var namedFunctions =
            staticProgram.NamedFunctions;

        var availableFunctions =
            namedFunctions
            .ToFrozenDictionary(
                kvp => kvp.Key,
                kvp => kvp.Value.interf);

        var classNames = new HashSet<DeclQualifiedName>();

        foreach (var kvp in namedFunctions)
        {
            if (kvp.Key.Namespaces.Count is 0)
            {
                // Global namespace function, belongs to global class
                classNames.Add(new DeclQualifiedName([], "Global"));
            }
            else
            {
                var className = new DeclQualifiedName(
                    Namespaces: [.. kvp.Key.Namespaces.SkipLast(1)],
                    DeclName: kvp.Key.Namespaces[^1]);

                classNames.Add(className);
            }
        }

        var classes =
            classNames
            .ToFrozenDictionary(
                keySelector: cn => cn,
                elementSelector:
                cn =>
                {
                    var functionsInClass =
                        namedFunctions
                        .Where(kvp =>
                        {
                            return
                            kvp.Key.Namespaces.SequenceEqual([.. cn.Namespaces, cn.DeclName]);
                        })
                        .ToFrozenDictionary(
                            kvp => kvp.Key.DeclName,
                            kvp => kvp.Value);

                    return
                    StaticProgramCSharpClass.FromDeclarations(
                        className: cn.DeclName,
                        functionsInClass,
                        availableFunctions);
                });

        return new StaticProgramCSharp(Classes: classes);
    }
}
