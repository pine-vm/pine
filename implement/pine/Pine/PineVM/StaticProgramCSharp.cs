using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Pine.CompilePineToDotNet;
using Pine.Core;
using Pine.Core.Addressing;
using Pine.Core.CodeAnalysis;
using Pine.Core.PopularEncodings;
using System;
using System.Collections.Frozen;
using System.Collections.Generic;
using System.Linq;

namespace Pine.Pine.PineVM;

public record StaticProgramCSharp(
    IReadOnlyDictionary<DeclQualifiedName, StaticProgramCSharpClass> ModulesClasses,
    ClassDeclarationSyntax CommonValueClass)
{
    private const string CommonValueClassName = "CommonReusedValues";

    public static StaticProgramCSharp FromStaticProgram(
        StaticProgram staticProgram)
    {
        var literalsValues =
            PineValue.CollectAllComponentsFromRoots(CollectLiteralsValues(staticProgram));

        var valueHashCache =
            new ConcurrentPineValueHashCache();

        var commonValueClass =
            StaticValueClassDeclaration(
                blobValues: literalsValues.blobs,
                listValues: literalsValues.lists,
                className: CommonValueClassName,
                valueHashCache);

        var availableFunctions =
            staticProgram.NamedFunctions
            .ToFrozenDictionary(
                kvp => kvp.Key,
                kvp => kvp.Value.interf);

        var classNames = new HashSet<DeclQualifiedName>();

        foreach (var kvp in staticProgram.NamedFunctions)
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
                        staticProgram.NamedFunctions
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
                        availableFunctions,
                        commonValueClass.availableDecls);
                });

        return
            new StaticProgramCSharp(
                ModulesClasses: classes,
                CommonValueClass: commonValueClass.classDeclaration);
    }

    public static (ClassDeclarationSyntax classDeclaration, IReadOnlyDictionary<PineValue, DeclQualifiedName> availableDecls)
        StaticValueClassDeclaration(
        IReadOnlySet<PineValue.BlobValue> blobValues,
        IReadOnlySet<PineValue.ListValue> listValues,
        string className,
        ConcurrentPineValueHashCache valueHashCache)
    {
        var mutatedDict = new Dictionary<PineValue, string>();

        var listValuesOrdered =
            listValues
            .OrderBy(l => l.NodesCount)
            .ToList();

        var memberDeclarations = new List<MemberDeclarationSyntax>();

        ExpressionSyntax? OverrideDefaultExpression(PineValue v)
        {
            if (mutatedDict.TryGetValue(v, out var existingName))
            {
                return SyntaxFactory.IdentifierName(existingName);
            }

            return null;
        }

        void AddMemberDeclaration(
            string DeclName,
            PineValue value)
        {
            var declExpression =
                CompileToCSharp.CompileToCSharpLiteralExpression(
                    value,
                    overrideDefaultExpression: OverrideDefaultExpression);

            memberDeclarations.Add(
                SyntaxFactory.FieldDeclaration(
                    attributeLists: SyntaxFactory.List<AttributeListSyntax>(),
                    modifiers: SyntaxFactory.TokenList(
                        SyntaxFactory.Token(SyntaxKind.PublicKeyword),
                        SyntaxFactory.Token(SyntaxKind.StaticKeyword),
                        SyntaxFactory.Token(SyntaxKind.ReadOnlyKeyword)),
                    declaration: SyntaxFactory.VariableDeclaration(
                        SyntaxFactory.ParseTypeName(nameof(PineValue)),
                        SyntaxFactory.SeparatedList(
                        [
                            SyntaxFactory.VariableDeclarator(
                                SyntaxFactory.Identifier(DeclName))
                            .WithInitializer(
                                SyntaxFactory.EqualsValueClause(declExpression.exprSyntax))
                        ]))));

            mutatedDict[value] = DeclName;
        }

        foreach (var blobValue in blobValues)
        {
            var declName = NameValueDeclaration(blobValue, valueHashCache);

            AddMemberDeclaration(declName, blobValue);
        }

        foreach (var listValue in listValuesOrdered)
        {
            var declName = NameValueDeclaration(listValue, valueHashCache);

            AddMemberDeclaration(declName, listValue);
        }

        var availableDecls =
            mutatedDict
            .ToFrozenDictionary(
                kvp => kvp.Key,
                kvp => new DeclQualifiedName([className], kvp.Value));

        var classDeclaration =
            SyntaxFactory.ClassDeclaration(className)
            .WithModifiers(
                SyntaxFactory.TokenList(
                    SyntaxFactory.Token(SyntaxKind.PublicKeyword),
                    SyntaxFactory.Token(SyntaxKind.StaticKeyword)))
            .WithMembers(SyntaxFactory.List(memberDeclarations));

        return (classDeclaration, availableDecls);
    }

    public static string NameValueDeclaration(
        PineValue value,
        ConcurrentPineValueHashCache valueHashCache)
    {
        if (value is PineValue.BlobValue blob)
        {
            if (IntegerEncoding.ParseSignedIntegerStrict(blob).IsOkOrNullable() is { } asInt)
            {
                if (IntegerEncoding.EncodeSignedInteger(asInt) == blob)
                {
                    var intRepr =
                        asInt < 0
                        ?
                        "neg_" + (-asInt).ToString()
                        :
                        asInt.ToString();

                    return $"Blob_Int_{intRepr}";
                }
            }

            if (StringEncoding.StringFromValue(blob).IsOkOrNull() is { } asString)
            {
                var allCharsCompatible = true;

                for (var i = 0; i < asString.Length; i++)
                {
                    var c = asString[i];

                    if (!(char.IsAsciiLetterOrDigit(c) || c == '_'))
                    {
                        allCharsCompatible = false;
                        break;
                    }
                }

                if (allCharsCompatible && StringEncoding.ValueFromString(asString) == blob)
                {
                    return $"Blob_Str_{asString}";
                }
            }

            var declHash = valueHashCache.GetHash(blob);

            return $"Blob_{Convert.ToHexStringLower(declHash.Span)[..8]}";
        }

        if (value is PineValue.ListValue list)
        {
            var declHash = valueHashCache.GetHash(list);

            return $"List_{Convert.ToHexStringLower(declHash.Span)[..8]}";
        }

        throw new NotImplementedException(
            $"Naming not implemented for value type {value.GetType()}");
    }

    public static IEnumerable<PineValue> CollectLiteralsValues(
        StaticProgram staticProgram)
    {
        return
            staticProgram.NamedFunctions
            .SelectMany(kvp => CollectLiteralsValues(kvp.Value.body));
    }

    public static IEnumerable<PineValue> CollectLiteralsValues<FuncId>(
        StaticExpression<FuncId> expression)
    {
        foreach (var expr in expression.EnumerateAllDescendants(skipDescendants: null))
        {
            if (expr is StaticExpression<FuncId>.Literal lit)
                yield return lit.Value;
        }
    }
}
