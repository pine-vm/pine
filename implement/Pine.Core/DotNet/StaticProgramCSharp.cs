using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Pine.Core.Addressing;
using Pine.Core.CodeAnalysis;
using Pine.Core.PineVM;
using Pine.Core.PopularEncodings;
using System;
using System.Collections.Frozen;
using System.Collections.Generic;
using System.Linq;

namespace Pine.Core.DotNet;

public record StaticProgramCSharp(
    IReadOnlyDictionary<DeclQualifiedName, StaticProgramCSharpClass> ModulesClasses,
    ClassDeclarationSyntax CommonValueClass,
    StaticProgramCSharpClass GlobalAnonymousClass,
    ClassDeclarationSyntax DispatcherClass,
    DeclarationSyntaxContext DeclarationSyntaxContext)
{
    private const string CommonValueClassName = "CommonReusedValues";

    private const string GlobalAnonFunctionsClassName = "Global_Anonymous";

    public static DeclarationSyntaxContext DeclarationSyntaxContextDefault()
    {
        UsingDirectiveSyntax UsingAliasForType(Type type, string alias)
        {
            // Also prepend the ::global prefix to avoid conflicts with local aliases

            var generalTypeName =
                (NameSyntax)CompileTypeSyntax.TypeSyntaxFromType(type, DeclarationSyntaxContext.None);

            var segments =
                PineCSharpSyntaxFactory.QualifiedNameSegments(generalTypeName);

            var nameWithGlobal =
                PineCSharpSyntaxFactory.QualifiedNameSyntaxFromSegments(
                    SyntaxFactory.AliasQualifiedName(
                        SyntaxFactory.IdentifierName("global"),
                        segments[0]),
                    [.. segments.Skip(1)]);

            return
                SyntaxFactory.UsingDirective(
                    alias: SyntaxFactory.NameEquals(SyntaxFactory.IdentifierName(alias)),
                    name: nameWithGlobal);
        }

        UsingDirectiveSyntax UsingAliasForTypeDefault(Type type)
        {
            return
                UsingAliasForType(type, "Pine_" + type.Name);
        }

        return
            new DeclarationSyntaxContext(
                UsingDirectives:
                [
                    UsingAliasForTypeDefault(typeof(PineValue)),
                    UsingAliasForTypeDefault(typeof(KernelFunction)),
                    UsingAliasForTypeDefault(typeof(PineKernelValues)),
                    UsingAliasForTypeDefault(typeof(PineValueExtension)),
                    UsingAliasForTypeDefault(typeof(Internal.KernelFunctionFused)),
                    UsingAliasForTypeDefault(typeof(Internal.KernelFunctionSpecialized)),

                    UsingAliasForTypeDefault(typeof(IntegerEncoding)),
                    UsingAliasForTypeDefault(typeof(StringEncoding)),
                    UsingAliasForTypeDefault(typeof(ExpressionEncoding)),
                    UsingAliasForTypeDefault(typeof(ParseExpressionException)),

                    UsingAliasForTypeDefault(typeof(Builtins.ImmutableConcatBuilder)),
                    UsingAliasForTypeDefault(typeof(Builtins.ImmutableSliceBuilder)),
                    UsingAliasForTypeDefault(typeof(Builtins.MutatingConcatBuilder)),
                ],
                CurrentNamespace: null);
    }

    public static StaticProgramCSharp FromStaticProgram(
        StaticProgram staticProgram) =>
        FromStaticProgram(staticProgram, DeclarationSyntaxContextDefault());

    public static StaticProgramCSharp FromStaticProgram(
        StaticProgram staticProgram,
        DeclarationSyntaxContext declarationSyntaxContext)
    {
        var valuesToReuseRoots =
            CollectValuesToReuse(staticProgram)
            .ToFrozenSet();

        var valuesToReuse =
            PineValue.CollectAllComponentsFromRoots(valuesToReuseRoots);

        var blobsFiltered =
            valuesToReuse.blobs
            .Where(b => !ExcludeValueFromDeclarationsEmittedForReuse(b))
            .ToFrozenSet();

        var listsFiltered =
            valuesToReuse.lists
            .Where(l => !ExcludeValueFromDeclarationsEmittedForReuse(l))
            .ToFrozenSet();

        var valueHashCache =
            new ConcurrentPineValueHashCache();

        var commonValueClass =
            StaticValueClassDeclaration(
                blobValues: blobsFiltered,
                listValues: listsFiltered,
                className: CommonValueClassName,
                declarationSyntaxContext,
                valueHashCache);

        static DeclQualifiedName MapFunctionNameToGlobalAnon(DeclQualifiedName fnName)
        {
            if (fnName.Namespaces.Count is 0)
            {
                return new DeclQualifiedName(GlobalAnonFunctionsClassName.Split('.'), fnName.DeclName);
            }

            return fnName;
        }

        var availableFunctions =
            staticProgram.NamedFunctions
            .ToFrozenDictionary(
                kvp => MapFunctionNameToGlobalAnon(kvp.Key),
                kvp => kvp.Value.interf);

        var moduleNames = new HashSet<DeclQualifiedName>();

        foreach (var kvp in staticProgram.NamedFunctions)
        {
            if (kvp.Key.Namespaces.Count is not 0)
            {
                var className =
                    new DeclQualifiedName(
                        Namespaces: [.. kvp.Key.Namespaces.SkipLast(1)],
                        DeclName: kvp.Key.Namespaces[^1]);

                moduleNames.Add(className);
            }
        }

        var globalAnonymousFunctions =
            staticProgram.NamedFunctions
            .Where(ns => ns.Key.Namespaces.Count is 0)
            .ToFrozenDictionary(
                keySelector:
                kvp => kvp.Key.DeclName,
                elementSelector:
                kvp =>
                {
                    return (kvp.Value.interf, kvp.Value.body);
                });

        StaticProgramCSharpClass ClassFromDeclarations(
            DeclQualifiedName className,
            IReadOnlyDictionary<string, (StaticFunctionInterface interf, StaticExpression<DeclQualifiedName> body)> declarations)
        {
            var declarationsMapped =
                declarations
                .ToFrozenDictionary(
                    kvp => kvp.Key,
                    kvp =>
                    {
                        var bodyMapped =
                        StaticExpression<DeclQualifiedName>.MapFunctionIdentifier(
                            kvp.Value.body,
                            MapFunctionNameToGlobalAnon);
                        return (kvp.Value.interf, bodyMapped);
                    });

            return
                StaticProgramCSharpClass.FromDeclarations(
                    className,
                    declarationsMapped,
                    availableFunctions,
                    commonValueClass.availableDecls,
                    declarationSyntaxContext);
        }

        var modulesClasses =
            moduleNames
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
                            kvp =>
                            {
                                return (kvp.Value.interf, kvp.Value.body);
                            });

                    return ClassFromDeclarations(className: cn, functionsInClass);
                });

        var dispatcherClassDecl =
            DispatcherClassDeclaration(
                staticProgram.NamedFunctions
                .ToFrozenDictionary(
                    kvp => MapFunctionNameToGlobalAnon(kvp.Key),
                    kvp => (kvp.Value.origExpr, kvp.Value.interf, kvp.Value.constraint)),
                valueHashCache: valueHashCache,
                existingAvailableDecls: commonValueClass.availableDecls,
                declarationSyntaxContext: declarationSyntaxContext);

        var globalAnonClassDecl =
            ClassFromDeclarations(
                className: new DeclQualifiedName([], GlobalAnonFunctionsClassName),
                globalAnonymousFunctions);

        return
            new StaticProgramCSharp(
                ModulesClasses: modulesClasses,
                CommonValueClass: commonValueClass.classDeclaration,
                GlobalAnonymousClass: globalAnonClassDecl,
                DispatcherClass: dispatcherClassDecl,
                DeclarationSyntaxContext: declarationSyntaxContext);
    }

    public static (ClassDeclarationSyntax classDeclaration, IReadOnlyDictionary<PineValue, DeclQualifiedName> availableDecls)
        StaticValueClassDeclaration(
        IReadOnlySet<PineValue.BlobValue> blobValues,
        IReadOnlySet<PineValue.ListValue> listValues,
        string className,
        DeclarationSyntaxContext declarationSyntaxContext,
        ConcurrentPineValueHashCache valueHashCache)
    {
        var mutatedDict = new Dictionary<PineValue, string>();

        var blobValuesOrdered =
            blobValues
            .Order(CSharpDeclarationOrder.BlobValueDeclarationOrder.Instance)
            .ToList();

        var listValuesOrdered =
            listValues
            .Order(CSharpDeclarationOrder.ValueDeclarationOrder.Instance)
            .ToList();

        var memberDeclarations = new List<MemberDeclarationSyntax>();

        ExpressionSyntax? OverrideDefaultExpression(PineValue v)
        {
            if (mutatedDict.TryGetValue(v, out var existingName))
            {
                return SyntaxFactory.ParseName(existingName);
            }

            return null;
        }

        void AddMemberDeclaration(PineValue value)
        {
            var declName = NameValueDeclaration(value, valueHashCache);

            var declExpression =
                PineCSharpSyntaxFactory.CompileToCSharpLiteralExpression(
                    value,
                    overrideDefaultExpression: OverrideDefaultExpression,
                    declarationSyntaxContext);

            memberDeclarations.Add(
                SyntaxFactory.FieldDeclaration(
                    attributeLists: SyntaxFactory.List<AttributeListSyntax>(),
                    modifiers: SyntaxFactory.TokenList(
                        SyntaxFactory.Token(SyntaxKind.PublicKeyword),
                        SyntaxFactory.Token(SyntaxKind.StaticKeyword),
                        SyntaxFactory.Token(SyntaxKind.ReadOnlyKeyword)),
                    declaration:
                    SyntaxFactory.VariableDeclaration(
                        CompileTypeSyntax.TypeSyntaxFromType(
                            typeof(PineValue), declarationSyntaxContext),
                        SyntaxFactory.SeparatedList(
                        [
                            SyntaxFactory.VariableDeclarator(
                                SyntaxFactory.Identifier(declName))
                            .WithInitializer(
                                SyntaxFactory.EqualsValueClause(declExpression.exprSyntax))
                        ]))));

            mutatedDict[value] = declName;
        }

        foreach (var blobValue in blobValuesOrdered)
        {
            AddMemberDeclaration(blobValue);
        }

        foreach (var listValue in listValuesOrdered)
        {
            AddMemberDeclaration(listValue);
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

    public static bool ExcludeValueFromDeclarationsEmittedForReuse(PineValue v)
    {
        if (v == PineKernelValues.TrueValue || v == PineKernelValues.FalseValue)
            return true;

        if (v == PineValue.EmptyList || v == PineValue.EmptyBlob)
            return true;

        return false;
    }

    public static ClassDeclarationSyntax DispatcherClassDeclaration(
        IReadOnlyDictionary<DeclQualifiedName, (Expression origExpr, StaticFunctionInterface interf, PineValueClass constraint)> namedFunctions,
        DeclarationSyntaxContext declarationSyntaxContext,
        IReadOnlyDictionary<PineValue, DeclQualifiedName> existingAvailableDecls,
        ConcurrentPineValueHashCache valueHashCache)
    {
        ExpressionSyntax? OverrideLiteralDefaultExpression(PineValue v)
        {
            if (existingAvailableDecls.TryGetValue(v, out var existingName))
            {
                return SyntaxFactory.ParseName(existingName.FullName);
            }

            return null;
        }

        ExpressionSyntax LiteralExpressionSyntax(PineValue v)
        {
            if (OverrideLiteralDefaultExpression(v) is { } existingExpr)
            {
                return existingExpr;
            }

            var expr =
                PineCSharpSyntaxFactory.CompileToCSharpLiteralExpression(
                    v,
                    overrideDefaultExpression: OverrideLiteralDefaultExpression,
                    declarationSyntaxContext);

            return expr.exprSyntax;
        }

        var groupedByOrigExpr =
            namedFunctions
            .GroupBy(kvp => kvp.Value.origExpr)
            .ToFrozenDictionary(
                g => g.Key,
                g => g.ToFrozenDictionary(
                    kvp => kvp.Key,
                    kvp => (kvp.Value.interf, kvp.Value.constraint)));


        // Build methods per distinct origExpr
        var dispatchMethodDeclarations = new List<MemberDeclarationSyntax>();

        var mutatedDictName = "dict";

        var environmentParamName = "environment";

        var environmentParamType =
            CompileTypeSyntax.TypeSyntaxFromType(
                typeof(PineValue),
                context: declarationSyntaxContext);

        var environmentParam =
            SyntaxFactory.Parameter(SyntaxFactory.Identifier(environmentParamName))
            .WithType(environmentParamType);

        // var dict = new System.Collections.Generic.Dictionary<PineValue, System.Func<PineValue, PineValue?>>();
        var dictDeclaration =
            SyntaxFactory.LocalDeclarationStatement(
                SyntaxFactory.VariableDeclaration(
                    SyntaxFactory.IdentifierName("var"))
                .WithVariables(
                    SyntaxFactory.SingletonSeparatedList(
                        SyntaxFactory.VariableDeclarator(SyntaxFactory.Identifier(mutatedDictName))
                        .WithInitializer(
                            SyntaxFactory.EqualsValueClause(
                                SyntaxFactory.ObjectCreationExpression(
                                    CompileTypeSyntax.TypeSyntaxFromType(
                                        typeof(Dictionary<PineValue, Func<PineValue, PineValue?>>),
                                        context: declarationSyntaxContext))
                                        .WithArgumentList(SyntaxFactory.ArgumentList()))))));

        // Build field 'dispatcherDictionary' and builder method
        var buildMethodStatements = new List<StatementSyntax>
        {
            dictDeclaration
        };

        foreach (var functionGroup in groupedByOrigExpr)
        {
            var origExpr = functionGroup.Key;
            var origExprValue = ExpressionEncoding.EncodeExpressionAsValue(origExpr);

            var originalExprHash = valueHashCache.GetHash(origExprValue);

            var methodName =
                "Dispatch_" + Convert.ToHexStringLower(originalExprHash.Span)[..8];

            // Build the method body: check constraints for each candidate, then call corresponding function
            var methodBodyStatements = new List<StatementSyntax>();

            // Order candidates by specificity (more specific first)
            var candidates =
                functionGroup.Value
                .OrderByDescending(k => k.Value.constraint, PineValueClassSpecificityComparer.Instance)
                .ToArray();

            foreach (var kvp in candidates)
            {
                var functionName = kvp.Key; // DeclQualifiedName
                var interf = kvp.Value.interf;
                var constraint = kvp.Value.constraint;

                // Build condition expression for constraints: conjunction of equality checks
                ExpressionSyntax? conditionExpr = null;

                foreach (var item in constraint.ParsedItems)
                {
                    var valueAtPathExpr =
                        PineCSharpSyntaxFactory.BuildCSharpExpressionToGetItemFromPathOrEmptyList(
                            SyntaxFactory.IdentifierName(environmentParamName),
                            item.Key,
                            declarationSyntaxContext);

                    var expectedValueExpr = LiteralExpressionSyntax(item.Value);

                    var equalityExpr =
                        SyntaxFactory.BinaryExpression(
                            SyntaxKind.EqualsExpression,
                            valueAtPathExpr,
                            expectedValueExpr);

                    conditionExpr =
                        conditionExpr is null
                        ? equalityExpr
                        : SyntaxFactory.BinaryExpression(
                            SyntaxKind.LogicalAndExpression,
                            CompiledCSharpExpression.EnsureIsParenthesizedForComposition(conditionExpr),
                            CompiledCSharpExpression.EnsureIsParenthesizedForComposition(equalityExpr));
                }

                // Default to 'true' when no constraints
                conditionExpr ??= SyntaxFactory.LiteralExpression(SyntaxKind.TrueLiteralExpression);

                // Inside the 'if', extract parameters and call the function
                var insideIfStatements = new List<StatementSyntax>();
                var argIdentifiers = new List<string>();

                for (var i = 0; i < interf.ParamsPaths.Count; i++)
                {
                    var path = interf.ParamsPaths[i];

                    var argName = "arg_" + string.Join("_", path);

                    argIdentifiers.Add(argName);

                    var pathArrayExpr2 =
                        SyntaxFactory.ArrayCreationExpression(
                            SyntaxFactory.ArrayType(
                                SyntaxFactory.PredefinedType(SyntaxFactory.Token(SyntaxKind.IntKeyword)))
                            .WithRankSpecifiers(
                                SyntaxFactory.SingletonList(
                                    SyntaxFactory.ArrayRankSpecifier(
                                        SyntaxFactory.SingletonSeparatedList<ExpressionSyntax>(
                                            SyntaxFactory.OmittedArraySizeExpression())))))
                        .WithInitializer(
                            SyntaxFactory.InitializerExpression(
                                SyntaxKind.ArrayInitializerExpression,
                                SyntaxFactory.SeparatedList(
                                    path.Select(index => (ExpressionSyntax)PineCSharpSyntaxFactory.ExpressionSyntaxForIntegerLiteral(index))
                                )));

                    var valueFromPathInvocation =
                        PineCSharpSyntaxFactory.BuildCSharpExpressionToGetItemFromPathOrEmptyList(
                            SyntaxFactory.IdentifierName(environmentParamName),
                            path,
                            declarationSyntaxContext);

                    // Declare local: var arg_i = CodeAnalysis.ValueFromPathInValue(environment, path);
                    insideIfStatements.Add(
                        SyntaxFactory.LocalDeclarationStatement(
                            SyntaxFactory.VariableDeclaration(SyntaxFactory.IdentifierName("var"))
                            .WithVariables(
                                SyntaxFactory.SingletonSeparatedList(
                                    SyntaxFactory.VariableDeclarator(SyntaxFactory.Identifier(argName))
                                    .WithInitializer(SyntaxFactory.EqualsValueClause(valueFromPathInvocation))))));
                }

                // return Function(...)
                var invocation =
                    SyntaxFactory.InvocationExpression(
                        SyntaxFactory.ParseName(functionName.FullName))
                    .WithArgumentList(
                        SyntaxFactory.ArgumentList(
                            SyntaxFactory.SeparatedList(
                                argIdentifiers.Select(arg =>
                                    SyntaxFactory.Argument(
                                        SyntaxFactory.IdentifierName(arg))))));

                var returnStatement = SyntaxFactory.ReturnStatement(invocation);

                insideIfStatements.Add(returnStatement);

                methodBodyStatements.Add(
                    SyntaxFactory.IfStatement(
                        condition: conditionExpr,
                        statement: SyntaxFactory.Block(insideIfStatements)));
            }

            // None matched
            methodBodyStatements.Add(SyntaxFactory.ReturnStatement(
                SyntaxFactory.LiteralExpression(SyntaxKind.NullLiteralExpression)));

            var methodDecl =
                SyntaxFactory.MethodDeclaration(
                    returnType: SyntaxFactory.NullableType(
                        CompileTypeSyntax.TypeSyntaxFromType(typeof(PineValue), declarationSyntaxContext)),
                    identifier: SyntaxFactory.Identifier(methodName))
                .WithModifiers(
                    SyntaxFactory.TokenList(
                        SyntaxFactory.Token(SyntaxKind.PublicKeyword),
                        SyntaxFactory.Token(SyntaxKind.StaticKeyword)))
                .WithParameterList(
                    SyntaxFactory.ParameterList(
                        SyntaxFactory.SingletonSeparatedList(environmentParam)))
                .WithBody(SyntaxFactory.Block(methodBodyStatements));

            dispatchMethodDeclarations.Add(methodDecl);

            // dict[key] = Dispatch_xxx;
            var encodedExprValueSyntax = LiteralExpressionSyntax(origExprValue);

            buildMethodStatements.Add(
                SyntaxFactory.ExpressionStatement(
                    SyntaxFactory.AssignmentExpression(
                        SyntaxKind.SimpleAssignmentExpression,
                        SyntaxFactory.ElementAccessExpression(
                            SyntaxFactory.IdentifierName(mutatedDictName))
                        .WithArgumentList(
                            SyntaxFactory.BracketedArgumentList(
                                SyntaxFactory.SingletonSeparatedList(
                                    SyntaxFactory.Argument(encodedExprValueSyntax)))),
                        SyntaxFactory.IdentifierName(methodName))));
        }

        // return dict;
        buildMethodStatements.Add(SyntaxFactory.ReturnStatement(SyntaxFactory.IdentifierName(mutatedDictName)));

        var buildDispatcherMethod =
            SyntaxFactory.MethodDeclaration(
                returnType: CompileTypeSyntax.TypeSyntaxFromType(
                    typeof(IReadOnlyDictionary<PineValue, Func<PineValue, PineValue?>>),
                    context: declarationSyntaxContext),
                identifier: SyntaxFactory.Identifier("BuildDispatcherDictionary"))
            .WithModifiers(
                SyntaxFactory.TokenList(
                    SyntaxFactory.Token(SyntaxKind.PublicKeyword),
                    SyntaxFactory.Token(SyntaxKind.StaticKeyword)))
            .WithBody(SyntaxFactory.Block(buildMethodStatements));

        var dispatcherField =
            SyntaxFactory.FieldDeclaration(
                SyntaxFactory.VariableDeclaration(
                    CompileTypeSyntax.TypeSyntaxFromType(
                        typeof(IReadOnlyDictionary<PineValue, Func<PineValue, PineValue?>>),
                        context: declarationSyntaxContext))
                .WithVariables(
                    SyntaxFactory.SingletonSeparatedList(
                        SyntaxFactory.VariableDeclarator(SyntaxFactory.Identifier("dispatcherDictionary"))
                        .WithInitializer(
                            SyntaxFactory.EqualsValueClause(
                                SyntaxFactory.InvocationExpression(
                                    SyntaxFactory.IdentifierName("BuildDispatcherDictionary"))
                                .WithArgumentList(SyntaxFactory.ArgumentList()))))))
            .WithModifiers(
                SyntaxFactory.TokenList(
                    SyntaxFactory.Token(SyntaxKind.PublicKeyword),
                    SyntaxFactory.Token(SyntaxKind.StaticKeyword)));

        var classMembers = new List<MemberDeclarationSyntax>
        {
            dispatcherField,
            buildDispatcherMethod
        };

        classMembers.AddRange(dispatchMethodDeclarations);

        var classDeclaration =
            SyntaxFactory.ClassDeclaration("Dispatcher")
            .WithModifiers(
                SyntaxFactory.TokenList(
                    SyntaxFactory.Token(SyntaxKind.PublicKeyword),
                    SyntaxFactory.Token(SyntaxKind.StaticKeyword)))
            .WithMembers(SyntaxFactory.List(classMembers));

        return classDeclaration;
    }

    public static string NameValueDeclaration(
        PineValue value,
        ConcurrentPineValueHashCache valueHashCache)
    {
        if (value == PineKernelValues.TrueValue)
            return "Bool_True";

        if (value == PineKernelValues.FalseValue)
            return "Bool_False";

        if (value is PineValue.BlobValue blob)
        {
            if (blob.Bytes.Length is 4)
            {
                var asInt = System.Buffers.Binary.BinaryPrimitives.ReadInt32BigEndian(blob.Bytes.Span);

                if (UnicodeUtility.IsValidUnicodeScalar(asInt))
                {
                    var charRepr = char.ConvertFromUtf32(asInt);

                    if (NameCharInDeclaration(charRepr[0]) is { } charName)
                    {
                        return $"Blob_Char_{charName}";
                    }
                }
            }

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
            if (list.Items.Length is 0)
                return "List_Empty";

            if (list.Items.Length is 1)
            {
                if (NameValueDeclaration(list.Items.Span[0], valueHashCache) is { } itemName)
                {
                    return $"List_Single_{itemName}";
                }
            }

            var declHash = valueHashCache.GetHash(list);

            return $"List_{Convert.ToHexStringLower(declHash.Span)[..8]}";
        }

        throw new NotImplementedException(
            $"Naming not implemented for value type {value.GetType()}");
    }

    public static IEnumerable<PineValue> CollectValuesToReuse(
        StaticProgram staticProgram)
    {
        foreach (var kvp in staticProgram.NamedFunctions)
        {
            foreach (var lit in CollectLiteralsValues(kvp.Value.body))
                yield return lit;

            foreach (var v in CollectValuesRootsFromValueClass(kvp.Value.constraint))
                yield return v;

            yield return ExpressionEncoding.EncodeExpressionAsValue(kvp.Value.origExpr);
        }
    }

    public static IEnumerable<PineValue> CollectValuesRootsFromValueClass(
        PineValueClass pvc)
    {
        foreach (var item in pvc.ParsedItems)
        {
            yield return item.Value;
        }
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

    public static string? NameCharInDeclaration(char c)
    {
        if (c is '\'')
            return "quote";

        if (c is '"')
            return "doublequote";

        if (c is '\\')
            return "backslash";

        if ('0' <= c && c <= '9')
        {
            return "digit_" + c;
        }

        if (c is '_')
            return "underscore";

        if (c is '-')
            return "hyphen";

        if (c is '+')
            return "plus";

        if (c is '.')
            return "dot";

        if (c is ',')
            return "comma";

        if (c is ';')
            return "semicolon";

        if (c is ':')
            return "colon";

        if (c is '!')
            return "exclamation";

        if (c is '?')
            return "question";

        if (c is '@')
            return "at";

        if (c is '#')
            return "hash";

        if (c is '€')
            return "euro";

        if (c is '£')
            return "pound";

        if (c is '¥')
            return "yen";

        if (c is '$')
            return "dollar";

        if (c is '¢')
            return "cent";

        if (c is '%')
            return "percent";

        if (c is '^')
            return "caret";

        if (c is '&')
            return "ampersand";

        if (c is '*')
            return "asterisk";

        if (c is '(')
            return "parenopen";

        if (c is ')')
            return "parenclose";

        if (c is '[')
            return "bracketopen";

        if (c is ']')
            return "bracketclose";

        if (c is '{')
            return "braceopen";

        if (c is '}')
            return "braceclose";

        if (c is '<')
            return "less";

        if (c is '>')
            return "greater";

        if (c is '=')
            return "equals";

        if (c is '/')
            return "slash";

        if (c is '`')
            return "backtick";

        if (c is '~')
            return "tilde";

        if (c is '|')
            return "pipe";

        // Distinguish types of white space

        if (char.IsWhiteSpace(c))
        {
            if (c is ' ')
                return "space";

            if (c is '\t')
                return "tab";

            if (c is '\n')
                return "newline";

            if (c is '\r')
                return "carriagereturn";

            if (c is '\v')
                return "verticaltab";

            if (c is '\f')
                return "formfeed";

            // Non-breaking space

            if (c is '\u00A0')
                return "nobreakspace";

            // Other Unicode white space characters

            return "whitespace_" + ((int)c).ToString("x4");
        }

        if (char.IsAsciiLetter(c))
        {
            return "letter_" + c;
        }

        return null;
    }
}
