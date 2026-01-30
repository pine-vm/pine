using Pine.Core.Elm.ElmSyntax;
using Pine.Core.Elm.ElmSyntax.SyntaxModel;
using Pine.Core.Files;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text;

using AbstractSyntaxTypes = Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;

// Alias to avoid ambiguity with System.Range
using Range = Pine.Core.Elm.ElmSyntax.SyntaxModel.Range;

namespace Pine.Core.Elm.ElmCompilerInDotnet;

/// <summary>
/// Provides functionality to infer type annotations for Elm declarations
/// and add them to the syntax tree.
/// </summary>
public class AddInferredTypeAnnotations
{
    private static readonly Location s_locationZero = new(0, 0);

    private static readonly Range s_rangeZero = new(s_locationZero, s_locationZero);

    /// <summary>
    /// Creates a new syntax node with the specified value at the zero range.
    /// </summary>
    private static Node<T> MakeNode<T>(T value) =>
        new(s_rangeZero, value);

    /// <summary>
    /// Infers and adds type annotations for all declarations in all modules within the given app code tree.
    /// This is a convenience method that wraps <see cref="InferAndAddTypeAnnotationsByModuleName"/>
    /// and includes all declarations (top-level and local) without any filtering.
    /// </summary>
    /// <param name="appCodeTree">The file tree containing Elm source files to process.</param>
    /// <returns>
    /// A result containing either an error message or a function that takes a module name
    /// and returns the annotated file for that module.
    /// </returns>
    public static Result<string, Func<IReadOnlyList<string>, Result<string, File>>> InferAndAddTypeAnnotationsForAllDeclarationsByModuleName(
        FileTree appCodeTree)
    {
        static bool IncludeDeclaration(IReadOnlyList<string> declarationPath)
        {
            return true;
        }

        var appResult = InferAndAddTypeAnnotationsByModuleName(appCodeTree);

        if (appResult.IsErrOrNull() is { } err)
        {
            return err;
        }

        if (appResult.IsOkOrNull() is not { } fileForModuleName)
        {
            throw new NotImplementedException(
                "Unexpected result type from InferAndAddTypeAnnotations: " + appResult.GetType().FullName);
        }

        Result<string, File> AddAnnotations(IReadOnlyList<string> moduleName)
        {
            var fileResult = fileForModuleName(moduleName);

            if (fileResult.IsErrOrNull() is { } err)
            {
                return err;
            }

            if (fileResult.IsOkOrNull() is not { } genericFile)
            {
                throw new NotImplementedException(
                    "Unexpected result type from InferAndAddTypeAnnotations: " + fileResult.GetType().FullName);
            }

            return genericFile(IncludeDeclaration);
        }

        return Result<string, Func<IReadOnlyList<string>, Result<string, File>>>.ok(AddAnnotations);
    }

    /// <summary>
    /// Infers and adds type annotations for declarations in a module.
    /// Returns a function that takes a module name and returns a function to get
    /// the annotated file with a filter for which declarations to include.
    /// </summary>
    public static Result<string, Func<IReadOnlyList<string>, Result<string, Func<Func<IReadOnlyList<string>, bool>, File>>>> InferAndAddTypeAnnotationsByModuleName(
        FileTree appCodeTree)
    {
        // Parse all module files
        var elmModuleFiles =
            appCodeTree.EnumerateFilesTransitive()
            .Where(file => file.path.Last().EndsWith(".elm", StringComparison.OrdinalIgnoreCase))
            .ToImmutableArray();

        // Dictionary to store parsed files by module name
        var parsedFilesByModuleName = new Dictionary<string, File>(StringComparer.Ordinal);
        var abstractFilesList = new List<AbstractSyntaxTypes.File>();

        foreach (var moduleFile in elmModuleFiles)
        {
            var moduleText = Encoding.UTF8.GetString(moduleFile.fileContent.Span);

            var parseResult = ElmSyntaxParser.ParseModuleText(moduleText);

            if (parseResult.IsErrOrNull() is { } err)
            {
                return $"Failed to parse module: {err}";
            }

            if (parseResult.IsOkOrNull() is not { } parsedFile)
            {
                throw new NotImplementedException(
                    "Unexpected result type from ParseModuleText: " + parseResult.GetType().FullName);
            }

            var moduleName = Module.GetModuleName(parsedFile.ModuleDefinition.Value).Value;
            var moduleNameStr = string.Join(".", moduleName);

            parsedFilesByModuleName[moduleNameStr] = parsedFile;

            // Convert to abstract syntax for canonicalization
            var abstractFile = AbstractSyntaxTypes.FromFullSyntaxModel.Convert(parsedFile);
            abstractFilesList.Add(abstractFile);
        }

        // Apply canonicalization to resolve all references to fully qualified forms
        // Use CanonicalizeAllowingErrors to continue even with undefined local variable references
        // This enables IDE functionality (like "add inferred type annotation") even when
        // the file contains some invalid references
        var canonicalizeResult = Canonicalization.CanonicalizeAllowingErrors(abstractFilesList);

        if (canonicalizeResult.IsErrOrNull() is { } canonErr)
        {
            return $"Canonicalization failed: {canonErr}";
        }

        var canonicalizedModules =
            canonicalizeResult.IsOkOrNull() ??
            throw new NotImplementedException(
                "Unexpected result type from Canonicalization: " + canonicalizeResult.GetType().FullName);

        // Build combined function signatures from ALL canonicalized modules
        var allSignatures = ImmutableDictionary<string, TypeInference.InferredType>.Empty;
        var canonicalizedFilesByModuleName = new Dictionary<string, AbstractSyntaxTypes.File>(StringComparer.Ordinal);

        foreach (var (moduleName, (canonicalizedFile, _errors)) in canonicalizedModules)
        {
            // We ignore canonicalization errors (like undefined local variables) since they
            // don't prevent us from inferring types for cross-module references
            var moduleNameStr = string.Join(".", moduleName);
            canonicalizedFilesByModuleName[moduleNameStr] = canonicalizedFile;

            var moduleSignatures = TypeInference.BuildFunctionSignaturesMap(canonicalizedFile, moduleNameStr);
            allSignatures = allSignatures.SetItems(moduleSignatures);
        }

        Result<string, Func<Func<IReadOnlyList<string>, bool>, File>> TypeAnnotationsForModuleName(
            IReadOnlyList<string> moduleName)
        {
            var moduleNameStr = string.Join(".", moduleName);

            if (!parsedFilesByModuleName.TryGetValue(moduleNameStr, out var parsedFile))
            {
                return Result<string, Func<Func<IReadOnlyList<string>, bool>, File>>.err(
                    $"Module not found: {moduleNameStr}");
            }

            if (!canonicalizedFilesByModuleName.TryGetValue(moduleNameStr, out var canonicalizedFile))
            {
                return Result<string, Func<Func<IReadOnlyList<string>, bool>, File>>.err(
                    $"Canonicalized module not found: {moduleNameStr}");
            }

            File GetAnnotatedFile(Func<IReadOnlyList<string>, bool> includeDeclaration)
            {
                // Infer type annotations using canonicalized file and combined signatures from all modules
                var typeAnnotations = InferTypeAnnotationsForFile(canonicalizedFile, moduleNameStr, includeDeclaration, allSignatures);

                // Apply type annotations to the original (non-canonicalized) file for output
                return SetTypeAnnotation.SetTypeAnnotations(
                    parsedFile,
                    typeAnnotations,
                    declarationWithoutEntry: null,
                    entryWithoutDeclaration: null);
            }

            return Result<string, Func<Func<IReadOnlyList<string>, bool>, File>>.ok(GetAnnotatedFile);
        }

        return Result<string, Func<IReadOnlyList<string>, Result<string, Func<Func<IReadOnlyList<string>, bool>, File>>>>.ok(
            TypeAnnotationsForModuleName);
    }

    /// <summary>
    /// Infers type annotations for all declarations in a file, including local declarations in let-blocks.
    /// </summary>
    private static ImmutableDictionary<IReadOnlyList<string>, TypeAnnotation> InferTypeAnnotationsForFile(
        AbstractSyntaxTypes.File file,
        string moduleName,
        Func<IReadOnlyList<string>, bool> includeDeclaration,
        ImmutableDictionary<string, TypeInference.InferredType> allModuleSignatures)
    {
        var annotations = ImmutableDictionary<IReadOnlyList<string>, TypeAnnotation>.Empty
            .WithComparers(EnumerableExtensions.EqualityComparer<IReadOnlyList<string>>());

        // Use the combined signatures from all modules
        var functionSignatures = allModuleSignatures;

        foreach (var declaration in file.Declarations)
        {
            if (declaration.Value is AbstractSyntaxTypes.Declaration.FunctionDeclaration funcDecl)
            {
                var funcName = funcDecl.Function.Declaration.Value.Name.Value;
                var declarationPath = new[] { funcName };

                if (!includeDeclaration(declarationPath))
                {
                    continue;
                }

                // Expression and arguments are already in abstract syntax (canonicalized)
                var abstractExpression = funcDecl.Function.Declaration.Value.Expression;
                var abstractArguments = funcDecl.Function.Declaration.Value.Arguments;

                // Skip if already has a type annotation
                if (funcDecl.Function.Signature is not null)
                {
                    // Still need to recursively traverse for let-block declarations
                    annotations = CollectTypeAnnotationsInExpression(
                        abstractExpression.Value,
                        declarationPath,
                        abstractArguments,
                        moduleName,
                        functionSignatures,
                        includeDeclaration,
                        annotations);
                    continue;
                }

                // Use TypeInference to infer the function type
                var (returnType, parameterTypes) = TypeInference.InferFunctionDeclarationType(
                    abstractExpression.Value,
                    abstractArguments,
                    moduleName,
                    functionSignatures);

                if (returnType is not TypeInference.InferredType.UnknownType)
                {
                    // Build the full function type using TypeInference
                    var fullType = TypeInference.BuildFunctionType(
                        abstractArguments,
                        parameterTypes,
                        returnType);

                    // Convert the inferred type to a type annotation
                    var typeAnnotation = InferredTypeToTypeAnnotation(fullType);
                    if (typeAnnotation is not null)
                    {
                        annotations = annotations.Add(declarationPath, typeAnnotation);
                    }
                }

                // Recursively traverse for let-block declarations
                annotations = CollectTypeAnnotationsInExpression(
                    abstractExpression.Value,
                    declarationPath,
                    abstractArguments,
                    moduleName,
                    functionSignatures,
                    includeDeclaration,
                    annotations);
            }
        }

        return annotations;
    }

    /// <summary>
    /// Recursively collects type annotations for declarations within an expression (e.g., let-blocks).
    /// Uses TypeInference for actual type inference and only converts InferredType to TypeAnnotation.
    /// </summary>
    private static ImmutableDictionary<IReadOnlyList<string>, TypeAnnotation> CollectTypeAnnotationsInExpression(
        AbstractSyntaxTypes.Expression expression,
        IReadOnlyList<string> currentPath,
        IReadOnlyList<Node<AbstractSyntaxTypes.Pattern>> parentArguments,
        string moduleName,
        IReadOnlyDictionary<string, TypeInference.InferredType> functionSignatures,
        Func<IReadOnlyList<string>, bool> includeDeclaration,
        ImmutableDictionary<IReadOnlyList<string>, TypeAnnotation> annotations)
    {
        switch (expression)
        {
            case AbstractSyntaxTypes.Expression.LetExpression letExpr:
                // Process each declaration in the let-block
                foreach (var letDecl in letExpr.Value.Declarations)
                {
                    if (letDecl.Value is AbstractSyntaxTypes.Expression.LetDeclaration.LetFunction letFunc)
                    {
                        var localFuncName = letFunc.Function.Declaration.Value.Name.Value;
                        var localPath = currentPath.Append(localFuncName).ToArray();

                        if (!includeDeclaration(localPath))
                        {
                            continue;
                        }

                        // Skip if already has a type annotation
                        if (letFunc.Function.Signature is not null)
                        {
                            // Still recurse into the expression
                            annotations = CollectTypeAnnotationsInExpression(
                                letFunc.Function.Declaration.Value.Expression.Value,
                                localPath,
                                letFunc.Function.Declaration.Value.Arguments,
                                moduleName,
                                functionSignatures,
                                includeDeclaration,
                                annotations);
                            continue;
                        }

                        // Use TypeInference to infer the type of the local declaration
                        var (returnType, parameterTypes) = TypeInference.InferFunctionDeclarationType(
                            letFunc.Function.Declaration.Value.Expression.Value,
                            letFunc.Function.Declaration.Value.Arguments,
                            moduleName,
                            functionSignatures);

                        if (returnType is not TypeInference.InferredType.UnknownType)
                        {
                            // Build the full function type using TypeInference
                            var fullType = TypeInference.BuildFunctionType(
                                letFunc.Function.Declaration.Value.Arguments,
                                parameterTypes,
                                returnType);

                            // Convert the inferred type to a type annotation
                            var typeAnnotation = InferredTypeToTypeAnnotation(fullType);
                            if (typeAnnotation is not null)
                            {
                                annotations = annotations.Add(localPath, typeAnnotation);
                            }
                        }

                        // Recurse into the local function's expression
                        annotations = CollectTypeAnnotationsInExpression(
                            letFunc.Function.Declaration.Value.Expression.Value,
                            localPath,
                            letFunc.Function.Declaration.Value.Arguments,
                            moduleName,
                            functionSignatures,
                            includeDeclaration,
                            annotations);
                    }
                }

                // Recurse into the let-block's body expression
                annotations = CollectTypeAnnotationsInExpression(
                    letExpr.Value.Expression.Value,
                    currentPath,
                    parentArguments,
                    moduleName,
                    functionSignatures,
                    includeDeclaration,
                    annotations);
                break;

            case AbstractSyntaxTypes.Expression.IfBlock ifBlock:
                annotations = CollectTypeAnnotationsInExpression(ifBlock.Condition.Value, currentPath, parentArguments, moduleName, functionSignatures, includeDeclaration, annotations);
                annotations = CollectTypeAnnotationsInExpression(ifBlock.ThenBlock.Value, currentPath, parentArguments, moduleName, functionSignatures, includeDeclaration, annotations);
                annotations = CollectTypeAnnotationsInExpression(ifBlock.ElseBlock.Value, currentPath, parentArguments, moduleName, functionSignatures, includeDeclaration, annotations);
                break;

            case AbstractSyntaxTypes.Expression.CaseExpression caseExpr:
                annotations = CollectTypeAnnotationsInExpression(caseExpr.CaseBlock.Expression.Value, currentPath, parentArguments, moduleName, functionSignatures, includeDeclaration, annotations);
                foreach (var caseCase in caseExpr.CaseBlock.Cases)
                {
                    annotations = CollectTypeAnnotationsInExpression(caseCase.Expression.Value, currentPath, parentArguments, moduleName, functionSignatures, includeDeclaration, annotations);
                }
                break;

            case AbstractSyntaxTypes.Expression.LambdaExpression lambda:
                annotations = CollectTypeAnnotationsInExpression(lambda.Lambda.Expression.Value, currentPath, lambda.Lambda.Arguments, moduleName, functionSignatures, includeDeclaration, annotations);
                break;

            case AbstractSyntaxTypes.Expression.Application app:
                foreach (var arg in app.Arguments)
                {
                    annotations = CollectTypeAnnotationsInExpression(arg.Value, currentPath, parentArguments, moduleName, functionSignatures, includeDeclaration, annotations);
                }
                break;

            case AbstractSyntaxTypes.Expression.OperatorApplication opApp:
                annotations = CollectTypeAnnotationsInExpression(opApp.Left.Value, currentPath, parentArguments, moduleName, functionSignatures, includeDeclaration, annotations);
                annotations = CollectTypeAnnotationsInExpression(opApp.Right.Value, currentPath, parentArguments, moduleName, functionSignatures, includeDeclaration, annotations);
                break;

            case AbstractSyntaxTypes.Expression.ParenthesizedExpression paren:
                annotations = CollectTypeAnnotationsInExpression(paren.Expression.Value, currentPath, parentArguments, moduleName, functionSignatures, includeDeclaration, annotations);
                break;

            case AbstractSyntaxTypes.Expression.TupledExpression tuple:
                foreach (var elem in tuple.Elements)
                {
                    annotations = CollectTypeAnnotationsInExpression(elem.Value, currentPath, parentArguments, moduleName, functionSignatures, includeDeclaration, annotations);
                }
                break;

            case AbstractSyntaxTypes.Expression.ListExpr list:
                foreach (var elem in list.Elements)
                {
                    annotations = CollectTypeAnnotationsInExpression(elem.Value, currentPath, parentArguments, moduleName, functionSignatures, includeDeclaration, annotations);
                }
                break;

            case AbstractSyntaxTypes.Expression.RecordExpr record:
                foreach (var field in record.Fields)
                {
                    annotations = CollectTypeAnnotationsInExpression(field.Value.valueExpr.Value, currentPath, parentArguments, moduleName, functionSignatures, includeDeclaration, annotations);
                }
                break;
        }

        return annotations;
    }

    /// <summary>
    /// Converts an InferredType to a TypeAnnotation.
    /// </summary>
    private static TypeAnnotation? InferredTypeToTypeAnnotation(TypeInference.InferredType inferredType)
    {
        return inferredType switch
        {
            TypeInference.InferredType.IntType =>
                new TypeAnnotation.Typed(
                    MakeNode<(IReadOnlyList<string> ModuleName, string Name)>(([], "Int")),
                    []),

            TypeInference.InferredType.FloatType =>
                new TypeAnnotation.Typed(
                    MakeNode<(IReadOnlyList<string> ModuleName, string Name)>(([], "Float")),
                    []),

            TypeInference.InferredType.StringType =>
                new TypeAnnotation.Typed(
                    MakeNode<(IReadOnlyList<string> ModuleName, string Name)>(([], "String")),
                    []),

            TypeInference.InferredType.CharType =>
                new TypeAnnotation.Typed(
                    MakeNode<(IReadOnlyList<string> ModuleName, string Name)>(([], "Char")),
                    []),

            TypeInference.InferredType.BoolType =>
                new TypeAnnotation.Typed(
                    MakeNode<(IReadOnlyList<string> ModuleName, string Name)>(([], "Bool")),
                    []),

            TypeInference.InferredType.NumberType =>
                // "number" is the polymorphic numeric type in Elm
                new TypeAnnotation.GenericType("number"),

            TypeInference.InferredType.TypeVariable typeVar =>
                // Type variable like 'a', 'b', etc.
                new TypeAnnotation.GenericType(typeVar.Name),

            TypeInference.InferredType.TupleType tupleType =>
                CreateTupleTypeAnnotation(tupleType.ElementTypes),

            TypeInference.InferredType.RecordType recordType =>
                CreateRecordTypeAnnotation(recordType.Fields),

            TypeInference.InferredType.FunctionType functionType =>
                CreateFunctionTypeAnnotation(functionType.ArgumentType, functionType.ReturnType),

            TypeInference.InferredType.ListType listType =>
                CreateListTypeAnnotation(listType.ElementType),

            TypeInference.InferredType.ChoiceType customType =>
                CreateCustomTypeAnnotation(customType.ModuleName, customType.TypeName, customType.TypeArguments),

            _ => null
        };
    }

    /// <summary>
    /// Creates a tuple type annotation from element types.
    /// </summary>
    private static TypeAnnotation? CreateTupleTypeAnnotation(IReadOnlyList<TypeInference.InferredType> elementTypes)
    {
        var typeAnnotations = new List<Node<TypeAnnotation>>();

        foreach (var elemType in elementTypes)
        {
            if (InferredTypeToTypeAnnotation(elemType) is not { } typeAnnotation)
            {
                return null;
            }

            typeAnnotations.Add(MakeNode(typeAnnotation));
        }

        if (typeAnnotations.Count is 0)
        {
            return new TypeAnnotation.Unit();
        }

        var first = typeAnnotations[0];

        var rest =
            typeAnnotations.Skip(1)
            .Select(ta => (s_locationZero, ta))
            .ToList();

        return new TypeAnnotation.Tupled(
            new SeparatedSyntaxList<Node<TypeAnnotation>>.NonEmpty(first, rest));
    }

    /// <summary>
    /// Creates a record type annotation from fields (name and type pairs).
    /// </summary>
    private static TypeAnnotation? CreateRecordTypeAnnotation(
        IReadOnlyList<(string FieldName, TypeInference.InferredType FieldType)> fields)
    {
        if (fields.Count is 0)
        {
            // Empty record
            return new TypeAnnotation.Record(
                new RecordDefinition(
                    new SeparatedSyntaxList<Node<RecordField>>.Empty()));
        }

        var recordFields = new List<Node<RecordField>>();

        foreach (var (fieldName, fieldType) in fields)
        {
            var fieldTypeAnnotation = InferredTypeToTypeAnnotation(fieldType);
            if (fieldTypeAnnotation is null)
            {
                return null;
            }

            var recordField = new RecordField(
                MakeNode(fieldName),
                s_locationZero,
                MakeNode(fieldTypeAnnotation));

            recordFields.Add(MakeNode(recordField));
        }

        var first = recordFields[0];

        var rest =
            recordFields.Skip(1)
            .Select(rf => (s_locationZero, rf))
            .ToList();

        return new TypeAnnotation.Record(
            new RecordDefinition(
                new SeparatedSyntaxList<Node<RecordField>>.NonEmpty(first, rest)));
    }

    /// <summary>
    /// Creates a function type annotation from argument and return types.
    /// </summary>
    private static TypeAnnotation? CreateFunctionTypeAnnotation(
        TypeInference.InferredType argumentType,
        TypeInference.InferredType returnType)
    {
        var argAnnotation = InferredTypeToTypeAnnotation(argumentType);
        var retAnnotation = InferredTypeToTypeAnnotation(returnType);

        if (argAnnotation is null || retAnnotation is null)
        {
            return null;
        }

        return new TypeAnnotation.FunctionTypeAnnotation(
            MakeNode(argAnnotation),
            s_locationZero,
            MakeNode(retAnnotation));
    }

    /// <summary>
    /// Creates a List type annotation from an element type.
    /// </summary>
    private static TypeAnnotation? CreateListTypeAnnotation(TypeInference.InferredType elementType)
    {
        var elementAnnotation = InferredTypeToTypeAnnotation(elementType);
        if (elementAnnotation is null)
        {
            return null;
        }

        return new TypeAnnotation.Typed(
            MakeNode<(IReadOnlyList<string> ModuleName, string Name)>(([], "List")),
            [MakeNode(elementAnnotation)]);
    }

    /// <summary>
    /// Creates a custom type annotation from module name, type name, and type arguments.
    /// </summary>
    private static TypeAnnotation? CreateCustomTypeAnnotation(
        IReadOnlyList<string> moduleName,
        string typeName,
        IReadOnlyList<TypeInference.InferredType> typeArguments)
    {
        var typeArgNodes = new List<Node<TypeAnnotation>>();
        foreach (var arg in typeArguments)
        {
            var argAnnotation = InferredTypeToTypeAnnotation(arg);
            if (argAnnotation is null)
            {
                return null;
            }
            typeArgNodes.Add(MakeNode(argAnnotation));
        }

        return new TypeAnnotation.Typed(
            MakeNode<(IReadOnlyList<string> ModuleName, string Name)>((moduleName, typeName)),
            typeArgNodes);
    }
}
