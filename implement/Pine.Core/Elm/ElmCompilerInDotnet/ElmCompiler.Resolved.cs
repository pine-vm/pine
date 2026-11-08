using Pine.Core.CodeAnalysis;
using Pine.Core.Encodings;
using Pine.Core.Functional;
using Pine.Core.PineLanguage;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.Core.Elm.ElmCompilerInDotnet;

public partial class ElmCompiler
{
    private static Result<string, (PineValue compiledEnvValue, CompilationPipelineStageResults pipelineStageResults)>
        EmitCompilerProgram(
        CompilerModel.Program program,
        CompilationPipelineStageResults pipeline,
        bool disableGenericApplicationChainConsolidation,
        DeclarationUnits.DeclarationUnitCache storage)
    {
        var compilation =
            CompilerModel.Compilation.Compile(
                program,
                disableGenericApplicationChainConsolidation,
                input =>
                {
                    if (storage.Emission.TryGetValue(input, out var cached))
                        return cached;

                    var result = CompilerModel.Compilation.CompileScc(input);
                    storage.Emission.Set(input, result);
                    return result;
                });

        if (compilation.IsErrOrNull() is { } error)
        {
            if (error is CompilationError.InDeclaration declarationError)
            {
                var artifact = DeclQualifiedName.FromString(declarationError.DeclarationName);

                var pending =
                    new Queue<DeclQualifiedName>(pipeline.RootDeclarationNames.Order());

                var visited = new HashSet<DeclQualifiedName>();
                DeclQualifiedName? target = null;

                while (pending.TryDequeue(out var source))
                {
                    if (!visited.Add(source))
                        continue;

                    if (pipeline.SourceFunctionIds.TryGetValue(source, out var id) &&
                        id.SourceDeclaration.Equals(artifact))
                    {
                        target = source;
                        break;
                    }

                    foreach (var reference in pipeline.SourceReferences.GetValueOrDefault(source) ?? [])
                        pending.Enqueue(reference.Declaration);
                }

                if (target is null)
                    throw new InvalidOperationException("An emission diagnostic has no source binding on a demanded path.");

                var directDependencies =
                    pipeline.SourceReferences.ToImmutableDictionary(
                        entry => entry.Key.FullName,
                        entry =>
                        (IReadOnlySet<string>)entry.Value.Select(reference => reference.Declaration.FullName).ToHashSet(
                            StringComparer.Ordinal));

                var chain =
                    FindDeclarationDependencyChain(
                        pipeline.RootDeclarationNames.Select(name => name.FullName).ToHashSet(StringComparer.Ordinal),
                        target.FullName,
                        directDependencies);

                var located =
                    BuildSourceDependencyChain(
                        [.. chain.Select(item => DeclQualifiedName.FromString(item.DeclarationName))],
                        pipeline.Canonicalized,
                        pipeline.SourceFilePaths,
                        pipeline.SourceReferences);

                return
                    new CompilationError.DeclarationCompilationDiagnostic(
                        target.FullName,
                        [target.FullName],
                        declarationError.InnerError,
                        located).ToString();
            }

            return error.ToString();
        }

        var compiled = compilation.Extract(error => throw new InvalidOperationException(error.ToString()));

        var entries =
            pipeline.RootDeclarationNames.Order()
            .GroupBy(name => string.Join(".", name.Namespaces), StringComparer.Ordinal)
            .Select(
                group => PineValue.List(
                    [
                    StringEncoding.ValueFromString(group.Key),
                    PineValue.List(
                        [
                        .. group.Select(
                            name => PineValue.List(
                                [
                                StringEncoding.ValueFromString(name.DeclName),
                                compiled[pipeline.SourceFunctionIds[name]].CompiledValue
                                ]))
                        ])
                    ]))
            .ToArray();

        return (PineValue.List([.. entries]), pipeline);
    }
}
