using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
using System;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.Core.Elm.ElmCompilerInDotnet;

public partial class ElmCompiler
{
    private static Result<string, (PineValue compiledEnvValue, CompilationPipelineStageResults pipelineStageResults)>
        EmitCompilerProgram(
        CompilerModel.Program program,
        CompilationPipelineStageResults pipeline,
        bool disableGenericApplicationChainConsolidation)
    {
        var compilation = CompilerModel.Compilation.Compile(program, disableGenericApplicationChainConsolidation);

        if (compilation.IsErrOrNull() is { } error)
        {
            if (error is CompilationError.InDeclaration declarationError)
            {
                var target = DeclQualifiedName.FromString(declarationError.DeclarationName);

                var directDependencies =
                    pipeline.SourceReferences.ToImmutableDictionary(
                        entry => entry.Key.FullName,
                        entry =>
                        (System.Collections.Generic.IReadOnlySet<string>)entry.Value.Select(reference => reference.Declaration.FullName).ToHashSet(
                            StringComparer.Ordinal));

                var chain =
                    FindDeclarationDependencyChain(
                        pipeline.RootDeclarationNames.Select(name => name.FullName).ToHashSet(StringComparer.Ordinal),
                        target.FullName,
                        directDependencies);

                var located =
                    BuildSourceDependencyChain(
                        chain.Select(item => DeclQualifiedName.FromString(item.DeclarationName)).ToArray(),
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
                                compiled[new CompilerModel.FunctionId.Source(name)].CompiledValue
                                ]))
                        ])
                    ]))
            .ToArray();

        return (PineValue.List([.. entries]), pipeline);
    }
}
