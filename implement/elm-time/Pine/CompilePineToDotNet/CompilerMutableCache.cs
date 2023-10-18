using System;
using System.Collections.Concurrent;

namespace Pine.CompilePineToDotNet;

using CompileExpressionFunctionBlockResult =
    Result<string, (Microsoft.CodeAnalysis.CSharp.Syntax.BlockSyntax blockSyntax, CompiledExpressionDependencies dependencies)>;

public class CompilerMutableCache
{
    readonly ConcurrentDictionary<PineValue, Result<string, PineVM.Expression>> decodeExpressionFromValueCache = new();

    readonly ConcurrentDictionary<PineValue, ReadOnlyMemory<byte>> valueHashCache = new();

    readonly ConcurrentDictionary<CompileExpressionFunctionParameters, CompileExpressionFunctionBlockResult>
        compileExpressionCache = new();

    public record CompileExpressionFunctionParameters(
        PineVM.Expression Expression,
        FunctionCompilationEnvironment Environment);

    public CompileExpressionFunctionBlockResult
        CompileToCSharpFunctionBlockSyntax(
        PineVM.Expression expression,
        FunctionCompilationEnvironment environment) =>
        CompileToCSharpFunctionBlockSyntax(
            new CompileExpressionFunctionParameters(expression, environment));

    public CompileExpressionFunctionBlockResult
        CompileToCSharpFunctionBlockSyntax(
        CompileExpressionFunctionParameters parameters) =>
        compileExpressionCache.GetOrAdd(
            parameters,
            valueFactory: _ => CompileToCSharp.CompileToCSharpFunctionBlockSyntax(
                parameters.Expression,
                parameters.Environment));

    public Result<string, PineVM.Expression> DecodeExpressionFromValue(PineValue pineValue) =>
        decodeExpressionFromValueCache.GetOrAdd(
            pineValue,
            valueFactory: PineVM.PineVM.DecodeExpressionFromValueDefault);

    public ReadOnlyMemory<byte> ComputeHash(PineValue pineValue) =>
        valueHashCache
        .GetOrAdd(
            pineValue,
            valueFactory: v => PineValueHashTree.ComputeHash(v, other => ComputeHash(other)));
}
