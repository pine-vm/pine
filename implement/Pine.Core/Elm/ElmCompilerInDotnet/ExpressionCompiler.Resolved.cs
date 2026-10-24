using Pine.Core.CodeGen.Pine;
using Pine.Core.Elm.ElmCompilerInDotnet.CompilerModel;
using Pine.Core.Functional;
using Pine.Core.PineLanguage;
using System;
using System.Collections.Generic;
using System.Linq;

using IR = Pine.Core.Elm.ElmCompilerInDotnet.IntermediateRepresentation;

using PineExpression = Pine.Core.PineLanguage.Expression;

namespace Pine.Core.Elm.ElmCompilerInDotnet;

public partial class ExpressionCompiler
{
    internal static Result<CompilationError, PineExpression> CompileResolvedReference(
        Reference reference, ExpressionCompilationContext context) =>
        reference switch
        {
            Reference.Local local =>
            context.TryGetLocalBinding(local.Binding) is { } value
            ?
            value
            :
            context.TryGetParameterIndex(local.Binding) is { } index
            ?
            BuiltinHelpers.BuildPathToParameter(index)
            :
            new CompilationError.UnresolvedReference(local.Binding.ToString(), context.CurrentModuleName),

            Reference.Function function => ResolvedFunctionValue(function.Id, context),
            Reference.Constructor constructor => CompileGlobalReference(constructor.Name, context),
            Reference.Native native => CompileGlobalReference(native.Name, context),

            _ =>
            throw new NotImplementedException(
                $"{nameof(CompileResolvedReference)} does not handle reference variant: {reference.GetType().Name}")
        };

    private static Result<CompilationError, PineExpression> ResolvedFunctionValue(
        FunctionId id,
        ExpressionCompilationContext context)
    {
        var functions = context.ResolvedFunctions;

        if (functions.MemberArities.TryGetValue(id, out var arity))
        {
            var index = functions.Layout.IndexOf(id);

            if (index < 0)
                throw new InvalidOperationException("A same-SCC reference is absent from the recursive member layout.");

            var body =
                ExpressionBuilder.BuildExpressionForPathInExpression([0, index], PineExpression.EnvironmentInstance);

            var environment =
                ExpressionBuilder.BuildExpressionForPathInExpression([0], PineExpression.EnvironmentInstance);

            return
                arity is 0
                ?
                new PineExpression.Eval(body, PineExpression.ListInst([environment]))
                :
                FunctionValueBuilder.EmitCurriedFunctionTemplateWithLeadingArgsFromEncodedBodyExpression(
                    body,
                    arity,
                    [],
                    environment);
        }

        if (!functions.ExternalFunctions.TryGetValue(id, out var external))
            return new CompilationError.UnresolvedReference(id.SourceDeclaration.FullName, context.CurrentModuleName);

        return
            external.ParameterCount is 0
            ?
            new PineExpression.Eval(
                PineExpression.LitralInst(external.EncodedBody),
                PineExpression.ListInst([PineExpression.LitralInst(PineValue.List([.. external.EnvFunctions]))]))
            :
            PineExpression.LitralInst(external.CompiledValue);
    }

    private static Result<CompilationError, PineExpression> CompileArgumentsForResolvedApplication(
        PineExpression function, IReadOnlyList<IR.Expression> arguments, ExpressionCompilationContext context)
    {
        var compiled = new List<PineExpression>();

        foreach (var argument in arguments)
        {
            var result = Compile(argument, context);

            if (result.IsErrOrNull() is { } error)
                return error;

            compiled.Add(result.Extract(error => throw new InvalidOperationException(error.ToString())));
        }

        return CompileGenericFunctionApplication(function, compiled);
    }

    private static Result<CompilationError, PineExpression> CompileResolvedApplication(
        FunctionId id, IReadOnlyList<IR.Expression> arguments, ExpressionCompilationContext context)
    {
        var functions = context.ResolvedFunctions;
        var compiled = new List<PineExpression>();

        foreach (var argument in arguments)
        {
            var result = Compile(argument, context);

            if (result.IsErrOrNull() is { } error)
                return error;

            compiled.Add(result.Extract(error => throw new InvalidOperationException(error.ToString())));
        }

        var sameScc = functions.MemberArities.TryGetValue(id, out var arity);
        CompiledCallable? external = null;

        if (!sameScc)
        {
            if (!functions.ExternalFunctions.TryGetValue(id, out external))
                return new CompilationError.UnresolvedReference(id.SourceDeclaration.FullName, context.CurrentModuleName);

            arity = external.ParameterCount;
        }

        if (arity is 0)
            return ResolvedFunctionValue(id, context).Map(value => CompileGenericFunctionApplication(value, compiled));

        PineExpression body;
        PineExpression environment;

        if (sameScc)
        {
            var index = functions.Layout.IndexOf(id);

            if (index < 0)
                throw new InvalidOperationException("Missing recursive member layout entry.");

            body = ExpressionBuilder.BuildExpressionForPathInExpression([0, index], PineExpression.EnvironmentInstance);
            environment = ExpressionBuilder.BuildExpressionForPathInExpression([0], PineExpression.EnvironmentInstance);
        }
        else
        {
            body = PineExpression.LitralInst(external!.EncodedBody);
            environment = PineExpression.LitralInst(PineValue.List([.. external.EnvFunctions]));
        }

        if (compiled.Count < arity)
        {
            return
                sameScc
                ?
                FunctionValueBuilder.EmitCurriedFunctionTemplateWithLeadingArgsFromEncodedBodyExpression(
                    body,
                    arity,
                    compiled,
                    environment)
                :
                FunctionValueBuilder.EmitCurriedFunctionTemplateWithLeadingArgsFromEncodedBody(
                    external!.EncodedBody,
                    arity,
                    compiled,
                    external.EnvFunctions);
        }

        PineExpression application =
            new PineExpression.Eval(body, PineExpression.ListInst([environment, .. compiled.Take(arity)]));

        if (compiled.Count > arity)
            application = CompileGenericFunctionApplication(application, [.. compiled.Skip(arity)]);

        return application;
    }
}
