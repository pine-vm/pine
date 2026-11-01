using Pine.Core.Elm.Syntax.Interpreter;
using Pine.Core.Files;
using Pine.Core.Functional;
using System;

namespace ElmTime.ElmInteractive;

/// <summary>A stateful interactive host over immutable syntax-interpreter snapshots.</summary>
public sealed class InteractiveSessionSyntax : IInteractiveSession
{
    private static readonly Lazy<Result<string, ElmInterpreterEnvironment>> s_library =
        new(() => ElmInterpreterAppPreparation.Prepare(null));

    private readonly System.Threading.Lock _submissionLock = new();

    private readonly ElmSyntaxInterpreter.EvaluationConfig _evaluationConfig;

    private Result<string, ElmInterpreterEnvironment> _environment;

    private bool _disposed;

    public InteractiveSessionSyntax(
        FileTree? appCodeTree,
        ElmSyntaxInterpreter.EvaluationConfig? evaluationConfig = null)
    {
        _environment = appCodeTree is null ? s_library.Value : ElmInterpreterAppPreparation.Prepare(appCodeTree);
        _evaluationConfig = evaluationConfig ?? ElmSyntaxInterpreter.EvaluationConfig.Default;
    }

    public InteractiveSessionSyntax(
        ElmInterpreterEnvironment environment,
        ElmSyntaxInterpreter.EvaluationConfig? evaluationConfig = null)
    {
        _environment = environment;
        _evaluationConfig = evaluationConfig ?? ElmSyntaxInterpreter.EvaluationConfig.Default;
    }

    public Result<string, IInteractiveSession.SubmissionResponse> Submit(string submission)
    {
        lock (_submissionLock)
        {
            ObjectDisposedException.ThrowIf(_disposed, this);

            if (_environment.IsErrOrNull() is { } preparationError)
                return "Failed preparing interactive environment: " + preparationError;

            var environment = _environment.Extract(error => throw new InvalidOperationException(error));
            var preparation = environment.PrepareSubmission(submission);

            if (preparation.IsErrOrNull() is { } submissionError)
                return "Failed preparing submission: " + submissionError;

            var prepared = preparation.Extract(error => throw new InvalidOperationException(error));
            var nextEnvironment = environment;
            Result<ElmInterpretationError, Pine.Core.Internal.PineValueInProcess> evaluated;

            switch (prepared)
            {
                case ElmInterpreterSubmission.Expression expression:
                    evaluated = expression.Prepared.Evaluate(_evaluationConfig);
                    break;

                case ElmInterpreterSubmission.Declaration declaration:
                    nextEnvironment = declaration.Environment;

                    if (nextEnvironment.Program.Declarations[declaration.Name] is not
                        ElmSyntaxInterpreter.PreparedDeclaration.FunctionDeclaration)
                    {
                        _environment = nextEnvironment;

                        return
                            new IInteractiveSession.SubmissionResponse(
                                new ElmInteractive.EvaluatedStruct("Declared " + declaration.Name.DeclName));
                    }

                    evaluated = nextEnvironment.Invoke(declaration.Name, [], _evaluationConfig);
                    break;

                default:
                    throw new NotImplementedException(
                        $"{nameof(Submit)} does not handle submission variant: {prepared.GetType().Name}");
            }

            if (evaluated.IsErrOrNull() is { } evaluationError)
                return evaluationError.ToString();

            var value = evaluated.Extract(error => throw new InvalidOperationException(error.ToString()));
            var rendered = ElmSyntaxInterpreter.RenderAsElmExpression(value).expressionString;
            var response = new IInteractiveSession.SubmissionResponse(new ElmInteractive.EvaluatedStruct(rendered));

            _environment = nextEnvironment;
            return response;
        }
    }

    public void Dispose()
    {
        lock (_submissionLock)
        {
            _disposed = true;
        }
    }
}
