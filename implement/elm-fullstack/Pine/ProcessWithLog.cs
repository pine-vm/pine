using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Pine;

public record ProcessWithLog<LogEntryT, ResultT>(
     (LogEntryT logEntry, Func<ProcessWithLog<LogEntryT, ResultT>> nextStep) LogEntry = default,
     ResultT Result = default)
{
    public ProcessWithLog<LogEntryT, NewResultT> Continue<NewResultT>(
        Func<ResultT, ProcessWithLog<LogEntryT, NewResultT>> continuation) =>
        Result == null ?
        new ProcessWithLog<LogEntryT, NewResultT>(LogEntry: (LogEntry.logEntry, () => LogEntry.nextStep().Continue(continuation)))
        :
        continuation(Result);

    public ProcessWithLog<LogEntryT, NewResultT> MapResult<NewResultT>(
        Func<ResultT, NewResultT> resultMap) =>
        Continue(result => new ProcessWithLog<LogEntryT, NewResultT>(Result: resultMap(result)));

    public ProcessWithLog<LogEntryT, ResultT> WithLogEntryAdded(
        LogEntryT logEntry) =>
        Continue(result => new ProcessWithLog<LogEntryT, ResultT>(LogEntry:
            (logEntry, () => new ProcessWithLog<LogEntryT, ResultT>(Result: result))));

    public ProcessWithLog<LogEntryT, ResultT> WithLogEntryFromResultAdded(
        Func<ResultT, LogEntryT> logEntry) =>
        Continue(result => new ProcessWithLog<LogEntryT, ResultT>(LogEntry:
            (logEntry(result), () => new ProcessWithLog<LogEntryT, ResultT>(Result: result))));
}

static public class ProcessWithLogExtension
{
    static public ProcessWithLog<LogEntryT, Result<ErrT, NewOkT>> ResultAndThenMap<LogEntryT, ErrT, OkT, NewOkT>(
        this ProcessWithLog<LogEntryT, Result<ErrT, OkT>> orig,
        Func<OkT, Result<ErrT, NewOkT>> andThen) =>
        orig.MapResult(previousResult =>
        previousResult.Ok == null ?
        new Result<ErrT, NewOkT>(Err: previousResult.Err) :
        andThen(previousResult.Ok));

    static public ProcessWithLog<LogEntryT, Result<ErrT, NewOkT>> ResultMap<LogEntryT, ErrT, OkT, NewOkT>(
        this ProcessWithLog<LogEntryT, Result<ErrT, OkT>> orig,
        Func<OkT, NewOkT> map) =>
        orig.ResultAndThenMap(previousResult => new Result<ErrT, NewOkT>(Ok: map(previousResult)));

    static public ProcessWithLog<LogEntryT, Result<ErrT, NewOkT>> ResultMapContinue<LogEntryT, ErrT, OkT, NewOkT>(
        this ProcessWithLog<LogEntryT, Result<ErrT, OkT>> orig,
        Func<OkT, ProcessWithLog<LogEntryT, NewOkT>> map) =>
        orig.ResultAndThenContinue(previousOk => map(previousOk).MapResult(newOk => new Result<ErrT, NewOkT>(Ok: newOk)));

    static public ProcessWithLog<LogEntryT, Result<ErrT, NewOkT>> ResultAndThenContinue<LogEntryT, ErrT, OkT, NewOkT>(
        this ProcessWithLog<LogEntryT, Result<ErrT, OkT>> orig,
        Func<OkT, ProcessWithLog<LogEntryT, Result<ErrT, NewOkT>>> andThen) =>
        orig.Continue(previousResult =>
        previousResult.Ok == null ?
        new ProcessWithLog<LogEntryT, Result<ErrT, NewOkT>>(Result: new Result<ErrT, NewOkT>(Err: previousResult.Err)) :
        andThen(previousResult.Ok));

    static public ProcessWithLog<LogEntryT, Result<ErrT, OkT>> ResultAddLogEntryIfOk<LogEntryT, ErrT, OkT>(
        this ProcessWithLog<LogEntryT, Result<ErrT, OkT>> orig,
        Func<OkT, LogEntryT> addLogEntry) =>
        ResultAndThenContinue(
            orig,
            ok => new ProcessWithLog<LogEntryT, Result<ErrT, OkT>>(
                LogEntry: (addLogEntry(ok), () => new ProcessWithLog<LogEntryT, Result<ErrT, OkT>>(Result: new Result<ErrT, OkT>(Ok: ok)))));

    static public ProcessWithLog<LogEntryT, Result<ErrT, OkT>> ResultAddLogEntriesIfOk<LogEntryT, ErrT, OkT>(
        this ProcessWithLog<LogEntryT, Result<ErrT, OkT>> orig,
        Func<OkT, IEnumerable<LogEntryT>> addLogEntries) =>
        ResultAndThenContinue(
            orig,
            ok => addLogEntries(ok)
            .Reverse()
            .Aggregate(
                seed: new ProcessWithLog<LogEntryT, Result<ErrT, OkT>>(Result: new Result<ErrT, OkT>(Ok: ok)),
                (prev, logEntry) => new ProcessWithLog<LogEntryT, Result<ErrT, OkT>>(LogEntry: (logEntry, () => prev))));

    static public ResultT LogToActions<LogEntryT, ResultT>(
        this ProcessWithLog<LogEntryT, ResultT> firstStep,
        Action<LogEntryT> logAction)
    {
        if (firstStep.LogEntry.nextStep != null)
        {
            logAction(firstStep.LogEntry.logEntry);

            return LogToActions(firstStep.LogEntry.nextStep(), logAction);
        }

        return firstStep.Result;
    }

    static public (IImmutableList<LogEntryT> log, ResultT result) LogToList<LogEntryT, ResultT>(
         this ProcessWithLog<LogEntryT, ResultT> firstStep)
    {
        if (firstStep.LogEntry.nextStep != null)
        {
            var (log, result) = LogToList(firstStep.LogEntry.nextStep());

            return (log.Insert(0, firstStep.LogEntry.logEntry), result);
        }

        return (ImmutableList<LogEntryT>.Empty, firstStep.Result);
    }
}
