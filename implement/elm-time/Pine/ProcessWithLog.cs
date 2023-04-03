using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Pine;

public abstract record ProcessWithLog<LogEntryT, ResultT>
{
    public record LogEntry(
        LogEntryT logEntry,
        Func<ProcessWithLog<LogEntryT, ResultT>> nextStep)
        : ProcessWithLog<LogEntryT, ResultT>;

    public record Result(ResultT Value)
        : ProcessWithLog<LogEntryT, ResultT>;

    public ProcessWithLog<LogEntryT, NewResultT> Continue<NewResultT>(
        Func<ResultT, ProcessWithLog<LogEntryT, NewResultT>> continuation) =>
        this switch
        {
            LogEntry logEntry =>
            new ProcessWithLog<LogEntryT, NewResultT>.LogEntry(logEntry.logEntry, () => logEntry.nextStep().Continue(continuation)),

            Result result => continuation(result.Value),

            _ => throw new NotImplementedException()
        };

    public ProcessWithLog<LogEntryT, NewResultT> MapResult<NewResultT>(
        Func<ResultT, NewResultT> resultMap) =>
        Continue(result => new ProcessWithLog<LogEntryT, NewResultT>.Result(resultMap(result)));

    public ProcessWithLog<LogEntryT, ResultT> WithLogEntryAdded(
        LogEntryT logEntry) =>
        Continue(result => new LogEntry(logEntry, () => new Result(result)));

    public ProcessWithLog<LogEntryT, ResultT> WithLogEntryFromResultAdded(
        Func<ResultT, LogEntryT> logEntry) =>
        Continue(result => new LogEntry(
            logEntry(result),
            () => new Result(result)));
}

public static class ProcessWithLogExtension
{
    public static ProcessWithLog<LogEntryT, Result<ErrT, NewOkT>> ResultAndThenMap<LogEntryT, ErrT, OkT, NewOkT>(
        this ProcessWithLog<LogEntryT, Result<ErrT, OkT>> orig,
        Func<OkT, Result<ErrT, NewOkT>> andThen) =>
        orig.MapResult(previousResult => previousResult.AndThen(andThen));

    public static ProcessWithLog<LogEntryT, Result<ErrT, NewOkT>> ResultMap<LogEntryT, ErrT, OkT, NewOkT>(
        this ProcessWithLog<LogEntryT, Result<ErrT, OkT>> orig,
        Func<OkT, NewOkT> map) =>
        orig.ResultAndThenMap(previousResult => Result<ErrT, NewOkT>.ok(map(previousResult)));

    public static ProcessWithLog<LogEntryT, Result<ErrT, NewOkT>> ResultMapContinue<LogEntryT, ErrT, OkT, NewOkT>(
        this ProcessWithLog<LogEntryT, Result<ErrT, OkT>> orig,
        Func<OkT, ProcessWithLog<LogEntryT, NewOkT>> map) =>
        orig.ResultAndThenContinue(previousOk => map(previousOk).MapResult(newOk => Result<ErrT, NewOkT>.ok(newOk)));

    public static ProcessWithLog<LogEntryT, Result<ErrT, NewOkT>> ResultAndThenContinue<LogEntryT, ErrT, OkT, NewOkT>(
        this ProcessWithLog<LogEntryT, Result<ErrT, OkT>> orig,
        Func<OkT, ProcessWithLog<LogEntryT, Result<ErrT, NewOkT>>> andThen) =>
        orig.Continue(previousResult =>
        previousResult
        .Unpack(
            fromErr: error => new ProcessWithLog<LogEntryT, Result<ErrT, NewOkT>>.Result(Result<ErrT, NewOkT>.err(error)),
            fromOk: ok => andThen(ok)));

    public static ProcessWithLog<LogEntryT, Result<ErrT, OkT>> ResultAddLogEntryIfOk<LogEntryT, ErrT, OkT>(
        this ProcessWithLog<LogEntryT, Result<ErrT, OkT>> orig,
        Func<OkT, LogEntryT> addLogEntry) =>
        ResultAndThenContinue(
            orig,
            ok => new ProcessWithLog<LogEntryT, Result<ErrT, OkT>>.LogEntry(
                addLogEntry(ok),
                () => new ProcessWithLog<LogEntryT, Result<ErrT, OkT>>.Result(Result<ErrT, OkT>.ok(ok))));

    public static ProcessWithLog<LogEntryT, Result<ErrT, OkT>> ResultAddLogEntriesIfOk<LogEntryT, ErrT, OkT>(
        this ProcessWithLog<LogEntryT, Result<ErrT, OkT>> orig,
        Func<OkT, IEnumerable<LogEntryT>> addLogEntries) =>
        ResultAndThenContinue(
            orig,
            ok => addLogEntries(ok)
            .Reverse()
            .Aggregate(
                seed: (ProcessWithLog<LogEntryT, Result<ErrT, OkT>>)new ProcessWithLog<LogEntryT, Result<ErrT, OkT>>.Result(Result<ErrT, OkT>.ok(ok)),
                func: (prev, logEntry) => new ProcessWithLog<LogEntryT, Result<ErrT, OkT>>.LogEntry(logEntry, () => prev)));

    public static ResultT LogToActions<LogEntryT, ResultT>(
        this ProcessWithLog<LogEntryT, ResultT> firstStep,
        Action<LogEntryT> logAction)
    {
        ResultT continueWithLogEntry(ProcessWithLog<LogEntryT, ResultT>.LogEntry logEntry)
        {
            logAction(logEntry.logEntry);

            return LogToActions(logEntry.nextStep(), logAction);
        }

        return firstStep switch
        {
            ProcessWithLog<LogEntryT, ResultT>.LogEntry logEntry => continueWithLogEntry(logEntry),

            ProcessWithLog<LogEntryT, ResultT>.Result result => result.Value,

            _ => throw new NotImplementedException()
        };
    }

    public static (IImmutableList<LogEntryT> log, ResultT result) LogToList<LogEntryT, ResultT>(
         this ProcessWithLog<LogEntryT, ResultT> firstStep)
    {
        (IImmutableList<LogEntryT> log, ResultT result) continueWithLogEntry(ProcessWithLog<LogEntryT, ResultT>.LogEntry logEntry)
        {
            var (log, result) = LogToList(logEntry.nextStep());

            return (log.Insert(0, logEntry.logEntry), result);
        }

        return firstStep switch
        {
            ProcessWithLog<LogEntryT, ResultT>.LogEntry logEntry => continueWithLogEntry(logEntry),

            ProcessWithLog<LogEntryT, ResultT>.Result result => (ImmutableList<LogEntryT>.Empty, result.Value),

            _ => throw new NotImplementedException()
        };
    }
}
