using System.Collections.Generic;

namespace Pine.Core.Elm.ElmSyntax;

/// <summary>
/// Aggregated performance counters collected during evaluation of an Elm expression by
/// <see cref="ElmSyntaxInterpreter"/>. The granularity matches the high-level shape of the
/// interpreter's CEK-style trampoline rather than the lower-level intermediate VM:
/// a single trampoline iteration corresponds to either evaluating one sub-expression or
/// consuming one continuation, function applications are split into name-resolved calls
/// and applications of evaluated function values (closures), and built-in invocations
/// (<c>Pine_builtin</c> / <c>Pine_kernel</c>) are counted separately so callers can
/// distinguish primitive work from interpreted work.
/// </summary>
/// <param name="InstructionLoopCount">
/// Total number of iterations of the trampoline loop. Each iteration either evaluates one
/// sub-expression in <em>Eval</em> mode or consumes one continuation in <em>Return</em> mode.
/// </param>
/// <param name="DirectFunctionApplicationCount">
/// Number of direct (name-based) function applications: every dispatch through a syntactic
/// <c>FunctionOrValue</c> reference, i.e. <see cref="ElmSyntaxInterpreter.Application"/> entries
/// for top-level user-defined functions, constructors, or built-ins identified by name.
/// </param>
/// <param name="FunctionValueApplicationCount">
/// Number of applications of an already-evaluated function value. This counts every time the
/// interpreter applies arguments to an <see cref="ElmValue.ElmFunction"/> closure obtained as
/// the result of evaluating a sub-expression — typically a higher-order argument, a let-bound
/// partial application, or a lambda.
/// </param>
/// <param name="PineBuiltinInvocationCount">
/// Number of times the <see cref="ElmSyntaxInterpreter.PineBuiltinResolver(ElmSyntaxInterpreter.Application)"/>
/// resolved an application — i.e. forwarded an Elm call to <see cref="KernelFunction"/>
/// because its module name is one of the recognised Pine pseudo-module names
/// (<c>Pine_builtin</c> or <c>Pine_kernel</c>).
/// </param>
public readonly record struct ElmSyntaxInterpreterPerformanceCounters(
    long InstructionLoopCount,
    long DirectFunctionApplicationCount,
    long FunctionValueApplicationCount,
    long PineBuiltinInvocationCount)
{
    /// <summary>
    /// Returns the element-wise sum of two <see cref="ElmSyntaxInterpreterPerformanceCounters"/> instances.
    /// </summary>
    public static ElmSyntaxInterpreterPerformanceCounters Add(
        ElmSyntaxInterpreterPerformanceCounters a,
        ElmSyntaxInterpreterPerformanceCounters b) =>
        new(
            InstructionLoopCount: a.InstructionLoopCount + b.InstructionLoopCount,
            DirectFunctionApplicationCount: a.DirectFunctionApplicationCount + b.DirectFunctionApplicationCount,
            FunctionValueApplicationCount: a.FunctionValueApplicationCount + b.FunctionValueApplicationCount,
            PineBuiltinInvocationCount: a.PineBuiltinInvocationCount + b.PineBuiltinInvocationCount);
}

/// <summary>
/// Formats <see cref="ElmSyntaxInterpreterPerformanceCounters"/> as a human-readable
/// multi-line string suitable for use in test assertions and diagnostic output. The format
/// mirrors <c>Pine.Core.Interpreter.IntermediateVM.PerformanceCountersFormatting</c>: one
/// counter per line, integers rendered using
/// <see cref="CommandLineInterface.FormatIntegerForDisplay(long)"/>.
/// </summary>
public static class ElmSyntaxInterpreterPerformanceCountersFormatting
{
    /// <summary>
    /// Formats the given <see cref="ElmSyntaxInterpreterPerformanceCounters"/> as a multi-line string,
    /// with one counter per line and integers rendered using
    /// <see cref="CommandLineInterface.FormatIntegerForDisplay(long)"/> for readability.
    /// </summary>
    public static string FormatCounts(ElmSyntaxInterpreterPerformanceCounters counters) =>
        string.Join(
            "\n",
            EnumerateCountLines(counters));

    private static IEnumerable<string> EnumerateCountLines(ElmSyntaxInterpreterPerformanceCounters counters)
    {
        yield return "InstructionLoopCount: " + CommandLineInterface.FormatIntegerForDisplay(counters.InstructionLoopCount);
        yield return "DirectFunctionApplicationCount: " + CommandLineInterface.FormatIntegerForDisplay(counters.DirectFunctionApplicationCount);
        yield return "FunctionValueApplicationCount: " + CommandLineInterface.FormatIntegerForDisplay(counters.FunctionValueApplicationCount);
        yield return "PineBuiltinInvocationCount: " + CommandLineInterface.FormatIntegerForDisplay(counters.PineBuiltinInvocationCount);
    }
}
