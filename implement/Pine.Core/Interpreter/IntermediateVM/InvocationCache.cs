using System.Collections.Generic;

namespace Pine.Core.Interpreter.IntermediateVM;

/// <summary>
/// Key for caching results from function application in the VM.
/// </summary>
public readonly record struct EvalCacheEntryKey(PineValue ExprValue, StackFrameInput StackFrameInput);

/// <summary>
/// Caches results from function application in the VM.
/// </summary>
public sealed class InvocationCache : Dictionary<EvalCacheEntryKey, PineValue>;

