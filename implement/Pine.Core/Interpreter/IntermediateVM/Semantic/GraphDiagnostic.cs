namespace Pine.Core.Interpreter.IntermediateVM.Semantic;

/// <summary>Stable classifications of malformed semantic graph data.</summary>
public enum GraphDiagnosticCode
{
    /// <summary>A required object or collection is absent.</summary>
    MissingData,
    /// <summary>The entry block does not exist.</summary>
    MissingEntry,
    /// <summary>A dictionary key differs from the contained block's ID.</summary>
    BlockIdMismatch,
    /// <summary>A value ID is defined more than once in the function.</summary>
    DuplicateDefinition,
    /// <summary>An operand is not defined earlier in this block or as a block parameter.</summary>
    UndefinedValue,
    /// <summary>A semantic type is not supported.</summary>
    InvalidType,
    /// <summary>An operand or result does not have the required type.</summary>
    TypeMismatch,
    /// <summary>A positional binding has the wrong number of values.</summary>
    ArityMismatch,
    /// <summary>An edge names a nonexistent block.</summary>
    MissingTarget,
    /// <summary>An environment projection contains a negative index.</summary>
    InvalidPath,
    /// <summary>A builtin name is not a Pine primitive.</summary>
    InvalidBuiltin,
    /// <summary>A switch repeats an exact literal.</summary>
    DuplicateCase,
    /// <summary>A known call names a function absent from the signature table and root.</summary>
    UnknownFunction,
    /// <summary>A declared contract differs from the resolved contract.</summary>
    SignatureMismatch,
    /// <summary>A call site ID occurs more than once in the function.</summary>
    DuplicateCallSite,
    /// <summary>A continuation requests a nonexistent returned result slot.</summary>
    InvalidReturnedResult,
}

/// <summary>
/// A structural location, independent of graph layout. Block is the dictionary key; Operation is
/// zero-based. Edge is zero for jump/invoke, zero/one for branch, and the case index for switch
/// (default follows the cases). Member identifies a field or positional operand within that location.
/// Null coordinates denote an enclosing location, not a synthetic graph ID.
/// </summary>
public sealed record GraphLocation(
    FunctionId? Function,
    PineBlockId? Block = null,
    int? Operation = null,
    int? Edge = null,
    PineVirtualValueId? Value = null,
    string Member = "");

/// <summary>A deterministic validation failure with a machine-readable code and location.</summary>
public sealed record GraphDiagnostic(GraphDiagnosticCode Code, GraphLocation Location, string Reason);
