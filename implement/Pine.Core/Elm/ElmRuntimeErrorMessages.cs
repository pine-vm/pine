namespace Pine.Core.Elm;

/// <summary>Stable diagnostic text shared by syntax interpretation and compiled Elm runtime operations.</summary>
public static class ElmRuntimeErrorMessages
{
    /// <summary>A record field was read from another kind of value.</summary>
    public const string RecordAccessOnNonRecord = "Attempted to access a field on a non-record value.";

    /// <summary>A record update received another kind of value.</summary>
    public const string RecordUpdateOnNonRecord = "Expected a record value to update, but got something else.";

    /// <summary>No case arm matched the scrutinee.</summary>
    public const string CaseNoMatch = "Case expression did not match any arm.";

    internal const string RecordFieldNotFoundPrefix = "Record does not contain a field named '";

    internal const string RecordFieldNotFoundSuffix = "'.";

    internal const string RecordUpdateFieldNotFoundPrefix = "Cannot update field \"";

    internal const string RecordUpdateFieldNotFoundSuffix = "\" because it does not exist on the record.";

    /// <summary>A field lookup exhausted the record's fields.</summary>
    public static string RecordFieldNotFound(string name) =>
        RecordFieldNotFoundPrefix + name + RecordFieldNotFoundSuffix;

    /// <summary>An update referenced a field absent from the record.</summary>
    public static string RecordUpdateFieldNotFound(string name) =>
        RecordUpdateFieldNotFoundPrefix + name + RecordUpdateFieldNotFoundSuffix;
}
