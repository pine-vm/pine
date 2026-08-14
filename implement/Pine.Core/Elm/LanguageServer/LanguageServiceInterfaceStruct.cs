using Pine.Core.CodeAnalysis;

namespace Pine.Core.Elm.LanguageServer;

/// <summary>
/// Defines the compiled functions exposed by the Elm language service.
/// </summary>
public sealed record LanguageServiceInterfaceStruct(
    FunctionRecord InitState,
    FunctionRecord HandleRequestInCurrentWorkspace);
