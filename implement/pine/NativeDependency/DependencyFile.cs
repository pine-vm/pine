using System.Collections.Generic;

namespace ElmTime.NativeDependency;


public record DependencyFile(
    string HashBase16,
    string ExpectedFileName,
    IReadOnlyList<string> RemoteSources);
