using Pine.Core;
using System;

namespace ElmTime.ElmInteractive;


public interface IInteractiveSessionConfig
{
    string CompilerId { get; }

    IInteractiveSession InteractiveSessionFromAppCode(BlobTreeWithStringPath? appCode);
}

public record InteractiveSessionConfig(
    string CompilerId,
    Func<BlobTreeWithStringPath?, IInteractiveSession> SessionFromAppCode)
    : IInteractiveSessionConfig
{
    public IInteractiveSession InteractiveSessionFromAppCode(BlobTreeWithStringPath? appCode) =>
        SessionFromAppCode(appCode);
}
