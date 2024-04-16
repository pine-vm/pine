using Pine;
using System;

namespace ElmTime.ElmInteractive;


public interface IInteractiveSessionConfig
{
    string CompilerId { get; }

    IInteractiveSession InteractiveSessionFromAppCode(TreeNodeWithStringPath? appCode);
}

public record InteractiveSessionConfig(
    string CompilerId,
    Func<TreeNodeWithStringPath?, IInteractiveSession> SessionFromAppCode)
    : IInteractiveSessionConfig
{
    public IInteractiveSession InteractiveSessionFromAppCode(TreeNodeWithStringPath? appCode) =>
        SessionFromAppCode(appCode);
}
