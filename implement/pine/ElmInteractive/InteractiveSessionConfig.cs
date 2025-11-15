using Pine.Core.Files;
using System;

namespace ElmTime.ElmInteractive;


public interface IInteractiveSessionConfig
{
    string CompilerId { get; }

    IInteractiveSession InteractiveSessionFromAppCode(FileTree? appCode);
}

public record InteractiveSessionConfig(
    string CompilerId,
    Func<FileTree?, IInteractiveSession> SessionFromAppCode)
    : IInteractiveSessionConfig
{
    public IInteractiveSession InteractiveSessionFromAppCode(FileTree? appCode) =>
        SessionFromAppCode(appCode);
}
