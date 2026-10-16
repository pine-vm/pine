namespace Pine.Core.CLI;

/// <summary>Controls ANSI colors in command-line output.</summary>
public enum CommandLineColorMode
{
    /// <summary>Detect terminal color support.</summary>
    Auto,

    /// <summary>Enable colors even when output is redirected.</summary>
    Always,

    /// <summary>Disable colors.</summary>
    Never,
}
