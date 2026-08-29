namespace Pine.Core.LanguageServerProtocol;

/// <summary>
/// Identifies an LSP participant by name and optional version string.
/// </summary>
public record ParticipentInfo
{
    /// <summary>
    /// Initializes participant info for an LSP client or server advertisement.
    /// </summary>
    public ParticipentInfo(
        string Name,
        string? Version)
    {
        this.Name = Name;
        this.Version = Version;
    }

    /// <summary>
    /// Name advertised for the participant in protocol messages.
    /// </summary>
    public string Name { get; init; }

    /// <summary>
    /// Optional version string advertised for the participant in protocol messages.
    /// </summary>
    public string? Version { get; init; }

    /// <summary>
    /// Deconstructs this participant info into its name and optional version string.
    /// </summary>
    public void Deconstruct(
        out string Name,
        out string? Version)
    {
        Name = this.Name;
        Version = this.Version;
    }
}
