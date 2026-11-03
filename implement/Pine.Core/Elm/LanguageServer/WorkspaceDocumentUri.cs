using System;

namespace Pine.Core.Elm.LanguageServer;

/// <summary>Normalizes file URI drive letters without unescaping filename delimiters.</summary>
public static class WorkspaceDocumentUri
{
    /// <summary>
    /// Makes VS Code's escaped drive colon equivalent to the filesystem URI spelling.
    /// Other path components retain their escaping and case.
    /// </summary>
    public static Uri Normalize(Uri uri)
    {
        if (!uri.IsFile || uri.Host.Length > 0)
            return uri;

        var path = uri.GetComponents(UriComponents.Path, UriFormat.UriEscaped).TrimStart('/');

        if (path.Length < 2 || !char.IsAsciiLetter(path[0]))
            return uri;

        var colonLength =
            path[1] == ':'
            ?
            1
            :
            path.AsSpan(1).StartsWith("%3A", StringComparison.OrdinalIgnoreCase) ? 3 : 0;

        var driveLength = 1 + colonLength;

        if (colonLength == 0 || (path.Length > driveLength && path[driveLength] != '/'))
            return uri;

        var remainder = path.Length == driveLength ? "/" : path[driveLength..];
        return new Uri("file:///" + char.ToUpperInvariant(path[0]) + ":" + remainder);
    }
}
