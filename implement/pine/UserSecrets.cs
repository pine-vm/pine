using Pine;
using System;
using System.Collections.Immutable;
using System.IO;
using System.Text;

namespace ElmTime;

public static class UserSecrets
{
    private static string FileStoreDirectory =>
        Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData), "elm-time", "user-secrets");

    private static IFileStore FileStore => new FileStoreFromSystemIOFile(FileStoreDirectory);

    public static void StorePasswordForSite(string site, string password)
    {
        FileStore.SetFileContent(
            ImmutableList.Create(System.Web.HttpUtility.UrlEncode(site.TrimEnd('/'))), Encoding.UTF8.GetBytes(password));
    }

    public static string? LoadPasswordForSite(string site)
    {
        if (site == null)
            return null;

        var fileContent =
            FileStore.GetFileContent(ImmutableList.Create(System.Web.HttpUtility.UrlEncode(site.TrimEnd('/'))));

        if (fileContent == null)
            return null;

        return Encoding.UTF8.GetString(fileContent.Value.Span);
    }
}
