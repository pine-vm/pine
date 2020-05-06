using System;
using System.Collections.Immutable;
using System.IO;
using System.Text;
using Kalmit;

namespace elm_fullstack
{
    static public class UserSecrets
    {
        static string fileStoreDirectory =>
            Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData), "elm-fullstack", "user-secrets");

        static IFileStore fileStore => new FileStoreFromSystemIOFile(fileStoreDirectory);

        static public void StorePasswordForSite(string site, string password)
        {
            fileStore.SetFileContent(
                ImmutableList.Create(System.Web.HttpUtility.UrlEncode(site.TrimEnd('/'))), Encoding.UTF8.GetBytes(password));
        }

        static public string LoadPasswordForSite(string site)
        {
            if (site == null)
                return null;

            var fileContent =
                fileStore.GetFileContent(ImmutableList.Create(System.Web.HttpUtility.UrlEncode(site.TrimEnd('/'))));

            if (fileContent == null)
                return null;

            return Encoding.UTF8.GetString(fileContent);
        }
    }
}