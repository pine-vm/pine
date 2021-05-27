using System;
using System.Collections.Immutable;
using System.IO;
using System.Linq;
using System.Text;
using Pine;

namespace elm_fullstack
{
    static public class UserSecrets
    {
        static string FileStoreDirectory =>
            Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData), "elm-fullstack", "user-secrets");

        static IFileStore FileStore => new FileStoreFromSystemIOFile(FileStoreDirectory);

        static public void StorePasswordForSite(string site, string password)
        {
            FileStore.SetFileContent(
                ImmutableList.Create(System.Web.HttpUtility.UrlEncode(site.TrimEnd('/'))), Encoding.UTF8.GetBytes(password));
        }

        static public string LoadPasswordForSite(string site)
        {
            if (site == null)
                return null;

            var fileContent =
                FileStore.GetFileContent(ImmutableList.Create(System.Web.HttpUtility.UrlEncode(site.TrimEnd('/'))));

            if (fileContent == null)
                return null;

            return Encoding.UTF8.GetString(fileContent as byte[] ?? fileContent.ToArray());
        }
    }
}