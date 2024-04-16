using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Http;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Linq;
using System.Threading.Tasks;

namespace Pine;

/// <summary>
/// This server packages popular parts of git repositories into zip archives and caches them.
/// 
/// https://github.blog/2020-12-21-get-up-to-speed-with-partial-clone-and-shallow-clone/
/// </summary>
public class GitPartialForCommitServer
{
    public static string ZipArchivePathPrefix => "/git/partial-for-commit/zip/";

    public static string ZipArchivePathFromCommit(string commit) => ZipArchivePathPrefix + commit;

    public static Task Run(
        IReadOnlyList<string> urls,
        IReadOnlyList<string> gitCloneUrlPrefixes,
        string fileCacheDirectory)
    {
        var builder = WebApplication.CreateBuilder();
        var app = builder.Build();

        app.Urls.Clear();
        urls.ToList().ForEach(app.Urls.Add);

        var fileCache = new CacheByFileName
        (
            new FileStoreFromSystemIOFile(Path.Combine(fileCacheDirectory, ZipArchivePathPrefix.TrimStart('/')))
        );

        /*
         * https://docs.microsoft.com/en-us/aspnet/core/fundamentals/minimal-apis?view=aspnetcore-6.0
         * */

        app.MapGet(ZipArchivePathPrefix + "{commitId}", (string commitId, HttpRequest httpRequest) =>
        {
            using var bodyReader = new StreamReader(httpRequest.Body);

            var bodyString = bodyReader.ReadToEndAsync().Result;

            var cloneUrls = bodyString.Split(new[] { '\n', '\r' }, System.StringSplitOptions.RemoveEmptyEntries);

            var supportedCloneUrls =
                cloneUrls
                .Where(c => gitCloneUrlPrefixes.Any(prefix => c.ToLowerInvariant().StartsWith(prefix.ToLowerInvariant())))
                .ToImmutableList();

            if (!cloneUrls.Any())
            {
                return Results.BadRequest("Missing clone URL. Use one line in the request body for each clone URL.");
            }

            if (supportedCloneUrls.IsEmpty)
            {
                return Results.BadRequest(
                    "None of the given clone URLs is enabled here. Only URLs with the following " +
                    gitCloneUrlPrefixes.Count + " prefixes are supported: " + string.Join(", ", gitCloneUrlPrefixes));
            }

            System.ReadOnlyMemory<byte> loadWithFreshClone()
            {
                var files =
                    LoadFromGitHubOrGitLab.GetRepositoryFilesPartialForCommitViaEnvironmentGitCheckout(
                        cloneUrl: supportedCloneUrls[0],
                        commit: commitId);

                var zipArchive = ZipArchive.ZipArchiveFromEntries(files);

                System.Console.WriteLine(
                    "Cloned for commit " + commitId + ": Got " + files.Count + " files. Size of zip archive: " + zipArchive.Length + " bytes.");

                return zipArchive;
            }

            return Results.Bytes(
                contents: fileCache.GetOrUpdate(commitId, loadWithFreshClone),
                contentType: "application/zip",
                fileDownloadName: "git-partial-for-commit-" + commitId + ".zip");
        });

        return app.RunAsync();
    }
}
