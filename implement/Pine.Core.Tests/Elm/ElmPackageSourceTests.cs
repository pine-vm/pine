using AwesomeAssertions;
using Pine.Elm;
using System;
using System.IO;
using System.IO.Compression;
using System.Text;
using System.Threading.Tasks;
using Xunit;

namespace Pine.Core.Tests.Elm;

public class ElmPackageSourceTests
{
    [Fact]
    public async Task Load_package_uses_GitHub_repository_cache_path()
    {
        var cacheDirectory =
            Path.Combine(
                Path.GetTempPath(),
                "pine-elm-package-source-tests",
                Guid.NewGuid().ToString("N"));

        var currentCachePath =
            Path.Combine(cacheDirectory, "github.com", "cmditch", "elm-bigint@2.0.1.zip");

        var legacyCachePath =
            Path.Combine(cacheDirectory, "cmditch-elm-bigint@2.0.1.zip");

        Directory.CreateDirectory(Path.GetDirectoryName(currentCachePath)!);

        try
        {
            await File.WriteAllBytesAsync(currentCachePath, BuildPackageZip("current"));
            await File.WriteAllBytesAsync(legacyCachePath, BuildPackageZip("legacy"));

            var packageFiles =
                await ElmPackageSource.LoadElmPackageAsync(
                    "cmditch/elm-bigint",
                    "2.0.1",
                    [cacheDirectory]);

            Encoding.UTF8.GetString(packageFiles[["marker.txt"]].Span)
                .Should().Be("current");
        }
        finally
        {
            Directory.Delete(cacheDirectory, recursive: true);
        }
    }

    private static byte[] BuildPackageZip(string marker)
    {
        using var memoryStream = new MemoryStream();

        using (var archive = new ZipArchive(memoryStream, ZipArchiveMode.Create, leaveOpen: true))
        {
            var elmJsonEntry = archive.CreateEntry("elm-bigint-2.0.1/elm.json");

            using (var elmJsonStream = elmJsonEntry.Open())
            {
                elmJsonStream.Write(Encoding.UTF8.GetBytes("{}"));
            }

            var markerEntry = archive.CreateEntry("elm-bigint-2.0.1/marker.txt");

            using var markerStream = markerEntry.Open();
            markerStream.Write(Encoding.UTF8.GetBytes(marker));
        }

        return memoryStream.ToArray();
    }
}
