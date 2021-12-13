using System.IO;

namespace Pine;

public record CacheByFileName(string CacheDirectory)
{
    public byte[] GetOrUpdate(string fileName, System.Func<byte[]> getNew)
    {
        var cacheFilePath = Path.Combine(CacheDirectory, fileName);

        if (File.Exists(cacheFilePath))
        {
            try
            {
                return File.ReadAllBytes(cacheFilePath);
            }
            catch { }
        }

        var file = getNew();

        try
        {
            var directory = Path.GetDirectoryName(cacheFilePath);

            if (directory != null)
                Directory.CreateDirectory(directory);

            File.WriteAllBytes(cacheFilePath, file);
        }
        catch (System.Exception e)
        {
            System.Console.WriteLine("Failed to write cache entry: " + e.ToString());
        }

        return file;
    }
}
