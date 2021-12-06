using System.IO;

namespace Pine;

public class CacheByFileName
{
    public string CacheDirectory { init; get; }

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
            Directory.CreateDirectory(Path.GetDirectoryName(cacheFilePath));

            File.WriteAllBytes(cacheFilePath, file);
        }
        catch (System.Exception e)
        {
            System.Console.WriteLine("Failed to write cache entry: " + e.ToString());
        }

        return file;
    }
}
