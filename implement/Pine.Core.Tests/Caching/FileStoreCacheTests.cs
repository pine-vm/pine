using AwesomeAssertions;
using Pine.Core.Caching;
using Pine.Core.IO;
using System;
using System.Collections.Immutable;
using System.IO;
using System.Linq;
using System.Security.Cryptography;
using System.Text;
using Xunit;

namespace Pine.Core.Tests.Caching;

public class FileStoreCacheTests
{
    private sealed class TextCodec : ICacheCodec<string>
    {
        public ReadOnlyMemory<byte> Encode(string value) => Encoding.UTF8.GetBytes(value);
        public string Decode(ReadOnlyMemory<byte> bytes) => new UTF8Encoding(false, true).GetString(bytes.Span);
    }

    private static FileStoreCache<string, string> Cache(IFileStore store) =>
        new(store, ImmutableList.Create("pine", "test-cache", "v1"),
            input => SHA256.HashData(Encoding.UTF8.GetBytes(input)), new TextCodec());

    [Fact]
    public void File_store_cache_uses_caller_subtree_and_restores_values_through_new_instances()
    {
        var store = new FileStoreFromConcurrentDictionary();
        Cache(store).TryGetValue("key", out _).Should().BeFalse();
        Cache(store).Set("key", "persisted");
        var path = store.ListFiles().Single();
        path.Take(3).Should().Equal("pine", "test-cache", "v1");
        string.Concat(path.Skip(3)).Should().Be(
            Convert.ToHexStringLower(SHA256.HashData(Encoding.UTF8.GetBytes("key"))));
        Cache(store).TryGetValue("key", out var value).Should().BeTrue();
        value.Should().Be("persisted");
        Cache(store).TryGetValue("other", out _).Should().BeFalse();
    }

    [Fact]
    public void Actual_files_survive_store_and_cache_recreation()
    {
        var directory = Path.GetFullPath(Path.Combine("artifacts", "cache-tests", Guid.NewGuid().ToString("N")));
        try
        {
            Cache(new FileStoreFromSystemIOFile(directory)).Set("key", "saved on disk");
            Cache(new FileStoreFromSystemIOFile(directory)).TryGetValue("key", out var value).Should().BeTrue();
            value.Should().Be("saved on disk");
        }
        finally
        {
            if (Directory.Exists(directory))
                Directory.Delete(directory, recursive: true);
        }
    }

    [Fact]
    public void Payload_corruption_is_reported_instead_of_returning_a_false_hit()
    {
        var store = new FileStoreFromConcurrentDictionary();
        var cache = Cache(store);
        cache.Set("key", "persisted");
        var path = store.ListFiles().Single();
        var content = store.GetFileContent(path)!.Value.ToArray();
        content[^1] ^= 1;
        store.SetFileContent(path, content);
        Action read = () => cache.TryGetValue("key", out _);
        read.Should().Throw<InvalidDataException>();
    }

    [Fact]
    public void Read_through_promotes_hits_but_storage_failures_do_not_publish_to_the_front()
    {
        var store = new FileStoreFromConcurrentDictionary();
        var front = new MemoryCache<string, string, long>(0);
        Cache(store).Set("key", "from backing");
        var layered = new LayeredCache<string, string>(front, Cache(store));
        layered.TryGetValue("key", out var value).Should().BeTrue();
        value.Should().Be("from backing");
        front.Count.Should().Be(1);
        store.DeleteFile(store.ListFiles().Single());
        layered.TryGetValue("key", out _).Should().BeTrue();
        layered.TryGetValue("missing", out _).Should().BeFalse();
        front.Count.Should().Be(1);

        var failingStore = new FileStoreFromWriterAndReader(
            new DelegatingFileStoreWriter(_ => throw new IOException("write failed"),
                _ => throw new IOException("write failed"), _ => throw new IOException("write failed")), store);
        var failingLayer = new LayeredCache<string, string>(front, Cache(failingStore));
        Action write = () => failingLayer.Set("new", "must not be published");
        write.Should().Throw<IOException>();
        front.TryGetValue("new", out _).Should().BeFalse();
    }
}
