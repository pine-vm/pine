using Pine.Core;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.ElmInteractive;

public record struct CompiledModulesCacheEntry(
    IReadOnlyList<string> CompiledModules,
    ElmTime.ElmInteractive.ElmInteractive.PineValueJson CompiledValueJson,
    PineValue CompiledValue);

public interface ICompiledModulesCache
{
    CompiledModulesCacheEntry? GetClosestBase(IReadOnlyList<string> modulesTexts);

    void Set(IReadOnlyList<string> modulesTexts, PineValue value);
}

public class CompiledModulesFileCache(IFileStore fileStore)
    : ICompiledModulesCache
{
    public static string ComputeHashNameForModulesList(IReadOnlyList<string> modulesTexts)
    {
        var hash = ComputeHashForModulesList(modulesTexts);

        return Convert.ToHexStringLower(hash.Span);
    }

    public static string ComputeFileNameForModulesList(IReadOnlyList<string> modulesTexts)
    {
        return ComputeHashNameForModulesList(modulesTexts)[..16] + ".json";
    }

    public static ReadOnlyMemory<byte> ComputeHashForModulesList(IReadOnlyList<string> modulesTexts)
    {
        if (modulesTexts.Count is 0)
        {
            return ReadOnlyMemory<byte>.Empty;
        }

        var lastModuleBytes = System.Text.Encoding.UTF8.GetBytes(modulesTexts.Last());

        var lastModuleHash = System.Security.Cryptography.SHA256.HashData(lastModuleBytes);

        var previousModules = modulesTexts.Take(modulesTexts.Count - 1).ToImmutableArray();

        var previousModulesHash =
            ComputeHashForModulesList(previousModules);

        var aggregateBytes = (byte[])[.. previousModulesHash.ToArray(), .. lastModuleHash];

        return System.Security.Cryptography.SHA256.HashData(aggregateBytes);
    }

    public CompiledModulesCacheEntry? GetClosestBase(IReadOnlyList<string> modulesTexts)
    {
        if (modulesTexts.Count is 0)
        {
            return null;
        }

        var hashName = ComputeFileNameForModulesList(modulesTexts);

        var fileContent = fileStore.GetFileContent([hashName]);

        if (fileContent.HasValue)
        {
            var environmentJsonString = System.Text.Encoding.UTF8.GetString(fileContent.Value.Span);

            var environmentJson =
                System.Text.Json.JsonSerializer.Deserialize<ElmTime.ElmInteractive.ElmInteractive.PineValueJson>(
                    environmentJsonString,
                    options: ElmTime.ElmInteractive.ElmInteractive.compilerInterfaceJsonSerializerOptions);

            var environmentPineValue =
                ElmTime.ElmInteractive.ElmInteractive.ParsePineValueFromJson(
                    environmentJson!,
                    parentDictionary: null);

            return
                new CompiledModulesCacheEntry(
                    modulesTexts,
                    environmentJson!,
                    environmentPineValue);
        }

        return GetClosestBase([.. modulesTexts.Take(modulesTexts.Count - 1)]);
    }

    public void Set(IReadOnlyList<string> modulesTexts, PineValue value)
    {
        var hashName = ComputeFileNameForModulesList(modulesTexts);

        var (environmentJson, _) =
            ElmTime.ElmInteractive.ElmInteractive.FromPineValueBuildingDictionary(
                value,
                ElmTime.ElmInteractive.ElmInteractive.CompilationCache.Empty);

        var environmentJsonString =
            System.Text.Json.JsonSerializer.Serialize(environmentJson.json,
                options: ElmTime.ElmInteractive.ElmInteractive.compilerInterfaceJsonSerializerOptions);

        var fileContent = System.Text.Encoding.UTF8.GetBytes(environmentJsonString);

        fileStore.SetFileContent([hashName], fileContent);
    }
}
