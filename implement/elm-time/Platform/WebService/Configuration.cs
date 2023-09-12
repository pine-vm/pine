using ElmTime.JavaScript;
using Microsoft.AspNetCore.Hosting;
using Microsoft.AspNetCore.Http;
using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.DependencyInjection;
using Pine;
using System;
using System.Collections.Generic;

namespace ElmTime.Platform.WebService;

public static class Configuration
{
    public static string ApiPersistentProcessStatePath => "/api/process/state";

    public static string AdminPasswordSettingKey => "adminPassword";

    public static string PublicWebHostUrlsSettingKey => "publicWebHostUrls";

    public static string[] PublicWebHostUrlsDefault => ["http://*", "https://*"];

    /// <summary>
    ///  https://en.wikipedia.org/wiki/Basic_access_authentication
    /// </summary>
    public static string BasicAuthenticationForAdmin(string? password) => ":" + password;

    public static IWebHostBuilder WithProcessStoreFileStore(
        this IWebHostBuilder orig,
        IFileStore fileStore) =>
        orig.ConfigureServices(serviceCollection => serviceCollection.AddSingleton(new FileStoreForProcessStore(fileStore)));

    public static IWebHostBuilder WithSettingProcessStoreDirectoryPath(
        this IWebHostBuilder orig,
        string processStoreDirectoryPath) =>
        orig.WithProcessStoreFileStore(new FileStoreFromSystemIOFile(processStoreDirectoryPath));

    public static IWebHostBuilder WithSettingProcessStoreSeparateReaderDirectoryPath(
        this IWebHostBuilder orig,
        string? processStoreSeparateReaderDirectoryPath) =>
        orig.ConfigureServices(serviceCollection => serviceCollection.AddSingleton(
            new FileStoreForProcessStoreReader(
                processStoreSeparateReaderDirectoryPath == null
                ? null
                : new FileStoreFromSystemIOFile(processStoreSeparateReaderDirectoryPath))));

    public static IWebHostBuilder WithDeployZipArchive(
        this IWebHostBuilder orig,
        byte[] zipArchive) =>
        orig.ConfigureServices(serviceCollection => serviceCollection.AddSingleton(new DeployZipArchive(zipArchive)));

    public static IWebHostBuilder WithDeployZipArchiveFromFilePath(
        this IWebHostBuilder orig,
        string deployFilePath) =>
        orig.WithDeployZipArchive(System.IO.File.ReadAllBytes(deployFilePath));

    public static IWebHostBuilder WithSettingAdminPassword(
        this IWebHostBuilder orig,
        string? adminPassword) =>
        orig.UseSetting(AdminPasswordSettingKey, adminPassword);

    public static IWebHostBuilder WithSettingDateTimeOffsetDelegate(
        this IWebHostBuilder orig,
        Func<DateTimeOffset> getDateTimeOffset) =>
        orig.ConfigureServices(services => services.AddSingleton(getDateTimeOffset));

    public static IWebHostBuilder WithSettingPublicWebHostUrls(
        this IWebHostBuilder orig,
        IEnumerable<string>? urls) =>
        orig.UseSetting(PublicWebHostUrlsSettingKey, urls is null ? null : string.Join(",", urls));

    public static IReadOnlyList<string> GetSettingPublicWebHostUrls(
        this IConfiguration? configuration) =>
        configuration?.GetValue<string>(PublicWebHostUrlsSettingKey)?.Split(',', ';') ??
        PublicWebHostUrlsDefault;

    public static IWebHostBuilder WithJavaScriptEngineFactory(
        this IWebHostBuilder orig,
        Func<IJavaScriptEngine> javaScriptEngineFactory) =>
        orig.ConfigureServices(serviceCollection => serviceCollection.AddSingleton(javaScriptEngineFactory));

    internal static DateTimeOffset GetDateTimeOffset(HttpContext context) =>
        context.RequestServices.GetService<Func<DateTimeOffset>>()!();
}

public record FileStoreForProcessStore(IFileStore fileStore);

public record FileStoreForProcessStoreReader(IFileStore? fileStore);

public record DeployZipArchive(byte[] zipArchive);
