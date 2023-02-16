using System;
using System.Collections.Generic;
using Microsoft.AspNetCore.Hosting;
using Microsoft.AspNetCore.Http;
using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.DependencyInjection;
using Pine;

namespace ElmTime.Platform.WebServer;

static public class Configuration
{
    static public string ApiPersistentProcessStatePath => "/api/process/state";

    static public string AdminPasswordSettingKey => "adminPassword";

    static public string PublicWebHostUrlsSettingKey = "publicWebHostUrls";

    static public string[] PublicWebHostUrlsDefault => new[] { "http://*", "https://*" };

    //  https://en.wikipedia.org/wiki/Basic_access_authentication
    static public string BasicAuthenticationForAdmin(string? password) => ":" + password;

    static public IWebHostBuilder WithProcessStoreFileStore(
        this IWebHostBuilder orig,
        IFileStore fileStore) =>
        orig.ConfigureServices(serviceCollection => serviceCollection.AddSingleton(new FileStoreForProcessStore(fileStore)));

    static public IWebHostBuilder WithSettingProcessStoreDirectoryPath(
        this IWebHostBuilder orig,
        string processStoreDirectoryPath) =>
        orig.WithProcessStoreFileStore(new FileStoreFromSystemIOFile(processStoreDirectoryPath));

    static public IWebHostBuilder WithSettingProcessStoreSeparateReaderDirectoryPath(
        this IWebHostBuilder orig,
        string? processStoreSeparateReaderDirectoryPath) =>
        orig.ConfigureServices(serviceCollection => serviceCollection.AddSingleton(
            new FileStoreForProcessStoreReader(
                processStoreSeparateReaderDirectoryPath == null
                ? null
                : new FileStoreFromSystemIOFile(processStoreSeparateReaderDirectoryPath))));

    static public IWebHostBuilder WithWebAppConfigurationZipArchive(
        this IWebHostBuilder orig,
        byte[] zipArchive) =>
        orig.ConfigureServices(serviceCollection => serviceCollection.AddSingleton(new WebAppConfigurationZipArchive(zipArchive)));

    static public IWebHostBuilder WithWebAppConfigurationZipArchiveFromFilePath(
        this IWebHostBuilder orig,
        string webAppConfigurationFilePath) =>
        orig.WithWebAppConfigurationZipArchive(System.IO.File.ReadAllBytes(webAppConfigurationFilePath));

    static public IWebHostBuilder WithSettingAdminPassword(
        this IWebHostBuilder orig,
        string? adminPassword) =>
        orig.UseSetting(AdminPasswordSettingKey, adminPassword);

    static public IWebHostBuilder WithSettingDateTimeOffsetDelegate(
        this IWebHostBuilder orig,
        Func<DateTimeOffset> getDateTimeOffset) =>
        orig.ConfigureServices(services => services.AddSingleton(getDateTimeOffset));

    static public IWebHostBuilder WithSettingPublicWebHostUrls(
        this IWebHostBuilder orig,
        string[] urls) =>
        orig.UseSetting(PublicWebHostUrlsSettingKey, string.Join(",", urls));

    static public IReadOnlyList<string> GetSettingPublicWebHostUrls(
        this IConfiguration? configuration) =>
        configuration?.GetValue<string>(PublicWebHostUrlsSettingKey)?.Split(new[] { ',', ';' }) ??
        PublicWebHostUrlsDefault;

    static internal DateTimeOffset GetDateTimeOffset(HttpContext context) =>
        context.RequestServices.GetService<Func<DateTimeOffset>>()!();
}

public record FileStoreForProcessStore(IFileStore fileStore);

public record FileStoreForProcessStoreReader(IFileStore? fileStore);

public record WebAppConfigurationZipArchive(byte[] zipArchive);
