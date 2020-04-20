using Microsoft.AspNetCore.Hosting;
using Microsoft.AspNetCore.Http;
using Microsoft.Extensions.DependencyInjection;
using System;

namespace Kalmit.PersistentProcess.WebHost
{
    static public class Configuration
    {
        static public string AdminPath => "/elm-fullstack-admin";

        static public string ApiPersistentProcessStatePath => "/api/process/state";

        static public string WithSettingFrontendWebElmMakeAppendixSettingKey => "frontendWebElmMakeAppendix";

        static public string AdminRootPasswordSettingKey => "adminRootPassword";

        static public string PublicWebHostUrlsSettingKey = "publicWebHostUrls";

        static public string AdminRootUserName => "root";

        //  https://en.wikipedia.org/wiki/Basic_access_authentication
        static public string BasicAuthenticationForAdminRoot(string password) =>
            AdminRootUserName + ":" + password;

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
            string processStoreSeparateReaderDirectoryPath) =>
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

        static public IWebHostBuilder WithSettingFrontendWebElmMakeAppendix(
            this IWebHostBuilder orig,
            string frontendWebElmMakeAppendix) =>
            orig.UseSetting(WithSettingFrontendWebElmMakeAppendixSettingKey, frontendWebElmMakeAppendix);

        static public IWebHostBuilder WithSettingAdminRootPassword(
            this IWebHostBuilder orig,
            string adminRootPassword) =>
            orig.UseSetting(AdminRootPasswordSettingKey, adminRootPassword);

        static public IWebHostBuilder WithSettingDateTimeOffsetDelegate(
            this IWebHostBuilder orig,
            Func<DateTimeOffset> getDateTimeOffset) =>
            orig.ConfigureServices(services => services.AddSingleton<Func<DateTimeOffset>>(getDateTimeOffset));

        static public IWebHostBuilder WithSettingPublicWebHostUrls(
            this IWebHostBuilder orig,
            string[] urls) =>
            orig.UseSetting(PublicWebHostUrlsSettingKey, String.Join(",", urls));

        static internal DateTimeOffset GetDateTimeOffset(HttpContext context) =>
            context.RequestServices.GetService<Func<DateTimeOffset>>()();
    }

    public class FileStoreForProcessStore
    {
        readonly public IFileStore fileStore;

        public FileStoreForProcessStore(IFileStore fileStore)
        {
            this.fileStore = fileStore;
        }
    }

    public class FileStoreForProcessStoreReader
    {
        readonly public IFileStore fileStore;

        public FileStoreForProcessStoreReader(IFileStore fileStore)
        {
            this.fileStore = fileStore;
        }
    }

    public class WebAppConfigurationZipArchive
    {
        readonly public byte[] zipArchive;

        public WebAppConfigurationZipArchive(byte[] zipArchive)
        {
            this.zipArchive = zipArchive;
        }
    }
}
