using System;
using Microsoft.AspNetCore.Hosting;
using Microsoft.AspNetCore.Http;
using Microsoft.Extensions.DependencyInjection;

namespace Kalmit.PersistentProcess.WebHost
{
    static public class Configuration
    {
        static public string ProcessStoreDirectoryPathSettingKey => "processStoreDirectoryPath";

        static public string WebAppConfigurationFilePathSettingKey => "webAppConfigurationFilePath";

        static public IWebHostBuilder WithSettingProcessStoreDirectoryPath(
            this IWebHostBuilder orig,
            string processStoreDirectoryPath) =>
            orig.UseSetting(ProcessStoreDirectoryPathSettingKey, processStoreDirectoryPath);

        static public IWebHostBuilder WithSettingWebAppConfigurationFilePath(
            this IWebHostBuilder orig,
            string webAppConfigurationFilePath) =>
            orig.UseSetting(WebAppConfigurationFilePathSettingKey, webAppConfigurationFilePath);

        static public IWebHostBuilder WithSettingDateTimeOffsetDelegate(
            this IWebHostBuilder orig,
            Func<DateTimeOffset> getDateTimeOffset) =>
            orig.ConfigureServices(services => services.AddSingleton<Func<DateTimeOffset>>(getDateTimeOffset));

        static internal DateTimeOffset GetDateTimeOffset(HttpContext context) =>
            context.RequestServices.GetService<Func<DateTimeOffset>>()();
    }
}
