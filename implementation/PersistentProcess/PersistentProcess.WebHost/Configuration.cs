using System;
using Microsoft.AspNetCore.Hosting;
using Microsoft.Extensions.DependencyInjection;

namespace Kalmit.PersistentProcess.WebHost
{
    static public class Configuration
    {
        static public string ProcessStoreDirectoryPathSettingKey => "processStoreDirectoryPath";

        static public string ElmAppFilePathSettingKey => "elmAppFilePath";

        static public IWebHostBuilder WithSettingProcessStoreDirectoryPath(
            this IWebHostBuilder orig,
            string processStoreDirectoryPath) =>
            orig.UseSetting(ProcessStoreDirectoryPathSettingKey, processStoreDirectoryPath);

        static public IWebHostBuilder WithSettingElmAppFilePath(
            this IWebHostBuilder orig,
            string elmAppFilePath) =>
            orig.UseSetting(ElmAppFilePathSettingKey, elmAppFilePath);

        static public IWebHostBuilder WithSettingDateTimeOffsetDelegate(
            this IWebHostBuilder orig,
            Func<DateTimeOffset> getDateTimeOffset) =>
            orig.ConfigureServices(services => services.AddSingleton<Func<DateTimeOffset>>(getDateTimeOffset));
    }
}
