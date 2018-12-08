using System;
using System.Linq;
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Hosting;
using Microsoft.AspNetCore.Http;
using Microsoft.AspNetCore.Http.Extensions;
using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.DependencyInjection;

namespace Kalmit.PersistentProcess.WebHost
{
    public class Startup
    {
        public void ConfigureServices(IServiceCollection services)
        {
            var serviceProvider = services.BuildServiceProvider();
            var config = serviceProvider.GetService<IConfiguration>();

            var webAppConfigFile = System.IO.File.ReadAllBytes(config.GetValue<string>(Configuration.WebAppConfigurationFilePathSettingKey));
            var webAppConfig = WebAppConfiguration.FromFiles(ZipArchive.EntriesFromZipArchive(webAppConfigFile).ToList());
            services.AddSingleton<WebAppConfiguration>(webAppConfig);

            var getDateTimeOffset = serviceProvider.GetService<Func<DateTimeOffset>>();

            if (getDateTimeOffset == null)
            {
                getDateTimeOffset = () => DateTimeOffset.UtcNow;
                services.AddSingleton<Func<DateTimeOffset>>(getDateTimeOffset);
            }

            var processStoreDirectory = config.GetValue<string>(Configuration.ProcessStoreDirectoryPathSettingKey);
            var processStore = new Kalmit.ProcessStore.ProcessStoreInFileDirectory(
                processStoreDirectory,
                () => getDateTimeOffset().ToString("yyyy-MM-dd") + ".composition.jsonl");

            services.AddSingleton<ProcessStore.IProcessStoreReader>(processStore);
            services.AddSingleton<ProcessStore.IProcessStoreWriter>(processStore);
            services.AddSingleton<IPersistentProcess>(BuildPersistentProcess);

            Asp.ConfigureServices(services);
        }

        static PersistentProcessWithHistoryOnFileFromElm019Code BuildPersistentProcess(IServiceProvider services)
        {
            var elmAppFile = services.GetService<WebAppConfiguration>()?.ElmAppFile;

            if (elmAppFile == null)
                return null;

            return new PersistentProcessWithHistoryOnFileFromElm019Code(
                services.GetService<ProcessStore.IProcessStoreReader>(), elmAppFile);
        }

        public void Configure(
            IApplicationBuilder app,
            IHostingEnvironment env,
            WebAppConfiguration webAppConfig,
            ProcessStore.IProcessStoreWriter processStoreWriter,
            Func<DateTimeOffset> getDateTimeOffset)
        {
            if (env.IsDevelopment())
            {
                app.UseDeveloperExceptionPage();
            }

            var nextHttpRequestIndex = 0;

            var cyclicReductionStoreLock = new object();
            DateTimeOffset? cyclicReductionStoreLastTime = null;
            var cyclicReductionStoreDistanceSeconds = (int)TimeSpan.FromHours(1).TotalSeconds;

            app
            .Use(async (context, next) => await Asp.MiddlewareFromWebAppConfig(webAppConfig, context, next))
            .Run(async (context) =>
            {
                var persistentProcess = context.RequestServices.GetService<IPersistentProcess>();

                var currentDateTime = getDateTimeOffset();
                var timeMilli = currentDateTime.ToUnixTimeMilliseconds();
                var httpRequestIndex = System.Threading.Interlocked.Increment(ref nextHttpRequestIndex);

                var httpRequestId = timeMilli.ToString() + "-" + httpRequestIndex.ToString();

                var httpEvent = AsPersistentProcessInterfaceHttpRequestEvent(context, httpRequestId);

                var interfaceEvent = new InterfaceToHost.Event
                {
                    httpRequest = httpEvent,
                };

                var serializedInterfaceEvent = Newtonsoft.Json.JsonConvert.SerializeObject(interfaceEvent, jsonSerializerSettings);

                string serializedResponse = null;

                lock (processStoreWriter)
                {
                    var (responses, compositionRecord) = persistentProcess.ProcessEvents(new[] { serializedInterfaceEvent });

                    processStoreWriter.AppendSerializedCompositionRecord(compositionRecord.serializedCompositionRecord);

                    serializedResponse = responses.Single();
                }

                InterfaceToHost.ResponseOverSerialInterface structuredResponse = null;

                try
                {
                    structuredResponse =
                        Newtonsoft.Json.JsonConvert.DeserializeObject<InterfaceToHost.ResponseOverSerialInterface>(
                            serializedResponse);
                }
                catch (Exception parseException)
                {
                    throw new Exception(
                        "Failed to parse event response from app. Looks like the loaded elm app is not compatible with the interface.\nResponse from app follows:\n" + serializedResponse,
                        parseException);
                }

                if (structuredResponse?.decodeSuccess == null)
                {
                    throw new Exception("Hosted app failed to decode the event: " + structuredResponse.decodeError);
                }

                var completeHttpResponseResponse =
                    structuredResponse?.decodeSuccess
                    ?.FirstOrDefault(response => response?.completeHttpResponse?.httpRequestId == httpRequestId);

                if (completeHttpResponseResponse != null)
                {
                    context.Response.StatusCode = completeHttpResponseResponse.completeHttpResponse.response.statusCode;
                    await context.Response.WriteAsync(completeHttpResponseResponse.completeHttpResponse.response?.bodyAsString ?? "");
                }
                else
                {
                    throw new NotImplementedException("Async HTTP Response is not implemented yet.");
                }

                System.Threading.Thread.MemoryBarrier();
                var cyclicReductionStoreLastAge = currentDateTime - cyclicReductionStoreLastTime;

                if (!(cyclicReductionStoreLastAge?.TotalSeconds < cyclicReductionStoreDistanceSeconds))
                {
                    if (System.Threading.Monitor.TryEnter(cyclicReductionStoreLock))
                    {
                        try
                        {
                            var afterLockCyclicReductionStoreLastAge = currentDateTime - cyclicReductionStoreLastTime;

                            if (afterLockCyclicReductionStoreLastAge?.TotalSeconds < cyclicReductionStoreDistanceSeconds)
                                return;

                            var reductionRecord = persistentProcess.ReductionRecordForCurrentState();

                            lock (processStoreWriter)
                            {
                                processStoreWriter.StoreReduction(reductionRecord);
                            }

                            cyclicReductionStoreLastTime = currentDateTime;
                            System.Threading.Thread.MemoryBarrier();
                        }
                        finally
                        {
                            System.Threading.Monitor.Exit(cyclicReductionStoreLock);
                        }
                    }
                }
            });
        }

        static Newtonsoft.Json.JsonSerializerSettings jsonSerializerSettings = new Newtonsoft.Json.JsonSerializerSettings
        {
            DefaultValueHandling = Newtonsoft.Json.DefaultValueHandling.Ignore,
        };

        static InterfaceToHost.HttpRequestEvent AsPersistentProcessInterfaceHttpRequestEvent(
            HttpContext httpContext,
            string httpRequestId)
        {
            return new InterfaceToHost.HttpRequestEvent
            {
                httpRequestId = httpRequestId,

                requestContext = new InterfaceToHost.HttpRequestContext
                {
                    clientAddress = httpContext.Connection.RemoteIpAddress?.ToString(),
                },

                request = new InterfaceToHost.HttpRequest
                {
                    method = httpContext.Request.Method,
                    uri = httpContext.Request.GetDisplayUrl(),
                    bodyAsString = new System.IO.StreamReader(httpContext.Request.Body).ReadToEnd(),
                }
            };
        }
    }
}
