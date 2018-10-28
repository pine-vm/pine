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
            var config = services.BuildServiceProvider().GetService<IConfiguration>();
            var processStoreDirectory = config.GetValue<string>(Configuration.ProcessStoreDirectoryPathSettingKey);
            var processStore = new Kalmit.ProcessStore.ProcessStoreInFileDirectory(processStoreDirectory);

            services.AddSingleton<ProcessStore.IProcessStoreReader>(processStore);
            services.AddSingleton<ProcessStore.IProcessStoreWriter>(processStore);
            services.AddSingleton<IPersistentProcess>(BuildPersistentProcess);
        }

        static PersistentProcessWithHistoryOnFileFromElm019Code BuildPersistentProcess(IServiceProvider services)
        {
            var config = services.GetService<IConfiguration>();

            var elmAppFilePath = config.GetValue<string>(Configuration.ElmAppFilePathSettingKey);
            var elmAppFile = System.IO.File.ReadAllBytes(elmAppFilePath);

            return new PersistentProcessWithHistoryOnFileFromElm019Code(
                services.GetService<ProcessStore.IProcessStoreReader>(), elmAppFile);
        }

        public void Configure(
            IApplicationBuilder app,
            IHostingEnvironment env,
            ProcessStore.IProcessStoreWriter processStoreWriter,
            IPersistentProcess persistentProcess)
        {
            if (env.IsDevelopment())
            {
                app.UseDeveloperExceptionPage();
            }

            var nextHttpRequestIndex = 0;

            app.Run(async (context) =>
            {
                var timeMilli = DateTimeOffset.UtcNow.ToUnixTimeMilliseconds();
                var httpRequestIndex = System.Threading.Interlocked.Increment(ref nextHttpRequestIndex);

                var httpRequestId = timeMilli.ToString() + "-" + httpRequestIndex.ToString();

                var httpEvent = AsPersistentProcessInterfaceHttpRequestEvent(context, httpRequestId);

                var interfaceEvent = new InterfaceToHost.Event
                {
                    httpRequest = httpEvent,
                };

                var serializedInterfaceEvent = Newtonsoft.Json.JsonConvert.SerializeObject(interfaceEvent, jsonSerializerSettings);

                var (responses, compositionRecord) = persistentProcess.ProcessEvents(new[] { serializedInterfaceEvent });

                processStoreWriter.AppendSerializedCompositionRecord(compositionRecord.serializedCompositionRecord);

                var serializedResponse = responses.Single();

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
