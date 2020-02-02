using System;

namespace demo_memory_leak
{
    class Program
    {
        static void Main(string[] args)
        {
            var numberOfEventsPerBatch = 200_000;

            var numberOfBatches = 15;

            Console.WriteLine("Time to set up monitoring/profiler. Press any key to continue");
            Console.ReadKey();

            void reportMemoryUse()
            {
                Console.WriteLine("Memory use: Process private memory / MB: " + (System.Diagnostics.Process.GetCurrentProcess().PrivateMemorySize64 / 1_000_000));
            }

            reportMemoryUse();

            var totalEvents = 0;

            var elmApp = Kalmit.PersistentProcess.Test.TestSetup.GetElmAppFromFilePath("./elm-app");

            using (var process = Kalmit.ProcessFromElm019Code.ProcessFromElmCodeFiles(
                elmCodeFiles: elmApp).process)
            {
                string lastEventResponse = null;

                for (var batchIndex = 0; batchIndex < numberOfBatches; ++batchIndex)
                {
                    var batchStopwatch = System.Diagnostics.Stopwatch.StartNew();

                    for (var i = 0; i < numberOfEventsPerBatch; ++i)
                    {
                        lastEventResponse = process.ProcessEvent("Event " + totalEvents.ToString());
                        ++totalEvents;
                    }

                    batchStopwatch.Stop();

                    Console.WriteLine(
                        "Completed batch " + batchIndex + " of " + numberOfBatches +
                        " in " + batchStopwatch.ElapsedMilliseconds + " ms, lastEventResponse: " + lastEventResponse);

                    reportMemoryUse();

                    Console.WriteLine("Press any key to continue.");
                    Console.ReadKey();
                }
            }

            reportMemoryUse();

            Console.WriteLine("Completed. Press any key to finish.");
            Console.ReadKey();
        }
    }
}
