using System;
using System.Text.Json;

namespace native_tools;

public class EchoJson
{
    public static int EchoJsonLoop()
    {
        while (true)
        {
            var lineText = Console.ReadLine()?.Trim();

            if (string.IsNullOrEmpty(lineText))
            {
                continue;
            }

            try
            {
                var json = JsonSerializer.Deserialize<JsonElement>(lineText);
                Console.WriteLine(JsonSerializer.Serialize(json) + "\n");
            }
            catch (Exception ex)
            {
                Console.Error.WriteLine("Failed with runtime exception:\n" + ex);
                return 1;
            }
        }
    }
}
