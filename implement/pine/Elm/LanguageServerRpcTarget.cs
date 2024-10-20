using Pine.Core.LanguageServerProtocol;
using StreamJsonRpc;

namespace Pine.Elm;

public record LanguageServerRpcTarget(LanguageServer Server)
{
    public static IJsonRpcMessageFormatter JsonRpcMessageFormatterDefault() =>
        new SystemTextJsonFormatter()
        {
            JsonSerializerOptions = new System.Text.Json.JsonSerializerOptions
            {
                PropertyNamingPolicy = System.Text.Json.JsonNamingPolicy.CamelCase,
            }
        };


    [JsonRpcMethod("initialize")]
    public InitializeResult Initialize(InitializeParams initializeParams)
    {
        return Server.Initialize(initializeParams);
    }
}
