using Pine.Core.Json;

namespace ElmTime.Gui;

[System.Text.Json.Serialization.JsonConverter(typeof(JsonConverterForChoiceType))]
public abstract record MessageToHost
{
    public record ReadAdminInterfaceConfigRequest : MessageToHost;
}
