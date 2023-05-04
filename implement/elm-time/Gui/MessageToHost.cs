namespace ElmTime.Gui;

[System.Text.Json.Serialization.JsonConverter(typeof(Pine.Json.JsonConverterForChoiceType))]
public abstract record MessageToHost
{
    public record ReadAdminInterfaceConfigRequest : MessageToHost;
}
