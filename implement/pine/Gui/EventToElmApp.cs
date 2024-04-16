using System.Collections.Generic;

namespace ElmTime.Gui;


[System.Text.Json.Serialization.JsonConverter(typeof(Pine.Json.JsonConverterForChoiceType))]
public abstract record EventToElmApp
{
    public record ReadAdminInterfaceConfigEvent(AdminInterfaceConfig Config)
        : EventToElmApp;
}

public record AdminInterfaceConfig(
    string elmTimeVersionId,
    IReadOnlyList<HttpRoute> httpRoutes,
    IReadOnlyList<StateShim.InterfaceToHost.NamedExposedFunction> databaseFunctions);

public record HttpRoute(
    string path,
    IReadOnlyList<string> methods);
