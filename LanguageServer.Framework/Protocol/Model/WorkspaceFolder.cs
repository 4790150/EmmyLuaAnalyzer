﻿using System.Text.Json.Serialization;

namespace EmmyLua.LanguageServer.Framework.Protocol.Model;

public record struct WorkspaceFolder(string Uri, string Name)
{
    /**
     * The associated URI for this workspace folder.
     */
    [JsonPropertyName("uri")]
    public string Uri { get; } = Uri;

    /**
     * The name of the workspace folder. Used to refer to this workspace folder in the user interface.
     */
    [JsonPropertyName("name")]
    public string Name { get; } = Name;
}