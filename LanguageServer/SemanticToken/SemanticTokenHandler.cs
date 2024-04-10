﻿using LanguageServer.Server;
using LanguageServer.Util;
using OmniSharp.Extensions.LanguageServer.Protocol.Client.Capabilities;
using OmniSharp.Extensions.LanguageServer.Protocol.Document;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;

namespace LanguageServer.SemanticToken;

// ReSharper disable once ClassNeverInstantiated.Global
public class SemanticTokenHandler(ServerContext context) : SemanticTokensHandlerBase
{
    private SemanticTokensAnalyzer Analyzer { get; } = new();

    protected override SemanticTokensRegistrationOptions CreateRegistrationOptions(SemanticTokensCapability capability,
        ClientCapabilities clientCapabilities)
    {
        return new()
        {
            DocumentSelector = ToSelector.ToTextDocumentSelector(context.LuaWorkspace),
            Legend = Analyzer.Legend,
            Range = true,
            Full = true,
        };
    }

    protected override Task Tokenize(SemanticTokensBuilder builder, ITextDocumentIdentifierParams identifier,
        CancellationToken cancellationToken)
    {
        context.ReadyRead(() =>
        {
            var uri = identifier.TextDocument.Uri.ToUnencodedString();
            var semanticModel = context.LuaWorkspace.Compilation.GetSemanticModel(uri);
            if (semanticModel is not null)
            {
                Analyzer.Tokenize(builder, semanticModel, cancellationToken);
            }
        });
        
        return Task.CompletedTask;
    }

    protected override Task<SemanticTokensDocument> GetSemanticTokensDocument(ITextDocumentIdentifierParams @params,
        CancellationToken cancellationToken)
    {
        return Task.FromResult(new SemanticTokensDocument(Analyzer.Legend));
    }
}