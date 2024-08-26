using EmmyLua.CodeAnalysis.Compilation.Analyzer.FlowAnalyzer.ControlFlow;
using EmmyLua.CodeAnalysis.Compilation.Analyzer.ResolveAnalyzer;
using EmmyLua.CodeAnalysis.Compilation.Search;
using EmmyLua.CodeAnalysis.Document;
using EmmyLua.CodeAnalysis.Syntax.Node;
using EmmyLua.CodeAnalysis.Syntax.Node.SyntaxNodes;

namespace EmmyLua.CodeAnalysis.Compilation.Analyzer;

public class AnalyzeContext(List<LuaDocument> documents, SearchContext searchContext)
{
    public List<LuaDocument> LuaDocuments { get; } = documents;

    public SearchContext SearchContext { get; } = searchContext;

    public List<UnResolved> UnResolves { get; } = [];

    // TODO
    public List<InferenceUsage> UseInfers { get; } = [];

    public Dictionary<SyntaxElementId, ControlFlowGraph> ControlFlowGraphs { get; } = new();

    public ControlFlowGraph? GetControlFlowGraph(LuaBlockSyntax block)
    {
        return ControlFlowGraphs.GetValueOrDefault(block.UniqueId);
    }
}
