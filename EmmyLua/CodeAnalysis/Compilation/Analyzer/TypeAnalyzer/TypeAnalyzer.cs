using EmmyLua.CodeAnalysis.Compilation.Analyzer.ResolveAnalyzer;
using EmmyLua.CodeAnalysis.Compilation.Search;
using EmmyLua.CodeAnalysis.Syntax.Node;
using EmmyLua.CodeAnalysis.Syntax.Node.SyntaxNodes;

namespace EmmyLua.CodeAnalysis.Compilation.Analyzer.TypeAnalyzer
{
    public class TypeAnalyzer(LuaCompilation compilation) : LuaAnalyzer(compilation, "Type")
    {
        public override void Analyze(AnalyzeContext analyzeContext)
        {
            var searchContext = new SearchContext(Compilation, new SearchContextFeatures() { Cache = false });
            foreach (var document in analyzeContext.LuaDocuments)
            {
                AnalyzeNode(document.SyntaxTree.SyntaxRoot, document.SyntaxTree);
            }
        }

        private void AnalyzeNode(LuaSyntaxNode node, Syntax.Tree.LuaSyntaxTree syntaxTree)
        {
            switch (node.Kind)
            {
                case Syntax.Kind.LuaSyntaxKind.AssignStat:
                    {
                        if (node is LuaAssignStatSyntax luaAssignStat)
                        {
                            var varList = luaAssignStat.VarList.ToList();
                            var exprList = luaAssignStat.ExprList.ToList();
                            LuaExprSyntax? lastValidExpr = null;
                            var retId = 0;
                            var count = varList.Count;
                            for (var i = 0; i < count; i++)
                            {
                                var varExpr = varList[i];
                                var expr = exprList.ElementAtOrDefault(i);
                                if (expr is not null)
                                {
                                    lastValidExpr = expr;
                                    retId = 0;
                                }
                                else
                                {
                                    retId++;
                                }

                                LuaExprRef? relatedExpr = null;
                                if (lastValidExpr is not null)
                                {
                                    relatedExpr = new LuaExprRef(lastValidExpr, retId);
                                }

                                if (varExpr is not null && expr is not null)
                                {
                                    var varDeclaration = Compilation.Db.QueryLocalDeclaration(varExpr);
                                    var exprDeclaration = Compilation.Db.QueryLocalDeclaration(expr);
                                    if (varDeclaration  is not null && exprDeclaration is not null)
                                    {
                                        if (varDeclaration.Type != exprDeclaration.Type)
                                        {
                                            syntaxTree.PushDiagnostic(new Diagnostics.Diagnostic(Diagnostics.DiagnosticSeverity.Error,
                                                Diagnostics.DiagnosticCode.TypeNotMatch,
                                                $"TypeNotMatch {varExpr} = {expr}",
                                                node.Range));
                                        }
                                    }
                                }
                            }
                        }
                        break;
                    }
            }

            foreach (var child in node.ChildrenNode)
            {
                AnalyzeNode(child, syntaxTree);
            }
        }
    }
}
