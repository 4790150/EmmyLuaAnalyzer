using EmmyLua.CodeAnalysis.Compilation.Analyzer.DeclarationAnalyzer;
using EmmyLua.CodeAnalysis.Compilation.Analyzer.ResolveAnalyzer;
using EmmyLua.CodeAnalysis.Compilation.Reference;
using EmmyLua.CodeAnalysis.Compilation.Search;
using EmmyLua.CodeAnalysis.Compilation.Symbol;
using EmmyLua.CodeAnalysis.Syntax.Node;
using EmmyLua.CodeAnalysis.Syntax.Node.SyntaxNodes;
using EmmyLua.CodeAnalysis.Type;
using System.Security.AccessControl;
using System.Xml.Linq;

namespace EmmyLua.CodeAnalysis.Compilation.Analyzer.TypeAnalyzer
{
    public class TypeAnalyzer : LuaAnalyzer
    {
        private SearchContext? searchContext;

        public TypeAnalyzer(LuaCompilation compilation)
            : base(compilation, "Type")
        {
            //searchContext = new SearchContext(Compilation, new SearchContextFeatures() { Cache = false });
        }

        public override void Analyze(AnalyzeContext analyzeContext)
        {
            searchContext = analyzeContext.SearchContext;
            foreach (var document in analyzeContext.LuaDocuments)
            {
                AnalyzeNode(document.SyntaxTree.SyntaxRoot, document.SyntaxTree);
            }
        }

        public bool MatchType(LuaType? leftType, LuaType? rightType)
        {
            if (null == leftType)return false;
            if (null == rightType)return false;
            if (null == searchContext) return false;

            if (Builtin.Any.IsSameType(leftType, searchContext))
                return true;
            if (Builtin.Number.IsSameType(leftType, searchContext) && Builtin.Integer.IsSameType(rightType, searchContext))
                return true;

            if (leftType.IsSameType(rightType, searchContext))
                return true;
            if (rightType.SubTypeOf(leftType, searchContext))
                return true;

            return false;
        }

        private void AnalyzeNode(LuaSyntaxNode node, Syntax.Tree.LuaSyntaxTree syntaxTree)
        {
            switch (node)
            {
                case LuaLocalStatSyntax localStatSyntax:
                    {
                        AnalyzeLocalStat(localStatSyntax);
                        break;
                    }
                case LuaForRangeStatSyntax forRangeStatSyntax:
                    {
                        AnalyzeForRangeStat(forRangeStatSyntax);
                        break;
                    }
                case LuaForStatSyntax forStatSyntax:
                    {
                        AnalyzeForStat(forStatSyntax);
                        break;
                    }
                case LuaFuncStatSyntax funcStatSyntax:
                    {
                        AnalyzeMethod(funcStatSyntax);
                        break;
                    }
                case LuaClosureExprSyntax closureExprSyntax:
                    {
                        AnalyzeClosureExpr(closureExprSyntax);
                        break;
                    }
                case LuaAssignStatSyntax assignStatSyntax:
                    {
                        AnalyzeAssignStat(assignStatSyntax);
                        break;
                    }
                case LuaDocTagClassSyntax tagClassSyntax:
                    {
                        AnalyzeTagClass(tagClassSyntax);
                        break;
                    }
                case LuaDocTagAliasSyntax tagAliasSyntax:
                    {
                        AnalyzeTagAlias(tagAliasSyntax);
                        break;
                    }
                case LuaDocTagEnumSyntax tagEnumSyntax:
                    {
                        AnalyzeTagEnum(tagEnumSyntax);
                        break;
                    }
                case LuaDocTagInterfaceSyntax tagInterfaceSyntax:
                    {
                        AnalyzeTagInterface(tagInterfaceSyntax);
                        break;
                    }
                case LuaDocTagTypeSyntax typeSyntax:
                    {
                        AnalyzeTagType(typeSyntax);
                        break;
                    }
                case LuaTableExprSyntax tableSyntax:
                    {
                        AnalyzeTableExpr(tableSyntax);
                        break;
                    }
                case LuaDocTableTypeSyntax tableTypeSyntax:
                    {
                        AnalyzeLuaTableType(tableTypeSyntax);
                        break;
                    }
                case LuaSourceSyntax sourceSyntax:
                    {
                        AnalyzeSource(sourceSyntax);
                        break;
                    }
                case LuaNameExprSyntax nameExpr:
                    {
                        AnalyzeNameExpr(nameExpr);
                        break;
                    }
                case LuaIndexExprSyntax indexExpr:
                    {
                        IndexIndexExpr(indexExpr);
                        break;
                    }
                case LuaDocNameTypeSyntax docNameType:
                    {
                        IndexDocNameType(docNameType);
                        break;
                    }
                case LuaDocTagMetaSyntax:
                    {
                        AnalyzeMeta();
                        break;
                    }
                case LuaDocTagDiagnosticSyntax diagnosticSyntax:
                    {
                        AnalyzeTagDiagnostic(diagnosticSyntax);
                        break;
                    }
                case LuaDocTagModuleSyntax moduleSyntax:
                    {
                        AnalyzeTagModule(moduleSyntax);
                        break;
                    }
                case LuaDocTagDeprecatedSyntax deprecatedSyntax:
                    {
                        AnalyzeSimpleTag(deprecatedSyntax);
                        break;
                    }
                case LuaDocTagNodiscardSyntax nodiscardSyntax:
                    {
                        AnalyzeSimpleTag(nodiscardSyntax);
                        break;
                    }
                case LuaDocTagAsyncSyntax asyncSyntax:
                    {
                        AnalyzeSimpleTag(asyncSyntax);
                        break;
                    }
                case LuaDocTagGenericSyntax genericSyntax:
                    {
                        AnalyzeSimpleTag(genericSyntax);
                        break;
                    }
                case LuaDocTagParamSyntax paramSyntax:
                    {
                        AnalyzeSimpleTag(paramSyntax);
                        break;
                    }
                case LuaDocTagReturnSyntax returnSyntax:
                    {
                        AnalyzeSimpleTag(returnSyntax);
                        break;
                    }
                case LuaDocTagSeeSyntax seeSyntax:
                    {
                        AnalyzeSimpleTag(seeSyntax);
                        break;
                    }
                case LuaDocTagAsSyntax asSyntax:
                    {
                        AnalyzeSimpleTag(asSyntax);
                        break;
                    }
                case LuaDocTagVisibilitySyntax visibilitySyntax:
                    {
                        AnalyzeSimpleTag(visibilitySyntax);
                        break;
                    }
                case LuaDocTagVersionSyntax versionSyntax:
                    {
                        AnalyzeSimpleTag(versionSyntax);
                        break;
                    }
                case LuaDocTagOverloadSyntax overloadSyntax:
                    {
                        AnalyzeSimpleTag(overloadSyntax);
                        break;
                    }
                case LuaDocTagMappingSyntax mappingSyntax:
                    {
                        AnalyzeSimpleTag(mappingSyntax);
                        break;
                    }
                case LuaDocTagNamespaceSyntax namespaceSyntax:
                    {
                        AnalyzeTagNamespace(namespaceSyntax);
                        break;
                    }
                case LuaDocTagUsingSyntax usingSyntax:
                    {
                        AnalyzeTagUsing(usingSyntax);
                        break;
                    }
                case LuaDocTagSourceSyntax sourceSyntax:
                    {
                        AnalyzeTagSource(sourceSyntax);
                        break;
                    }
                case LuaCallExprSyntax callExprSyntax:
                    {
                        AnalyzeCallExpr(callExprSyntax);
                        break;
                    }
            }

            foreach (var child in node.ChildrenNode)
            {
                AnalyzeNode(child, syntaxTree);
            }
        }


        private void AnalyzeCallExpr(LuaCallExprSyntax callExprSyntax)
        {
            if (null == searchContext)
                return;
            if (null == callExprSyntax.ArgList)
                return;

            var fun = callExprSyntax.PrefixExpr;
            var funType = searchContext.Infer(fun);
            if (funType is LuaMethodType methodType)
            {
                var argTypes = new List<LuaType>();
                if (fun is LuaIndexExprSyntax indexExpr && indexExpr.IsColonIndex)
                {
                    var argSelf = searchContext.Infer(indexExpr.PrefixExpr);
                    argTypes.Add(argSelf);
                }
                foreach (var arg in callExprSyntax.ArgList.ArgList)
                {
                    argTypes.Add(searchContext.Infer(arg));
                }

                var parameterSymbols = new List<LuaSymbol>();
                if (methodType.MainSignature.Self != null)
                {
                    parameterSymbols.Add(methodType.MainSignature.Self);
                }
                parameterSymbols.AddRange(methodType.MainSignature.Parameters);

                var paramTypes = new List<LuaType?>();
                if (methodType is LuaGenericMethodType genericMethodType)
                {
                    Dictionary<string, LuaType> genericTypes = new Dictionary<string, LuaType>();

                    for (int i = 0; i < parameterSymbols.Count; i++)
                    {
                        if (parameterSymbols[i].Type is LuaNamedType nameType)
                        {
                            if (genericMethodType.GenericParams.ContainsKey(nameType.Name))
                            {
                                if (i < argTypes.Count)
                                {
                                    var argType = argTypes[i];
                                    paramTypes.Add(argType);

                                    if (genericTypes.TryGetValue(nameType.Name, out var genericType) && genericType != argType)
                                    {
                                        callExprSyntax.Tree.PushDiagnostic(new Diagnostics.Diagnostic(Diagnostics.DiagnosticSeverity.Error,
                                            Diagnostics.DiagnosticCode.TypeNotFound,
                                            $"The type arguments for method '{callExprSyntax.Name}' cannot be inferred from the usage.Try specifying the type arguments explicitly.",
                                            callExprSyntax.Range));
                                        continue;
                                    }

                                    genericTypes[nameType.Name] = argType;
                                    continue;
                                }
                            }
                        }

                        paramTypes.Add(parameterSymbols[i].Type);
                    }
                }
                else
                {
                    for (int i = 0; i < parameterSymbols.Count; i++)
                    {
                        paramTypes.Add(parameterSymbols[i].Type);
                    }
                }


                if (argTypes.Count > paramTypes.Count)
                {
                    callExprSyntax.Tree.PushDiagnostic(new Diagnostics.Diagnostic(Diagnostics.DiagnosticSeverity.Error,
                        Diagnostics.DiagnosticCode.Unused,
                        $"method {callExprSyntax.Name} don't takes {argTypes.Count} arguments",
                        callExprSyntax.Range));
                }

                int indexParam = 0;
                for (int indexArg = 0; indexArg < argTypes.Count; indexArg++)
                {
                    if (indexParam < paramTypes.Count)
                    {
                        ParamMatch(paramTypes[indexParam], argTypes[indexArg], callExprSyntax, methodType);
                    }
                    indexParam++;
                }
            }
        }

        private void ParamMatch(LuaType? parameterType, LuaType? argType, LuaCallExprSyntax callExprSyntax, LuaMethodType methodType)
        {
            if (!MatchType(parameterType, argType))
            {
                callExprSyntax?.Tree.PushDiagnostic(new Diagnostics.Diagnostic(Diagnostics.DiagnosticSeverity.Error,
                    Diagnostics.DiagnosticCode.TypeNotMatch,
                    $"TypeNotMatch {parameterType} = {argType}",
                    callExprSyntax.Range));
            }
        }

        private void AnalyzeTagSource(LuaDocTagSourceSyntax sourceSyntax)
        {
            
        }

        private void AnalyzeTagUsing(LuaDocTagUsingSyntax usingSyntax)
        {
            
        }

        private void AnalyzeTagNamespace(LuaDocTagNamespaceSyntax namespaceSyntax)
        {
            
        }

        private void AnalyzeSimpleTag(LuaDocTagSyntax tagSyntax)
        {
            
        }

        private void AnalyzeTagModule(LuaDocTagModuleSyntax moduleSyntax)
        {
            
        }

        private void AnalyzeTagDiagnostic(LuaDocTagDiagnosticSyntax diagnosticSyntax)
        {
            
        }

        private void AnalyzeMeta()
        {
            
        }

        private void IndexDocNameType(LuaDocNameTypeSyntax docNameType)
        {
            
        }

        private void IndexIndexExpr(LuaIndexExprSyntax indexExpr)
        {
            
        }

        private void AnalyzeNameExpr(LuaNameExprSyntax nameExpr)
        {
            
        }

        private void AnalyzeSource(LuaSourceSyntax sourceSyntax)
        {
            
        }

        private void AnalyzeLuaTableType(LuaDocTableTypeSyntax tableTypeSyntax)
        {
            
        }

        private void AnalyzeTableExpr(LuaTableExprSyntax tableSyntax)
        {
            
        }

        private void AnalyzeTagType(LuaDocTagTypeSyntax typeSyntax)
        {
            
        }

        private void AnalyzeTagInterface(LuaDocTagInterfaceSyntax tagInterfaceSyntax)
        {
            
        }

        private void AnalyzeTagEnum(LuaDocTagEnumSyntax tagEnumSyntax)
        {
            
        }

        private void AnalyzeTagAlias(LuaDocTagAliasSyntax tagAliasSyntax)
        {
            
        }

        private void AnalyzeTagClass(LuaDocTagClassSyntax tagClassSyntax)
        {
            
        }

        private void AnalyzeAssignStat(LuaAssignStatSyntax assignStatSyntax)
        {
            if (null == searchContext)
                return;

            var varList = assignStatSyntax.VarList.ToList();
            var exprList = assignStatSyntax.ExprList.ToList();
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

                if (varExpr is not null && expr is not null)
                {
                    var varExprType = searchContext.Infer(varExpr);
                    var exprType = searchContext.Infer(expr);
                    if (varExprType is not null && exprType is not null)
                    {
                        if (!MatchType(varExprType, exprType))
                        {
                            assignStatSyntax.Tree.PushDiagnostic(new Diagnostics.Diagnostic(Diagnostics.DiagnosticSeverity.Error,
                                Diagnostics.DiagnosticCode.TypeNotMatch,
                                $"TypeNotMatch {varExprType} = {exprType}",
                                assignStatSyntax.Range));
                        }
                    }
                }
            }
        }

        private void AnalyzeClosureExpr(LuaClosureExprSyntax closureExprSyntax)
        {
            
        }

        private void AnalyzeMethod(LuaFuncStatSyntax luaFuncStat)
        {
            switch (luaFuncStat)
            {
                case { IsLocal: true, LocalName.Name: { } name, ClosureExpr: { } closureExpr }:
                    {
                        var declaration = new LuaSymbol(
                            name.RepresentText,
                            null,
                            new MethodInfo(
                                new(luaFuncStat.LocalName),
                                new(luaFuncStat)
                            ),
                            SymbolFeature.Local
                        );

                        break;
                    }
                case { IsLocal: false, NameExpr: { Name: { } name2 } nameExpr, ClosureExpr: { } closureExpr }:
                    {


                        break;
                    }
                case { IsMethod: true, IndexExpr: { } indexExpr, ClosureExpr: { } closureExpr }:
                    {

                        var paramList = closureExpr.ParamList;
                        break;
                    }
            }
        }

        private void AnalyzeForStat(LuaForStatSyntax forStatSyntax)
        {
            
        }

        private void AnalyzeForRangeStat(LuaForRangeStatSyntax forRangeStatSyntax)
        {
            
        }

        private void AnalyzeLocalStat(LuaLocalStatSyntax localStatSyntax)
        {
            if (null == searchContext)
                return;

            var nameList = localStatSyntax.NameList.ToList();
            var exprList = localStatSyntax.ExprList.ToList();
            LuaExprSyntax? lastValidExpr = null;
            var count = nameList.Count;
            var retId = 0;
            for (var i = 0; i < count; i++)
            {
                var localName = nameList[i];
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

                if (localName is not null && expr is not null)
                {
                    //var leftSymbol = searchContext.FindDeclaration(localName);
                    //var localNameType = leftSymbol == null ? null : leftSymbol.Type;
                    var localNameType = searchContext.Infer(localName);
                    var exprType = searchContext.Infer(expr);
                    if (localNameType is not null && exprType is not null)
                    {
                        if (!MatchType(localNameType, exprType))
                        {
                            localStatSyntax.Tree.PushDiagnostic(new Diagnostics.Diagnostic(Diagnostics.DiagnosticSeverity.Error,
                                Diagnostics.DiagnosticCode.TypeNotMatch,
                                $"TypeNotMatch {localNameType} = {exprType}",
                                localStatSyntax.Range));
                        }
                    }
                }
            }
        }
    }
}
