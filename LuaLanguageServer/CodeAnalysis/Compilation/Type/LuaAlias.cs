﻿using LuaLanguageServer.CodeAnalysis.Compilation.Analyzer.Declaration;
using LuaLanguageServer.CodeAnalysis.Compilation.Analyzer.Infer;

namespace LuaLanguageServer.CodeAnalysis.Compilation.Type;

public class LuaAlias(string name, ILuaType baseType) : LuaType(TypeKind.Alias), ILuaNamedType
{
    public string Name { get; } = name;

    public ILuaType BaseType { get; } = baseType;

    public override IEnumerable<Declaration> GetMembers(SearchContext context)
    {
        return BaseType.GetMembers(context);
    }

    public override bool SubTypeOf(ILuaType other, SearchContext context)
    {
        return BaseType.SubTypeOf(other, context);
    }

    protected override ILuaType OnSubstitute(SearchContext context)
    {
        return BaseType.Substitute(context);
    }
}
