﻿using System.Text;
using EmmyLua.CodeAnalysis.Compilation.Analyzer.Declaration;
using EmmyLua.CodeAnalysis.Compilation.Analyzer.Infer;
using EmmyLua.CodeAnalysis.Syntax.Node.SyntaxNodes;

namespace EmmyLua.CodeAnalysis.Compilation.Type;

public class LuaMethod(
    Signature mainSignature,
    List<Signature>? overloadSignatures = null,
    List<Declaration>? genericParameters = null)
    : LuaType(TypeKind.Method)
{
    public Signature MainSignature { get; } = mainSignature;

    public List<Declaration>? GenericParameters { get; } = genericParameters;

    public List<Signature>? OverloadSignatures { get; } = overloadSignatures;

    public ILuaType? SelfType { get; internal set; }

    public void ProcessSignature(Func<Signature, bool> process, SearchContext context)
    {
        if (!process(MainSignature))
        {
            return;
        }

        if (OverloadSignatures is not null)
        {
            foreach (var signature in OverloadSignatures)
            {
                if (!process(signature))
                {
                    break;
                }
            }
        }
    }

    private static int MatchCount(List<Declaration> parameters, List<LuaExprSyntax> arguments, SearchContext context)
    {
        var matched = 0;
        for (; matched < parameters.Count; matched++)
        {
            if (arguments.Count <= matched)
            {
                return matched;
            }

            var arg = arguments.ElementAtOrDefault(matched);
            var param = parameters[matched];
            if (param.Type is { } type)
            {
                if (!type.AcceptExpr(arg, context))
                {
                    return matched;
                }
            }
        }

        return matched;
    }

    public Signature FindPerfectSignature(
        LuaCallExprSyntax callExpr,
        SearchContext context)
    {
        var isColonCall = false;
        if (callExpr.PrefixExpr is LuaIndexExprSyntax indexExpr)
        {
            isColonCall = indexExpr.IsColonIndex;
        }

        var args = callExpr.ArgList?.ArgList.ToList() ?? [];
        var perfectSignature = MainSignature;
        var perfectCount = 0;
        ProcessSignature(signature =>
        {
            var count = 0;
            var isColonDefine = signature.ColonDefine;
            switch ((isColonCall, isColonDefine))
            {
                case (true, false):
                {
                    if (signature.Parameters.FirstOrDefault() is { Name: "self" })
                    {
                        count++;
                        count += MatchCount(signature.Parameters.Skip(1).ToList(), args, context);
                    }

                    break;
                }
                case (false, true):
                {
                    var declarations = new List<Declaration> { new VirtualDeclaration("self", SelfType) };
                    declarations.AddRange(signature.Parameters);
                    count += MatchCount(declarations, args, context);
                    break;
                }
                case (true, true):
                {
                    count++;
                    break;
                }
            }

            if (count > perfectCount)
            {
                perfectSignature = signature;
                perfectCount = count;
            }

            return true;
        }, context);

        return perfectSignature;
    }

    public override IEnumerable<Declaration> GetMembers(SearchContext context) => Enumerable.Empty<Declaration>();

    public override bool SubTypeOf(ILuaType other, SearchContext context)
    {
        if (ReferenceEquals(this, other))
        {
            return true;
        }

        if (other is LuaMethod method)
        {
            if (MainSignature.SubTypeOf(method.MainSignature, context))
            {
                return true;
            }
        }

        return false;
    }
}

public class Signature(
    bool colon,
    List<Declaration> parameters,
    ILuaType? returnTypes)
{
    public bool ColonDefine { get; } = colon;

    public ILuaType? ReturnTypes { get; } = returnTypes;

    public List<Declaration> Parameters { get; } = parameters;

    public ILuaType? Variadic
    {
        get
        {
            if (Parameters.LastOrDefault() is { Name: "..." } lastParam)
            {
                return lastParam.FirstDeclaration.Type;
            }

            return null;
        }
    }

    public bool SubTypeOf(Signature other, SearchContext context)
    {
        if (ReferenceEquals(this, other))
        {
            return true;
        }

        if (other.ColonDefine != ColonDefine)
        {
            return false;
        }

        for (var i = 0; i < Parameters.Count; i++)
        {
            var luaType = Parameters[i].Type;
            var type = other.Parameters.ElementAtOrDefault(i)?.Type;
            if (type != null && luaType != null && !luaType.SubTypeOf(type, context))
            {
                return false;
            }
        }

        return true;
    }

    public string ToDisplayString(SearchContext context)
    {
        var sb = new StringBuilder();
        sb.Append("fun(");

        var first = true;
        if (ColonDefine)
        {
            sb.Append("self");
            first = false;
        }

        foreach (var parameter in Parameters)
        {
            if (!first)
            {
                sb.Append(", ");
            }

            first = false;
            sb.Append($"{parameter.Name}: {parameter.Type?.ToDisplayString(context) ?? "any"}");
        }

        sb.Append(")");
        if (ReturnTypes != null)
        {
            sb.Append("=> ");
            sb.Append(ReturnTypes.ToDisplayString(context));
        }

        return sb.ToString();
    }
}
