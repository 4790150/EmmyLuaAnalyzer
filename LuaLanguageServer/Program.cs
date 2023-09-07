﻿// using LuaLanguageServer.LanguageServer;

// var server = new LanguageServer();
// await server.StartAsync(args);

using System.Diagnostics;
using LuaLanguageServer.CodeAnalysis.Syntax.Tree;
using LuaLanguageServer.CodeAnalysis.Workspace;

var tree = LuaSyntaxTree.ParseText(
    """
    ---@enum b  you baby
    ---| "hhello" #hiljioio
    ---@class A #gvoejg
    ---@class B 哈哈哈
    ---@param b number number
    """);

Console.Write(tree.SyntaxRoot.DebugSyntaxInspect());
//
// 计算执行时间
// var sw = new Stopwatch();
// sw.Start();
// var w = LuaWorkspace.Create("");
//
// sw.Stop();
// Console.WriteLine($"耗时: {sw.ElapsedMilliseconds} ms");
