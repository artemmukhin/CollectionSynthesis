#I @"../../YaccConstructor/Bin/Release/v40"

#r @"YC.Common.dll"
#r @"YC.GLLGenerator.dll"
#r @"YC.YardFrontend.dll"
#r @"YC.Conversions.dll"
#r @"YC.YardPrinter.dll"

open Yard.Generators.GLL
open Yard.Frontends.YardFrontend
open Yard.Core.Conversions.ExpandMeta 

module GLLAbstractParserSimpleTests =
    let fe = new YardFrontend()
    let gen = new GLL()
    let meta = new ExpandMeta()
    let printer = new Yard.Generators.YardPrinter.YardPrinter()

    let generate() = 
        let g = fe.ParseGrammar "CFG.yrd"
        let transformed = {g with grammar = meta.ConvertGrammar(g.grammar)}
        printfn "%A" <| printer.Generate(transformed,true)

GLLAbstractParserSimpleTests.generate()
