import fs from 'fs';
import path from 'path';
import * as commander from 'commander';
import { PassThroughLexer, PickLexer } from "../ui/syntax/lexer";
import { Ast, SpiralParser } from "../ui/syntax/parser";


class Collector {
    curly: SpiralParser

    constructor() {
        var lex = new PickLexer({'{': '{', '}': '}', ';': ';', '):': '\\)\\s*:',
                                 'cline': '//.*',
                                 'copen': '/\\*', 'cclose': '\\*/',
                                 'direct': '#.*'});
        var curly = new SpiralParser(
            SpiralParser.compile(`
                S -> null | A S
                A -> _ | ";" | "):" | cline | direct | copen C cclose | "{" S "}"
                C -> null | K C
                K -> _ | ";" | "):" | cline | direct | "{" | "}"
            `, {autokens: true}, {rigid: ['S', 'A'], grouped: ['S', 'A']}),
            {lexer: new PassThroughLexer(lex)}
        );

        this.curly = curly;
    }

    process(program: string) {
        var ast = this.curly.parse([program]);
        return [...this.findFunctionBodies(ast)];
    }

    *findFunctionBodies(ast: Ast<SpiralParser.Token>) {
        var decl = null, colon = null;
        for (let a of ast.subtrees) {
            switch (a.subtrees[0].type) {
            case '_':
                let val = a.subtrees[0].token.value,
                    mo = val.match(/^(class|struct|union)\s/);
                if (mo) {
                    console.log(val);
                    decl = 'class';
                }
                else decl = null;
                break;
            case '{':
                //console.log(Ast.toText(a.subtrees[1]));
                if (decl === 'class') {
                    //yield* this.findFunctionBodies(a.subtrees[1]);
                }
                else 
                    yield {curly: a, colon};
                decl = colon = null;
                break;
            case '):':
                colon = a;
                break;
            case ';':
                decl = colon = null;
                break;
            }
        }
        return ast;
    }
}


class HeaderExtraction {
    collector = new Collector
    readonly CUE = '__cppnator_header'

    process(name: string, program: string) {
        let df = this._mkguard(name);
        return `#ifndef ${df}\n#define ${df}\n#define ${this.CUE}\n\n` +
               `${this._process(program)}\n\n#endif\n`;
    }

    _process(program: string) {
        var redact = this.collector.process(program);
        return TextSource.interpolate(program, 
            redact.map(({curly, colon}) => ({
                start: colon ? colon.range.start + 1 : curly.range.start,
                end: curly.range.end,
                text: '/*redacted*/;'
            }))
        );
    }

    _mkguard(name: string) {
        let tok = path.basename(name).toUpperCase().replace(/[,.:+]/g, '_');
        return `${tok}_${this._mkuid()}_H`;
    }
    _mkuid() { return Date.now(); }
}


namespace TextSource {
    /**
     * Returns `true` if the spans share a line
     */
    export function areOnSameLine(inp: string,
                    at1: {start: number, end: number}, 
                    at2: {start: number, end: number}) {
        if (at2.start > at1.end) {
            return !inp.substring(at2.start, at1.end).includes('\n')
        }
        else return false; /** @todo */
    }
    /**
     * Utility function for replacing some elements within a source file.
     * @param inp source text
     * @param elements places to interpolate some text into
     */
    export function interpolate(inp: string, elements: {start: number, end: number, text: string}[]) {
        var out = '', i = 0;
        for (let el of elements.sort((a, b) => a.start - b.start)) {
            out += inp.substring(i, el.start) + el.text;
            i = el.end;
        }
        return out + inp.substring(i);
    }
}


function main() {
    var prog = new commander.Command()
        .argument('<infile>')
        .option('-o <outfile>', 'name of output file')
        .action((infile, opts) => { opts.infile = infile; })
        .parse();
        
    var opts = prog.opts();

    var hextract = new HeaderExtraction(),
    res = hextract.process(opts.infile, fs.readFileSync(opts.infile,'utf-8'));

    let outfile = opts.o ?? opts.infile.replace(/([.]cpp|)$/, '.h');
    fs.writeFileSync(outfile, res);
}

//"const int n; class A { A() : g(9) { }; void go() { some impl } /* hi */ void come() };");


//if (module.id === '.') {   // @todo detect main with kremlin
main();
//}
export { Collector, main }