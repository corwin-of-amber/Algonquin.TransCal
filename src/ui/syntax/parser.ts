import assert from 'assert';
import nearley from "nearley";
import Compile from 'nearley/lib/compile';
import { Token } from './lexer';



interface Ast<Tok = nearley.Token> {
    type: string;
    children: Ast<Tok>[];
    range?: CodeRange;
    text?: string;
    token?: Tok;
}

interface CodeRange<P = number> {
    start: P;
    end: P;
}

class Parser extends nearley.Parser {
    initial: any;

    constructor(grammar: nearley.Grammar, options?: nearley.ParserOptions) {
        super(Parser.prepare(grammar), options);
        this.initial = this.save();
    }

    static prepare(grammar: nearley.Grammar & {Rigid?: string[]}) {
        var rigid = grammar.Rigid || [];
        for (const rule of grammar.rules) {
            rule.postprocess = rigid.includes(rule.name)
                ? (data: any[]) => this.unfold(data, rule.name)
                : rule.symbols.length === 1
                    ? (data: any[]) => data[0]
                    : (data: any[]) => Object.assign(data, { type: rule.name });
        }
        return grammar;
    }

    parse<Tok>(program: string[]) {
        this.restart();
        for (let chunk of program)
            this.feed(chunk);
        return this.end<Tok>();
    }

    restart() {
        this.restore(this.initial);
    }

    end<Tok>() {
        // For non-ambigious grammar, this is what we what
        // See: https://nearley.js.org/docs/parser#a-note-on-ambiguity
        var ast = this.results[0];
        if (!ast) throw new Error("syntax error");
        return this.setRanges(this.toTree<Tok>(ast));
    }

    toTree<Tok>(ast: any): Ast<Tok> {
        if (ast.value) {  /* token */
            return {
                type: ast.type,
                token: ast,
                children: null,
                range: ast.range,
            };
        } else if (Array.isArray(ast)) {
            return {
                type: (<any>ast).type,
                children: ast.map(s => this.toTree<Tok>(s))
            };
        }
        else assert(false, 'malformed parse tree');
    }

    setRanges<Tok>(ast: Ast<Tok>, offset = 0) {
        for (let s of ast.children || []) {
            this.setRanges(s, offset);
            offset = s.range.end;
        }
        if (!ast.range) {
            ast.range = this.joinRanges((ast.children || []).map(s => s.range), offset);
        }
        return ast;
    }

    private joinRanges(rngs: CodeRange[], pos?: number): CodeRange {
        let p = {start: pos, end: pos};
        for (let {start, end} of rngs) {
            if (p.start === undefined || start < p.start) p.start = start;
            if (p.end === undefined || end > p.end) p.end = end;
        }
        return p.start === undefined ? undefined : p;
    }

    static unfold(data: any[], type: string) {
        function* iter() {
            for (const d of data) {
                if (d.type === type) yield* d;
                else yield d;
            }
        }
        return Object.assign([...iter()], { type });
    }

    static compile(grammar: string, opts: CompileOptions = {}) {
        var compiled = Parser.nearleyc(grammar);
        if (opts.autokens) {
            for (let rule of compiled.rules) {
                rule.symbols = rule.symbols.map(s =>
                    typeof s === 'string' && !(s in compiled.byName) ? 
                    {type: s} : s)
            }
        }
        return compiled;
    }

    static nearleyc(grammar: string) {
        var parserGrammar = nearley.Grammar.fromCompiled(require('nearley/lib/nearley-language-bootstrapped.js'));
        var p = new nearley.Parser(parserGrammar);
        p.feed(grammar); p.feed('\n');
        var compiled = Compile(p.results[0], {})
        return nearley.Grammar.fromCompiled({
            ParserStart: compiled.start,
            ParserRules: compiled.rules
        });
    }
}

type CompileOptions = {
    autokens?: boolean   // automatically turn all terminals into tokens
}


class SpiralParser extends Parser {
    input = new Rope

    parse<Tok>(program: (string | Token)[]) {
        return super.parse<Tok>(<any>program);  // @oops
    }

    feed(chunk: string | Token) {
        this.input.push(chunk);
        return super.feed(<any>chunk);  // @oops
    }

    end<Tok>() {
        var t = super.end<Tok>();
        console.log('(*)', JSON.parse(JSON.stringify(t)));
        return this.innerFiller(t, {start: 0, end: this.input.length + 1});
    }

    innerFiller<Tok>(ast: Ast<Tok>, range: CodeRange): Ast<Tok> {
        if (ast.children) {
            var absorb = (ast.type == 'E') || ast.type == 'S1' || (ast.type == 'Q');
            var acc = [],
                pos = absorb ? range.start : ast.range.start,
                end = absorb ? range.end : ast.range.end,
                push = (i: Generator<string | {}>) => {
                    for (let x of i) acc.push(this._promote(x));
                };

            for (let i = 0; i < ast.children.length; i++) {
                var c = ast.children[i], d = ast.children[i + 1];
                c = this.innerFiller(c, {start: pos, end: d ? d.range.start : end});
                if (pos >= 0 && c.range.start > pos) {
                    push(this.input.slice(pos, c.range.start));
                    if (pos < ast.range.start) ast.range.start = pos;
                }

                acc.push(c);
                pos = c.range.end;
            }

            if (absorb) {
                push(this.input.slice(pos, range.end));
                if (range.end > ast.range.end) ast.range.end = range.end;
            }

            ast.children = acc;
        }
        return ast;
    }

    _promote<Tok>(data: string | {}): Ast<Tok> {
        return typeof data === 'string' ? this._textNode(data) : <Ast<Tok>>data;
    }

    _textNode(text: string) {
        return {type: '_', text, children: null};
    }
}


/**
 * A silly rope with only `push` and `slice`.
 */
class Rope {
    chunks: (string | {})[] = []

    push(chunk: string | {}) {
        this.chunks.push(chunk);
    }

    get length() {
        return this.chunks.map(c => typeof c === 'string' ? c.length : 0)
                          .reduce((a, b) => a + b);
    }

    *slice(start: number, end: number = Infinity) {
        var pos = 0;
        for (let c of this.chunks) {
            if (typeof c === 'string') {
                var s = Math.max(0, start - pos),
                    e = Math.min(c.length, end - pos);
                if (s < e) yield c.slice(s, e);
                pos += c.length;
            }
            else if (start < pos && pos < end) {
                yield c;
            }
        }
    }
}


export { Parser, SpiralParser };
export type { Ast, CodeRange };
