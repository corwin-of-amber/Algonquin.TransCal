import moo from "moo";



class SkippingLexer implements nearley.Lexer {
    lexer: moo.Lexer
    skip: Set<string>

    constructor(lexer: moo.Lexer) {
        this.lexer = lexer;
        this.skip = new Set(["WS", "COMMENT"]);
    }

    next() {
        do {
            var token = this.lexer.next();
            if (!(token != null && this.skip.has(token.type!))) return token;
        } while (true);
    }

    reset(chunk: any, info: any) {
        this.lexer.reset(chunk, info);
    }
    formatError(token: any, message?: string) {
        return this.lexer.formatError(token, message);
    }
    save() {
        return this.lexer.save();
    }
    has(name: string) {
        return this.lexer.has(name);
    }
}


class PickLexer implements nearley.Lexer {
    regexp: RegExp
    input: string
    chunkOffset: number
    iter: Iterator<Token, void>

    types: Map<string, string>

    constructor(tokendefs: {[type: string]: string}) {
        this.types = new Map(
            Object.keys(tokendefs).map((ty, i) => [`T${i}`, ty]));
        this.regexp = new RegExp(
            Object.values(tokendefs).map((re, i) => `(?<T${i}>${re})`).join('|'), 'g');
    }

    start(input: string) {
        this.input = input;
        this.iter = this._iter(input);
        return this;
    }

    *_iter(input: string): Generator<Token, void> {
        let index = 0;
        for (let mo of input.matchAll(this.regexp)) {
            for (let ttok of this._skip(input, index, mo.index))
                yield ttok;
            yield this.tokenOf(mo);
            index = mo.index + mo[0].length;
        }
        for (let ttok of this._skip(input, index, input.length))
            yield ttok;
    }

    _skip(input: string, from: number, to: number): Token[] {
        return to > from ?
            this._mktext(input.slice(from, to),
                         {start: from, end: to}) : [];
    }

    reset(chunk: string, state: PickLexer.State) {
        //if (this.input) 
        //    this.chunkOffset += this.input.length;
        //else
            this.chunkOffset = 0;
        this.start(chunk);
    }

    next(): Token {
        var next = this.iter.next();
        return next.done ? null : (next.value as Token);
    }

    formatError(e: any, msg: string) {
        return `${msg} at '${e.value}'`;
    }

    save(): PickLexer.State {
        /** @todo? */
        return {};
    }

    tokenOf(mo: RegExpMatchArray): Token {
        var type = this.kindOf(mo),
            range = {start: this.chunkOffset + mo.index,
                     end: this.chunkOffset + mo.index + mo[0].length};
        return {type, value: mo[0], range};
    }

    kindOf(mo: RegExpMatchArray) {
        for (let [k, v] of Object.entries(mo.groups)) {
            if (v) return this.types.get(k) ?? k;
        }
        /** assert(false); /**/
    }

    _mktext(text: string, range?: CodeRange): Token[] {
        text = text.trim();
        return text ? [{type: '_', value: text, range}] : [];
    }
}

namespace PickLexer {
    export interface State extends nearley.LexerState {
        //chunkOffset: number
    }
}



class PassThroughLexer implements nearley.Lexer {
    lexer: nearley.Lexer
    admit: Set<string>
    buf?: Token[]

    constructor(lexer: nearley.Lexer) {
        this.lexer = lexer;
        this.admit = new Set;
    }

    reset(data: string | Token, state?: nearley.LexerState): void {
        if (typeof data === 'string') {
            this.lexer.reset(data, state);
            this.buf = undefined;
        }
        else {
            this.buf = [data]; //this._filter([data]);
        }
    }
    next(): nearley.Token {
        return this.buf ? this.buf.shift() : this.lexer.next();
    }
    save(): nearley.LexerState {
        return this.lexer.save();
    }
    formatError(token: nearley.Token, message: string): string {
        return this.lexer.formatError(token, message);
    }

    _filter(tokens: Token[]) {
        return tokens.filter(tok => this.admit.has(tok.type));
    }
}


type Token = {type: string, range?: CodeRange} & nearley.Token
type CodeRange = {start: number, end: number}


/**
 * @deprecated
 */
class ParenthesisBalance {
    regexp: RegExp

    constructor(parens: Spec) {
        this.regexp = new RegExp(
            parens.map(({l,r}, i) => `(?<L${i}>${l})|(?<R${i}>${r})`).join('|'), 'g');
    }

    process(text: string) {
        var stack = [], buf = [], pos = 0;
        console.log(text);
        for (let mo of text.matchAll(this.regexp)) {
            if (mo.index > pos)
                buf.push({root: {kind: '_', text: text.slice(pos, mo.index)}})
            pos = mo.index + mo[0].length;
            
            var tok = this.tokenOf(mo);
            if (tok.kind.startsWith('L')) {
                stack.push({tok, startIdx: buf.length});
            }
            else if (tok.kind.startsWith('R')) {
                var open = stack.pop();
                if (!(open && this.matches(open.tok, tok)))
                    throw new Error('unbalanced parenthesis');
                buf.push({root: open.tok, subtrees: buf.splice(open.startIdx)});
            }
        }

        if (stack.length)
            throw new Error('unbalanced parenthesis');
        
        if (pos < text.length)
            buf.push({root: {kind: '_', text: text.slice(pos)}})

        return {root: {kind: '.'}, subtrees: buf};
    }

    tokenOf(mo: RegExpMatchArray) {
        var kind = this.kindOf(mo);
        return {kind};
    }

    kindOf(mo: RegExpMatchArray) {
        for (let [k, v] of Object.entries(mo.groups)) {
            if (v) return k;
        }
        /** assert(false); /**/
    }

    matches(open: {kind: string}, close: {kind: string}) {
        // assumes open is L.. and close is R..
        return open.kind.slice(1) == close.kind.slice(1);
    }
}


type Spec = PairSpec[];
type PairSpec = {l: string, r: string, scope?: string};


export { SkippingLexer, PickLexer, Token, PassThroughLexer, ParenthesisBalance }
