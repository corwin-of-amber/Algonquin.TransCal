import { Grammar, Rule } from 'nearley';
import { Tree, preorderWalk, postorderWalk, treeClone } from './infra/tree';
import { PickLexer, PassThroughLexer, Token } from './syntax/lexer';
import { Ast, SpiralParser } from './syntax/parser';

import sexpParse from 's-expression';
import { enumerate } from './infra/itertools';
import { ViviMap } from './infra/collections';
import { Hyperedge, Hypergraph, HypernodeId } from './graphs/hypergraph';
import { RewriteRule } from './graphs/rewrites';



class SexpFrontend {
    edges: Hyperedge[] = []
    maxId = 0

    add(name: string, statements: string[]) {
        var exprs: CompiledSexp[] = [];
        for (let [i, stmt] of enumerate(statements)) {
            try {
                exprs.push(this.compile([stmt], true)[0]);
            }
            catch (e) {
                console.error(`(in ${name}:${i})`, e);
            }
        }
        this.incorporate(exprs);
        return exprs;
    }

    compile(sexps: (Sexp | string)[], incorporate = false): CompiledSexp[] {
        var asts = sexps.map(sexp => this.sexpToTree(sexp)),
            edges = [...this.edgesOf(asts, incorporate)];
        return asts.map(ast => ({ast, 
            edges: edges.filter(e => e.origin == ast),
            head: edges.find(e => e.node == ast)
        }));
    }

    asHypergraph() {
        return new Hypergraph(this.edges);
    }

    sexpToTree(sexp: Sexp | String | string): Ast<Token> {
        if (sexp instanceof String)
            return {type: sexp.valueOf(), subtrees: [], token: {type: '""', value: sexp.valueOf()}};
        sexp = SexpFrontend.sexpPromote(sexp);
        var type = sexp[0].toString(), /* should be a string */
            subtrees = sexp.slice(1).map(c =>
                this.sexpToTree(typeof c === 'string' ? [c] : c));
        return {type, subtrees};
    }

    astToText(ast: Ast<Token>) {
        if (ast.subtrees === undefined || ast.subtrees.length === 0)
            return ast.type;
        else
            return `(${ast.type} ${ast.subtrees.map(s => this.astToText(s)).join(' ')})`
    }

    incorporate(exprs: CompiledSexp[]) {
        for (let expr of exprs)
            this.edges.push(...expr.edges);
    }

    *edgesOf(forest: Ast<Token>[], incorporate = false):
                Generator<Hyperedge & {origin: Ast<Token>, node: Ast<Token>}> {
        // assign ids to nodes
        let idmap = generateIds(forest, incorporate ? this.maxId + 1 : 1);
        if (incorporate) this.maxId = Math.max(...idmap.values());

        for (let t of forest) {
            for (let node of preorderWalk(t)) {
                yield {
                    op: node.type,
                    target: idmap.get(node),
                    sources: node.subtrees.map(s => idmap.get(s)),
                    origin: t, node
                 };
            }
        }
    }

    static sexpPromote(sexp: Sexp | string): Sexp {
        return typeof sexp === 'string' ? sexpParse(sexp) : sexp;
    }
}

type Sexp = (string | String | Sexp)[];
type CompiledHyperedge = Hyperedge & {origin: Ast<Token>, node: Ast<Token>};
type CompiledSexp = {
    ast: Ast<Token>,
    edges: CompiledHyperedge[],
    head: CompiledHyperedge
};


class RuleProcessor {

    edgesToRule(fromEdges: CompiledHyperedge[], toEdges: CompiledHyperedge[], name?: string) {
        let vars = this.getVars([...fromEdges, ...toEdges]),
            rule = [this.edgesToRuleSide(fromEdges, vars),
                    this.edgesToRuleSide(toEdges, vars)];
                    
        this.compactIdsInplace([].concat(...rule));
        let varIds = new Map([...vars.entries()].map(([k, v]) => [k, v[0]]));
        return new RewriteRule(name ?? '<rule>', varIds, rule[0], rule[1]);
    }

    edgesToRuleSide(edges: CompiledHyperedge[], vars = this.getVars(edges)) {
        return edges.map(e =>
            (e.sources.length === 0 && vars.has(e.op)) ? undefined
                : this.remapVars(e, vars)
        ).filter(x => x);
    }

    getVars(edges: CompiledHyperedge[]) {
        var vars = new ViviMap<string, HypernodeId[]>().withFactory(() => []);
        for (let e of edges) {
            if (e.sources.length === 0 && this.isVar(e))
                vars.get(e.op).push(e.target);
        }
        return vars;
    }

    remapVars(e: Hyperedge, vars: Map<string, HypernodeId[]>) {
        let canonize = (hnid: HypernodeId) => {
            for (let v of vars.values())
                if (v.includes(hnid)) return v[0];
            return hnid;
        }
        return {...e, 
            target: canonize(e.target),
            sources: e.sources.map(canonize)
        };
    }

    compactIdsInplace(es: Hyperedge[]) {
        var defrag = [],
            visit = (hnid: HypernodeId) => defrag.includes(hnid) || defrag.push(hnid),
            recall = (hnid: HypernodeId) => defrag.indexOf(hnid) + 1;
        for (let e of es) 
            [e.target, ...e.sources].forEach(visit);
        
        for (let e of es) {
            e.target = recall(e.target);
            e.sources = e.sources.map(recall);
        }
        return es;
    }

    isVar(symbol: string | CompiledHyperedge) {
        if (typeof symbol === 'string')
            return /^\w+$/.test(symbol);
        else
            return this.isVar(symbol.op) && symbol.node.token?.type != '""';
    }
}


class VernacFrontend {
    passes: flexiparse.Pass[]
    asts: Ast<SpiralParser.Token>[] = []
    rules: RewriteRule[] = []
    labeled = new Map<string, HypernodeId>()

    sexpFe = new SexpFrontend
    rp = new RuleProcessor

    constructor() {
        var lex = new PickLexer({'(': '\\(', ')': '\\)', '""': '"[^"]*"'});
        var layer1 = new SpiralParser(Object.assign(
            new Grammar([
                new Rule('S', ['LA']),
                new Rule('LA', []), new Rule('LA', ['A', 'LA']),
                new Rule('A', [{type: '_'}]),
                new Rule('A', ['()']),
                new Rule('A', [{type: '""'}]),
                new Rule('()', [{type: '('}, 'S', {type: ')'}])
            ]), {grouped: ['S', '()']}), {lexer: lex});
    
        lex = new PickLexer({'rewrite': 'rewrite', ':-': ':-', '->': '->'});
        var layer2 = new SpiralParser(
            SpiralParser.compile(`
                S -> RW | ASRT
                RW -> "rewrite" NM "()" "->" "()"
                NM -> "\\"\\"" | null
                ASRT -> LBL ":-" "()"
                LBL -> "_" | null
            `, {autokens: true}, {grouped: ['RW', 'ASRT', 'NM', 'LBL']}),
            {lexer: new PassThroughLexer(lex)}
        );

        this.passes = [
            new flexiparse.Pass(layer1, []), 
            new flexiparse.Pass(layer2, ['S']).with({deep: false})
        ];
    }

    add(name: string, statements: string[]) {
        for (let stmt of statements) {
            let ast = this.passes[0].parser.parse([stmt]);
            this.asts.push(treeClone(ast));
            try {
                ast = this.passes[1].reprocess(ast);
                this.asts.push(ast);
                let s = ast.subtrees;
                switch (ast.type) {
                case 'RW': 
                    this.addRewrite(s[2], s[4],
                        s[1].subtrees ? s[1].subtrees[0].token.inner.token.value : undefined);
                    break;
                case 'ASRT':
                    let [u] = this.sexpFe.add(name, [Ast.toText(s[2])]);
                    if (s[0].subtrees[0])
                        this.addLabel(s[0].subtrees[0].token.value, u.head.target);
                    break;
                }
            }
            catch (e) { console.error(`(in ${name}, statement '${stmt}')`, e); }
        }
    }

    addRewrite(from: Ast, to: Ast, name?: string) {
        console.log('----', Ast.toText(from), Ast.toText(to))
        //this.sexpFe.add('incoming rule', [Ast.toText(from), Ast.toText(to)]);
        var [cfrom, cto] = this.sexpFe.compile(
                [from, to].map(t => Ast.toText(t)));
        cto.head.target = cfrom.head.target;
        this.rules.push(this.rp.edgesToRule(cfrom.edges, cto.edges, name));
    }

    addLabel(lab: string, target: HypernodeId) {
        this.labeled.set(lab, target);
        this.sexpFe.edges.push({op: lab, target, sources: []});
    }
}


namespace flexiparse {

    export class Pass<Token extends SpiralParser.Token = SpiralParser.Token> {
        descend: (node: Ast<Token>) => Ast<Token>[] = node => node.subtrees
        deep = true

        constructor(
            public parser: SpiralParser, 
            public drill: string[]) {    
        }

        with(attrs: any) {
            Object.assign(this, attrs);  /** @todo typing */
            return this;
        }

        reprocess(ast: Ast<Token>) {
            var delayed = [];
            for (let node of preorderWalk(ast)) {
                if (this.drill.includes(node.type)) {
                    let subexpr = this.descend(node),
                        reast = this.reroot(this.parser.parse(subexpr));
                    // prune tree here and later attach `reast` instead
                    node.subtrees = [];
                    node.type = reast.type;
                    delayed.push(() => node.subtrees = reast.subtrees);
                }
            }
            // recursively reprocess
            for (let op of delayed) op();
            return ast;
        }
    
        reroot(ast: Ast<Token>) {
            for (let o of preorderWalk(ast)) {
                if (typeof o.token === 'object' && o.token.inner) {
                    let t = this.deep ? this.reprocess(o.token.inner as Ast<Token>)
                                      : o.token.inner;
                    o.type = o.token.inner.type;
                    o.subtrees = t.subtrees as Ast<Token>[];
                }
            }
            return ast;
        }
    }
}

function wip_flexiparse() {
    var lex;
    var lvl1 = {
        lex: lex = new PickLexer({'[': '\\[', ']': '\\]', '{': '{', '}': '}', '(': '\\(', ')': '\\)'}),
        pars: new SpiralParser(Object.assign(
            new Grammar([
            new Rule('E', []), new Rule('E', ['P', 'E']),
            new Rule('E', [{type: '_'}, 'E']),
            new Rule('P', ['[]']), new Rule('P', ['{}']), new Rule('P', ['()']),
            new Rule('[]', [{type: '['}, 'E', {type: ']'}]),
            new Rule('{}', [{type: '{'}, 'E', {type: '}'}]),
            new Rule('()', [{type: '('}, 'E', {type: ')'}])
        ]), {rigid: ['E'], grouped: ['E', 'P', '_']}), {lexer: lex})
    };

    var lvl2 = {
        lex: lex = new PickLexer({'::': '::', '++': '\\+\\+', '=': '='}),
        pars: new SpiralParser(Object.assign(
            SpiralParser.compile(`
                E -> S1
                S1 -> S1 ++ S2 | S1 + S2 | S2
                S2 -> Q "::" S2 | Q
                Q -> null | A Q
                A -> _ | "()" | "[]"
            `, {autokens: true}),
            {rigid: [], grouped: ['A', '_']}), {lexer: new PassThroughLexer(lex)})
    };

    var lvl3 = {
        lex: lex = new PickLexer({'id': '\\S+'}),
        pars: new SpiralParser(Object.assign(
            SpiralParser.compile(`
                E -> A | E A
                A -> id | "()" | "[]"
            `, {autokens: true}),
            {rigid: ['E'], grouped: ['A']}), {lexer: new PassThroughLexer(lex)})
    }

    //const input = 'x :: (xs ++ ys)';
    const input = 'S (xs s) ++ (x :: nil)';

    var ast1 = lvl1.pars.parse<SpiralParser.Token>([input]);
    console.log(ast1);

    function reprocess<Tok extends SpiralParser.Token>(
                ast: Ast<Tok>, parser, drill = ['E'],
                subexpr = (node: Ast<Tok>) => node.subtrees)
    {
        var delayed = [];
        for (let node of preorderWalk(ast)) {
            if (drill.includes(node.type)) {
                let reast = reroot(parser.parse(subexpr(node)), parser);
                // prune tree here and later attach `reast` instead
                node.subtrees = [];
                node.type = reast.type;
                delayed.push(() => node.subtrees = reast.subtrees as Ast<Tok>[]);
            }
        }
        // recursively reprocess
        for (let op of delayed) op();
        return ast;
    }
    
    function reroot<Tok extends SpiralParser.Token>(ast: Ast<Tok>, parser) {
        for (let o of preorderWalk(ast)) {
            if (typeof o.token === 'object' && o.token.inner) {
                o.type = o.token.inner.type;
                o.subtrees = reprocess(o.token.inner, parser).subtrees as Ast<Tok>[];
            }
        }
        return ast;
    }

    try {
        var ast2 = reprocess(ast1, lvl2.pars); //lvl2.pars.parse<SpiralParser.Token>(ast1.children);
        try {
            var ast3 = reprocess(ast2, lvl3.pars, ['_'],
                u => [(<any>u.token).value] /** @oops */);
            //expressionHypergraph([ast2]);
            return [ast3];
        }
        catch (e) {
            console.error(e);
            return [ast2];
        }
    }
    catch (e) {
        console.error(e);
        return [ast1];
    }
}

function expressionHypergraph<Tok extends Token>(forest: Ast<Tok>[]) {
    // assign ids to nodes
    let idmap = generateIds(forest);

    for (let t of forest) {
        for (let node of postorderWalk(t)) {
            switch (node.type) {
            case 'E':
                console.log(idmap.get(node), node.subtrees[1].token.type,
                    [0, 2].map(i => idmap.get(node.subtrees[i])));
                break;
            case '()':
                idmap.set(node, idmap.get(node.subtrees[1]));
                break;
            case '_':
                console.log(idmap.get(node), node.token.type);
                break;
            }
        }
    }
}

function generateIds<T>(forest: Tree<T>[], start = 0) {
    let idmap = new Map<Tree<T>, number>(), i = start;

    for (let t of forest)
        for (let node of preorderWalk(t))
            idmap.set(node, i++);

    return idmap;
}


export { SexpFrontend, VernacFrontend, wip_flexiparse }