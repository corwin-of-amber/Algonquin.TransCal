import { Grammar, Rule } from 'nearley';
import { Tree, preorderWalk, postorderWalk } from './infra/tree';
import { PickLexer, PassThroughLexer, Token } from './syntax/lexer';
import { Ast, SpiralParser } from './syntax/parser';

import sexpParse from 's-expression';
import { enumerate } from './infra/itertools';
import { Hyperedge, Hypergraph } from './graphs/hypergraph';



class SexpFrontend {
    edges: Hyperedge[] = []

    add(name: string, statements: string[]) {
        var exprs = [];
        for (let [i, stmt] of enumerate(statements)) {
            try {
                exprs.push(this.sexpToTree(sexpParse(stmt)));
            }
            catch (e) {
                console.error(`(in ${name}:${i})`, e);
            }
        }
        this.edges.push(...this.edgesOf(exprs));
    }

    asGraph() {
        return Hypergraph.toGraph(this.edges);
    }

    sexpToTree(sexp: Sexp): Ast<Token> {
        var type = sexp[0].toString(), /* should be a string */
            subtrees = sexp.slice(1).map(c =>
                this.sexpToTree(typeof c === 'string' ? [c] : c));
        return {type, subtrees}
    }

    *edgesOf(forest: Ast<Token>[]) {
        // assign ids to nodes
        let idmap = generateIds(forest);

        for (let t of forest) {
            for (let node of postorderWalk(t)) {
                yield {
                    op: node.type,
                    target: idmap.get(node),
                    sources: node.subtrees.map(s => idmap.get(s))
                 };
            }
        }
    }
}

type Sexp = (string | Sexp)[];


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


export { SexpFrontend, wip_flexiparse }