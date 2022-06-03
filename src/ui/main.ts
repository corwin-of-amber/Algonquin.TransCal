import { Graph } from 'graphlib';

// @ts-ignore
import Egraph from './components/egraph.vue';
import { createComponent } from './infra/vue-dev';

import { SexpFrontend, VernacFrontend, wip_flexiparse } from './frontend';
import { Ast } from './syntax/parser';
import { forestToGraph } from './infra/tree';
import { Hypergraph } from './graphs/hypergraph';
import { Backend } from './graphs/backend-interface';

import { svgToPng } from './infra/gfx';
import { drawDocument } from 'rasterizehtml';


function astsToGraph(forest: Ast<any>[]) {
    return forestToGraph(forest, node => ({label: node.type}));
}

const SAMPLES = {
    'rev-snoc': [
        'u1 :- (rev (:+ l y))',
        'u2 :- (:: y (rev l))',
        's :- (:: x xs)',
        // -- induction l
        'v1 :- (rev (:+ xs y))',
        'v2 :- (:: y (rev xs))',
        'rose :- (?~ l s)',
        'rose :- (?~ v1 v2)'
    ],
    'plus-s': [
        'u1 :- (+ (S n) m)',
        'u2 :- (+ n (S m))',
        's :- (S k)',
        // -- induction n
        'v1 :- (+ (S k) m)',
        'v2 :- (+ k (S m))',
        'rose :- (?~ n s)',
        'rose :- (?~ v1 v2)'
    ],
    'plus-0': [
        'u1 :- (+ n 0)',
        'u2 :- (n)',
        'gold :- (?~ n 0)',
        'rose :- (?~ n (S k))',
        'rose :- (?~ (+ k 0) k)'
    ],
    'plus-comm': [
        'u1 :- (+ n m)',
        'u2 :- (+ m n)',
        's :- (S k)',
        // -- induction n
        'gold :- (?~ n 0)'
    ]
}

async function main() {
    var app = createComponent<{egraph: Graph, layoutStylesheet: any}>(Egraph)
                .mount(document.body);

    /*
    let ast = wip_flexiparse() as Ast<any>[],
        g = astsToGraph(ast);
    */

    //let fe = new SexpFrontend;
    //fe.add('inline', ['(+ (S n) m)', '(+ n (S m))'])

    //app.egraph = fe.asGraph();

    let vfe = new VernacFrontend;
    vfe.add('rules', [
        'rewrite ":+ []" (:+ [] x) -> (:: x [])',
        'rewrite ":+ ::" (:+ (:: x xs) y) -> (:: x (:+ xs y))',
        'rewrite "rev []" (rev []) -> ([])',
        'rewrite "rev ::" (rev (:: x xs)) -> (:+ (rev xs) x)',
        'rewrite "+ 0" (+ "0" m) -> (id m)',
        'rewrite "+ S" (+ (S n) m) -> (S (+ n m))',
        'rewrite "S +" (S (+ n m)) -> (+ (S n) m)'
    ]);
    
    const name = 'plus-0';

    vfe.add(name, SAMPLES[name]);

    app.layoutStylesheet = {
        graph: {rankdir: "BT", ranksep: 0.3, nodesep: 0.4}
    };

    //app.egraph = astsToGraph(vfe.asts);

    var input = vfe.sexpFe.asHypergraph(),
        [u1, u2] = ['u1', 'u2'].map(u => vfe.labeled.get(u));
    //if (u1 && u2) g.merge(u1, u2);
    app.egraph = input.toGraph();

    var be = new Backend;
    be.opts.rewriteDepth = 3;
    be.writeProblem({input: input.edges, rules: vfe.rules});
    var g = await be.solve();

    app.egraph = g.foldColorInfo().toGraph();

    Object.assign(window, {app, vfe, svgToPng, drawDocument});
}


window.addEventListener('load', () => main());