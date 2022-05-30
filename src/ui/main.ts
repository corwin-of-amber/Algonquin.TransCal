import { Graph } from 'graphlib';

// @ts-ignore
import Egraph from './components/egraph.vue';
import { createComponent } from './infra/vue-dev';

import { SexpFrontend, VernacFrontend, wip_flexiparse } from './frontend';
import { Ast } from './syntax/parser';
import { forestToGraph } from './infra/tree';
import { Hypergraph } from './graphs/hypergraph';
import { Backend } from './graphs/backend-interface';


function astsToGraph(forest: Ast<any>[]) {
    return forestToGraph(forest, node => ({label: node.type}));
}

async function main() {
    var app = createComponent<{egraph: Graph, layoutStylesheet: any}>(Egraph)
                .mount(document.body);

    /*
    let ast = wip_flexiparse() as Ast<any>[],
        g = astsToGraph(ast);
    */

    let fe = new SexpFrontend;
    fe.add('inline', ['(+ (S n) m)', '(+ n (S m))'])

    //app.egraph = fe.asGraph();

    let vfe = new VernacFrontend;
    vfe.add('rules', [
        'rewrite ":+ []" (:+ [] x) -> (:: x [])',
        'rewrite ":+ ::" (:+ (:: x xs) y) -> (:: x (:+ xs y))',
        'rewrite "rev []" (rev []) -> ([])',
        'rewrite "rev ::" (rev (:: x xs)) -> (:+ (rev xs) x)'
    ]);
    vfe.add('prog', [
        'u1 :- (rev (:+ l y))',
        'u2 :- (:: y (rev l))',
        's :- (:: x xs)',
        'v1 :- (rev (:+ xs y))',
        'v2 :- (:: y (rev xs))',
        'rose :- (?~ l s)',
        'rose :- (?~ v1 v2)'
        //'l :- (s)',
        //'v1 :- (v2)'
    ]);

    app.layoutStylesheet = {
        graph: {rankdir: "BT", ranksep: 0.3, nodesep: 0.4}
    };

    //app.egraph = astsToGraph(vfe.asts);

    var g = vfe.sexpFe.asHypergraph(),
        [u1, u2] = ['u1', 'u2'].map(u => vfe.labeled.get(u));
    //if (u1 && u2) g.merge(u1, u2);
    //if (u1 && u2) g.edges.push({op: '?->', target: 100, sources: [u1, u2]});
    app.egraph = g.toGraph();
    

    var be = new Backend;
    be.writeProblem({input: g.edges, rules: vfe.rules});
    var g = await be.solve();

    app.egraph = g.toGraph();

    Object.assign(window, {app, vfe});
}


window.addEventListener('load', () => main());