import { Graph } from 'graphlib';

// @ts-ignore
import Egraph from './components/egraph.vue';
import { createComponent } from './infra/vue-dev';

import { SexpFrontend, VernacFrontend, wip_flexiparse } from './frontend';
import { Ast } from './syntax/parser';
import { forestToGraph } from './infra/tree';


function astsToGraph(forest: Ast<any>[]) {
    return forestToGraph(forest, node => ({label: node.type}));
}

async function main() {
    var app = createComponent<{egraph: Graph}>(Egraph)
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
        'rewrite (rev (:: x xs)) -> (:+ (rev xs) x)'
    ]);
    vfe.add('prog', [
        ':- (rev (:+ (:: x xs) y))'
    ]);

    console.log(vfe.asts);
    //app.egraph = astsToGraph(vfe.asts);
    let g = vfe.sexpFe.asGraph();
    g.setGraph({ranksep: 0.3, nodesep: 0.4});
    app.egraph = g;

    Object.assign(window, {app, g});
}


window.addEventListener('load', () => main());