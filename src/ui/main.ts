import { Graph } from 'graphlib';

// @ts-ignore
import Egraph from './components/egraph.vue';
import { createComponent } from './infra/vue-dev';

import { SexpFrontend, wip_flexiparse } from './frontend';
import { Ast } from './syntax/parser';
import { forestToGraph } from './infra/tree';


async function main() {
    var app = createComponent<{egraph: Graph}>(Egraph)
                .mount(document.body);

    let ast = wip_flexiparse() as Ast<any>[],
        g = forestToGraph(ast, node => ({label: node.type}));

    let fe = new SexpFrontend;
    fe.add('inline', ['(+ (S n) m)', '(+ n (S m))'])

    app.egraph = fe.asGraph();

    Object.assign(window, {app, ast, g});
}


window.addEventListener('load', () => main());