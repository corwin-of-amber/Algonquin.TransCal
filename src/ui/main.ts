import { Graph } from 'graphlib';

// @ts-ignore
import Egraph from './components/egraph.vue';
import { createComponent } from './infra/vue-dev';

import { wip_flexiparse } from './frontend';
import { Ast } from './syntax/parser';
import { treeToGraph } from './infra/tree';


async function main() {
    var app = createComponent<{egraph: Graph}>(Egraph)
                .mount(document.body);

    let ast = wip_flexiparse() as Ast<any>,
        g = treeToGraph(ast, node => ({label: node.type}));

    app.egraph = g;

    Object.assign(window, {app, ast, g});
}


window.addEventListener('load', () => main());