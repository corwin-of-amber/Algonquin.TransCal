import Vue from 'vue';
import Viz from 'viz.js';
import { Graph } from 'graphlib';

import { ref, reactive, computed } from 'vue';

// @ts-ignore
import Egraph from './ui/components/egraph.vue';
// @ts-ignore
import App from './ui/components/app.vue';

import { wip_flexiparse } from './ui/frontend';
import { GraphvizAdapter } from './ui/graphs/graphviz';


async function main() {
    var gref = ref(new Graph),
        app = Vue.createApp(App).mount(document.body);

    let ast = wip_flexiparse();

    let g = new Graph();
    g.setEdge('a', 'b', {label: "go"});

    let ga = new GraphvizAdapter();

    Object.assign(window, {app, Viz, GraphvizAdapter, g, ga, gref});
}


window.addEventListener('load', () => main());