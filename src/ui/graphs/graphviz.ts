import assert from 'assert';
import { Graph } from 'graphlib';
import dot from 'graphlib-dot';
import Viz from 'viz.js';

import './graphviz.css';


class GraphvizAdapter {

    viz = new Viz(<any>{ workerURL: '/node_modules/viz.js/full.render.js' })
    stylesheet: LayoutStylesheet = {}

    async render(graph: Graph) {
        if (this.stylesheet?.graph)
            this.setStyle(graph, this.stylesheet.graph);
        this.setIds(graph);
        let dotText = dot.write(graph);
        return new GraphvizSvg(graph,
            this.strip(await this.viz.renderSVGElement(dotText)));
    }

    setStyle(graph: Graph, props: object) {
        var gl = graph.graph() as any;
        gl = typeof gl === 'string' ? {label: gl} : gl as object;
        graph.setGraph({...gl, ...props});
    }

    /**
     * Sets `id` attributes defaulting to the graphlib ids of the nodes.
     * @param graph 
     */
    setIds(graph: Graph) {
        for (let u of graph.nodes()){
            graph.setNode(u, {id: u, ...graph.node(u)});
        }
    }

    /**
     * Removes inline styling -- allowing elements to be styled by CSS.
     * @param svg 
     * @returns 
     */
    strip<E extends SVGElement>(svg: E) {
        for (let el of svg.querySelectorAll('[stroke],[fill]')) {
            el.removeAttribute('stroke');
            el.removeAttribute('fill');
        }
        return svg;
    }
}

type LayoutStylesheet = {
    graph?: {ranksep?: number | string, nodesep?: number | string}
}

class GraphvizSvg {
    graph: Graph
    svg: SVGSVGElement

    constructor(graph: Graph, svg: SVGSVGElement) {
        this.graph = graph;
        this.svg = svg;
    }

    nodeFromElement(el: SVGElement) {
        let node = el.closest('.node');
        return node ? this.graph.node(node.id) : undefined;
    }

    elementFromNode(node: number | string | {id: string}) {
        let id = typeof node === 'object' ? node.id : node;
        assert(id);
        for (let {el, node} of this.iterNodeElements()) {
            if (node.id == id) return el;
        }
    }

    *iterNodeElements() {
        for (let el of this.svg.querySelectorAll('.node')) {
            let node = this.nodeFromElement(el as SVGElement);
            if (node) yield {el, node};
        }
    }
}


export { GraphvizAdapter, GraphvizSvg }