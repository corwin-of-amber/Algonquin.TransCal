import { Graph } from 'graphlib';
import dot from 'graphlib-dot';
import Viz from 'viz.js';


class GraphvizAdapter {

    viz = new Viz(<any>{ workerURL: '/node_modules/viz.js/full.render.js' })
    stylesheet: LayoutStylesheet = {}

    async render(graph: Graph) {
        if (this.stylesheet?.graph)
            this.setStyle(graph, this.stylesheet.graph);
        this.setIds(graph);
        let dotText = dot.write(graph);
        return this.strip(await this.viz.renderSVGElement(dotText));
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
    strip(svg: SVGElement) {
        svg.removeAttribute('stroke');
        svg.removeAttribute('fill');
        for (let c of svg.children) this.strip(c as SVGElement);
        return svg;
    }
}

type LayoutStylesheet = {
    graph?: {ranksep?: number | string, nodesep?: number | string}
}


export { GraphvizAdapter }