import { Graph } from 'graphlib';
import dot from 'graphlib-dot';
import Viz from 'viz.js';


class GraphvizAdapter {

    viz = new Viz(<any>{ workerURL: '/node_modules/viz.js/full.render.js' })

    async render(graph: Graph) {
        let dotText = dot.write(graph);
        return await this.viz.renderSVGElement(dotText);
    }
}


export { GraphvizAdapter }