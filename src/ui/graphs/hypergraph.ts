import { Graph } from 'graphlib';
import { enumerate } from '../infra/itertools';


type HypernodeId = number;
type Hyperedge = {
    op: string,
    target: HypernodeId,
    sources: HypernodeId[]
}


namespace Hypergraph {

    const STYLES = {
        hypernode: {
            class: 'hypernode',
            shape: 'rect',
            margin: "0.05,0", width: "0.1", height: "0.1",
            pad: "0.05"
        },
        hyperedge: {
            class: 'hyperedge',
            shape: 'ellipse',
            margin: "0.05,0", width: "0.1", height: "0.1",
            pad: "0.05"
        }
    }

    export function toGraph(edges: Hyperedge[]) {
        var g = new Graph();
        for (let [i, e] of enumerate(edges)) {
            var eid = `e${i}`;
            g.setNode(eid, {label: e.op, ...STYLES.hyperedge});
            g.setEdge(eid, nodeHelper(g, e.target), {arrowhead: "normal"});
            for (let u of e.sources)
                g.setEdge(nodeHelper(g, u), eid, {arrowhead: "none"});
        }

        return g;
    }

    function nodeHelper(g: Graph, id: number) {
        var u = `${id}`;
        if (!g.node(u))
            g.setNode(u, {label: id, ...STYLES.hypernode});
        return u;
    }

    export function exportToCpp(edges: Hyperedge[]) {
        return edges.map(exportEdgeToCpp).join('\n') + '\n';
    }

    export function exportEdgeToCpp(e: Hyperedge) {
        return `${e.op} ${[e.target, ...e.sources].join(' ')}`;
    }
}



export { Hypergraph, HypernodeId, Hyperedge }