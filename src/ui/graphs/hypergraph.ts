import assert from 'assert';
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
        nucleus: {
            class: 'hyperedge--nucleus',
            shape: 'ellipse',
            margin: "0.05,0", width: "0.1", height: "0.1",
            pad: "0.05"
        },
        source: (i: number, k: number) => ({
            class: 'hyperedge--source',
            ...k > 1 ? {headlabel: i} : {},
            data: {is: 'source', index: i, outOf: k},
            arrowhead: "none", labelfontsize: "10pt"
        }),
        target: {
            class: 'hyperedge--target',
            arrowhead: "normal" ,
            data: {is: 'target'}           
        }
    }

    export function toGraph(edges: Hyperedge[]) {
        var g = new Graph();
        for (let [i, e] of enumerate(edges)) {
            var eid = `e${i}`, k = e.sources.length;
            g.setNode(eid, {label: e.op, ...STYLES.nucleus});
            g.setEdge(eid, nodeHelper(g, e.target), STYLES.target);
            for (let [i, u] of enumerate(e.sources))
                g.setEdge(nodeHelper(g, u), eid, STYLES.source(i, k));
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

    export function importFromCpp(text: string): Hyperedge[] {
        let lines = text.split('\n').map(s => s.trim()).filter(s => s);
        return lines.map(ln => importEdgeFromCpp(ln));
    }

    export function importEdgeFromCpp(s: string): Hyperedge {
        let splt = s.split(/\s+/),
            ids = splt.slice(1).map(x => parseInt(x));
        assert(ids.every(x => typeof x === 'number'));
        return {op: splt[0], target: ids[0], sources: ids.slice(1)};
    }
}



export { Hypergraph, HypernodeId, Hyperedge }