import assert from 'assert';
import { Graph } from 'graphlib';
import { enumerate } from '../infra/itertools';


type HypernodeId = number;
type Hyperedge = {
    op: string,
    target: HypernodeId,
    sources: HypernodeId[]
}


class Hypergraph {
    edges: Hyperedge[]

    constructor(edges: Hyperedge[] = []) {
        this.edges = edges;
    }

    merge(u: HypernodeId, ...vs: HypernodeId[]) {
        this.edges = this.edges.map(e =>
            Hypergraph.replaceInEdge(e, vs, u));
    }

    filterEdges(p: (e: Hyperedge) => boolean) {
        return new Hypergraph(this.edges.filter(p));
    }

    nodeCount() {
        return this.nodes().size;
    }

    nodes() {
        return new Set(Hypergraph.iterNodeOccurrences(this.edges));
    }

    toGraph() { return Hypergraph.toGraph(this.edges); }
    exportToCpp() { return Hypergraph.exportToCpp(this.edges); }
    static importFromCpp(s: string) {
        return new Hypergraph(Hypergraph.importEdgesFromCpp(s));
    }
}


namespace Hypergraph {

    const STYLES = {
        hypernode: {
            class: 'hypernode',
            shape: 'rect',
            margin: "0.05,0", width: "0.1", height: "0.1",
            pad: "0.05"
        },
        nucleus: (edge: Hyperedge) => ({
            class: 'hyperedge--nucleus' + (edge.sources.length == 0 ? ' nullary' : ''),
            label: edge.op,
            shape: 'ellipse',
            margin: "0.05,0", width: "0.1", height: "0.1",
            pad: "0.05",
            data: {edge, indegree: edge.sources.length}
        }),
        source: (i: number, arity: number) => ({
            class: 'hyperedge--source',
            ...arity > 1 ? {headlabel: i} : {},
            arrowhead: "none", labelfontsize: "10pt",
            data: {is: 'source', index: i, arity}
        }),
        target: (arity: number) => ({
            class: 'hyperedge--target' + (arity == 0 ? ' nullary' : ''),
            arrowhead: "normal" ,
            data: {is: 'target'}           
        })
    }

    export function toGraph(edges: Hyperedge[]) {
        var g = new Graph();
        for (let [i, e] of enumerate(edges)) {
            var eid = `e${i}`, k = e.sources.length;
            g.setNode(eid, STYLES.nucleus(e));
            g.setEdge(eid, nodeHelper(g, e.target), STYLES.target(k));
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

    export function *iterNodeOccurrences(edges: Iterable<Hyperedge>) {
        for (let e of edges) {
            yield e.target; yield* e.sources;
        }
    }

    export function exportToCpp(edges: Hyperedge[]) {
        return edges.map(exportEdgeToCpp).join('\n') + '\n';
    }

    export function exportEdgeToCpp(e: Hyperedge) {
        return `${e.op} ${[e.target, ...e.sources].join(' ')}`;
    }

    export function importEdgesFromCpp(text: string): Hyperedge[] {
        let lines = text.split('\n').map(s => s.trim())
                                    .filter(s => s && !s.startsWith('// '));
        return lines.map(ln => importEdgeFromCpp(ln));
    }

    export function importEdgeFromCpp(s: string): Hyperedge {
        let splt = s.split(/\s+/),
            ids = splt.slice(1).map(x => parseInt(x));
        assert(ids.every(x => typeof x === 'number'));
        return {op: splt[0], target: ids[0], sources: ids.slice(1)};
    }

    export function replaceInEdge(e: Hyperedge, oneOf: HypernodeId[], with_: HypernodeId) {
        let f = (u: HypernodeId) => oneOf.includes(u) ? with_ : u;
        return {...e, target: f(e.target), sources: e.sources.map(f)};
    }
}



export { Hypergraph, HypernodeId, Hyperedge }