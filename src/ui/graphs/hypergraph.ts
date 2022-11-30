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

    merge(us: HypernodeId[]) {
        if (us.length > 1)
            this.mergeInto(us[0], ...us.slice(1));
    }

    mergeInto(u: HypernodeId, ...vs: HypernodeId[]) {
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

    toGraph(format = new Hypergraph.GraphFormatting) { 
        return Hypergraph.toGraph(this.edges, format);
    }
    exportToCpp() { return Hypergraph.exportToCpp(this.edges); }
    static importFromCpp(s: string) {
        return new Hypergraph(Hypergraph.importEdgesFromCpp(s));
    }
}


namespace Hypergraph {

    export const STYLES = {
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

    export class GraphFormatting {
        create(): Graph { return new Graph(); }
        edgeTo(g: Graph, eid: string, edge: Hyperedge) {
            let k = edge.sources.length;
            g.setNode(eid, STYLES.nucleus(edge));
            g.setEdge(eid, this.nodeHelper(g, edge.target), STYLES.target(k));
        }
        edgeFrom(g: Graph, eid: string, edge: Hyperedge,
                 index: number, source: HypernodeId) {
            let k = edge.sources.length;
            g.setEdge(this.nodeHelper(g, source), eid, STYLES.source(index, k));
        }
        nodeHelper(g: Graph, id: number) {
            var u = `${id}`;
            if (!g.node(u))
                g.setNode(u, {label: id, ...STYLES.hypernode});
            return u;
        }
        cleanup(g: Graph) { }
    }

    export function toGraph(edges: Hyperedge[], format = new GraphFormatting) {
        let g = format.create();
        for (let [i, e] of enumerate(edges)) {
            var eid = `e${i}`
            format.edgeTo(g, eid, e);
            for (let [i, u] of enumerate(e.sources))
                format.edgeFrom(g, eid, e, i, u);
        }
        format.cleanup(g);
        return g;
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