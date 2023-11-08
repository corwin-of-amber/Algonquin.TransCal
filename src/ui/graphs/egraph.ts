import assert from 'assert';
import _ from 'lodash';
import { Graph } from 'graphlib';
import { ifind } from '../infra/itertools';
import { Hypergraph, Hyperedge, HypernodeId } from './hypergraph';


class EGraph extends Hypergraph {
    colors: EGraph.ColorScheme
    ematch: EGraph.EMatch

    constructor(edges: Hyperedge[] = [], colors?: EGraph.ColorScheme) {
        super(edges);
        this.colors = colors ?? new EGraph.ColorScheme
        this.ematch = new EGraph.EMatch(this);
    }

    static fromHypergraph(g: Hypergraph) {
        let eg = new EGraph(g.edges);
        return eg;
    }

    /** Like `Hypergraph.merge`, but merge into smallest id */
    merge(us: HypernodeId[]) {
        let rep = _.min(us);
        super.merge([rep, ...us.filter(u => u != rep)]);
    }

    mergeInto(u: HypernodeId, ...vs: HypernodeId[]) {
        super.mergeInto(u, ...vs);
        this.colors.mergeInto(u, ...vs);
    }

    filterEdges(p: (e: Hyperedge) => boolean): EGraph {
        return new EGraph(super.filterEdges(p).edges, this.colors);
    }

    *edgesByOp(op: string): Generator<Hyperedge> {
        for (let e of this.edges)
            if (e.op === op) yield e;
    }

    congruenceClosureLeaves() {
        let byOp = new Map<string, Hyperedge[]>();
        for (let e of this.edges) {
            if (e.sources.length === 0) {
                let v = byOp.get(e.op) ?? [];
                v.push(e);
                byOp.set(e.op, v);
            }
        }
        for (let es of byOp.values()) {
            let targets = new Set(es.map(e => e.target));
            this.merge([...targets]);
        }
        return this.removeDuplicateEdges();
    }

    removeDuplicateEdges() {
        let hashcons = new Map<string, Hyperedge[]>();
        for (let e of this.edges) {
            let k = JSON.stringify([e.op, e.sources, e.target]),
                v = hashcons.get(k) ?? [];
            v.push(e);
            hashcons.set(k, v);
        }
        for (let es of hashcons.values()) {
            this.edges = this.edges.filter(e => es.indexOf(e) <= 0);
        }
        return this;
    }

    foldColorInfo() {
        this.extractColorInfo();
        let g = this.withoutColors();
        g.colors = this.colors;
        return g;
    }

    unfoldColorInfo() {
        return new EGraph([...this.edges, ...this.exportColorInfo()], this.colors);
    }

    extractColorInfo() {
        for (let e of this.edges) {
            switch (e.op) {
            case '?~': this.colors.merge(e.target, e.sources); break;
            case '?>': this.colors.declareParent(e.sources[0], e.target); break;
            case '?.': this.colors.add(e.target, e.sources[0]); break;  // deprecated
            }
        }
        // fill in color names
        for (let e of this.edges) {
            let pe: EGraph.ColorInfo
            if (e.sources.length === 0 &&
                (pe = this.colors.palette.get(e.target)) && pe.name === '?') {
                pe.name = e.op;
            }
        }
    }

    *exportColorInfo(): Generator<Hyperedge> {
        for (let e of this.colors.eclasses) {
            yield {op: '?~', target: e.color, sources: e.members} as Hyperedge;
        }
        for (let [id, clr] of this.colors.palette.entries()) {
            yield {op: clr.name, target: id, sources: []} as Hyperedge;
            if (clr.parent !== undefined)
                yield {op: '?>', target: clr.parent, sources: [id]} as Hyperedge;
        }
    }

    withoutColors() {
        return this.filterEdges(e => !EGraph.COLOR_OPS.includes(e.op)
            && !this.colors.palette.has(e.target));
    }

    collapseColors(...colors: (HypernodeId | string)[]) {
        for (let c of colors)
            this.colors.collapseBy(this, c);
    }

    toGraph(format = new EGraph.ClusteredFormatting): Graph {
        let g = super.toGraph(format);
        this.colors.applyToGraph(g);
        // Grouping is only available for the first color
        if (this.colors.lookup('clr#0') && g.isCompound())
            this.colors.groupBy(g, 'clr#0');
        return g;
    }


    static importFromCpp(s: string) {
        return new EGraph(Hypergraph.importEdgesFromCpp(s));
    }

    static COLOR_OPS = ['?.', '?~', '?>'];
}


namespace EGraph {

    export class ColorScheme {
        eclasses: ColorGroup[] = []  // vertices merged with color
        vertices: ColorGroup[] = []  // colored vertices
        palette: Palette = new Map

        /**
         * Creates a color merge (generates a new colored e-class if not present)
         */
        merge(color: HypernodeId, members: HypernodeId[]) {
            this.declareColor(color);
            let c = this.eclasses.filter(c =>
                c.color === color && members.some(u => c.members.includes(u)));
            if (c[0])  /** @todo c.length > 1 */
                c[0].members.push(...members.filter(u => !c[0].members.includes(u)));
            else
                this.eclasses.push({color, members});
        }

        /**
         * Responds to `mergeInto` being performed on the underlying egraph.
         * Updates all eclasses to reflect the rename.
         */
        mergeInto(u: HypernodeId, ...vs: HypernodeId[]) {
            let with_ = <T>(l: T[], u: T) => l.includes(u) ? l : l.concat([u]),
                without = <T>(l: T[], vs: T[]) => l.filter(u => !vs.includes(u));
                    
            for (let l of [this.eclasses, this.vertices])
                for (let cg of l)
                    if (vs.some(u => cg.members.includes(u)))
                        cg.members = with_(without(cg.members, vs), u);
            
            this.cleanup();  // remove singleton classes if occur
        }

        add(color: HypernodeId, node: HypernodeId) {
            this.declareColor(color);
            let c = this.vertices.find(g => g.color === color);
            if (c) c.members.push(node);
            else this.vertices.push({color, members: [node]});
        }

        declareColor(color: HypernodeId, info?: ColorInfo) {
            if (this.palette.has(color)) {
                if (info) this.palette.set(color, info);
            }
            else this.palette.set(color, {name: '?'});
        }

        declareParent(color: HypernodeId, parent: HypernodeId) {
            this.declareColor(color);
            let c = this.palette.get(color);
            assert(c !== undefined);
            c.parent = parent;
        }

        applyToGraph(g: Graph) {
            for (let c of this.vertices) {
                for (let u of c.members) {
                    let key = `${u}`, value = g.node(key);
                    if (value)
                        g.setNode(key, {...value, data: {...value.data, color: c.color}});
                }
            }
        }

        groupBy(g: Graph, color: HypernodeId | string | ColorInfo) {
            let [cid, info] = this.lookup(color) ?? oops(`no such color '${color}'`);
            for (let ec of this.eclasses) {
                if (ec.color === cid) {
                    let ecluster = `cluster_${info.name}_${ec.members[0]}`;
                    g.setNode(ecluster, {
                        class: `eclass--color`, 
                        data: {color: cid}, style: 'rounded'
                    });
                    for (let u of ec.members)
                        g.setParent(`cluster_${u}`, ecluster);
                }
            }
        }

        collapseBy(g: EGraph, color: HypernodeId | string | ColorInfo) {
            let [cid, info] = this.lookup(color) ?? oops(`no such color '${color}'`);
            for (let ec of this.eclasses) {
                if (ec.color === cid) {
                    g.merge(ec.members);
                }
            }
        }

        cleanup() {
            this.eclasses = this.eclasses.filter(c => c.members.length > 1);
        }

        lookup(color: HypernodeId | string | ColorInfo): [HypernodeId, ColorInfo] {
            switch (typeof color) {
            case 'number': return [color, this.palette.get(color)];
            case 'string': return this.byName(color);
            default:       return this.byInfo(color);
            }
        }

        byName(name: string) {
            return ifind(this.palette.entries(), ([k, v]) => v.name === name);
        }

        byInfo(info: ColorInfo) {
            return ifind(this.palette.entries(), ([k, v]) => v === info);
        }
    }

    export type ColorGroup = {
        color: HypernodeId
        members: HypernodeId[]
    }

    export type Palette = Map<HypernodeId, ColorInfo>
    export type ColorInfo = {name: string, parent?: HypernodeId}

    const STYLES = {
        cluster: {
            style: 'rounded',
            class: 'egraph--eclass'
        }
    }

    /**
     * Egg-style formatting with eclasses in clusters.
     */
    export class ClusteredFormatting extends Hypergraph.GraphFormatting {
        create(): Graph { 
            let g = new Graph({compound: true});
            g.setGraph({compound: true, clusterrank: 'local'});
            return g;
        }
        edgeTo(g: Graph, eid: string, edge: Hyperedge): void {
            g.setNode(eid, Hypergraph.STYLES.nucleus(edge));
            g.setParent(eid, this.nodeHelper(g, edge.target));
        }
        nodeHelper(g: Graph, id: number) {
            var u = `cluster_${id}`;  // the name prefix seems to be required?
            if (!g.node(u))
                g.setNode(u, {id: `${id}`, ...STYLES.cluster});
            return u;
        }
        cleanup(g: Graph) {
            // edges must point to nodes in graphviz, not clusters...
            let sing = [];
            for (let e of g.edges()) {
                let c = g.children(e.v), u0 = c?.[0];
                if (!u0) { console.warn(`cluster '${e.v}' has no children`); continue; }
                if (c.length == 1) sing.push(e.v);
                g.setEdge(g.children(e.v)[0], e.w, 
                    {...g.edge(e), ...c.length > 1 ? {ltail: e.v} : {}});
                g.removeEdge(e);
            }
            for (let u of sing) g.setNode(u, {...g.node(u),
                class: "singleton", pad: "0", margin: "1"});
        }
    }

    export class EMatch {
        g: EGraph

        constructor(g: EGraph) { this.g = g; }

        *singleEdge(op: string, target?: HypernodeId,
                    sources: HypernodeId[] = []): Generator<Hyperedge> {
            let compat = (u: HypernodeId, v: HypernodeId) =>
                            u === undefined || u == v;
            for (let e of this.g.edges) {
                if (e.op === op && compat(target, e.target) &&
                    e.sources.every((v, i) => compat(sources[i], v))) {
                    yield e;
                }
            }
        }
    
        *subgraph(pat: Hyperedge[], valuation: Map<HypernodeId, HypernodeId> = new Map) {
            if (pat.length === 0) {
                yield valuation;
            }
            else {
                let e0 = pat[0],
                    target = valuation.get(e0.target),
                    sources = e0.sources.map(u => valuation.get(u));
                for (let e of this.singleEdge(e0.op, target, sources)) {
                    let vt = new Map(valuation.entries());
                    vt.set(e0.target, e.target);
                    for (let [u0, u] of _.zip(e0.sources, e.sources))
                        vt.set(u0, u);
                
                    yield* this.subgraph(pat.slice(1), vt);
                }
            }
        }        
    }
}

function oops(msg: string): never {
    throw new Error(msg);
}


export { EGraph }