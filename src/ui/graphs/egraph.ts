import { Graph } from 'graphlib';
import { ifind } from '../infra/itertools';
import { Hypergraph, Hyperedge, HypernodeId } from './hypergraph';


class EGraph extends Hypergraph {
    colors: EGraph.ColorScheme

    constructor(edges: Hyperedge[] = [], colors?: EGraph.ColorScheme) {
        super(edges);
        this.colors = colors ?? new EGraph.ColorScheme
    }

    static fromHypergraph(g: Hypergraph) {
        let eg = new EGraph(g.edges);
        return eg;
    }

    filterEdges(p: (e: Hyperedge) => boolean): EGraph {
        return new EGraph(super.filterEdges(p).edges, this.colors);
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

    extractColorInfo() {
        for (let e of this.edges) {
            switch (e.op) {
            case '?~': this.colors.merge(e.target, e.sources); break;
            case '?.': this.colors.add(e.target, e.sources[0]); break;
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

    withoutColors() {
        return this.filterEdges(e => !EGraph.COLOR_OPS.includes(e.op)
            && !this.colors.palette.has(e.target));
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

    static COLOR_OPS = ['?.', '?~'];
}


namespace EGraph {

    export class ColorScheme {
        eclasses: ColorGroup[] = []  // vertices merged with color
        vertices: ColorGroup[] = []  // colored vertices
        palette: Palette = new Map

        merge(color: HypernodeId, members: HypernodeId[]) {
            this.declareColor(color);
            let c = this.eclasses.filter(c =>
                c.color === color && members.some(u => c.members.includes(u)));
            if (c[0])  /** @todo c.length > 1 */
                c[0].members.push(...members.filter(u => !c[0].members.includes(u)));
            else
                this.eclasses.push({color, members});
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
    export type ColorInfo = {name: string, cssValue?: string}

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
}

function oops(msg: string): never {
    throw new Error(msg);
}


export { EGraph }