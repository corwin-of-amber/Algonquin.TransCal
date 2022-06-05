import { Graph } from 'graphlib';
import { Hypergraph, Hyperedge, HypernodeId } from './hypergraph';


class EGraph extends Hypergraph {
    colors = new EGraph.ColorScheme

    filterEdges(p: (e: Hyperedge) => boolean): EGraph {
        return new EGraph(super.filterEdges(p).edges);
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
        return this.filterEdges(e => !EGraph.COLOR_OPS.includes(e.op));
    }

    toGraph() {
        let g = Hypergraph.toGraph(this.edges);
        this.colors.applyToGraph(g);
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
    }

    export type ColorGroup = {
        color: HypernodeId
        members: HypernodeId[]
    }

    export type Palette = Map<HypernodeId, ColorInfo>
    export type ColorInfo = {name: string, cssValue?: string}

}


export { EGraph }