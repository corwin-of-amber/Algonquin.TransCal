import { Hypergraph, Hyperedge, HypernodeId } from './hypergraph';


class RewriteRule {
    name: string
    vars: Map<string, HypernodeId>
    from: Hyperedge[]
    to: Hyperedge[]

    constructor(name: string, vars: Map<string, HypernodeId>, from: Hyperedge[], to: Hyperedge[]) {
        this.name = name;
        this.vars = vars;
        this.from = from;
        this.to = to;
    }

    exportToCpp() {
        let ees = (es: Hyperedge[]) =>
            es.map(e => Hypergraph.exportEdgeToCpp(e)).join('\n')
        return `${this.name}\n${ees(this.from)}\n\n${ees(this.to)}\n`;
    }
}


/**
 * Like RewriteRule, but with only the search part (`from`).
 */
class Pattern {
    vars: Map<string, HypernodeId>
    edges: Hyperedge[]
    head: HypernodeId

    constructor(vars: Map<string, HypernodeId>, edges: Hyperedge[], head: HypernodeId) {
        this.vars = vars;
        this.edges = edges;
        this.head = head;
    }

    fill(startId: HypernodeId, vals: Map<string, HypernodeId>) {
        let idvals = new Map([...vals.entries()]
                        .map(([nm, id]) => [this.vars.get(nm), id])),
            remapId = (id: HypernodeId) => idvals.get(id) ?? id + startId;

        return this.edges.map(e => ({...e,
            target: remapId(e.target),
            sources: e.sources.map(remapId)
        }));
    }
}


function revmap<K, V>(m: Map<K, V>): Map<V, K> {
    return new Map([...m.entries()].map(([k, v]) => [v, k]));
}


export { RewriteRule, Pattern }