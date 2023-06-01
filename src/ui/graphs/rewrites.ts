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


export { RewriteRule }