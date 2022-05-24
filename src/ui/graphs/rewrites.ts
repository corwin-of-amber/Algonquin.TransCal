import { Hypergraph, Hyperedge } from './hypergraph';


class RewriteRule {
    name: string
    from: Hyperedge[]
    to: Hyperedge[]

    constructor(name: string, from: Hyperedge[], to: Hyperedge[]) {
        this.name = name;
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