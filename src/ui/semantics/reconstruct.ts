import { Hypergraph, HypernodeId, Hyperedge } from '../graphs/hypergraph';
import { SExp } from './sexp';


function *reconstructTerms(g: Hypergraph, root: HypernodeId | Hyperedge) {
    if (typeof root === 'object') {    // Hyperedge
        let candidates = root.sources.map(u =>
            first(reconstructTerms(g, u)));
        yield new SExp(root.op, candidates);
    }
    else {                             // HypernodeId
        for (let e of g.getIncoming(root)) {
            yield* reconstructTerms(g, e);
        }
    }
}

function first<T>(it: Iterable<T>): T {
    for (let t of it) { return t; }
}


export { reconstructTerms }