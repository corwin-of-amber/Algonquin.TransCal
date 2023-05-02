import { Hypergraph, HypernodeId } from '../graphs/hypergraph';
import { SExp } from './sexp';


function *reconstructTerms(g: Hypergraph, root: HypernodeId) {
    for (let e of g.getIncoming(root)) {
        let candidates = e.sources.map(u =>
            first(reconstructTerms(g, u)));
        yield new SExp(e.op, candidates);
    }
}

function first<T>(it: Iterable<T>): T {
    for (let t of it) { return t; }
}


export { reconstructTerms }