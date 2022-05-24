import { Graph } from 'graphlib';
import { enumerate } from './itertools';


interface Tree<T> {
    subtrees?: Tree<T>[]
}

function *preorderWalk<T, K extends Tree<T>>(t: K): Generator<K> {
    yield t;
    for (let c of t.subtrees ?? [])
        yield* preorderWalk(c as K);
}

function *postorderWalk<T, K extends Tree<T>>(t: K): Generator<K> {
    for (let c of t.subtrees ?? [])
        yield* postorderWalk(c as K);
    yield t;
}

function treeToGraph<T, K extends Tree<T>>(t: K, labelFunc: (t: K) => any = () => undefined) {
    return forestToGraph([t], labelFunc);
}

function forestToGraph<T, K extends Tree<T>>(ts: K[], labelFunc: (t: K) => any = () => undefined) {
    let g = new Graph();
    let mp = new Map<Tree<T>, number>(), i = 0;

    let mkLabel = (node: K, lbl: any) =>
        ({data: node, ...(typeof lbl === 'object' ? lbl : lbl && {label: lbl})});

    for (let t of ts) {
        for (let node of preorderWalk(t))
            mp.set(node, i++);

        for (let node of preorderWalk(t)) {
            var u = `u${mp.get(node)}`;
            g.setNode(u, mkLabel(node, labelFunc(node)));
            for (let child of node.subtrees ?? []) 
                g.setEdge(u, `u${mp.get(child)}`);
        }
    }

    return g;
}

function mapNodes<T, K extends Tree<T>>(t: K, f: (t: K) => K): K {
    var nt = f(t);
    nt.subtrees = t.subtrees?.map(s => mapNodes(s, f));
    return nt;
}

function treeClone<T, K extends Tree<T>>(t: K) {
    return mapNodes(t, t => ({...t}));
}


export { Tree, preorderWalk, postorderWalk, treeToGraph, forestToGraph,
         mapNodes, treeClone }