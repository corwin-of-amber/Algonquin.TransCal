import { Graph } from 'graphlib';
import { enumerate } from './itertools';


interface Tree<T> {
    children?: Tree<T>[]
}

function *preorderWalk<T, K extends Tree<T>>(t: K): Generator<K> {
    yield t;
    for (let c of t.children ?? [])
        for (let s of preorderWalk(c))
            yield s as K;
}

function treeToGraph<T, K extends Tree<T>>(t: K, labelFunc: (t: K) => any) {
    let g = new Graph();
    let mp = new Map<Tree<T>, number>();
    for (let [i, node] of enumerate(preorderWalk(t)))
        mp.set(node, i);

    for (let node of preorderWalk(t)) {
        var u = `u${mp.get(node)}`;
        g.setNode(u, labelFunc(node));
        for (let child of node.children ?? []) 
            g.setEdge(u, `u${mp.get(child)}`);
    }

    return g;
}


export { Tree, preorderWalk, treeToGraph }