
interface Tree<T> {
    root: T
    subtrees: Tree<T>[]
}

class SExp implements Tree<Op> {
    root: Op
    subtrees: SExp[]

    constructor(root: Op, subtrees: SExp[] = []) {
        this.root = root;
        this.subtrees = subtrees;
    }

    isLeaf() { return this.subtrees.length === 0; }

    toString() {
        return this.isLeaf() ? this.root :
            `(${this.root} ${this.subtrees.map(s => s.toString()).join(' ')})`;
    }
}

type Op = string;


export { Tree, SExp }