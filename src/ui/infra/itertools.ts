
function *enumerate<T>(iterable: Iterable<T>): Generator<[number, T]> {
    let i = 0;
    for (let el of iterable) yield [i++, el];
}

function ifind<T>(iterable: Iterable<T>, p: (t: T) => boolean) {
    for (let t of iterable)
        if (p(t)) return t;
}


export { enumerate, ifind }