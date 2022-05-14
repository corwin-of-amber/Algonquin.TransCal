
function *enumerate<T>(iterable: Iterable<T>): Generator<[number, T]> {
    let i = 0;
    for (let el of iterable) yield [i++, el];
}


export { enumerate }