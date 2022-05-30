import fs from 'fs';
import path from 'path';
import child_process from 'child_process';
import concat from 'concat-stream';
import { Hypergraph, Hyperedge } from './hypergraph';
import { RewriteRule } from './rewrites';


class Backend {
    tempdir = new TempDir('data/tmp')
    exe = 'cpp/tc'

    writeProblem(desc: {input: Hyperedge[], rules: RewriteRule[]}) {
        this.writeGraph(desc.input);
        this.writeRules(desc.rules);
    }

    writeGraph(edges: Hyperedge[]) {
        this.tempdir.writeFile('input', Hypergraph.exportToCpp(edges));
    }

    writeRules(rules: RewriteRule[]) {
        this.tempdir.writeFile('rules',
            rules.map(rwr => rwr.exportToCpp()).join('\n'));
    }

    async solve() {
        let out = await this.execute();
        this.debugOut(out);
        return Hypergraph.importFromCpp(out);
    }

    private debugOut(out: string) {
        for (let mo of out.matchAll(/^[/][/] (.*)/gm))
            console.log(`%c[backend] %c${mo[1]}`, 'color: #880', 'color: green');
    }

    execute() {
        let p = child_process.spawn(this.exe, [this.tempdir.dirpath],
            {stdio: ['ignore', 'pipe', 'pipe']}),
            td = new TextDecoder();
        
        return new Promise<string>((resolve, reject) => {
            p.stdout.pipe(concat(buf => resolve(buf.length ? td.decode(buf) : '')));
            p.on('exit', (rc, sig) =>
                console.log(`%cbackend exited (rc=${rc})`, 'color: green'));
        });
    }
}


class TempDir {
    constructor(public dirpath: string) { }

    writeFile(fn: string, text: string) {
        fs.mkdirSync(this.dirpath, {recursive: true});
        fn = path.join(this.dirpath, fn);
        fs.writeFileSync(fn, text);
    }
}


export { Backend, TempDir }