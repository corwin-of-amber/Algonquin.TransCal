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

    execute() {
        let p = child_process.spawn(this.exe, [this.tempdir.dirpath],
            {stdio: ['ignore', 'pipe', 'pipe']}),
            td = new TextDecoder();
        
        return new Promise<string>((resolve, reject) => {
            p.stdout.pipe(concat(buf => resolve(td.decode(buf))))
            p.on('exit', (rc, sig) =>
                console.log(`%cbackend exited (rc=${rc})`, 'color: green'));
        });
    }
}


class TempDir {
    constructor(public dirpath: string) { }

    writeFile(fn: string, text: string) {
        console.log(`[${fn}]\n${text}\n\n`);
        fs.mkdirSync(this.dirpath, {recursive: true});
        fn = path.join(this.dirpath, fn);
        fs.writeFileSync(fn, text);
    }
}


export { Backend, TempDir }