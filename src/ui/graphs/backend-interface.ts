import fs from 'fs';
import path from 'path';
import child_process from 'child_process';
import concat from 'concat-stream';
import { Hypergraph, Hyperedge } from './hypergraph';
import { EGraph } from './egraph';
import { RewriteRule } from './rewrites';


class Backend {
    tempdir = new TempDir('data/tmp')
    exe: string
    opts: BackendOptions = {}

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
        return EGraph.importFromCpp(out);
    }

    private debugOut(out: string) {
        for (let mo of out.matchAll(/^[/][/] (.*)/gm))
            console.log(`%c[backend] %c${mo[1]}`, 'color: #880', 'color: green');
    }

    execute() {
        let p = child_process.spawn(this.exe, this.cmdargs(),
            {stdio: ['ignore', 'pipe', 'pipe']}),
            td = new TextDecoder();

        p.stderr.pipe(concat(buf => {/*todo*/}));
        
        return new Promise<string>((resolve, reject) => {
            p.stdout.pipe(concat(buf => resolve(buf.length ? td.decode(buf) : '')));
            p.on('exit', (rc, sig) =>
                console.log(`%cbackend exited (rc=${rc})`, 'color: green'));
        });
    }

    cmdargs() {
        return [...this.flags(), ...this.inputArgs()];
    }

    inputArgs() {
        return [this.tempdir.dirpath];
    }

    flags() { return []; }
}


class CppBackend extends Backend {
    exe = 'cpp/tc'

    flags() {
        return [].concat(...[
            this.opts.rewriteDepth ? ['--rw-depth', `${this.opts.rewriteDepth}`] : []
        ]);
    }
}


class EggBackend extends Backend {
    exe = "bin/thesy/fo_reason"

    inputArgs() { return [path.join(this.tempdir.dirpath, 'input')]; }
}


type BackendOptions = {
    rewriteDepth?: number
}

class TempDir {
    constructor(public dirpath: string) { }

    writeFile(fn: string, text: string) {
        fs.mkdirSync(this.dirpath, {recursive: true});
        fn = path.join(this.dirpath, fn);
        fs.writeFileSync(fn, text);
    }
}


export { Backend, CppBackend, EggBackend, TempDir }