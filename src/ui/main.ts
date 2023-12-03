import fs from 'fs';

import * as Vue from 'vue';
import type { ComponentPublicInstance } from 'vue';

// @ts-ignore
import App from './components/app.vue';

import { CompiledSexp, SexpFrontend, VernacFrontend } from './frontend';
import { Ast } from './syntax/parser';
import { forestToGraph } from './infra/tree';
import { EGraph } from './graphs/egraph';
import { CppBackend, EggBackend } from './graphs/backend-interface';

import { PointXY, vsum } from './infra/geom';
import { svgToPdf, svgToPng } from './infra/gfx';
import { Hyperedge, Hypergraph, HypernodeId } from './graphs/hypergraph';
import { reconstructTerms } from './semantics/reconstruct';
import { Pattern } from './graphs/rewrites';


function astsToGraph(forest: Ast<any>[]) {
    return forestToGraph(forest, node => ({label: node.type}));
}

const SAMPLES = {
    'rev-snoc': [
        'u1 :- (rev (:+ l y))',
        'u2 :- (:: y (rev l))',
        // -- induction l
        'v1 :- (rev (:+ xs y))',
        'v2 :- (:: y (rev xs))',
        //'azure :- (?~ l [])',
        'rose :- (?~ l (:: x xs))',
        'rose :- (?~ v1 v2)'
    ],
    'rev-snoc [cons case]': [
        'u1 :- (rev (:+ l y))',
        'u2 :- (:: y (rev l))',
        // -- induction l
        'v1 :- (rev (:+ xs y))',
        'v2 :- (:: y (rev xs))',
        'l :- (:: x xs)',
        'v1 :- (v2)'
    ],
    'rev-rev': [
        'u1 :- (rev (rev l))',
        'u2 :- (l)',
        // -- induction l
        'v1 :- (rev (rev xs))',
        'v2 :- (xs)',
        'azure :- (?~ l [])',
        'rose :- (?~ l (:: x xs))',
        'rose :- (?~ v1 v2)',
        //'rose :- (?~ (rev (:+ (rev xs) x)) (:: x (rev (rev xs))))',
        //'c :- (rev xs)',
        // -- induction xs
        //'w1 :- (rev (:+ zs x))',
        //'w2 :- (:: x (rev zs))',
        //'azure :- (?~ (rev xs) (:: z zs))',
        //'azure :- (?~ w1 w2)'
    ],
    'plus-s': [
        'u1 :- (+ (S n) m)',
        'u2 :- (+ n (S m))',
        's :- (S k)',
        // -- induction n
        'v1 :- (+ (S k) m)',
        'v2 :- (+ k (S m))',
        'rose :- (?~ n s)',
        'rose :- (?~ v1 v2)'
    ],
    'plus-0': [
        'u1 :- (+ n 0)',
        'u2 :- (n)',
        'gold :- (?~ n 0)',
        'rose :- (?~ n (S k))',
        //'mud :- (?~ (+ k 0) k)',
        //'rose :- (?> mud)'

        // Proof.
        // cli.color('mud', 'rose'); cli.merge('mud', ['"k"', '(+ "k" "0")']); cli.do()
    ],
    'plus-comm': [
        'u1 :- (+ n m)',
        'u2 :- (+ m n)',
        ':- (ghost)',
        // -- induction n
        //'gold :- (?~ n 0)',
        //'rose :- (?~ n (S "n\'"))',
        //'rose :- (?~ (+ "n\'" m) (+ m "n\'"))',        // n' + m = m + n'
        // -- induction m
        //'azure :- (?~ m 0)'
        //'mustard :- (?~ m (S j))',
        //'gold :- (?> mustard)',
        /*
        'rose :- (?> mud)',
        'mud :- (?~ (+ "n\'" m) (+ m "n\'"))',        // n' + m = m + n'
        'mud :- (?> moss)',
        'moss :- (?~ m (S "m\'"))',                    // m = S m'
        
        'mud :- (?> gray-1)',
        'mud :- (?> gray-2)',
        'gray-1 :- (?~ (+ n "m\'") (+ "m\'" n))',
        'gray-2 :- (?~ (S (+ "m\'" "n\'")) (+ "m\'" (S "n\'")))',   // S (m' + n') = m' + S n'
        */

        /*
        cli.case(undefined, '"n"', {gold: '"0"', rose: '(S "n\'")'})
        cli.case('gold', '"m"', {mustard: '"0"', azure: '(S "m\'")'})
        cli.case('azure', '"m\'"', {moss: '(+ "m\'" "n")'})
        */
    ],
    'max': [
        'u1 :- (max x y)',
        'rose :- (?~ (max x y) x)'
    ],
    'leq-len': [
        'u1 :- (leq (len (filter p xs)) (len xs))',
        'u2 :- (True)',
        'u3 :- ([])',
        //':- (ghost)',
        'rose :- (?~ xs [])'
    ]    
}

async function main() {
    var app = Vue.createApp(App).mount(document.body) as
                ComponentPublicInstance<App>;

    let vfe = new VernacFrontend;
    vfe.add('rules', [
        'rewrite ":+ []" (:+ [] x) -> (:: x [])',
        'rewrite ":+ ::" (:+ (:: x xs) y) -> (:: x (:+ xs y))',
        'rewrite "rev []" (rev []) -> ([])',
        'rewrite "rev ::" (rev (:: x xs)) -> (:+ (rev xs) x)',
        'rewrite "rev ::%" (:+ (rev xs) x) -> (rev (:: x xs))',
        'rewrite "+ 0" (+ "0" m) -> (id m)',
        'rewrite "+ S" (+ (S n) m) -> (S (+ n m))',
        'rewrite "+ S%" (S (+ n m)) -> (+ (S n) m)',
        
        'rewrite "filter []" (filter p []) -> ([])',
        'rewrite "filter ::" (filter p (:: x xs)) -> ' + 
                ' (ite (p x) (:: x (filter p xs)) (filter p xs))'
    ]);
    
    const name = 'plus-comm';

    vfe.add(name, SAMPLES[name]);

    //app.egraph = astsToGraph(vfe.asts);

    var input = vfe.sexpFe.asHypergraph();
    app.egraph = new EGraph(input.edges);

    async function process(input: Hypergraph) {
        var be = new CppBackend;
        be.opts.rewriteDepth = app.opts.rwDepth;
        be.writeProblem({input: input.edges, rules: vfe.rules});
        var g = await be.solve();
        // use this for no-op backend (just for rendering the input graph)
        //var g = EGraph.fromHypergraph(input).congruenceClosureLeaves();

        app.egraph = g.foldColorInfo();
    }

    Vue.watchEffect(() => { return process(input); });

    function step1() {
        let g = Vue.toRaw(app.egraph).clone();
        g.collapseColors('rose');
        g.removeDuplicateEdges();

        let named = (nm: string) => [...g.edgesByOp(nm)][0].target;

        console.log(named('u1'), named('u2'));

        let boughPats = {
            lhs: ['(+ x y)'],
            rhs: ['(+ y x)']
        };
        for (let pats of Object.values(boughPats)) {
            for (let cpat of vfe.sexpFe.compile(pats)) {
                console.log('%c%s', 'color: blue', vfe.sexpFe.astToText(cpat.ast));
                let pat = vfe.rp.sexpToPattern(cpat);
                console.log(pat.fill(50, new Map([['x', 7], ['y', 9]])));
                /*
                for (let vl of g.ematch.subgraph(pat.edges)) {
                    console.log(vl.get(pat.head), Object.fromEntries(
                        [...pat.vars.entries()].map(([nm, id]) =>
                            [nm, vl.get(id)])));
                }*/
            }
        }
    }

    function step2() {
        app.egraph.collapseColors('rose', 'mud', 'moss');
        app.egraph.removeDuplicateEdges();

        let boughPats = [
            '(S x)', '(S (S x))', '(S (S (+ m n)))',
            '(+ x y)', '(+ (S x) y)', '(+ (S (S x)) y)',
            /* [dups] '(+ x y)', '(+ (S x) y)', */ '(+ x (S y))'
        ];
        /*
        for (let cpat of vfe.sexpFe.compile(boughPats)) {
            console.log('%c%s', 'color: blue', vfe.sexpFe.astToText(cpat.ast));
            let pat = vfe.rp.edgesToPattern(cpat.edges);
            for (let vl of app.egraph.ematch.subgraph(pat.edges)) {
                console.log(Object.fromEntries(
                    [...pat.vars.entries()].map(([nm, id]) =>
                        [nm, vl.get(id)])));
            }
        }
        */
    }

    // Some UI actions
    app.events.on('egraph:select', ev => {
        if (ev.target.node) {
            console.log("%c%s", 'color: blue', ev.target.node.id);
            cli.displayTerms(ev.target.node.id);
        }
        if (ev.target.edge) {
            console.log("%c%s", 'color: blue', ev.target.edge.target);
            cli.displayTerms(ev.target.edge);
        }
    });

    // JavaScript exploration CLI
    let cli = new CLI(app);

    Object.assign(window, {app, vfe, svgToPng, cli});
    Object.assign(cli, {process, step1});
}

/**
 * This is a "cli" for use in the JavaScript console.
 */
class CLI {
    vue: ComponentPublicInstance<App>
    selection: {colorEclass?: EGraph.ColorGroup} = {}

    stacked: EGraph[] = []

    constructor(vue: ComponentPublicInstance<App>) {
        this.vue = vue;
        this.vue.events.on('eclass:select', ({eclass}) => {
            this.selection = {colorEclass: eclass};
        });
    }

    async exportSvg(svg = this._getSvg()) {
        svg = await this._svgEmbedStyles(svg);
        fs.writeFileSync('data/tmp/egraph.svg', svg.outerHTML);
    }

    async exportPdf(svg = this._getSvg()) {
        svg = await this._svgEmbedStyles(svg);
        svg.classList.add('compat');
        let pdf = await svgToPdf(svg);
        fs.writeFileSync('data/tmp/egraph.pdf', pdf.output());
    }

    async _svgEmbedStyles(svg) {
        let styles = await Promise.all(['graphviz.css', 'egraph.css'].map(async m => (await fetch(m)).text())),
            head = document.createElement('style');
        head.innerHTML = styles.join('\n');
        svg = svg.cloneNode(true) as SVGSVGElement;
        svg.prepend(head);
        return svg;
    }

    async exportPng(svg = this._getSvg()) {
        let blob = await svgToPng(svg);
        fs.writeFileSync('data/tmp/egraph.png',
            new Uint8Array(await blob.arrayBuffer()));
    }

    _getSvg() { return document.querySelector('svg'); }

    config(opts: {graphviz?: {ranksep?: number}}) {
        if (opts.graphviz) {
            if (opts.graphviz.ranksep)
                this.vue.override.ranksep = opts.graphviz.ranksep;
        }
    }

    moveOver(shift: PointXY) {
        let c = this.selection.colorEclass;
        if (!c) throw new Error('no color eclass selected');
        // @ts-ignore
        let overlay = this.vue.$refs.egraph._overlay;
        overlay.moveRel(c, shift);
    }

    displayTerms(u: HypernodeId | Hyperedge) {
        for (let t of this.reconstructTerms(u))
            console.log(" - " + t.toString());
    }

    reconstructTerms(u: HypernodeId | Hyperedge) {
        return reconstructTerms(this.vue.egraph, u);
    }

    _process() {
        // @ts-ignore
        this.process?.(this.vue.egraph.unfoldColorInfo());
    }

    do(statements: string | string[] = [], process = true) {
        if (typeof statements === 'string')
            statements = [statements]
        let v = this.fe();
        v.add('cli', statements);
        v.commit();

        if (process) this._process();
    }

    fe() {
        return new VernacFrontend().over(this.vue.egraph);
    }

    named(nm: string) {
        return [...this.vue.egraph.edgesByOp(nm)][0].target;
    }

    color(name: string, parent?: string) {
        let info: EGraph.ColorInfo = {name};
        if (parent) info.parent = this.vue.egraph.colors.lookup(parent)[0];
        let fe = this.fe(),
            _ = fe.sexpFe.add('cli(color)', [[name]]),
            id = _[0].head.target;
        fe.commit();
        this.vue.egraph.colors.declareColor(id, info);
    }

    lookup(src: string) {
        let fe = this.fe(), g: EGraph = this.vue.egraph;
        let pat = fe.rp.sexpToPattern(fe.sexpFe.compile([src])[0]);
        let matches = [...g.ematch.pattern(pat)];
        if (matches.length > 0)
            return matches[0]?.head;
        else
            throw new Error(`pattern not found: ${src}`);
    }

    merge(color: string, patterns: string[]) {
        let g: EGraph = this.vue.egraph;
        g.colors.merge(color, patterns.map(p => this.lookup(p)));
    }

    case(color: string, term: string, subterms: {[color: string]: string}, process = true) {
        let v = this.fe(), g: EGraph = this.vue.egraph,
            u = this.lookup(term),
            added: [string, CompiledSexp][] = Object.entries(subterms)
                .map(([nm, t]) => [nm, v.sexpFe.add('cli/case', [t])[0]]);
        v.commit();
        for (let [subcolor, uid] of added) {
            this.color(subcolor, color);
            g.colors.merge(subcolor, [u, uid.head.target]);
        }

        if (process) this._process();
    }

    collapse(...colors: string[]) {
        this.vue.egraph.collapseColors(...colors);
    }

    push() {
        this.stacked.push(this.vue.egraph);
        this.vue.egraph = Vue.toRaw(this.vue.egraph).clone();
    }

    pop() {
        if (this.stacked.length > 0)
            this.vue.egraph = this.stacked.pop();
    }
}


window.addEventListener('load', () => main());