import fs from 'fs';

import * as Vue from 'vue';
import type { ComponentPublicInstance } from 'vue';

// @ts-ignore
import App from './components/app.vue';

import { SexpFrontend, VernacFrontend } from './frontend';
import { Ast } from './syntax/parser';
import { forestToGraph } from './infra/tree';
import { EGraph } from './graphs/egraph';
import { CppBackend, EggBackend } from './graphs/backend-interface';

import { PointXY, vsum } from './infra/geom';
import { svgToPdf, svgToPng } from './infra/gfx';
import { Hyperedge, Hypergraph, HypernodeId } from './graphs/hypergraph';
import { reconstructTerms } from './semantics/reconstruct';


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
        'mud :- (?~ (+ k 0) k)',
        'rose :- (?> mud)'
    ],
    'plus-comm': [
        'u1 :- (+ n m)',
        'u2 :- (+ m n)',
        // -- induction n
        //'gold :- (?~ n 0)',
        'rose :- (?~ n (S n_))',
        //'rose :- (?~ (+ k m) (+ m k))',
        // -- induction m
        //'azure :- (?~ m 0)'
        //'mustard :- (?~ m (S j))',
        'gold :- (?> mustard)',
        'mud :- (?~ m (S m_))',
        'rose :- (?> mud)',
        //'moss :- (?~ (S (+ m_ n_)) (+ m_ (S n_)))',
        //'moss :- (?~ (+ n_ m) (+ m n_))',
        'mud :- (?> moss)',
        //'azure :- (?~ (+ n j) (+ j n))'
        //'kumquat :- (?~ (+ j 0) j)',
        'mustard :- (?> kumquat)'
    ],
    'max': [
        'u1 :- (max x y)',
        'rose :- (?~ (max x y) x)'
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
        'rewrite "+ S%" (S (+ n m)) -> (+ (S n) m)'
    ]);
    
    const name = 'plus-comm';

    vfe.add(name, SAMPLES[name]);

    //app.egraph = astsToGraph(vfe.asts);

    var input = vfe.sexpFe.asHypergraph();
    app.egraph = new EGraph(input.edges);

    Vue.watchEffect(async () => {

        var be = new EggBackend;
        be.opts.rewriteDepth = app.opts.rwDepth;
        be.writeProblem({input: input.edges, rules: vfe.rules});
        var g = await be.solve();
        // use this for no-op backend (just for rendering the input graph)
        //var g = EGraph.fromHypergraph(input).congruenceClosureLeaves();

        app.egraph = g.foldColorInfo();
    });

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
}

/**
 * This is a "cli" for use in the JavaScript console.
 */
class CLI {
    vue: ComponentPublicInstance<App>
    selection: {colorEclass?: EGraph.ColorGroup} = {}

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
}


window.addEventListener('load', () => main());