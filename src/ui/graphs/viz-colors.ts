import { EventEmitter } from 'events';
import { PointXY, vsum, centerOfMass } from '../infra/geom';
import { coordDomToSvg, createSvgElement } from '../infra/gfx';
import { EGraph } from './egraph';
import { GraphvizSvg } from './graphviz';
import { HypernodeId } from './hypergraph';


class ColorEGraphOverlay extends EventEmitter {
    g: EGraph
    rendered: GraphvizSvg
    overlay: SVGGElement
    eclasses: RenderedColorGroup[] = []

    epicenterShifts = new Map<EGraph.ColorGroup, PointXY>()

    constructor(g: EGraph, rendered: GraphvizSvg) {
        super();
        this.g = g;
        this.rendered = rendered;
    }

    apply() {
        for (let c of this.g.colors?.eclasses ?? []) {
            this.eclasses.push(this.renderColorGroupFor(c));
        }
    }

    reapply(eclasses: EGraph.ColorGroup[] = this.g.colors?.eclasses ?? []) {
        for (let c of eclasses) {
            let rcg = this._find(c);
            if (rcg) {
                this._removeRendered(rcg);
                rcg.el = this.renderColorGroupFor(c).el;
            }
            else
                this.eclasses.push(this.renderColorGroupFor(c));
        }
    }

    eclassFromElement(el: SVGElement): EGraph.ColorGroup {
        el = el.closest(`.${ECLASS}`);
        return el ? this.eclasses.find(rec => rec.el === el)?.c : undefined;
    }

    moveRel(eclass: EGraph.ColorGroup, rel: PointXY) {
        if (!this._find(eclass))
            throw new Error(`eclass not found in ColorEGraphOverlay`);
        this.epicenterShifts.set(eclass, vsum([
            this.epicenterShifts.get(eclass), rel].filter(x => x)));
        this.reapply([eclass]);
    }

    append(el: SVGElement) {
        if (!this.overlay)
            this.rendered.svg.append(this.overlay = createSvgElement<SVGGElement>('g'));
        this.overlay.append(el);
    }

    renderColorGroupFor(eclass: EGraph.ColorGroup) {
        let colorName = this.g.colors.lookup(eclass.color)[1].name,
            el = this.connectNodes(eclass.members, colorName,
                                   this.epicenterShifts.get(eclass));
        el.addEventListener('mousedown', () => {
            this.emit('eclass:select', {eclass});
        });
        return {c: eclass, colorName, el};
    }

    connectNodes(ids: HypernodeId[], colorName: string, centerShift: PointXY = {x: 0, y: 0}) {
        let svg = this.rendered.svg;
        let bbox = svg.getBoundingClientRect();
        let elOf = (u: HypernodeId) => this.rendered.elementFromNode(u);
        let centerOf = (el: SVGElement) => {
            let r = el.getBoundingClientRect();
            return coordDomToSvg(this.rendered.svg,
                {x: (r.left + r.right) / 2 - bbox.left, y: (r.top + r.bottom) / 2 - bbox.top});
        };

        let centers = ids.map(u => bind(elOf, centerOf)(u)).filter(x => x);
        let epicenter = vsum([centerOfMass(centers), centerShift]);

        let eclass = withClass(createSvgElement<SVGGElement>('g'), ECLASS);
        eclass.setAttribute('data-color', colorName);
        for (let pt of centers)
            eclass.append(withClass(lineBetween(epicenter, pt), RAY));
        eclass.append(withClass(circleAt(epicenter), EPICENTER));
        this.append(eclass);
        return eclass;
    }

    _find(c: EGraph.ColorGroup) {
        return this.eclasses.find(x => x.c === c);
    }

    _remove(c: EGraph.ColorGroup) {
        let rcg = this._find(c);
        if (rcg) this._removeRendered(rcg);
    }

    _removeRendered(rcg: RenderedColorGroup) {
        rcg.el.remove();
    }
}

const ECLASS = 'egraph--color-overlay--eclass';
const EPICENTER = 'egraph--color-overlay--epicenter';
const RAY = 'egraph--color-overlay--ray';

type RenderedColorGroup = {c: EGraph.ColorGroup, colorName: string, el: SVGGElement};


function bind(...funcs: ((a: any) => any)[]) {    /* option monad */
    return (u: any) => {
        for (let f of funcs)
            if (u !== undefined) u = f(u); else return undefined;
        return u;
    }
}


function circleAt(pt: PointXY, r = 4) {
    let ci = createSvgElement('circle');
    ci.setAttribute('cx', `${pt.x}`);
    ci.setAttribute('cy', `${pt.y}`);
    ci.setAttribute('r', `${r}`);
    return ci;
}

function lineBetween(pt1: PointXY, pt2: PointXY) {
    let li = createSvgElement('path');
    li.setAttribute('d', `M${pt1.x} ${pt1.y} L${pt2.x} ${pt2.y}`);
    return li;
}

function withClass<E extends Element>(el: E, ...classNames: string[]) {
    for (let cn of classNames) el.classList.add(cn);
    return el;
}


export { ColorEGraphOverlay }