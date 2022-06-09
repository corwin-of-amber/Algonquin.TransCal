import _ from 'lodash';
import { coordDomToSvg, createSvgElement } from '../infra/gfx';
import { EGraph } from './egraph';
import { GraphvizSvg } from './graphviz';
import { HypernodeId } from './hypergraph';


class ColorEGraphOverlay {
    g: EGraph
    rendered: GraphvizSvg
    overlay: SVGGElement

    constructor(g: EGraph, rendered: GraphvizSvg) {
        this.g = g;
        this.rendered = rendered;
    }

    apply() {
        for (let c of this.g.colors?.eclasses ?? []) {
            this.connectNodes(c.members);
        }
    }

    append(el: SVGElement) {
        if (!this.overlay)
            this.rendered.svg.append(this.overlay = createSvgElement<SVGGElement>('g'));
        this.overlay.append(el);
    }

    connectNodes(ids: HypernodeId[]) {
        let svg = this.rendered.svg;
        let bbox = svg.getBoundingClientRect();
        let elOf = (u: HypernodeId) => this.rendered.elementFromNode(u);
        let centerOf = (el: SVGElement) => {
            let r = el.getBoundingClientRect();
            return coordDomToSvg(this.rendered.svg,
                {x: (r.left + r.right) / 2 - bbox.left, y: (r.top + r.bottom) / 2 - bbox.top});
        };

        let centers = ids.map(u => bind(elOf, centerOf)(u)).filter(x => x);
        let epicenter = centerOfMass(centers);

        let eclass = withClass(createSvgElement<SVGGElement>('g'), ECLASS);
        for (let pt of centers)
            eclass.append(withClass(lineBetween(epicenter, pt), RAY));
        eclass.append(withClass(circleAt(epicenter), EPICENTER));
        this.append(eclass);
    }
}

const ECLASS = 'egraph--color-overlay--eclass';
const EPICENTER = 'egraph--color-overlay--epicenter';
const RAY = 'egraph--color-overlay--ray';



type PointXY = {x: number, y: number};

function bind(...funcs: ((a: any) => any)[]) {    /* option monad */
    return (u: any) => {
        for (let f of funcs)
            if (u !== undefined) u = f(u); else return undefined;
        return u;
    }
}

function centerOfMass(pts: PointXY[]): PointXY {
    let n = pts.length;
    return {x: _.sum(pts.map(pt => pt.x)) / n, 
            y: _.sum(pts.map(pt => pt.y)) / n}
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