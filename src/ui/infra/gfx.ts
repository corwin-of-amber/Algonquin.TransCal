import { jsPDF } from 'jspdf';
import 'svg2pdf.js';
import { drawDocument } from 'rasterizehtml';


async function svgToPdf(svg: SVGSVGElement) {
    const width = svg.width.baseVal.value
    const height = svg.height.baseVal.value
    const pdf = new jsPDF(width > height ? 'l' : 'p', 'pt', [width, height])
    await pdf.svg(svg, {width, height});
    return pdf;
}

async function svgToPng(svg: SVGSVGElement) {
    return imageToPng(await svgToImage(svg));
}

async function svgToImage(svg: SVGSVGElement) {
    let d = createDocument({
        head: document.head.querySelectorAll('link[rel=stylesheet]'),
        body: [svg]
    });
    let h = await drawDocument(d, undefined, {zoom: 2});
    return h.image;
}

function createDocument(content: {head: Iterable<Element>, body: Iterable<Element>}) {
    let d = document.implementation.createHTMLDocument();
    for (let el of content.head) d.head.appendChild(el.cloneNode(true));
    for (let el of content.body) d.body.appendChild(el.cloneNode(true));
    return d;
}

function imageToPng(img: CanvasImageSource & {height: number, width: number}) {
    let canvas = document.createElement('canvas');
    canvas.width = img.width;
    canvas.height = img.height;
    canvas.getContext('2d').drawImage(img, 0, 0);
    return new Promise<Blob>(resolve => canvas.toBlob(resolve, 'png'));
}

function coordDomToSvg(el: SVGSVGElement, pt: {x: number, y: number}) {
    let svgpt = el.createSVGPoint(),
        matrix = el.getCTM().inverse();
    svgpt.x = pt.x; svgpt.y = pt.y;
    return svgpt.matrixTransform(matrix);
}

const SVG_NS = 'http://www.w3.org/2000/svg';

function createSvgElement<T extends SVGElement = SVGElement>(tagName: string) {
    return document.createElementNS(SVG_NS, tagName) as T;
}


export { svgToPdf, svgToImage, svgToPng, coordDomToSvg, createSvgElement }