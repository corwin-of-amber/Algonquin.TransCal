import { drawDocument } from 'rasterizehtml';


async function svgToPng(svg: SVGSVGElement) {
    return imageToPng(await svgToImage(svg));
}

async function svgToImage(svg: SVGSVGElement) {
    let d = createDocument({
        head: document.head.querySelectorAll('link[rel=stylesheet]'),
        body: [svg]
    });
    let h = await drawDocument(d, undefined);
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
    return new Promise(resolve => canvas.toBlob(resolve, 'png'));
}


export { svgToImage, svgToPng }