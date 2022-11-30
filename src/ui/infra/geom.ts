import _ from 'lodash';


type PointXY = {x: number, y: number};

function centerOfMass(pts: PointXY[]): PointXY {
    let n = pts.length, sum = vsum(pts);
    return {x: sum.x / n, y: sum.y / n};
}

function vsum(pts: PointXY[]) {
    return {x: _.sum(pts.map(pt => pt.x)), 
            y: _.sum(pts.map(pt => pt.y))};
}


export { PointXY, centerOfMass, vsum }