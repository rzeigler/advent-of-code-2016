"use strict";
const assert = require("assert");
const R = require("ramda");

const vectorAdd = R.zipWith(R.add);

const deltas = {
    "N": [0, 1],
    "S": [0, -1],
    "E": [1, 0],
    "W": [-1, 0]
};

const rotations = {
    "N": {
        "R": "E",
        "L": "W"
    },
    "S": {
        "R": "W",
        "L": "E"
    },
    "E": {
        "R": "S",
        "L": "N"
    },
    "W": {
        "R": "N",
        "L": "S"
    }
};

const origin = [0, 0];

const position = R.curry((coord, orient) => ({coord, orient}));
const orientLens = R.lensProp("orient");
const coordLens = R.lensProp("coord");

const rotate = R.curry((dir, position) => 
    R.over(orientLens, (o) => rotations[o][dir], position)
);

const advance = R.curry((count, position) => {
    const base = R.prop(R.view(orientLens, position), deltas);
    const delta = R.reduce(vectorAdd, origin, R.repeat(base, count));
    return R.over(coordLens, vectorAdd(delta), position);
});

const update = R.curry((cmd, position) => R.compose(
    advance(parseInt(cmd.slice(1), 10)),
    rotate(cmd[0])
)(position));

const start = position(origin, "N"); 

const thrush = (a, f) => f(a);

// The lazy way because I don't want to expose the repeat inside of advance
function intermediates([x1, y1], [x2, y2]) {
    assert(x1 === x2 || y1 === y2, "points cannot be on a diagonal");
    const step = x1 !== x2 ?
        [x1 > x2 ? -1 : 1, 0] :
        [0, y1 > y2 ? - 1 : 1];
    const count = x1 !== x2 ? Math.abs(x1 - x2) : Math.abs(y1 - y2);
    return R.scan(vectorAdd, [x1, y1], R.repeat(step - 1, count));
}

function findFirstRepeat(seen, locs) {
    const l = R.head(locs);
    assert(l, "ooops");
    if (R.contains(l, seen)) {
        return l;
    }
    return findFirstRepeat(R.append(l, seen), R.tail(locs));
}

module.exports = {
    position,
    rotate,
    advance,
    update,
    start,
    intermediates,
    impl: {
        "1": ([input]) => {
            const commands = input.split(", ");
            const {coord: [x, y]} = R.reduce(thrush, start, R.map(update, commands));
            return Math.abs(x) + Math.abs(y);
        },
        "2": ([input]) => {
            const commands = input.split(", ");
            // All terminal locations
            const locs = R.map(R.view(coordLens), R.scan(thrush, start, R.map(update, commands)));
            const pairs = R.aperture(2, locs);
            const expandedLocs = R.chain(R.apply(intermediates), pairs);
            const hq = findFirstRepeat([], expandedLocs);
            return hq;
        }
    }
};