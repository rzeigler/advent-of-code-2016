"use strict";

const assert = require("assert");
const R = require("ramda");
const {State, ReaderT} = require("akh");

const {nonEmptyLines} = require("../strings");

const UP = "U";
const DOWN = "D";
const LEFT = "L";
const RIGHT = "R";

const pos = R.curry((x, y) => [x, y]);

const limitDomain = R.curry((min, max, [x]) => R.allPass([R.lte(min), R.gte(max)])(x));

const limitRange = R.curry((line, comparison, [x, y]) => comparison(line(x), y));

const lookup = R.curry((grid, [x, y]) => grid[y][x]);

const update = R.curry((bounds, dir, [x, y]) => {
    const next = dir === UP ? [x, y - 1] :
                 dir === DOWN ? [x, y + 1] :
                 dir === LEFT ? [x - 1, y] :
                 dir === RIGHT ? [x + 1, y] : null;
    assert(next, "Unrecognized movement direction");
    return R.allPass(bounds)(next) ? next : [x, y];
});

const ReaderS = ReaderT(State);

const move = (direction) => new ReaderS(({bounds, grid}) => {
    const action = R.compose(
        R.map(lookup(grid)),
        R.chain(R.always(State.get)),
        State.modify
    );
    return action(update(bounds, direction));
});

// The result of a line is the final key entry on the line
// const line = (ds) => R.map(R.last, R.sequence(ReaderS.of, R.map(move, Array.from(ds))));
// String -> ReaderT env State String Coord
const line = ds => 
    R.compose(
        R.map(R.last),
        R.sequence(ReaderS.of),
        R.map(move),
        Array.from
    )(ds);

const code = (input) =>
    R.compose(
        R.map(R.join("")),
        R.sequence(ReaderS.of),
        R.map(line),
        nonEmptyLines
    )(input);

const boxGrid = [
    ["1", "2", "3"],
    ["4", "5", "6"],
    ["7", "8", "9"]
];

const boxBounds = [
    limitDomain(0, 2),
    limitRange(R.always(0), R.lte),
    limitRange(R.always(2), R.gte)
];

const boxStart = pos(1, 1);

const diamondGrid = [
    [null, null, "1", null, null],
    [null, "2",  "3", "4",  null],
    ["5",  "6",  "7", "8",  "9"],
    [null, "A",  "B", "C",  null],
    [null, null, "D", null, null]
];

const diamondBounds = [
    limitDomain(0, 4),
    limitRange(x => -x + 4, R.gte),
    limitRange(x => x + 2, R.gte),
    limitRange(x => -x + 2, R.lte),
    limitRange(x => x - 2, R.lte)
];

const diamondStart = pos(0, 3);

module.exports = {
    pos,
    limitDomain,
    limitRange,
    move,
    line,
    code,
    boxBounds,
    boxGrid,
    boxStart,
    diamondGrid,
    diamondBounds,
    diamondStart,
    impl: {
        "1": ([input]) => State.run(ReaderT.run(code(input), {bounds: boxBounds, grid: boxGrid}), boxStart).value,
        "2": ([input]) => State.run(ReaderT.run(code(input), {bounds: diamondBounds, grid: diamondGrid}), diamondStart).value,
    }
};
