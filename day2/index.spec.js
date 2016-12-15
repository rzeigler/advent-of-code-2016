const {expect} = require("chai");
const jsc = require("jsverify");
const R = require("ramda");
const {ReaderT, State} = require("akh");
const {
    pos,
    limitDomain,
    limitRange,
    boxBounds,
    boxGrid,
    move,
    line
} = require("./index");

describe("day2", () => {
    describe("pos", () => {
        jsc.property("constructs position tuples", jsc.nat, jsc.nat, (x, y) => {
            const [x1, y1] = pos(x, y);
            return x1 === x && y1 === y;
        });
    });
    describe("limitDomain", () => {
        jsc.property("constrains the domain", jsc.nat, jsc.nat, jsc.nat, (min, inner, max) =>
            limitDomain(min, max, [inner]) === (min <= inner && inner <= max));
    });
    describe("limitRange", () => {
        jsc.property("constrains the range", jsc.nat, jsc.nat, (x, y) =>
            limitRange(x => 2 * x, R.lte, [x, y]) === (2 * x <= y));
        jsc.property("constrains the range", jsc.nat, jsc.nat, (x, y) =>
            limitRange(x => 2 * x, R.gte, [x, y]) === (2 * x >= y));
    });
    describe("boxBounds", () => {
        it("should constrain in a box", () => {
            expect(R.allPass(boxBounds)([1, 1])).to.equal(true);
            expect(R.allPass(boxBounds)([3, 1])).to.equal(false);
            expect(R.allPass(boxBounds)([1, 3])).to.equal(false);
        });
    });
    describe("move", () => {
        it("should move up for a box grid", () => {
            const motion = move("U");
            const result = State.run(ReaderT.run(motion, {bounds: boxBounds, grid: boxGrid}), [1, 1]);
            // const result = motion.run({bounds: boxBounds, grid: boxGrid}).run([1, 1]);
            expect(result).to.eql({value: "2", state: [1, 0]});
        });
        it("should be chainable", () => {
            const motion = move("U").chain(R.always(move("L"))).chain(R.always(move("L")));
            const result = State.run(ReaderT.run(motion, {bounds: boxBounds, grid: boxGrid}), [1, 1]);
            expect(result).to.eql({value: "1", state: [0, 0]});
        });
    });
    describe.only("line", () => {
        it("should move along an element", () => {
            const motion = line("ULDDRR");
            const result = State.run(ReaderT.run(motion, {bounds: boxBounds, grid: boxGrid}), [1, 1]);
            expect(result).to.eql({value: "9", state: [2, 2]});
        });
    });
});