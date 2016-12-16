const {expect} = require("chai");
const {position, rotate, advance, intermediates, start} = require("./index");
describe("day1", () => {
    describe("rotate", () => {
        it("should rotate a position", () => {
            expect(rotate("L", start)).to.eql(position([0, 0], "W"));
        });
    });
    describe("advance", () => {
        it("should advance ahead", () => {
            expect(advance(3, start)).to.eql(position([0, 3], "N"));
        });
    });
    describe("intermediates", () => {
        it("should expand as expected", () => {
            expect(intermediates([1, 0], [3, 0])).to.eql([[1, 0], [2, 0], [3, 0]]);
        });
    });
});