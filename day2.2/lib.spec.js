const {expect} = require('chai');

const {coordToNumber, move, motion} = require('./lib');

describe('lib', function () {
    describe('coordToNumber', function () {
        it('should coord to number', function () {
            expect(coordToNumber([0, 0])).to.equal(1);
            expect(coordToNumber([2, 2])).to.equal(9);
            expect(coordToNumber([1, 1])).to.equal(5);
            expect(coordToNumber([0, 2])).to.equal(7);
            expect(coordToNumber([2, 0])).to.equal(3);
        });
    });
    it('move from 2,2', function () {
        expect(coordToNumber(move('L', [2, 2]))).to.equal(8);
        expect(coordToNumber(move('U', [2, 2]))).to.equal(6);
        expect(coordToNumber(move('D', [2, 2]))).to.equal(9);
        expect(coordToNumber(move('R', [2, 2]))).to.equal(9);
    })
    it('move from 1,1', function () {
        expect(coordToNumber(move('L', [1, 1]))).to.equal(4);
        expect(coordToNumber(move('U', [1, 1]))).to.equal(2);
        expect(coordToNumber(move('D', [1, 1]))).to.equal(8);
        expect(coordToNumber(move('R', [1, 1]))).to.equal(6);
    });
    it('move from 0,0', function () {
        expect(coordToNumber(move('L', [0, 0]))).to.equal(1);
        expect(coordToNumber(move('U', [0, 0]))).to.equal(1);
        expect(coordToNumber(move('D', [0, 0]))).to.equal(4);
        expect(coordToNumber(move('R', [0, 0]))).to.equal(2);
    });
    it('should make a motion from a dir', function () {
        expect(motion('U').runState([0, 0])).to.eq([1, [0, 0]]);
    })
});