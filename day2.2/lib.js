const kefir = require('kefir');
const R = require('ramda');
const {State} = require('./state');

const coords = [
    [1, 2, 3],
    [4, 5, 6],
    [7, 8, 9]
]

const cords = new Map([
                      ["02", "1"],
          ["11", "2"], ["12", "3"], ["13", "4"],
    ["20", "5"], ["21", "6"], ["22", "7"], ["23", "8"], ["24", "9"],
            ["31", "A"], ["32", "B"], ["33", "C"],
                        ["42", "D"]
    
]);

const start = [0, 2];

function coordToNumber([x, y]) {
    return cords.get(`${y}${x}`);
}

const bounded = R.curry((orElse, [x1, y1]) => {
    if (cords.has(`${y1}${x1}`)) {
        return [x1, y1];
    }
    return orElse;
})

const move = R.curry((dir, [x, y]) => {
    const boundedMove = bounded([x, y]);
    switch (dir) {
        case 'U':
            return boundedMove([x, y-1]);
        case 'D':
            return boundedMove([x, y+1]);
        case 'L':
            return boundedMove([x-1, y]);
        case 'R':
            return boundedMove([x+1, y])
            
    }
});

function motion(dir) {
    return State.wrap(before => {
        const after = move(dir, before)
        return [coordToNumber(after), after];
    });
}

function lineMotion(line) {
    return R.map(R.last, R.sequence(State.of, R.map(motion, Array.from(line))));
}

function fileMotion(file) {
    return R.sequence(State.of, R.map(lineMotion, R.filter(R.identity, file.split('\n'))));
}

module.exports = {
    coordToNumber,
    move,
    motion,
    start,
    motion,
    lineMotion,
    fileMotion
}