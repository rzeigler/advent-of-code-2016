const R = require('ramda');
const {State} = require('./state');

const coords = [
    [1, 2, 3],
    [4, 5, 6],
    [7, 8, 9]
]

const start = [1, 1]

function coordToNumber([x, y]) {
    return coords[y][x];
}

const bounded = R.curry((orElse, [x1, y1]) => {
    if (x1 < 3 && x1 >= 0 && y1 < 3 && y1 >= 0) {
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