const path = require('path');
const fs = require('fs');
const lib = require('./lib');
const R = require('ramda');

const input = fs.readFileSync(path.join(__dirname, 'input.txt'), {encoding: 'utf8'});

console.log(lib.fileMotion(input).runState(lib.start));

