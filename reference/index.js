/*eslint no-console:"off" */
"use strict";

const {
    day, 
    section,
    _
} = require("yargs")
    .demand("day")
    .number("day")
    .default("section", "1")
    .number("section")
    .demand(1)
    .argv;

const R = require("ramda");    
const fs = require("fs");
const path = require("path");

let selected = null,
    sectionImpl = null;
try {
    selected = require(path.join(__dirname, `day${day}`));
    sectionImpl = selected.impl[section];

    if (!sectionImpl) {
        console.error(`section ${section} not yet implemented for ${day}`);
        process.exit(1);
    }
    
} catch (e) {
    console.error(`day ${day} not yet implemented`);
    console.log(e);
}

console.log(sectionImpl(R.map(input => fs.readFileSync(input, {encoding: "utf8"}), _)));



    
    