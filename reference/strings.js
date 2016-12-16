const R = require("ramda");

const nonEmptyLines = R.compose(R.filter(R.identity), R.split("\n"));

module.exports = {nonEmptyLines};