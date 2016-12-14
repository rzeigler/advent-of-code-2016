function State(fn) {
    this.fn = fn;
}

State.prototype.map = function (f) {
    return new State((s) => {
        const [a, s1] = this.fn(s);
        return [f(a), s1];
    });
}

State.of = function (v) {
    return new State((s) => [v, s]);
}

State.prototype.of = State.of;

State.wrap = function (f) {
    return new State(f);
}

State.prototype.ap = function (other) {
    return new State(s => {
        const [f, s1] = this.fn(s);
        const [v, s2] = other.fn(s1);
        return [f(v), s2];
    });
}

State.prototype.chain = function (f) {
    return new State(s => {
        const [a, s1] = this.fn(s);
        const state = f(a);
        return state.fn(s1);
    });
}

State.prototype.runState = function (s) {
    return this.fn(s);
}

module.exports = {State};