exports.shiftKey = e => !!e.shiftKey;
exports.unsafeToMaybeAux = nothing => just => x => x === null || x === undefined ? nothing :  just(x);
exports.unsafePointerType = e && e.pointerType;
exports.unsafeKey = e && e.key;
exports.preventDefault = e => () => e && e.preventDefault && e.preventDefault()