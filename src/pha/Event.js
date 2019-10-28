exports.shiftKey = e => !!e.shiftKey;

exports.pointerTypeAux = nothing => just => e => e && e.pointerType ? just(e.pointerType) : nothing