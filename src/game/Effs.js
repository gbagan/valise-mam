exports.genNumber = Math.random;
exports.getLoc = () => Object.assign({}, window.location);
exports.setTimeout = ms => fn => () => setTimeout (fn, ms);
exports.relativePointerPositionAux = nothing => just => e => () => {
    const rect = e.currentTarget.getBoundingClientRect();
    if (!rect) return nothing;
    return e.clientX >= rect.left && e.clientX < rect.left + rect.width && e.clientY >= rect.top && e.clientY < rect.top + rect.height ?
    just ({
        x: (e.clientX - rect.left) / rect.width,
        y: (e.clientY - rect.top) / rect.height
    }) : nothing;
};

exports.releasePointerCaptureAux = ev => () => ev && ev.target && ev.pointerId != null &&
                        ev.target.releasePointerCapture && ev.target.releasePointerCapture(ev.pointerId);

