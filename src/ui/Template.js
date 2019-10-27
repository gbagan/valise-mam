exports.relativePointerPositionAux = nothing => just => e => () => {
    const rect = e.currentTarget.getBoundingClientRect();
    if (!rect) return nothing;
    return e.clientX >= rect.left && e.clientX < rect.left + rect.width && e.clientY >= rect.top && e.clientY < rect.top + rect.height ?
    just ({
        left: e.clientX - rect.left,
        top: e.clientY - rect.top,
        width: rect.width,
        height: rect.height,
    }) : nothing;
};