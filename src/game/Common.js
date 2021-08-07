exports.releasePointerCapture = ev => () => ev && ev.target && ev.pointerId != null &&
                        ev.target.releasePointerCapture && ev.target.releasePointerCapture(ev.pointerId);