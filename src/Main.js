exports.getLocationHref = () => window.location.href;

exports.setTimeout = ms => fn => () => setTimeout (fn, ms);