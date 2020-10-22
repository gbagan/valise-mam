exports.storagePutImpl = key => value => () => window.localStorage.setItem(key, value);
exports.storageGetImpl = nothing => just => key => () => {
    const val = window.localStorage.getItem(key);
    return val === null ? nothing : just(val);
}