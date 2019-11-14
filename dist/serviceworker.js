const CACHE = 'cache-valise-pwa';

self.addEventListener('install', e => {
    e.waitUntil(
        caches.open(CACHE).then(cache =>
            cache.addAll([
                '.',
                './index.html',
                './bundle.css',
                './bundle.js',
            ])
        )
    );
});

self.addEventListener('fetch', evt =>
    evt.respondWith(fromNetwork(evt.request, 3000)
        .catch(() => fromCache(evt.request))
    )
);
 
const fromNetwork = (request, timeout) =>
    new Promise((fulfill, reject) => {
        const timeoutId = setTimeout(reject, timeout);
        fetch(request).then(response => {
            clearTimeout(timeoutId);
            fulfill(response);
        }, reject);
    });
 
const fromCache = request =>
    caches.open(CACHE).then(cache =>
        cache.match(request).then(matching => matching || Promise.reject('no-match'))
    );