if ('serviceWorker' in navigator) {
    navigator.serviceWorker.register('./serviceworker.js', { scope: '.' }).then(() => {
        console.log('Service Worker enregistré correctement.');
    }).catch(error => {
        console.log('Erreur lors de l\'enregistrement du Service Worker : ', error);
    });
}