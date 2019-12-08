## Langage et modèle

La valise est écrite en Purescript langage purement fonctionnel très proche de Haskell et sur la librairie Purescript-Pha très proche du modèle TEA (The Elm Architecture).

L'application est composée de
- un modèle (ou état) immutable initial
- de messages (représentés par une somme de types)
- une fonction pure model -> vdom   où vdom est un arbre DOM virtuel
   - le vdom peut associer des messages à certaintements évènements (click de souris, etc)
- une fonction update msg -> model -> model qui créé un nouveau modele en fonction de l'ancien et du message reçu.
   - la fonction peut-être pure où utiliser certains effets non purs (génération aléatoire d'objects, sleep pendant une certaine période, etc)
   - ce ne sont pas réellement des effets mais des symboles qui devont être interprétés par l'application. Cela facile la testabilité de
      l'application
   - la fonction s'implèmente en général par un matching sur l'argument msg
   
 En plus de de cela, une application peut contenir
 - une liste de subscriptions: ceux sont des événements globaux non liés un élément spécifique du vdom.
 - un interpréteur qui interprète les symboles d'effets évoqués plus haut
 
 Voici est un exemple simpliste (sans effets)
 https://github.com/gbagan/purescript-pha
 et un exemple avec effets
 https://github.com/gbagan/purescript-pha/blob/master/examples/Random.purs
 
 Dans l'application Valise, l'état (ou modèle) est représenté par un record dans chaque attribut est le modèle d'un jeu particulier.
