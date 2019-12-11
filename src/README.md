# Installation

Dans un enveronnement avec npm ou yarn
- npm install (ou yarn install)
- npm build (ou yarn build)

Pour compiler le css
- npm buildcss (ou yarn buildcss)

# But du projet
- implémenter un maximum de jeux mais conserver une charte visuelle entre chaque jeu
- cela doit être léger à la fois en vitesse d'éxecution et en poids des fichiers
- les applis doivent fonctionner correctement sur tablette (ce qui n'est pas pour l'instant pas le cas
  pour pavage).
- le style de l'interface doit inspirer quelque chose d'artisinal dans son aspect visuel (proche d'un jeu réel en bois)
  Il y a surement des améliorations à faire à ce sujet?
- la valise doit rester libre et ne doit pas dépendre de solutions propriétaires

# Langage et modèle

## Langage
La valise est écrite en [Purescript](http://www.purescript.org/)
langage purement fonctionnel très proche de Haskell.
Il se rapproche de Elm qui est également un langage inspiré de Haskell et qui compile vers du Javascript
mais se différencie au niveau de son orientation. Elm se veut un langage très épuré est simple alors que Purescript mise plutôt sur le pouvoir d'expression en reprénant la plupart des concepts d'Haskell (monades, types de classe, n-rank polymorphism, kind polymorphism, etc) et y rajoute le concept de row polymorphism. La grande différence avec Haskell est que c'est un langage avec évaluation strict alors que Haskell est à évaluation paresseuse.


## Librairie et modèle
La valise est basée sur la librairie [Purescript-Pha](https://github.com/gbagan/purescript-pha/) (écrite par l'auteur actuel de la valise).
Celle ci est très proche du modèle TEA ([The Elm Architecture](https://dennisreimann.de/articles/elm-architecture-overview.html)).

Le principe de TEA et donc de Purescript-Pha est de donner seulement les instructions pour créer une vue, pas comment la modifier.
Pour cela, on se base sur un DOM virtuel. On génère du DOM Virtuel au lieu de vrai HTML.
La librairie s'occupe de générérer le html. En cas de mise à jour, la librairie compare l'ancien et nouveau DOM virtuel et essaie d'apporter un nombre minimal de modifications au HTML.

L'application est composée de
- un modèle (ou état) immutable initial
- de messages (représentés par une somme de types)
- une fonction pure model -> vdom   où vdom est un arbre DOM virtuel
   - le vdom peut associer des messages à certaintements évènements (click de souris, etc)
- une fonction update msg -> model -> model qui créé un nouveau modele en fonction de l'ancien et du message reçu.
   - la fonction peut-être pure où utiliser certains effets non purs à travers un run (voir plus bas)
   - ce ne sont pas réellement des effets mais des symboles qui devont être interprétés par l'application. Cela facile la testabilité de
      l'application et permet à update d'être pure.
   - la fonction s'implèmente en général par un matching sur l'argument msg

```
                     -------------
        |------------|  VDOM     |
        |            -------------
        msg               /|\
        |                  |
       \|/                 |
    -------------          |  view
    |  update   |          |
    -------------          |
        |                  |
        |              -------------
        ------------>  |  Etat     |
          met à jour   -------------
```                       


En plus de de cela, une application peut contenir
 - une liste de subscriptions: ceux sont des événements globaux non liés un élément spécifique du vdom.
 - un interpréteur qui interprète les éffets d'un run
 
 Voici est un exemple simpliste (sans effets)
 https://github.com/gbagan/purescript-pha
 et un exemple avec effets
 https://github.com/gbagan/purescript-pha/blob/master/examples/Random.purs

## runs
Contrairement à Elm qui utilse des commandes pour gérer les effets de bord,
Purescript-pha utilise des [runs](https://github.com/natefaubion/purescript-run) dans la fonction update.
Derrière ceci, on retrouve le concept de free monads et de variantes mais sans rentrer dans les détails.
Un run est un programme semblant impératif mais dans les effets de bord sont des objects attendant une interprétation.
De ce fait, un run est purement fonctionnel et facilement testable puisque l'on peut changer d'interpreter.

--------------------------------------------------------------------------

# Architecture de la valise
 
Dans l'application Valise, l'état (que l'on appelera état racine) est représenté par un record dont chaque attribut est l'état d'un jeu particulier.
De même le type des messages est une somme algébraique de types de messagages associés à chaque jeu.

Pour rendre l'implémentation d'un jeu plus aisé, la vue est "mappée", c'est à dire qu'elle transformera tous les messages du type d'un jeu en un message du type racine de messages.
De même, par l'intermédiaire d'un "lens", on peut transformer une fonction "update" travaillant sur l'état d'un jeu en une fonction travaillant sur l'état racine.

Cela permet de générer une application globale tout en permettant d'implémenter un jeu particulier comme si le type de son état et de ses messages étaient celui de l'application.


## Template pour les jeux
Hormis la page d'accueil et "preuves sans mot", à la fois le modèle et la vue de chaque jeu sont basées sur des templates.
Cela permet de
- gérer automatiquement l'historique des coups, le changement de joueurs, etc
- gérer automatiquement les fins de partie et messages de transition entre jeux.
- générer facilement le menu des options
- et faciliter de nombreuses autres taches

### Modèle
Du coté du modèle, cela consiste principalement en un object générique GState pour représenter l'état du jeu ainsi qu'une classe de types Game.

### L'état GState
GState est composée de records. Le premier "Core" est générique à tous les jeux et le second "Ext" est propre à un jeu.
Remarquez que tous les jeux ne se servent pas nécessairement de tous les attributs.

L'attribut le plus important est position. Celui indique comme son nom l'indique la position actuelle d'une partie.
Par exemple, pour une partie d'échecs, ce sera la position de chaque pièce. Cela ne comprend pas les paramètres de la partie
(dimensions du plateau, etc), ou des paramètres temporaires (position de la souris etc).
En principe, la position évolue uniquement à chaque coup (move) de l'utilisateur.

Les autres attributes communs sont:
```purescript
type CoreState pos ext = {
    position ∷ pos,
    history ∷ List pos,   -- liste des positions précédentes, on n'a pas à s'en soucier
    redoHistory ∷ List pos,  -- liste des positions pour redo, on n'a pas à s'en soucier
    dialog ∷ Dialog (GState pos ext),  -- la boite de dialogue que l'on souhaite ouvrir  
    turn ∷ Turn,             -- quel joueur doit jouer, on peut lire cet attribut 
                             -- mais n'a normalement pas à le modifier directement 
    nbRows ∷ Int,            -- nombre de lignes pour un jeu sur un plateau 2D 
                             -- mais aussi parfois nombre de cases pour un jeu 1D
    nbColumns ∷ Int,         -- nombre de colonnes pour un jeu sur un plateau 2D
    customSize ∷ Boolean,   -- si à True, permet à l'utilisateur de choisir 
                             -- la taille du plateau à sa guise dans une certaines limite 
    mode ∷ Mode,            -- mode pour les jeux à deux joueurs
    help ∷ Boolean,         -- si l'aide est activée ou non
    locked ∷ Boolean,       -- quand locked est à true, aucune action de l'utiliateur n'est possible
    showWin ∷ Boolean,      -- si à true, affiche le message de victoires
    scores ∷ M.Map String (Tuple Int pos),  -- liste des scores pour les jeux à score
    pointer ∷ Maybe PointerPosition   -- position du pointeur en % relativement au plateau de jeu
}
```

### Les Lenses

La valise fait un usage intensif de lenses. Un lens et une paire de fonctions qui focusent sur une partie d'un objet
la première fonction permet  de renvoyer l'élément ciblé et l'autre permet de renvoyer un nouvel objet avec l'object en focus modifié.
C'est un peu l'analogue fonctionnel des getters et setters de Java mais leur force réside dans le fait que les lens peuvent être composés. Dans la valise, on associera par exemple un lens à chaque attribut de GState.

[Un livre à ce sujet](https://leanpub.com/lenses)

### Messages et fonction update

Le type de message pour un jeu (que l'on appelera msg) doit contenir le type CoreMsg qui est le type des messages communs à tous les jeux.
msg doit également implément la la classe de type MsgWithCore.
coreUpdate est une fonction update pour les messages du type CoreMsg.
Un exemple:

```purescript
data Msg = Core CoreMsg | Message1 Int | Message2
instance withcore ∷ MsgWithCore Msg where core = Core

update ∷ Msg → Update State EFFS
update (Core msg) = coreUpdate msg
update (Message1 n) = ...
update Message2 = ...
```
Il est possible de rédefinir l'action à effectuer pour un message de CoreMsg de la façon suivante.

```purescript
data Msg = Core CoreMsg | Message1 Int | Message2
instance withcore ∷ MsgWithCore Msg where core = Core

update ∷ Msg → Update State EFFS
update (Core Undo) = ...
update (Core msg) = coreUpdate msg
update (Message1 n) = ...
update Message2 = ...
```



---------------------------------------------------------
3 classes sont présentes pour le modèle, n'oubliez pas que classe dans Haskell/Purescript est plus proche d'une interface que d'une vraie classe.

### La classe Game
 
Sa signature est 
```purescript
class Game pos ext mov | ext → pos mov where
    play ∷ GState pos ext → mov → Maybe pos
    initialPosition ∷ GState pos ext → Random pos
    isLevelFinished ∷ GState pos ext → Boolean
    sizeLimit ∷ GState pos ext → SizeLimit
    computerMove ∷ GState pos ext → Random (Maybe mov)
    onNewGame ∷ GState pos ext → Random (GState pos ext)
    updateScore ∷ GState pos ext → Tuple (GState pos ext) Boolean
```

Il prend trois paramètres
- pos est le type de la position
- ext est le type des attributs additionnels de GState
- mov est le type du coup (mov)

Les fonctions
- play prend en argument un état et coup et renvoie la position après le coup si celui-ci est légal ou sinon Nothing
- initialPosition renvoie une position en fonction des autres attributs en cas de nouvelle partie
                  la fonctionn peut-être potentiellement aléatoire, d'où le "Random" dans la signaure  
- isLevelFinished teste si une position est une fin de partie
- sizeLimit est utilisée uniquement pour les jeux sur un plateau 2D, il donnne les limites min et max du nombre de rangées et colonnes
- computerMove renvoie une proposition de coup par l'IA en fonction du mode choisi et avec éventuellement une part d'aléatoire.
   Renvoie Nothing s'il n'y a pas de coups valides
    ou le jeu n'est pas concerné par une IA.
- onNewGame est similaire à initialPosition mais réinitialise certains autres paramètres pour garder l'état cohérent
- updateScore renvoie un nouvel état en cas d'un record battu et un booléen indiquant s'il faut afficher un message de victoire
   lorsqu'une partie est finie. Pour les parties sans score, on renverra souvent ```state -> Tuple state true```` 
   Dans le cas d'une partie avec score, cette fonction est générée par la classe ScoreGame (voir plus bas)
   
### La classe ScoreGame
c'est une classe pour les jeux à score (la bêete, 8 reines, etc)

Sa signature est 

```purescript
class Game pos ext mov <= ScoreGame pos ext mov | ext → pos mov  where
    objective ∷ GState pos ext → Objective
    scoreFn ∷ GState pos ext → Int
    scoreHash ∷  GState pos ext → String
    isCustomGame ∷ GState pos ext → Boolean
 ```
Les paramètres sont les mêmes que pour Game.

Les fonctions
- objective: Minimize ou Maximize selon le type de jeu
- scoreFn: une fonction qui renvoie un score (entier) en fonction de la position actuelle
- scoreHash: Renvoie un string en fonction des paramètres de la partie
- isCustomGame: si c'est un custom game, le score de la partie est effacé après en avoir lancé une autre

La fonction
```purescript
updateScore' ∷ ∀pos ext mov. ScoreGame pos ext mov ⇒ ShowWinPolicy → GState pos ext → Tuple (GState pos ext) Boolean
```
donne une implémentation pour la fonction updateScore de Game

### La classe TwoPlayersGame

Sa signature est 
```purescript
class Game pos ext mov <= TwoPlayersGame pos ext mov | ext → pos mov  where
    isLosingPosition ∷ GState pos ext → Boolean
    possibleMoves ∷ GState pos ext → Array mov
```
Les fonctions:
- isLosingPosition teste si la position est perdante pour le joueur en cours
- possibleMoves renvoie la liste des mouvements légaux pour le joueur en cours

La fonction
```purescript
computerMove' ∷ ∀pos ext mov. TwoPlayersGame pos ext mov ⇒ GState pos ext → Random (Maybe mov)
```
donne une implémentation pour la fonction computerMove de Game

-----------------------------------------------------------------------

## Vue

La vue générée par la fonction template dans UI.Template

pour cela, il faut implémenter (éventuellement partiellement les attributs de
```purescript
type Elements a = {
    board ∷ VDom a,
    config ∷ VDom a,
    rules ∷ Array (VDom a),
    winTitle ∷ String,
    customDialog ∷ Unit → VDom a,
    scoreDialog ∷ Unit → VDom a
}
```
- board est le plateau principal. On toruve dans UI.Template plusieurs fonctions utilitaires pour aider à sa création
- config est le panneau des options à droite. On trouve dans UI.Icons plusieurs fonctions pour aider à sa création
- rules donne les régèles du jeu. C'est en général un tableau alternant du text et des br.
- winTitle: le message de victoire. La fonction winTitleFor2Players donne une implémentation par défaut pour les jeux à deux joueurs.
- customDialog et scoreDialog sont des boites de dialogue pour faire une pièce (ou autre objet) personnalisée et d'affichage du meilleur score respectivement

On trouvera également dans UI.Template les fonctions dndBoardProps et dndItemProps pour gérer le drag and drop.
