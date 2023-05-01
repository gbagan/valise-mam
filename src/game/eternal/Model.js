const hasEdge = (graph, v, w) => graph[v].includes(w)

const minBy = (list, fn) => {
    let min = undefined
    let bestScore = Infinity
    let n = list.length
    for (let i = 0; i < n; i++) {
        const x = list[i]
        const score = fn(x)
        if (score < bestScore) {
            bestScore = score
            min = x
        } 
    }
    return min
}

const maxBy = (list, fn) => minBy(list, x => -fn(x))

// teste si tous les éléments d'un tableau trié sont distincts
const allDifferent = list => {
    const n = list.length - 1
    for (let i = 0; i < n; i++) {
        if (list[i] === list[i+1])
            return false
    }
    return true
}


// renvoie les sous listes de taille k de [0, ... n-1]
const sublists = (n, k) => {
    if (k === 0) {
        return [[]]
    } else if (k > n) {
        return []
    } else {
        const sls = sublists(n - 1, k).slice()
        for (const l of sublists(n - 1, k - 1)) {
            sls.push(l.concat(n - 1))
        }
        return sls
    }
}

// une arène est un objet générique pour résoudre un jeu de reachability
// attributs
// - size: nombre de sommets de l'arène,
// - AConfs: tableau des configutations de type A (avant que l'attaquant ne joue)
// - BConfs: tableau des configutations de type B (après que l'attaquant ait joué)
// - isAConf: conf -> boolean   teste si ne configuration est de type A
// - neighbors: conf -> [conf]  renvoie la liste des configurations accessibles
// encode: conf -> Int   encode une configuration vers un entier entre 0 et size-1

const makeArenaGraph = arena => {
    const adj = new Array(arena.size)
    const reverseAdj = new Array(arena.size)
    
    const confs = [...arena.AConfs, ...arena.BConfs]

    for (const conf of confs) {
        const econf = arena.encode(conf)
        adj[econf] = []
        reverseAdj[econf] = []
    }

    for (const conf of confs) {
        for (const conf2 of arena.neighbors(conf)) {
            adj[arena.encode(conf)].push(conf2)
            reverseAdj[arena.encode(conf2)].push(conf)
        }
    }
    const attractor = computeAttractor(arena, adj, reverseAdj)
    return {adj, reverseAdj, attractor, ...arena}
}

const computeAttractor = (arena, adj, reverseAdj) => {
    const attractor = new Array(arena.size);
    const deg = new Array(arena.size);
    const stack = []

    for (const conf of arena.BConfs) {
        const econf = arena.encode(conf)
        const nbor = adj[econf]
        deg[econf] = nbor.length
        if (nbor.length === 0) { // configurations gagnantes pour l'attaquant
            stack.push(conf)
            attractor[econf] = 1
        }
    }

    while (stack.length > 0) {
        const elem = stack.shift();
        const eelem = arena.encode(elem);
        const elemval = attractor[eelem]
        for (const pred of reverseAdj[eelem]) {
            const epred = arena.encode(pred)
            if (!attractor[epred] && (arena.isAConf(pred) || --deg[epred] === 0)) {
                attractor[epred] = elemval+1
                stack.push(pred)
            }
        }
    }
    return attractor
}

// fonction générique travaillant sur une arène
const answer = (arenaGraph, conf) => {
    const defs = arenaGraph.adj[arenaGraph.encode(conf)]
    // on prilivégie les sommets qui ne sont pas dans l'attracteur
    return maxBy(defs, conf2 => arenaGraph.attractor[arenaGraph.encode(conf2)] || 1000)
}

// à partir d'ici les fonctions sont spécifiques au jeu Eternal Domination

// étant donné un graphe, une configuration renvoit l'ensemble des configurations
// accessibles depuis le déplacement de plusieurs gardes
// doit être appelé avec i = 0
function * multiMoves(graph, conf, i) {
    if (i === conf.length) {
        yield conf
    } else {
        for (const conf2 of multiMoves(graph, conf, i + 1)) {
            yield conf2
            for (const nbor of graph[conf2[i]]) {
                const conf3 = conf2.slice()
                conf3[i] = nbor
                yield conf3
            }
        }
    }
}

function * attackerPossibilities(graph, guards) {
    for (let attack = 0; attack < graph.length; attack++) {
        if (!guards.includes(attack)) {
            yield guards.concat(attack);
        }
    }
}
    
function * oneGuardPossibilities (graph, conf) {
    const attack = conf[conf.length - 1]
    const guards = conf.slice(0, conf.length - 1)
    
    for (let i = 0; i < guards.length; i++) {
        if (hasEdge(graph, guards[i], attack)) {
            const guards2 = guards.slice()
            guards2[i] = attack
            guards2.sort((a, b) => a - b)
            yield guards2
        }
    }
}
    
function * allGuardsPossibilities (graph, conf) {
    const gconf = conf.slice()
    const attack = gconf.pop()
    for (const conf2 of multiMoves(graph, gconf, 0)) {
        const conf3 = conf2.slice()
        conf3.sort((a, b) => a - b)
        if (allDifferent(conf3) && conf3.includes(attack)) {
            yield conf3
        }
    }
}

const oneRules = {
    attackerPossibilities,
    guardsPossibilities: oneGuardPossibilities
}
    
const allRules = {
    attackerPossibilities,
    guardsPossibilities: allGuardsPossibilities
}
    
const makeRules = name => name === 'one' ? oneRules : allRules;

export const guardsAnswerAux = nothing => just => edsgraph => guards => attack => {
    const ans = answer(edsgraph, guards.concat(attack))
    return !ans ? nothing : just(ans)
}

export const attackerAnswerAux = nothing => just => arenaGraph => conf => {
    const econf = arenaGraph.encode(conf)
    if(!arenaGraph.attractor[econf])
        return nothing
    const attacks = arenaGraph.adj[econf]
    const minattack = minBy(attacks, attack => arenaGraph.attractor[arenaGraph.encode(attack)] || 1000)
    return just(minattack[minattack.length-1])
}

export const makeEDSAux = graph => rulesName => k => {
    const n = graph.length
    const rules = makeRules(rulesName)
    const bconfs = []
    for (const conf of sublists(graph.length, k)) {
        for (let i = 0; i < n; i++) {
            if (!conf.includes(i)) {
                bconfs.push(conf.concat(i))
            }
        }
    }

    const arena = {
        size: (n+1) << k,
        AConfs: sublists(graph.length, k),
        BConfs: bconfs,
        isAConf: (conf => conf.length === k),
        neighbors: (conf => conf.length === k
            ? rules.attackerPossibilities(graph, conf)
            : rules.guardsPossibilities(graph, conf)
        ),
        // encode a configuration into an integer between 0 and size - 1
        encode: array => {
            let acc = 0
            const last = array[k] 
            for (let i = 0; i < k; i++) {
               acc += (1 << array[i])
            }
            if (last != null)
                acc += (last + 1) << n 
            return acc
        }
    }
    return makeArenaGraph(arena)
}
