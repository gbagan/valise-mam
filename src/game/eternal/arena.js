import {Map_dec} from '../util';

const makeArenaGraph = arena => {
    const adj = new Map();
    const reverseAdj = new Map();
    
    const confs = [...arena.AConfs(), ...arena.BConfs()];

    for (const conf of confs) {
        adj.set(conf.toString(), []);
        reverseAdj.set(conf.toString(), []);
    }

    for (const conf of confs) {
        for (const conf2 of arena.neighbors(conf)) {
            adj.get(conf.toString()).push(conf2);
            reverseAdj.get(conf2.toString()).push(conf);
        }
    }
    const attractor = computeAttractor(arena, adj, reverseAdj);
    return { adj, reverseAdj, attractor, ...arena };
};

const computeAttractor = (arena, adj, reverseAdj) => {
    const attractor = new Set();
    const deg = new Map();
    const stack = [];

    for (const conf of arena.BConfs()) {
        const nbor = adj.get(conf.toString());
        deg.set(conf.toString(), nbor.length);
        if (nbor.length === 0) { // final winning configurations for the attacker
            stack.push(conf);
            attractor.add(conf.toString());
        }
    }

    while (stack.length > 0) {
        const elem = stack.pop();

        for (const pred of reverseAdj.get(elem.toString())) {
            const rpred = pred.toString();
            if (!attractor.has(rpred) &&  (arena.isAConf(pred) || Map_dec(deg, pred) === 0)) {
                attractor.add(rpred);
                stack.push(pred);
            }
        }
    }
    return attractor;
};

export const startingConf = arenaGraph => {
    for (const conf of arenaGraph.AConfs()) {
        if (!arenaGraph.attractor.has(conf.toString())) {
            return conf;
        }
    }
    return null;
};

const answer = (arenaGraph, conf) =>
    arenaGraph.adj.get(conf.toString()).find(conf2 => !arenaGraph.attractor.has(conf2.toString()));

function * multiMoves(graph, conf, i) {
    if (i === conf.length) {
        yield conf;
    } else {
        for (const conf2 of multiMoves(graph, conf, i + 1)) {
            yield conf2;
            for (const nbor of graph[conf2[i]]) {
                const conf3 = conf2.slice();
                conf3[i] = nbor;
                yield conf3;
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
    const attack = conf[conf.length - 1];
    const guards = conf.slice(0, conf.length - 1);
    
    for (let i = 0; i < guards.length; i++) {
        if (hasEdge(graph, guards[i], attack)) {
            const guards2 = guards.slice();
            guards2[i] = attack;
            guards2.sort((a, b) => a - b);
            yield guards2;
        }
    }
}
    
function * allGuardsPossibilities (graph, conf) {
    const gconf = conf.slice();
    const attack = gconf.pop();
    for (const conf2 of multiMoves(graph, gconf, 0)) {
        const conf3 = conf2.slice();
        conf3.sort((a, b) => a - b);
        if (allDifferent(conf3) && conf3.includes(attack)) {
            yield conf3;
        }
    }
}
    
const oneRules = {
    attackerPossibilities,
    guardsPossibilities: oneGuardPossibilities
};
    
const allRules = {
    attackerPossibilities,
    guardsPossibilities: allGuardsPossibilities
};
    
const makeRules = name => name === 'one' ? oneRules : allRules;


export const guardsAnswer = (edsgraph, guards, attack) => {
    const ans = answer(edsgraph, sortBy(x => x, guards).concat(attack));
    if (!ans) {
        return null;
    }

    const perms =  [...permutations(ans)];
    return perms
        |> filter(all((guard, i) => guard === guards[i] || hasEdge(edsgraph.graph, guard, guards[i])))
        |> minBy(countBy((guard, i) => guard !== guards[i]))
};

export const makeEDS = (graph, k, rulesName) => {
    const rules = makeRules(rulesName);
    const arena = {
        AConfs: () => sublists(graph.length, k),
        BConfs: function * () {
            for (const conf of sublists(graph.length, k)) {
                for (let i = 0; i < graph.length; i++) {
                    if (!conf.includes(i)) {
                        yield conf.concat(i);
                    }
                }
            }
        },
        isAConf: conf => conf.length === k,
        neighbors: conf => conf.length === k
            ? rules.attackerPossibilities(graph, conf)
            : rules.guardsPossibilities(graph, conf)
    };

    const arenaGraph = makeArenaGraph(arena);
    return { graph, ...arenaGraph };
};
