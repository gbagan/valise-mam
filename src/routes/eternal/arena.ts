import { hasEdge, type AdjGraph } from "./types";
import { Arena } from "$lib/arena";
import { count, minBy, range } from "@gbagan/utils";
import { allDistinct, sublists } from "$lib/util";

type Conf = readonly number[];

function * multiMoves(graph: AdjGraph, conf: Conf, i=0): Generator<Conf> {
  if (i === conf.length) {
    yield conf
  } else {
    for (const conf2 of multiMoves(graph, conf, i + 1)) {
      yield conf2;
      for (const nbor of graph[conf2[i]]) {
        //const conf3 = conf2.slice();
        //conf3[i] = nbor;
        yield conf2.with(i, nbor);
      }
    }
  } 
}

function permutations<A>(arr: readonly A[]): A[][] {
  if (arr.length === 0) return [[]];
  
  const result = [];
  for (let i = 0; i < arr.length; i++) {
    const current = arr[i];
    const remaining = arr.toSpliced(i, 1);
    const remainingPerms = permutations(remaining);
    for (const perm of remainingPerms) {
      result.push([current].concat(perm));
    }
  }
  return result;
}

// une bonne permutation d'un tableau [v1, ..., vn] pour un ensemble de gardes [g1, ..., gn]
// est une permutation [u1, ..., vn]
// telle que pour tout i, ui = gi ou {ui, gi} est une arete du graphe
// et qui minimize le nombre de déplacements de gardes (i.e. le nombre de ui ≠ gi
function goodPermutation(graph: AdjGraph, guards: Conf, answer: Conf): number[] | undefined {
  const perms = permutations(answer).filter(guards2 => guards2.every((u, i) =>
    u === guards[i] || hasEdge(graph, u, guards[i])
  ));
  return minBy(perms, guards2 => count(guards2, (v, i) => v !== guards[i]));
}


class CommonArena {
  #guardsCount: number;
  protected graph: AdjGraph;
  #arena: Arena<Conf>;

  constructor(graph: AdjGraph, guardsCount: number, guardsMoves: (conf: Conf) => Generator<Conf>) {
    this.#guardsCount = guardsCount;
    this.graph = graph;
    const n = graph.length;
    const k = guardsCount;
    
    const encode = (array: Conf) => {
      const n = this.graph.length;
      const k = guardsCount;
      let acc = 0;
      const last = array[guardsCount];
      for (let i = 0; i < k; i++) {
        acc += (1 << array[i])
      }
      if (last !== undefined) {
        acc += (last + 1) << n;
      }
      return acc
    }
    
    const isAConf = (conf: Conf) => conf.length === guardsCount;

    const attackerMoves = (guards: Conf): Conf[] =>
      range(0, this.graph.length)
        .filter(attack => !guards.includes(attack))
        .map(attack => ([...guards, attack]));

    const neighbors = (conf: Conf) =>
      conf.length === guardsCount
      ? attackerMoves(conf)
      : Array.from(guardsMoves(conf));


    //const rules = makeRules(rulesName);
    const aconfs = sublists(graph.length, k);
    const bconfs = [];
    for (const conf of aconfs) {
      for (let i = 0; i < n; i++) {
        if (!conf.includes(i)) {
          bconfs.push(conf.concat(i))
        }
      }
    }

    this.#arena = new Arena((n+1) << k, aconfs, bconfs, isAConf, neighbors, encode);
  }
    
  // encode a configuration into an integer between 0 and size - 1
  protected encode(array: Conf): number {
    const n = this.graph.length;
    const k = this.#guardsCount;
    let acc = 0;
    const last = array[this.#guardsCount];
    for (let i = 0; i < k; i++) {
      acc += (1 << array[i])
    }
    if (last !== undefined) {
      acc += (last + 1) << n;
    }
    return acc
  }

  attackerAnswer(conf: Conf): number | undefined {
    return this.#arena.answerForB(conf)?.at(-1);
  }

  guardsAnswer(guards: Conf, attack: number) {
    return this.#arena.answerForA(guards.concat(attack));
  }
}

export class OneGuardArena extends CommonArena {
  constructor(graph: AdjGraph, guardsCount: number) {
    function *guardsMoves(conf: Conf): Generator<Conf> {
      const attack = conf.at(-1)!;
      const guards = conf.slice(0, -1);
    
      for (let i = 0; i < guards.length; i++) {
        if (hasEdge(graph, guards[i], attack)) {
          const guards2 = guards.with(i, attack);
          // todo nécessaire ?
          guards2.sort((a, b) => a - b)
          yield guards2;
        }
      }
    }
    super(graph, guardsCount, guardsMoves);
  }
}

export class ManyGuardsArena extends CommonArena {
  constructor(graph: AdjGraph, guardsCount: number) {
    function *guardsMoves(conf: Conf): Generator<Conf> {
      const attack = conf.at(-1)!;
      const guards = conf.slice(0, -1);
      for (let guards2 of multiMoves(graph, guards, 0)) {
        guards2 = guards2.toSorted((a, b) => a - b) // todo nécessaire ?
        if (allDistinct(guards2) && guards2.includes(attack)) {
          yield guards2
        }
      }
    }
    super(graph, guardsCount, guardsMoves);
  }

  guardsAnswer(guards: Conf, attack: number) {
    const ans = super.guardsAnswer(guards, attack);
    if (ans === undefined) {
      return undefined;
    } else {
      return goodPermutation(this.graph, guards, ans);
    }
  }
}