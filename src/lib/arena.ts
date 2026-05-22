 import { maxBy, minBy } from "@gbagan/utils";

// une arène est un objet générique pour résoudre un jeu de reachability
// attributs
// - size: nombre de sommets de l'arène,
// - AConfs: tableau des configutations de type A (avant que l'attaquant ne joue)
// - BConfs: tableau des configutations de type B (après que l'attaquant ait joué)
// - isAConf: teste si ne configuration est de type A
// - neighbors: renvoie la liste des configurations accessibles
// - encode: encode une configuration vers un entier entre 0 et size-1

export class Arena<Conf> {
  #size: number;
  #adj: Conf[][];
  #attractor: number[];
  #encode: (conf: Conf) => number;

  constructor(size: number, aConfs: readonly Conf[], bConfs: readonly Conf[], 
    isAConf: (conf: Conf) => boolean,
    neighbors: (conf: Conf) => Conf[],
    encode: (conf: Conf) => number
  ) {
    this.#size = size;
    this.#encode = encode;
    const adj: Conf[][] = new Array(this.#size);
    const reverseAdj: Conf[][] = new Array(this.#size);
    const confs = [...aConfs, ...bConfs];
    for (const conf of confs) {
      const econf = encode(conf);
      adj[econf] = [];
      reverseAdj[econf] = [];
    }
    for (const conf of confs) {
      for (const conf2 of neighbors(conf)) {
        adj[encode(conf)].push(conf2);
        reverseAdj[encode(conf2)].push(conf);
      }
    }
  
    this.#adj = adj;

    // compute attractor
    const attractor: number[] = new Array(this.#size);
    const deg: number[] = new Array(this.#size);
    const stack: Conf[] = [];

    for (const conf of bConfs) {
      const econf = encode(conf);
      const nbor = adj[econf];
      deg[econf] = nbor.length;
      if (nbor.length === 0) { // configurations gagnantes pour l'attaquant
        stack.push(conf);
        attractor[econf] = 1;
      }
    }

    while (stack.length > 0) {
      const elem = stack.shift()!;
      const eelem = encode(elem);
      const elemval = attractor[eelem]
      for (const pred of reverseAdj[eelem]) {
        const epred = encode(pred)
        if (!attractor[epred] && (isAConf(pred) || --deg[epred] === 0)) {
          attractor[epred] = elemval+1
          stack.push(pred);
        }
      }
    }
    this.#attractor = attractor;
  }

  answerForA(conf: Conf) {
    const defs = this.#adj[this.#encode(conf)];
    // on prilivégie les sommets qui ne sont pas dans l'attracteur
    return maxBy(defs, conf2 => this.#attractor.at(this.#encode(conf2)) ?? 255)
  }
  answerForB(conf: Conf) {
    const defs = this.#adj[this.#encode(conf)];
    return minBy(defs, conf2 => this.#attractor.at(this.#encode(conf2)) ?? 255)
  }
}