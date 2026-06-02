import { type Edge, type IGraph, Graph, MutableGraph } from "$lib/graph.svelte";
import { Mode } from "$lib/model/types";
import { CoreModel } from "$lib/model/core.svelte";
import { WithTwoPlayers } from "$lib/model/twoplayers.svelte";
import { WithSize } from "$lib/model/size.svelte";
import { times, range } from "@gbagan/utils";
import { allDistinct, generate2 } from "$lib/util";
import { edgesToGraph, GraphKind, hasEdge, Phase, Rules, type IModel, type Move, type Position } from "./types";
import { ManyGuardsArena, OneGuardArena } from "./arena";
import type { RandomGenerator } from "@gbagan/rng";

const path = (n: number): IGraph => new Graph(
  "Chemin",
  times(n, i => ({
      x: 0.5 + 0.35 * Math.cos(2 * i * Math.PI / n),
      y: 0.5 + 0.35 * Math.sin(2 * i * Math.PI / n)
    })),
  times(n - 1, i => [i, i+1])
);


const cycle = (n: number): IGraph => new Graph(
 "Cycle",
  times(n, i => ({
      x: 0.5 + 0.35 * Math.cos(2 * i * Math.PI / n),
      y: 0.5 + 0.35 * Math.sin(2 * i * Math.PI / n)
    })),
  times(n, i => [i, (i+1) % n])
);

// generate a grid graph
function grid(n: number, m: number): IGraph {
  const p = Math.max(n, m);
  return new Graph(
    "Grille",
    generate2(n, m, (i, j) => ({
      x: 0.15 + 0.7 * i / (p - 1),
      y: 0.1 + 0.7 * j / (p - 1)
    })),
    [
      ...generate2(n, m - 1, (i, j) => [i * m + j, i * m + j + 1] as Edge),
      ...generate2(n - 1, m, (i, j) => [i * m + j, i * m + j + m] as Edge)
    ]
  )
}

// generate a biclique graph
const biclique = (m: number, n: number): IGraph => new Graph(
  "Biclique",
  times(n + m, i => ({
    x: i < n ? 0.2 : 0.8,
    y: 0.75 - 0.7 * (i < n ? i : i - n) / (i < n ? n : m)
  })),
  generate2(n, m, (i, j) => [i, j + n]),
);

const C1 = WithSize<Position, Move>(6, 0, true)(CoreModel<Position, Move>);
const C2 = WithTwoPlayers<Position, Move>()(C1);

export default class extends C2 implements IModel {
  #phase = $state.raw(Phase.Preparation);
  #graphKind = $state.raw(GraphKind.Path);
  #rules = $state.raw(Rules.OneGuard);
  #customGraph: IGraph = $state(new MutableGraph());

  constructor() {
    super({ guards: [], attacked: null });
    this.newGame();
  }

  get phase() {
    return this.#phase;
  }

  get graphKind() {
    return this.#graphKind;
  }

  get rules() {
    return this.#rules;
  }

  get didMachineStart() {
    // hack pour empécher l'IA de commencer tant qu'on est en phase de préparation
    return super.didMachineStart || this.phase === Phase.Preparation;
  }

  // mutable
  #nextMove: readonly number[] = $derived(this.position.guards);
  
  get nextMove() {  
    return this.#nextMove;
  }

  readonly graph = $derived.by(() => {
    switch (this.graphKind) {
      case GraphKind.Path: return path(this.rows);
      case GraphKind.Cycle: return cycle(this.rows);
      case GraphKind.Biclique: return biclique(this.rows, this.columns);
      case GraphKind.Grid: return grid(this.rows, this.columns);
      default: return this.#customGraph;
    }
  })

  readonly guardCount = $derived(this.position.guards.length);
  readonly adjGraph = $derived(edgesToGraph(this.graph.vertices.length, this.graph.edges));

  #arena = $derived(
    this.mode === Mode.Duel 
    ? null 
    : this.rules === Rules.OneGuard
    ? new OneGuardArena(this.adjGraph, this.guardCount)
    : new ManyGuardsArena(this.adjGraph, this.guardCount)
  );

  validate = () => {
    if (this.phase === Phase.Preparation) {
      this.#phase = Phase.Game;
      //this.computerStarts = false;
    } else {
      this.playA(this.nextMove);
    }
  }

  addToNextMove(from: number, to: number, srcs: readonly number[], dests: readonly number[])
    : readonly number[]
  {
    if (from === to || hasEdge(this.adjGraph, from, to)) {
      const idx = srcs.indexOf(from);
      return idx !== -1 ? dests.with(idx, to) : dests;
    } else {
      return dests;
    }
  }

  isValidNextMove(dests: readonly number[]) {
    if (this.position.attacked === null) {
      return false;
    }
    const srcs = this.position.guards;
    return dests.includes(this.position.attacked)
      && srcs.every((src, i) => src === dests[i] || hasEdge(this.adjGraph, src, dests[i]))
      && allDistinct(dests)
  }


  play(move: Move): Position | null {
    if (typeof move === "number") { // attack
      if (this.position.guards.includes(move) || this.position.attacked !== null) {
        return null;
      } else {
        return {...this.position, attacked: move };
      }
    } else if (this.position.attacked === null || !this.isValidNextMove(move)) { // defense
      return null;
    } else {
      return { guards: move, attacked: null }
    }
  }

  // todo: à verifier
  initialPosition = () => ({ guards: [], attacked: null });

  onNewGame() {
    this.#nextMove = [];
    this.#phase = Phase.Preparation;
    // draggedGuard;
  }

  isLevelFinished = () => {
    const attacked = this.position.attacked
    return attacked !== null && this.position.guards.every(guard => !hasEdge(this.adjGraph, guard, attacked));
  }

  #sizeLimit = $derived.by(() => {
    switch (this.graphKind) {
      case GraphKind.Grid: return {minRows: 2, minCols: 2, maxRows: 5, maxCols: 5};
      //case "sun": return {minRows: 3, minC  ols: 0, maxRows: 6, maxCols: 0};
      case GraphKind.Biclique: return {minRows: 1, minCols: 1, maxRows: 6, maxCols: 6};
      case GraphKind.Custom: return {minRows: 0, minCols: 0, maxRows: 0, maxCols: 0};
      default: return {minRows: 3, minCols: 0, maxRows: 11, maxCols: 0};
    }
  });

  get sizeLimit() {
    return this.#sizeLimit;
  }

  levelFinished = $derived(this.isLevelFinished());

  #randomMove(rng: RandomGenerator) : Move | undefined {
    if (this.levelFinished) {
      return undefined;
    }
    const {guards, attacked} = this.position;
    if (attacked !== null) {
      // cannot be empty
      const candidates = guards.filter(g => hasEdge(this.adjGraph, g, attacked));
      return this.addToNextMove(rng.pick(candidates)!, attacked, guards, guards)
    } else {
      const candidates = range(0, this.graph.vertices.length).filter(x => !guards.includes(x));
      return rng.pick(candidates);
    }
  }
  
  machineMove(rng: RandomGenerator): Move | undefined {
    if (this.levelFinished) {
      return undefined;
    } else if (!this.#arena || this.mode === Mode.Random) {
      return this.#randomMove(rng);
    }
    const {guards, attacked} = this.position;
    if (attacked !== null) {
      return this.#arena.guardsAnswer(guards, attacked);
    } else {
      const ans = this.#arena.attackerAnswer(guards);
      if (ans !== undefined) {
        return ans;
      } else {
        return this.#randomMove(rng);
      }
    }
  }

  selectVertex(x: number) {
    let {guards, attacked} = this.position;
    if (this.phase === Phase.Preparation) {
      const idx = guards.indexOf(x);
      if (idx === -1) {
        guards = [...guards, x];
      } else {
        guards = guards.toSpliced(idx, 1);
      }
      this.position = { attacked, guards };
    } else if (attacked === null) {
      this.playA(x);
    }
  }

  moveGuard(from: number, to: number | null) {
    if (this.rules === Rules.OneGuard && to !== null) {
      const guards = this.position.guards;
      this.playA(this.addToNextMove(from, to, guards, guards))
    } else if (this.rules === Rules.ManyGuards) {
      this.#nextMove = this.addToNextMove(from, to ?? from, this.position.guards, this.nextMove);
    }
  }

  setRules = (rules: Rules) => this.newGame(() => {
    this.#rules = rules;
  });

  setGraphKind = (kind: GraphKind) => this.newGame(() => {
    this.#graphKind = kind;
    switch (kind) {
      case GraphKind.Path:
      case GraphKind.Cycle:
        this.columns = 0;
        this.rows = 6;
        break;
      case GraphKind.Grid:
        this.columns = 3;
        this.rows = 3;
        break;
      case GraphKind.Biclique:
        this.columns = 2;
        this.rows = 5;
    }
  })

  customize = () => {
    this.newGame(() =>this.openCustomizeDialog());
  }

  acceptCustomGraph = (graph: IGraph) => {
    if (graph.vertices.length > 0) {
      this.newGame(() => {
        this.#customGraph = graph;
        this.#graphKind = GraphKind.Custom;
      });
    }
    this.closeDialog();
  }
}