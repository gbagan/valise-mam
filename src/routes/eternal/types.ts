import { tabulate } from "$lib/util";
import type { ICoreModel, IScoreModel, ISizeModel, ITwoPlayersModel } from "$lib/model/types";
import type { Edge, IGraph } from "$lib/graph.svelte";
export enum GraphKind { Path, Cycle, Grid, Biclique, Custom }
export enum Rules { OneGuard, ManyGuards }
export enum Phase { Preparation, Game }
export type Position = {
  readonly guards: readonly number[],
  readonly attacked: number | null
};
export type Move = number | readonly number[]   // attack | defense;

export interface IModel extends ICoreModel<Position, Move>, ISizeModel, ITwoPlayersModel {
  readonly phase: Phase;
  readonly graphKind: GraphKind;
  readonly rules: Rules;
  readonly graph: IGraph;
  readonly guardCount: number;
  readonly nextMove: readonly number[];
  readonly levelFinished: boolean;
  moveGuard: (from: number, to: number | null) => void;
  selectVertex: (x: number) => void;
  customize: () => void;
  setGraphKind: (kind: GraphKind) => void;
  setRules: (rules: Rules) => void;
  acceptCustomGraph: (graph: IGraph) => void;
  validate: () => void;
}

export type AdjGraph = number[][];

export const hasEdge = (graph: AdjGraph, v: number, w: number) => graph[v].includes(w);

export function addEdge(graph: AdjGraph, u: number, v: number) {
  graph[u].push(v);
  graph[v].push(u);
}

export function edgesToGraph(n: number, edges: readonly Edge[]): AdjGraph {
  const g = tabulate(n, () => []);
  for (const [u, v] of edges) {
    addEdge(g, u, v);
  }
  return g;
}