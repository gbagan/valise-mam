import type { ICoreModel, IScoreModel } from "$lib/model/types";
import type { Edge, IGraph } from "$lib/graph.svelte";
export type Move = number | "raise";
export type Position = readonly Move[];

export interface IModel extends ICoreModel<Position, Move>, IScoreModel<Position> {
  readonly graphIndex: number | "custom";
  readonly graph: IGraph;
  edgesOf(position: Position): readonly Edge[];
  readonly positionEdges: readonly Edge[];
  readonly raiseCount: number;
  isLevelFinished: () => boolean;
  acceptCustomGraph(graph: IGraph): void;
  setGraphIndex: (index: number | "custom") => void;
}