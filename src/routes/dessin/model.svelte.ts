import { type Edge, Graph, type IGraph } from "$lib/graph.svelte";
import { CoreModel } from "$lib/model/core.svelte";
import { Objective, WithScore } from "$lib/model/score.svelte";
import graphs from "./graphs";
import type { IModel, Move, Position } from "./types";

export default class extends WithScore<Position, Move>()(CoreModel<Position, Move>) implements IModel {
  #graphIndex: number | "custom" = $state.raw(0);
  #customGraph: IGraph = new Graph();
  
  constructor() {
    super([]);
    this.newGame();
  }

  get graphIndex() {
    return this.#graphIndex;
  }

  readonly graph: IGraph = $derived(
    this.#graphIndex === "custom" ? this.#customGraph : graphs[this.#graphIndex]
  );

  protected play(x: Move): Position | null {
    const last = this.position.at(-1);
    if (x === "raise") {
      return typeof last === "number" ? [...this.position, x] : null;
    } if (typeof last === "number" && 
        (this.#containsEdge(this.positionEdges, x, last) 
        || !this.#containsEdge(this.graph.edges, x, last))) 
    {
      return null;
    } else {
      return [...this.position, x];
    }
  }

  protected initialPosition = () => [];
  isLevelFinished = () => this.positionEdges.length === this.graph.edges.length;

  protected objective = () => Objective.Minimize;
  protected score = () => this.raiseCount;
  protected scoreHash = () => this.#graphIndex === "custom" ? null : "" + this.#graphIndex;
  protected updateScore = () => this.updateScore2(true, "onNewRecord");

  edgesOf(position: Position): Edge[] {
    const res: Edge[] = [];
    const n = position.length - 1;
    for (let i = 0; i < n; i++) {
      let u = position[i];
      let v = position[i+1];
      if (u !== "raise" && v !== "raise") {
        if (u > v) {
          [v, u] = [u, v];
        }
        res.push([u, v]);
      }
    }
    return res;
  }

  readonly positionEdges: readonly Edge[] = $derived(this.edgesOf(this.position))

  readonly raiseCount: number = $derived(this.position.filter(x => x === "raise").length);

  #containsEdge(edges: readonly Edge[], u: number, v: number) {
    if (u > v) {
      [v, u] = [u, v];
    }
    return edges.findIndex(e => e[0] === u && e[1] === v) !== -1;
  }
  
  acceptCustomGraph = (graph: IGraph) => {
    this.newGame(() => {
      if (graph.vertices.length > 0 && graph.edges.length > 0) {
        this.#customGraph = graph;
        this.#graphIndex = "custom";
      }
      this.closeDialog();
    });
  }

  setGraphIndex = (i: number | "custom") => this.newGame(() => this.#graphIndex = i);
}