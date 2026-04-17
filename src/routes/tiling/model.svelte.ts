import { CoreModel } from "$lib/model/core.svelte";
import { WithSize } from "$lib/model/size.svelte";
import type { SizeLimit } from "$lib/model/types";
import { coords, mod, arrayOf } from "$lib/util";
import type { Coord, IModel, Move, Position, Tile, TileType } from "./types";

const rotate90 = (tile: Tile) => tile.map(([row, col]) => [col, -row]) as Tile;

function rotate(tile: Tile, n: number) {
  for (let i = 0; i < n; i++) {
    tile = rotate90(tile);
  }
  return tile;
}

const translate = (tile: Tile, [row, col]: Coord) => tile.map(([r,c]) => [row + r, col + c]) as Tile;

// une position représente pour chaque position dans la grille ce que contient la case
//   0: ne contient rien,
//   -1: contient un évier,
//    n > 0 contient un morceau de tuile
// les morceaux de tuiles ayant le même numéro appartiennent à la même tuile
// une partie est terminé si toute case contient un évier ou un morceau de tuile
// un coup peut consister à poser une tuile ou à en retirer une

const sizeLimit: SizeLimit = { minRows: 3, minCols: 3, maxRows: 10, maxCols: 10 };

export default class extends WithSize<Position, Move>()(CoreModel<Position, Move>) implements IModel {
  #rotation = $state(0);
  #tileType: TileType = $state("type1");
  #sinkCount = $state(0);
  #customTile: [number, number][] = $state([]);

  constructor() {
    super([]);
    this.resize(5, 5);
  }

  get rotation() {
    return this.#rotation;
  }

  get tileType() {
    return this.#tileType;
  }

  get sinkCount() {
    return this.#sinkCount;
  }

  tile: Tile = $derived.by(() => {
    switch (this.#tileType) {
      case "type1": return [ [0, 0], [0, 1] ];
      case "type2": return [ [0, 0], [0, 1], [0, -1] ];
      case "type3": return [ [0, 0], [0, 1], [1, 0] ];
      default: return this.#customTile;
    }
  });

  readonly customTileGrid = $derived.by(() => {
    const res = arrayOf(25, false);
    for (const [row, col] of this.#customTile) {
      res[row * 5 + col + 12] = true;
    }
    return res;
  });

  // renvoie la liste des positions où devra être posée une tuile,  -1 est une position invalide
  tilePositions(index: number): number[] {
    const columns = this.columns;
    let tile2 = rotate(this.tile, mod(this.#rotation, 4));
    tile2 = translate(tile2, coords(columns, index));
    return tile2.map(([row, col]) => 0 <= col && col < columns ? row * columns + col : - 1);
  }

  // teste si une tuile peut être posée à partir de la liste des positions otenues par tilePositions
  canPutTile = (tile: number[]) => tile.every(i => this.position[i] === 0);

  // renvoie la liste des positions des éviers
  sinks = $derived(this.position.map((v, i) => v === -1 ? i : null).filter(x => x !== null));

  protected play(index: Move): Position | null {
    const tilePos = this.tilePositions(index);
    if (this.canPutTile(tilePos)) {
      const m = Math.max(...this.position) + 1;
      const position = this.position.slice();
      for (const i of tilePos) {
        position[i] = m;
      }
      return position;
    } else if (this.position[index] > 0) {
      const c = this.position[index];
      return this.position.map(x => x === c ? 0 : x);
    } else {
      return null;
    }
  }

  protected isLevelFinished = () => this.position.every(x => x !== 0);
  protected initialPosition = () => arrayOf(this.columns * this.rows, 0);
  protected onNewGame() {
    this.#rotation = 0;
  }

  get sizeLimit() {
    return sizeLimit;
  }

  rotate = () => this.#rotation += 1;

  selectSquare(index: number) {
    if (this.sinks.length < this.#sinkCount) {
      this.position = this.position.with(index, -1);
    } else {
      this.playA(index);
    }
  }


  setSinkCount = (i: number) => this.newGame(() => this.#sinkCount = i);

  flipCustomTile(i: number) {
    let [row, col] = coords(5, i);
    row -= 2;
    col -= 2;
    const idx = this.#customTile.findIndex(([r, c]) => r === row && c === col);
    if (idx === -1) {
      this.#customTile.push([row, col])
    } else {
      this.#customTile.splice(idx, 1);
    }
  }

  setTileType = (type: TileType) => this.newGame(() => {
    this.#tileType = type;
    if (type === "custom") {
      this.openCustomizeDialog();
    }
  });
}