import { range } from "@gbagan/utils";
import { allDistinct, diffCoords } from '$lib/util';
import { CoreModel } from '$lib/model/core.svelte';
import { WithSize } from '$lib/model/size.svelte';
import type { SizeLimit } from '$lib/model/types';
import { Mode, type IModel, type Move, type Position } from './types';
import type { RandomGenerator } from "@gbagan/rng";

const sizeLimit: SizeLimit = { minRows: 2, minCols: 2, maxRows: 9, maxCols: 9 };

export default class extends WithSize<Position, Move>(4, 6)(CoreModel<Position, Move>) implements IModel {
  #exit: number | null = $state(null);
  #mode = $state(Mode.Mode1);

  constructor() {
    super([]);
    this.newGame();
  }

  get exit() {
    return this.#exit;
  }

  get mode() {
    return this.#mode;
  }

  // renvoie un chemin horizontal ou vertical entre u et v si celui ci existe (u exclus du chemin)
  #pathBetween(u: number, v: number): number[] | null {
    const [row, col] = diffCoords(this.columns, u, v);
    if (row === 0) {
      return u < v ? range(u+1, v+1) : range(u-1, v-1, -1);
    } else if (col === 0) {
      return u < v ? range(u + this.columns, v+1, this.columns) : range(u - this.columns, v-1, -this.columns);
    } else {
      return null;
    }
  }

  // teste si un chemin est valide (sans répétition de sommets)
  // les extrémités peuvent être identiques si le chemin forme un cycle hamiltonien)
  // on ne peut pas passer par le sommet de sortie sauf si c'est le sommet final
  #isValidPath(path: number[]) {
    if (this.#exit === null || path.length <= 2) {
      return true;
    }
    const path2 = path.slice(1);
    const path3 = path2.slice(0, -1);
    const begin = path[0];
    const end = path.at(-1)!;
    return allDistinct(path2)
        && !path3.includes(this.#exit)
        && !path3.includes(end)
        && (begin !== end
            || path.length === this.rows * this.columns 
                + (begin === this.#exit ? 1 : 0) 
                && end === this.#exit
            )
  }

  protected play(v: Move): Position | null {
    if (this.position.length === 0) {
      return this.#mode === Mode.Mode2 ? [v] : null;
    } else {
      const last = this.position.at(-1)!;
      const p = this.#pathBetween(last, v);
      if (p === null || p.length === 0) {
        return null;
      }
      const p2 = this.position.concat(p);
      return this.#isValidPath(p2) ? p2 : null;
    }
  }

  isLevelFinished = () =>
    this.#exit !== null 
    && this.position.length > 0
    && this.position.length === this.columns * this.rows
        + (this.#exit === this.position[0] ? 1 : 0);

  protected initialPosition = () => this.exit === null ? [] : [this.exit];
  
  protected onNewGame(rng: RandomGenerator) {
    this.#exit = this.#mode === Mode.Mode1 ? rng.int(0, this.rows * this.columns - 1) : null;
  }

  get sizeLimit() {
    return sizeLimit;
  }

  selectSquare(square: number) {
    if (this.position.length === 0) {
      this.position = [square];
    } else if (this.#exit === null) {
      this.#exit = square;
    } else {
      this.playA(square);
    }
  }

  setMode = (mode: Mode) => this.newGame(() => {
    this.#mode = mode;
  })
}