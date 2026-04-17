import { type SizeLimit } from '$lib/model/types';
import { CoreModel } from '$lib/model/core.svelte';
import { WithSize } from '$lib/model/size.svelte';
import { diffCoords, arrayOf } from '$lib/util';
import type { Mode, Move, Position } from './types';

const sizes: [number, number][] = [ [3, 3], [4, 4], [2, 10], [3, 10], [5, 5]]; 
const sizeLimit: SizeLimit = { minRows: 2, minCols: 2, maxRows: 12, maxCols: 12 };

export default class extends WithSize<Position, Move>()(CoreModel<Position, Move>) {
  #mode: Mode = $state(0);
  #level = $state(0); // le niveau en cours
  #maxLevels = $state([ 0, 1, 1, 0 ]);

  constructor() {
    super({light: [], played: [] });
    this.resize(3, 3);
  }

  get mode() {
    return this.#mode;
  }

  get level() {
    return this.#level;
  }

  get maxLevels() {
    return this.#maxLevels;
  }

  // indique si index1 est voisine de index2 selon le mode de jeu en cours
  // c'est à dire que si l'on active index1, index2 va changer de couleur
  #neighbor(index1: number, index2: number): boolean {
    const [row, col] = diffCoords(this.columns, index1, index2);
    return row * row + col * col === 1
      || this.#mode % 3 === 0 && index1 === index2
      || this.#mode >= 2 && index1 !== index2 && row * col === 0
  }

  // met à jour le tableau light en fonction du coup joué à la position index
  #toggleCell = (light: boolean[], index: number) =>
    light.map((b, i) => b !== this.#neighbor(index, i)); 
 
  play(move: Move): Position | null {
    const {light, played} = this.position;
    return {
      light: this.#toggleCell(light, move),
      played: played.with(move, !played[move])
    }
  };

  initialPosition(): Position {
    const size = this.rows * this.columns;
    // todo niveau aléatoire
    const light = arrayOf(size, true);
    return { light, played: arrayOf(size, false) }
  }

  isLevelFinished = () => this.position.light.every(b => !b);

  protected onNewGame() {
    if (this.#level < 5) {
      const [rows, columns] = sizes[this.#level];
      this.customSize = false;
      this.rows = rows;
      this.columns = columns;
    } else if (!this.customSize) {
      this.customSize = true;
      this.rows = 8;
      this.columns = 8;  
    }
  }

  get sizeLimit() {
    return sizeLimit;
  }

  // si le niveau est fini, on met à jour les nivaux débloqués
  // et on passe au niveau suivant
  async playA(i: number) {
    await super.playA(i);
    if (this.isLevelFinished()) {
      this.#maxLevels[this.#mode] =
        this.#level >= 4
          ? 6
          : this.#level + (this.#mode === 0 || this.#mode === 3 ? 1 : 2);
      // todo saveToStorage
      this.newGame(() => this.#level = Math.min(this.#level + 1, 6));
    }
  }

  changeMode = (i: Mode) => this.newGame(() => {
    this.#mode = i;
    this.#level = 0;
  });

  changeLevel = (i: number) => this.newGame(() => {
    this.#level = i;
  });
}