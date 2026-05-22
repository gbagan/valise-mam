import { range } from '@gbagan/utils';
import { random, shuffle } from '$lib/util';
import { CoreModel } from '$lib/model/core.svelte';
import type { IModel, Move, Position } from './types';

export default class extends CoreModel<Position, Move> implements IModel {
  #baseCount = $state(5);
  #missingPeg = $state(0);

  get baseCount() {
    return this.#baseCount;
  }

  get missingPeg () {
    return this.#missingPeg;
  }
  
  constructor() {
    super([]);
    this.newGame();
  }

  protected play(i: number): Position | null {
    const position = this.position;
    const j = this.#missingPeg;
    const x = position[i];
    const y = position[j];
    if ([1, this.#baseCount-1, -1, -this.#baseCount+1].includes((x >> 1) - (y >> 1))) {
      return position.with(i, y).with(j, x);
    } else {
      return null;
    }
  }

  isLevelFinished = () => this.position.every((i, j) => i >> 1 === j >> 1);
  protected initialPosition = () => shuffle(range(0, 2 * this.#baseCount));
  protected onNewGame() {
    this.#missingPeg = random(0, 2 * this.#baseCount);
  }

  setBaseCount = (i: number) => this.newGame(() => this.#baseCount = i);
}