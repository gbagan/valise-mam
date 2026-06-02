import { range, replaceAt } from '@gbagan/utils';
import { CoreModel } from '$lib/model/core.svelte';
import type { IModel, Move, Position } from './types';
import type { RandomGenerator } from '@gbagan/rng';

export default class extends CoreModel<Position, Move> implements IModel {
  #baseCount = $state.raw(5);
  #missingPeg = $state.raw(0);

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
      return replaceAt(position, [i, y], [j, x]);
    } else {
      return null;
    }
  }

  isLevelFinished = () => this.position.every((i, j) => i >> 1 === j >> 1);
  protected initialPosition = (rng: RandomGenerator) => rng.shuffle(range(0, 2 * this.#baseCount));
  protected onNewGame(rng: RandomGenerator) {
    this.#missingPeg = rng.int(0, 2 * this.#baseCount-1);
  }

  setBaseCount = (i: number) => this.newGame(() => this.#baseCount = i);
}