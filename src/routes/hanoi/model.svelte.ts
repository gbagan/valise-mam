import { range } from '@gbagan/utils';
import { CoreModel } from "$lib/model/core.svelte";
import type { IModel } from './types';

type Position = readonly (readonly number[])[];
type Move = {readonly from: number, readonly to: number};

export default class extends CoreModel<Position, Move> implements IModel {
  #diskCount = $state(4);
  
  constructor() {
    super([]);
    this.newGame();
  }

  get diskCount() {
    return this.#diskCount;
  }

  protected play({ from, to }: Move): Position | null {
    const position = this.position;
    const last = position[from].at(-1);
    if (from === to || last === undefined || last < (position[to].at(-1) ?? -1)) {
        return null;
    }
    const init = position[from].slice(0, -1);
    return position.with(from, init).with(to, position[to].concat([last]));
  }

  protected initialPosition = () => [range(0, this.#diskCount), [], []];
  protected isLevelFinished = () => this.position[0].length === 0 && this.position[1].length === 0;

  setDiskCount = (diskCount: number) => this.newGame(() => this.#diskCount = diskCount);
}