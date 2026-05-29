import { range } from '@gbagan/utils';
import { CoreModel } from '$lib/model/core.svelte';
import { WithCombinatorial } from '$lib/model/combinatorial.svelte';
import { WithSize } from '$lib/model/size.svelte';
import { SoapMode, type IModel, type Move, type Position } from './types';
import type { SizeLimit } from '$lib/model/types';
import type { RandomGenerator } from '@gbagan/rng';

const sizeLimit: SizeLimit = { minRows: 4, minCols: 4, maxRows: 10, maxCols: 10 };

const C1 = WithSize<Position, Move>()(CoreModel<Position, Move>);
const C2 = WithCombinatorial<Position, Move>()(C1);

export default class extends C2 implements IModel {
  #soap: readonly [number, number] | null = $state.raw(null);
  #soapMode = $state(SoapMode.Corner);

  constructor() {
    super({left: 0, right: 0, top: 0, bottom: 0});
    this.initWithSize(6, 7);
  }

  get soap() {
    return this.#soap;
  }

  get soapMode() {
    return this.#soapMode;
  }

  protected play([dir, x]: Move): Position | null {
    switch (dir) {
      case "left": return {...this.position, left: x};
      case "right": return {...this.position, right: x};
      case "top": return {...this.position, top: x};
      default: return {...this.position, bottom: x};
    }
  }

  isLevelFinished() {
    const {left, right, top, bottom} = this.position;
    return left === right - 1 && top === bottom - 1;
  }

  protected initialPosition = () => ({left: 0, right: this.columns, top: 0, bottom: this.rows});

  protected onNewGame(rng: RandomGenerator) {
    if (this.#soapMode === SoapMode.Custom) {
      this.#soap = null;
    } else {
      const row = this.#soapMode === SoapMode.Standard ? rng.int(0, this.rows - 1) : 0;
      const col = this.#soapMode !== SoapMode.Corner ? rng.int(0, this.columns - 1) : 0;
      this.#soap = [row, col];
    }
  }

  protected isLosingPosition(): boolean {
    if (this.#soap === null) {
      return false;
    }
    const [row, col] = this.#soap;
    const {left, right, top, bottom} = this.position;
    return ((col - left) ^ (right - col - 1) ^ (row - top) ^ (bottom - row - 1)) === 0;
  }

  possibleMoves(): Move[] {
    if (this.#soap === null) {
       return [];
    }
    const [row, col] = this.#soap;
    const {left, right, top, bottom} = this.position;
    const l = range(left+1, col+1).map(i => ["left", i] as Move);
    const r = range(col+1, right).map(i => ["right", i] as Move);
    const t = range(top+1, row+1).map(i => ["top", i] as Move);
    const b = range(row+1, bottom).map(i => ["bottom", i] as Move);
    return l.concat(r, t, b);
  }

  get sizeLimit() {
    return sizeLimit;
  }

  putSoap(s: [number, number]) {
    if (this.#soap === null) {
      this.#soap = s;
    }
  }

  setSoapMode = (mode: SoapMode) => this.newGame(() => this.#soapMode = mode);
}