import { arrayOf } from '@gbagan/utils';
import { diffCoords } from '$lib/util';
import { CoreModel } from '$lib/model/core.svelte';
import { Objective, WithScore } from '$lib/model/score.svelte';
import { WithSize } from '$lib/model/size.svelte';
import type { IModel, Move, Position } from './types';

const sizeLimit = { minRows: 1, minCols: 2, maxRows: 6, maxCols: 12 };

const C1 = WithSize<Position, Move>()(CoreModel<Position, Move>);
const C2 = WithScore<Position, Move>()(C1);

export default class extends C2 implements IModel {
  constructor() {
    super([]);
    this.resize(4, 4);
  }

  protected play({ from, to }: Move) {
    const position = this.position;
    const [row, col] = diffCoords(this.columns, from, to);
    const pfrom = position[from];
    const pto = position[to];
    if (pfrom > 0 && pfrom <= pto && row * row + col * col === 1) {
      return position.with(from, 0).with(to, pfrom+pto);
    } else {
      return null;
    }
  }

  protected initialPosition = () => arrayOf(this.rows * this.columns, 1);

  protected isLevelFinished () {
    const position = this.position;
    const columns = this.columns;
    return position.every((x, i) => {
      const y = (i + 1) % columns === 0 ? 0 : position[i + 1];
      const z = position[i + columns] ?? 0;
      return x * (y + z) === 0
    })
  }

  score = () => this.position.filter(v => v > 0).length;
  protected scoreHash = () => `${this.rows},${this.columns}`;
  protected objective = () => Objective.Minimize;
  protected updateScore = () => this.updateScore2(true, "always");

  get sizeLimit() {
    return sizeLimit;
  }
}