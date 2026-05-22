import { times } from '@gbagan/utils';
import { random } from '$lib/util';
import { CoreModel } from '$lib/model/core.svelte';
import { Turn } from '$lib/model/types';
import { WithCombinatorial } from '$lib/model/combinatorial.svelte';
import { type Position, type Move, type IModel } from './types';

const C = WithCombinatorial<Position, Move>()(CoreModel<Position, Move>);
export default class extends C implements IModel {
  #length = $state(10);
  #pileCount = $state(4);
  
  constructor() {
    super([]);
    this.newGame();
  }

  get length() {
    return this.#length;
  }

  get pileCount() {
    return this.#pileCount;
  }

  canPlay(move: Move) {
    const [p1, p2] = this.position[move.pile];
    return move.pos !== p1 && move.pos !== p2
        && (this.turn === Turn.Player1 ? move.pos < p2 : move.pos > p1)
  }

  protected play(move: Move): Position | null {
    if (!this.canPlay(move)) {
      return null;
    }
    const [p1, p2] = this.position[move.pile];
    return this.position.with(move.pile, this.turn === Turn.Player1 ? [move.pos, p2] : [p1, move.pos]);
  }

  protected initialPosition = () =>
    times(this.#pileCount, () => this.#length === 5 ? [0, 4] : [random(0, 5), random(5, 10)]) as Position;
  isLevelFinished = () =>
    this.position.every(([p1, p2]) =>
      p2 - p1 === 1 && p1 === (this.turn === Turn.Player2 ? this.#length - 2 : 0)
    );

  protected possibleMoves(): Move[] {
    const moves = [];
    for (let i = 0; i < this.#pileCount; i++) {
      for (let j = 0; j < this.#length; j++) {
        moves.push({pile: i, pos: j});
      }
    }

    const cmpKey = (move: Move) => {
      const [x, y] = this.position[move.pile];
      return this.turn === Turn.Player1 ? x - move.pos : move.pos - y;
    }

    return moves
      .filter(m => this.canPlay(m))
      .toSorted((m1, m2) => cmpKey(m1) - cmpKey(m2))
  }

  protected isLosingPosition() {
    return this.position.reduce((acc, [x, y]) => acc ^ (y - x - 1), 0) === 0;
  }
  
  setPileCount = (pileCount: number) => this.newGame(() => this.#pileCount = pileCount);
  setLength = (length: number) => this.newGame(() => this.#length = length);
}