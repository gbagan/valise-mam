import type { RandomGenerator } from "@gbagan/rng";
import { type Constructor, type CoreModel } from "./core.svelte";
import { WithTwoPlayers } from "./twoplayers.svelte";
import { Mode, type ITwoPlayersModel } from "./types";

export function WithCombinatorial<Pos, Move>() {
  return function <TBase extends Constructor<CoreModel<Pos, Move>>>(Base: TBase) {
    abstract class C extends WithTwoPlayers<Pos, Move>()(Base) implements ITwoPlayersModel {
      protected abstract isLosingPosition(): boolean;
      protected abstract possibleMoves(): Move[];

      protected machineMove(rng: RandomGenerator): Move | undefined {
        if (this.isLevelFinished()) {
          return undefined;
        }
        const moves = this.possibleMoves();
        let bestMove = undefined;
        if (this.mode === Mode.Expert) {
          const position = this.position;
          for (const move of moves) {
            let found = false;
            this.playHelper(move);
            if (this.isLosingPosition()) {
              found = true;
            }
            this.position = position;
            this.changeTurn();
            if (found) {
              bestMove = move;
              break;
            }
          }
        }
        if (bestMove !== undefined) {
          return bestMove;
        } else {
          return rng.pick(moves);
        }
      }
    }
    return C;
  }
}
