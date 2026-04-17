import { type Constructor, type CoreModel } from "./core.svelte";
import { WithTwoPlayers } from "./twoplayers.svelte";
import { sample } from "$lib/util";
import { Mode, type ITwoPlayersModel } from "./types";

export function WithCombinatorial<Pos, Move>() {
  return function <TBase extends Constructor<CoreModel<Pos, Move>>>(Base: TBase) {
    abstract class C extends WithTwoPlayers<Pos, Move>()(Base) implements ITwoPlayersModel {
      protected abstract isLosingPosition(): boolean;
      protected abstract possibleMoves(): Move[];

      protected machineMove(): Move | null {
        if (this.isLevelFinished()) {
          return null;
        }
        const moves = this.possibleMoves();
        let bestMove = null;
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
        if (bestMove !== null) {
          return bestMove;
        } else {
          return sample(moves);
        }
      }
    }
    return C;
  }
}
