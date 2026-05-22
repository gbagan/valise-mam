import { sleep } from "@gbagan/utils";
import { type Constructor, CoreModel } from "./core.svelte";
import { Mode, Turn, type ITwoPlayersModel } from "./types";

export function WithTwoPlayers<Position, Move>() {
  return function <TBase extends Constructor<CoreModel<Position, Move>>>(Base: TBase) {
    abstract class C extends Base implements ITwoPlayersModel {
      #turn = $state(Turn.Player1);
      #didMachineStart = $state(false);
      #mode = $state(Mode.Duel);

      protected abstract machineMove(): Move | undefined;

      get turn() {
        return this.#turn;
      }

      get didMachineStart() {
        return this.#didMachineStart;
      }

      get mode() {
        return this.#mode;
      }

      machineStarts = () => {
        this.#didMachineStart = true;
        this.#machinePlays();
      }

      setMode = (mode: Mode) => this.newGame(() => this.#mode = mode);

      protected changeTurn() {
        this.#turn = this.#turn === Turn.Player1 ? Turn.Player2 : Turn.Player1;
      }

      protected undoHelper() {
        if (super.undoHelper()) {
          if (this.#mode === Mode.Duel) {
            this.changeTurn();
          } else {
            this.#turn = this.#didMachineStart ? Turn.Player2 : Turn.Player1;
          }
          return true;
        } else {
          return false;
        }
      }

      protected redoHelper() {
        if (super.redoHelper()) {
          if (this.#mode === Mode.Duel) {
            this.changeTurn();
          } else {
            this.#turn = this.#didMachineStart ? Turn.Player2 : Turn.Player1;
          }
          return true;
        } else {
          return false;
        }
      }

      protected resetHelper() {
        if (super.resetHelper()) {
          this.#turn = this.#didMachineStart ? Turn.Player2 : Turn.Player1;
          return true;
        } else {
          return false;
        }
      }

      protected playHelper(move: Move, push?: boolean): boolean {
        if (super.playHelper(move, push)) {
          this.#turn = this.#turn === Turn.Player1 ? Turn.Player2 : Turn.Player1;
          return true;
        } else {
          return false;
        }
      }

      protected afterPlay() {
        if (this.#mode === Mode.Expert || this.#mode === Mode.Random) {
          this.lock(async () => {
            await sleep(1000);
            this.#machinePlays();
          })
        }
      }

      async #machinePlays() {
        const move = this.machineMove();
        if (move === undefined) {
          return;
        } 
        this.playHelper(move);
        if (this.isLevelFinished()) {
          await this.showVictory();
        }
      }

      protected resetAttributes() {
        super.resetAttributes();
        this.#turn = Turn.Player1;
        // this.#mode = Mode.Duel;
        this.#didMachineStart = false;
      }

      // un message qui indique à qui est le tour ou si la partie est finie
      turnMessage() {
        if (this.isLevelFinished()) {
          return "Partie finie"
        } else if ((this.#turn === Turn.Player1) !== this.#didMachineStart) {
          return "Tour du premier joueur"
        } else if (this.#mode === Mode.Duel) {
          return "Tour du second joueur"
        } else {
          return "Tour de la machine"
        }
      }

      winTitleFor2Player() {
        return this.#mode === Mode.Duel
          ? `Le ${this.#turn === Turn.Player1 ? "premier" : "second"} joueur gagne`
          : (this.#turn === Turn.Player2) !== this.#didMachineStart
          ? "Tu as gagné"
          : "La machine gagne";
      }
    }
    return C;
  }
}
