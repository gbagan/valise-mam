import { tick } from "svelte";
import { Dialog, type ICoreModel } from "./types";

export const VERSION = 1;

export abstract class CoreModel<Position, Move> implements ICoreModel<Position, Move> {
  #position: Position;
  #history: Position[] = $state.raw([]);
  #redoHistory: Position[] = $state.raw([]);
  #help = $state(false);
  #isVictoryShown = $state(false);
  #dialog = $state(Dialog.None);
  #newGameAction: (() => void) | null = $state(null);
  #locked: boolean = $state(false);

  protected abstract play(m: Move): Position | null;
  protected abstract initialPosition(): Position;
  protected abstract isLevelFinished(): boolean;
  protected onNewGame() {};

  constructor(position: Position) {
    this.#position = $state.raw(position);
  }

  get position() {
    return this.#position;
  }

  protected set position(pos: Position) {
    this.#position = pos;
  }

  get isVictoryShown() {
    return this.#isVictoryShown;
  }

  async showVictory() {
    this.#isVictoryShown = false;
    await tick();
    this.#isVictoryShown = true;
  }

  isHistoryEmpty = () => this.#history.length === 0;
  isRedoHistoryEmpty = () => this.#redoHistory.length === 0;
  historyLength = () => this.#history.length;

  get dialog() {
    return this.#dialog;
  }

  closeDialog = () => {
    this.#dialog = Dialog.None;
    this.#newGameAction = null;
  }

  openRulesDialog = () => {
    this.#dialog = Dialog.Rules;
  }

  openScoreDialog = () => {
    this.#dialog = Dialog.Score;
  }

  openCustomizeDialog = () => {
    this.#dialog = Dialog.Customize;
  }

  get locked() {
    return this.#locked;
  }

  lock = async (action: () => Promise<void>) => {
    this.#locked = true;
    await action();
    this.#locked = false;
  }

  get help() {
    return this.#help;
  }

  toggleHelp = () => {
    this.#help = !this.#help;
  }

  setHelp = (h: boolean) => {
    this.#help = h;
  }

  canPlay(move: Move): boolean {
    return this.play(move) !== null;
  }

  protected playHelper(move: Move, push?: boolean): boolean {
    const position = this.play(move);
    if (position === null)
      return false;
    if (push) {
      this.#history = [this.position, ...this.#history];
      this.#redoHistory = [];
    }
    this.#position = position;
    return true;
  }

  protected updateScore(): { isNewRecord: boolean, showWin: boolean } {
    return {
      isNewRecord: false,
      showWin: this.isLevelFinished()
    }
  }

  protected afterPlay() {};

  async playA(move: Move) {
    if (!this.playHelper(move, true)) {
      return
    }

    const {isNewRecord, showWin} = this.updateScore();
    if (isNewRecord) {
      this.onNewRecord();
    }
    
    if (showWin) {
      await this.showVictory();
    } else {
      this.afterPlay();
    }
  }

  protected resetAttributes() {
    this.#history = [];
    this.#redoHistory = [];
    this.#help = false;
    this.#dialog = Dialog.None;
  }

  newGame(action?: () => void) {
    if (!this.#newGameAction && action && this.#history.length > 0 && !this.isLevelFinished()) {
      this.#newGameAction = action;
      this.#dialog = Dialog.NewGame;
      return;
    }

    this.resetAttributes();

    (action || this.#newGameAction || (() => {}))();
    this.onNewGame();
    do {
      this.#position = this.initialPosition();
    } while (this.isLevelFinished());
    this.#newGameAction = null;
  }

  protected onNewRecord() {}

  protected undoHelper() {
    if (this.#history.length === 0) {
      return false;
    }
    const [position, ...nextHistory] = this.#history;
    this.#redoHistory = [this.#position, ...this.#redoHistory];
    this.#position = position;
    this.#history = nextHistory;
    return true;
  }

  undo = () => this.undoHelper();

  protected redoHelper() {
    if (this.#redoHistory.length === 0) {
      return false;
    }
    const [position, ...nextHistory] = this.#redoHistory;
    this.#history = [this.#position, ...this.#history];
    this.#position = position;
    this.#redoHistory = nextHistory;
    return true;
  }

  redo = () => this.redoHelper();

  protected resetHelper() {
    if (this.#history.length === 0) {
      return false;
    }
    const position = this.#history.at(-1)!;
    this.#history = [];
    this.#redoHistory = [];
    this.#position = position;
    return true;
  }

  reset = () => this.resetHelper();
}

export type Constructor<T> = abstract new (...args: any[]) => T;