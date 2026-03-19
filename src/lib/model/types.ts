export enum Dialog { None, Rules, Score, Customize, NewGame };

export interface ICoreModel<Position, Move> {
  readonly position: Position;
  canPlay(move: Move): boolean;
  readonly isVictoryShown: boolean
  showVictory: () => Promise<void>;
  isHistoryEmpty: () => boolean;
  isRedoHistoryEmpty: () => boolean;
  historyLength: () => number;
  readonly dialog: Dialog;
  closeDialog: () => void;
  openRulesDialog: () => void;
  openScoreDialog: () => void;
  openCustomizeDialog: () => void;
  readonly locked: boolean;
  lock: (action: () => Promise<void>) => Promise<void>;
  readonly help: boolean;
  setHelp: (help: boolean) => void;
  toggleHelp: () => void;
  playA: (move: Move) => Promise<void>;
  newGame: (action?: () => void) => void;
  undo: () => boolean;
  redo: () => boolean;
  reset: () => boolean;
}

export interface IScoreModel<Position> {
  bestScore: () => number | null;
  bestPosition: () => Position | null;
  loadRecords: () => void;
}

export function isScoreModel<Pos, Move>(model: ICoreModel<Pos, Move>):
  model is ICoreModel<Pos, Move> & IScoreModel<Pos>
{
  return typeof (model as any).bestScore === "function";
}

export interface SizeLimit {
  minRows: number;
  maxRows: number;
  minCols: number;
  maxCols: number;
}

export interface ISizeModel {
  readonly rows: number;
  readonly columns: number;
  readonly customSize: boolean;
  readonly sizeLimit: SizeLimit | null;
  resize: (rows: number, columns: number, customSize?: boolean) => void;
}

export function isSizeModel<Pos, Move>(model: ICoreModel<Pos, Move>): 
  model is ICoreModel<Pos, Move> & ISizeModel 
{
  return (model as Partial<ISizeModel>).rows !== undefined 
    && (model as Partial<ISizeModel>).columns !== undefined
    && (model as Partial<ISizeModel>).customSize !== undefined
}

export enum Mode { Random, Expert, Duel };
export enum Turn { Player1, Player2 }

export interface ITwoPlayersModel {
  readonly turn: Turn;
  readonly mode: Mode;
  readonly didMachineStart: boolean;
  machineStarts: () => void;
  setMode: (mode: Mode) => void;
  turnMessage: () => string;
  winTitleFor2Player: () => string;
}