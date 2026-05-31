import type { ICoreModel, ISizeModel } from "$lib/model/types";

export type Position = {light: boolean[], played: boolean[]};
export type Move = number;
export type Mode = 0 | 1 | 2 | 3;

export interface IModel extends ICoreModel<Position, Move>, ISizeModel {
  readonly mode: Mode;
  readonly level: number;
  readonly maxLevels: readonly number[];
  isLevelFinished: () => boolean;
  changeMode: (mode: Mode) => void;
  changeLevel: (level: number) => void;
  loadRecords: () => void;
}