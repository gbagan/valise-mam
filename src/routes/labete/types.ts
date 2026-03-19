import type { ICoreModel, IScoreModel, ISizeModel } from "$lib/model/types";

export type Position = boolean[];
export type Move = number;

export enum Mode { Standard, Cylinder, Torus };
export type Beast = [number, number][];
export type Beast2 = Beast[];
export enum BeastType { Type1, Type2, Type3, Type4, Custom };

export interface IModel extends ICoreModel<Position, Move>, IScoreModel<Position>, ISizeModel {
  readonly selectedColor: number;
  readonly squareColors: readonly number[]; // todo
  readonly nonTrappedBeast: readonly boolean[];
  readonly beastType: BeastType;
  readonly mode: Mode;
  readonly customBeastGrid: readonly boolean[];
  score: () => number;
  setMode: (mode: Mode) => void;
  setBeastType: (typ: BeastType) => void;
  decrementSelectedColor: () => void;
  incrementSelectedColor: () => void;
  fillZone: (start: number, end: number) => void;
  flipCustomBeast: (square: number) => void;
}