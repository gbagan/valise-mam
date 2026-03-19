import type { ICoreModel, ITwoPlayersModel } from "$lib/model/types";

export type Position = readonly (readonly [number, number])[];
export type Move = {readonly pile: number, readonly pos: number};

export interface IModel extends ICoreModel<Position, Move>, ITwoPlayersModel {
    readonly pileCount: number;
    readonly length: number;
    isLevelFinished: () => boolean;
    setPileCount: (count: number) => void;
    setLength: (length: number) => void;
}