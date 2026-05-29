import { CoreModel } from "$lib/model/core.svelte";
import { Objective, WithScore } from "$lib/model/score.svelte";
import { WithSize } from "$lib/model/size.svelte";
import type { SizeLimit } from "$lib/model/types";
import { arrayOf, times } from "@gbagan/utils";
import { diffCoords } from "$lib/util";
import { piecesList, type IModel, type Move, type Piece, type Position } from "./types";

function legalMoves(piece: Piece, x: number, y: number) {
  switch(piece) {
    case "Q": return (x * x - y * y) * x * y === 0;
    case "K": return x * x + y * y <= 2;
    case "R": return x * y === 0;
    case "B": return x * x - y * y === 0;
    case "N": return x * x + y * y === 5;
    default: return false;
  }
}

const sizeLimit: SizeLimit = { minRows:3, minCols: 3, maxRows: 9, maxCols: 9 };

const C1 = WithSize<Position, Move>()(CoreModel<Position, Move>);
const C2 = WithScore<Position, Move>()(C1);

export default class extends C2 implements IModel {
  #selectedPiece: Piece = $state("Q");
  #allowedPieces: Piece[] = $state(["R"]);
  #multiPieces = $state(false);
  #customLocalMoves = $state(arrayOf(25, false));
  #customDirections = $state(arrayOf(9, false));

  constructor() {
    super([]);
    this.initWithSize(8, 8);
  }

  get selectedPiece() {
    return this.#selectedPiece;
  }

  get allowedPieces(): readonly Piece[] {
    return this.#allowedPieces;
  }

  get multiPieces() {
    return this.#multiPieces;
  }

  get customLocalMoves(): readonly boolean[] {
    return this.#customLocalMoves;
  }

  get customDirections(): readonly boolean[] {
    return this.#customDirections;
  }

  // teste si la pièce de type "piece" à la position index1 peut attaquer la pièce à la position index2
  // suppose que la pièce est différent de Empty
  canCapture(piece: Piece, index1: number, index2: number): boolean {
    const [row, col] = diffCoords(this.columns, index1, index2);
    if (piece !== "custom") {
      return index1 !== index2 && legalMoves(piece, row, col);
    } else {
      return (row * row - col * col) * row * col === 0
        && this.#customDirections[3 * Math.sign(row) + Math.sign(col) + 4]
        || row * row + col * col <= 8
        && this.#customLocalMoves[5 * row + col + 12]
    }
  }

  // renvoie l'ensemble des positions pouvant être attaquées par une pièce
  // à la position index sous forme de tableau de booléens
  attackedBy = (piece: Piece, index: number) =>
    times(this.rows * this.columns, i => this.canCapture(piece, i, index));

  // ensemble des cases pouvant être attaquées par une pièce sur le plateau
  capturable = $derived.by(() => {
    const n = this.rows * this.columns;
    const res = arrayOf(n, false);
    this.position.forEach((piece, i) => {
      if (piece !== null) {
        const attacked = this.attackedBy(piece, i);
        for (let i = 0; i < n; i++) {
          res[i] ||= attacked[i];
        }
      }
    });

    return res;
  });

  isValidPosition = $derived(this.position.every((piece, i) =>
    piece === null || !this.capturable[i]
  ));

  protected play(i: Move): Position | null {
    const p = this.position[i] === null ? this.#selectedPiece : null;
    return this.position.with(i, p);
  }

  protected initialPosition = () => arrayOf(this.rows * this.columns, null);
  protected isLevelFinished = () => false;
  protected onNewGame = () => {
    this.#selectedPiece = this.#allowedPieces[0];
  }

  protected objective = () => Objective.Maximize;
  protected score = () => this.isValidPosition ? this.position.filter(p => p !== null).length : 0;
  protected scoreHash = () => this.#multiPieces || this.#allowedPieces.includes("custom")
    ? null
    : `${this.rows},${this.columns},${this.#allowedPieces[0]}`;

  protected updateScore = () => this.updateScore2(false, "never")

  get sizeLimit() {
    return sizeLimit;
  }

  setSelectedPiece(piece: Piece) {
    this.#selectedPiece = piece;
  }

  toggleMultiPieces = () => {
    this.#multiPieces = !this.#multiPieces;
    if (!this.#multiPieces) {
      this.#allowedPieces = this.#allowedPieces.slice(0, 1);
    }
  }

  changeAllowedPieces(piece: Piece) {
    this.newGame(() => {
      if (this.#multiPieces) {
        const pieces = piecesList.filter(p2 => (p2 === piece) !== this.#allowedPieces.includes(p2));
        if (pieces.length > 0) {
          this.#allowedPieces = pieces;
        }
      } else {
        this.#allowedPieces = [piece];
      }
    })
  }

  customize = () => this.newGame(() => {
    this.#allowedPieces = ["custom"];
    this.#multiPieces = false;
    this.openCustomizeDialog();
  });

  toggleCustomLocalMove(index: number) {
    if (index !== 12) {
      this.#customLocalMoves[index] = !this.#customLocalMoves[index];
    }
  }

  toggleCustomDirection(index: number) {
    if (index !== 4) {
      this.#customDirections[index] = !this.#customDirections[index];
    }
  }
}