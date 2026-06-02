import { arrayOf, count } from "@gbagan/utils";
import { coords, generate2, mod } from '$lib/util';
import { CoreModel } from '$lib/model/core.svelte';
import { Objective, WithScore } from '$lib/model/score.svelte';
import { WithSize } from '$lib/model/size.svelte';
import { type SizeLimit } from '$lib/model/types';
import { type IModel, type Position, type Move, type Beast, type Beast2, BeastType, Mode } from "./types";

const type1: Beast = [ [0, 0], [0, 1] ];
const type2: Beast = [ [0, 0], [0, 1], [0, -1] ];
const type3: Beast = [ [0, 0], [0, 1], [1, 1] ];
const beastTypes: Beast2[] = [ [ type1 ], [ type2 ], [ type3 ], [ type2, type3 ] ];

const rotate90 = (beast: Beast) => beast.map(([row, col]) => [-col, row]) as Beast;
const translate = (beast: Beast, row: number, col: number) => beast.map(([r, c]) => [r+row, c+col]) as Beast;

function allRotations(beast: Beast): Beast[] {
  const beast2 = rotate90(beast);
  const beast3 = rotate90(beast2);
  const beast4 = rotate90(beast3);    
  return [ beast, beast2, beast3, beast4 ];
}

// Fonction auxiliaire pour nonTrappedBeast.
// Il n'est pas nécessaire d'avoir une vraie fonction aléatoire
const pseudoRandomPick = <A>(arr: A[]) => arr[28921 % arr.length]

const sizeLimit: SizeLimit = { minRows: 2, minCols: 2, maxRows: 9, maxCols: 9 };

const C1 = WithSize<Position, Move>(5, 5)(CoreModel<Position, Move>);
const C2 = WithScore<Position, Move>()(C1);

export default class extends C2 implements IModel {
  #mode = $state.raw(Mode.Standard);
  #beastType = $state.raw(BeastType.Type1);
  #selectedColor = $state.raw(0);
  #squareColors: number[] = $state([]);
  #customBeast: Beast2 = $state([[]]);

  constructor() {
    super([]);
    this.newGame();
  }

  get mode() {
    return this.#mode;
  }

  get beastType() {
    return this.#beastType;
  }

  get selectedColor() {
    return this.#selectedColor;
  }

  get squareColors() {
    return this.#squareColors;
  }

  beast: Beast2 = $derived.by(() => {
    switch (this.#beastType) {
      case BeastType.Type1: return beastTypes[0];
      case BeastType.Type2: return beastTypes[1];
      case BeastType.Type3: return beastTypes[2];
      case BeastType.Type4: return beastTypes[3];
      default: return this.#customBeast;
    }
  });

  #allTranslations = (beast: Beast) =>
    generate2(this.rows, this.columns, (row, col) => translate(beast, row, col));

  // renvoie toutes les positions possibles pour une bête à plusieurs formes en prenant
  // en compte toutes les rotations et translations
  // peut contenir des positions hors du plateau
  #allBeastPositions(beast: Beast2): Beast[] {
    const res = [];
    for (const b of beast) {
      for (const b2 of allRotations(b)) {
        for (const b3 of this.#allTranslations(b2)) {
          res.push(b3);
        }
      }
    }
    return res;
  }

  #adaptBeast = (beast: Beast): Beast => beast.map(([row, col]) => {
    switch (this.#mode) {
      case Mode.Standard: return [row, col];
      case Mode.Cylinder: return [row, mod(col, this.columns)];
      default: return  [mod(row, this.rows), mod(col, this.columns)];
    }
  })

  // Renvoie toutes les emplacement possibles évitants les pièges pour la bête
  #nonTrappedBeasts = $derived.by(() => {
    const rows = this.rows
    const columns = this.columns;
    const isValidBeast = (beast: Beast) => beast.every(([row, col]) =>
      row >= 0 && row < rows && col >= 0 && col < columns
      && !this.position[row * columns + col]
    );
    return this.#allBeastPositions(this.beast).map(this.#adaptBeast).filter(isValidBeast);
  })

  // Renvoie un emplacement possible pour la bête sur le plateau sous forme d'un tableau de booléens
  // indicé par les positions du plateau.
  // Renvoie un tableau ne contenant que la valeur false si aucun emplacement pour la bête n'est possible
  nonTrappedBeast = $derived.by(() => {
    const res = arrayOf(this.rows * this.columns, false);
    if (this.#nonTrappedBeasts.length > 0) {
      const beast = pseudoRandomPick(this.#nonTrappedBeasts);
      for (const [row, col] of beast) {
        res[row * this.columns + col] = true;
      }
    }
    return res;
  });

  customBeastGrid = $derived.by(() => {
    const res = arrayOf(25, false);
    for (const [row, col] of this.#customBeast[0]) {
      res[row * 5 + col + 12] = true;
    }
    return res;
  });

  protected play = (i: Move) => this.position.with(i, !this.position[i]);
  isLevelFinished = () => this.#nonTrappedBeasts.length === 0;
  protected initialPosition = () => arrayOf(this.rows * this.columns, false);
  protected onNewGame() {
    this.#squareColors = arrayOf(this.rows * this.columns, 0);
  }

  protected objective = () => Objective.Minimize;
  score = () => count(this.position, x => x);
  protected scoreHash = () => this.#beastType === BeastType.Custom
    ? null
    : `${this.columns},${this.rows},${this.#mode},${this.#beastType}`;

  protected updateScore = () => this.updateScore2(true, "onNewRecord");

  get sizeLimit() {
    return sizeLimit;
  }

  setBeastType(type: BeastType) {
    this.#beastType = type;
    if (type === BeastType.Custom) {
      this.openCustomizeDialog();
      this.rows = Math.max(this.rows, 5);
      this.columns = Math.max(this.columns, 5);
    }
  }

  flipCustomBeast(i: number) {
    let [row, col] = coords(5, i);
    row -= 2;
    col -= 2;
    const idx = this.#customBeast[0].findIndex(([r, c]) => r === row && c === col);
    if (idx === -1) {
      this.#customBeast[0].push([row, col])
    } else {
      // todo à vérifier
      this.#customBeast[0].splice(idx, 1);
    }
  }

  decrementSelectedColor = () => {
    this.#selectedColor = (this.#selectedColor + 8) % 9;
  }

  incrementSelectedColor = () => {
    this.#selectedColor = (this.#selectedColor + 1) % 9;
  }

  fillZone(start: number, end: number) {
    let [row1, col1] = coords(this.columns, start);
    let [row2, col2] = coords(this.columns, end);
    if (row1 > row2) {
      [row1, row2] = [row2, row1];
    };
    if (col1 > col2) {
      [col1, col2] = [col2, col1];
    };
    for (let i = row1; i <= row2; i++) {
      for (let j = col1; j <= col2; j++) {
        this.#squareColors[i * this.columns + j] = this.#selectedColor;
      }
    }
  }
  //  this.startingSquare = null;
  //  this.startingPosition = null;

  setMode = (mode: Mode) => this.newGame(() => this.#mode = mode);
}