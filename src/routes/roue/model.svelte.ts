import { sleep, arrayOf, count } from "@gbagan/utils";
import { mod, swap } from '$lib/util';
import { CoreModel } from '$lib/model/core.svelte';
import type { IModel, Move, Position } from './types';

export default class extends CoreModel<Position, Move> implements IModel {
  #size = $state(5);
  #rotation = $state(0);  

  constructor() {
    super([]);
    this.newGame();
  }

  get size() {
    return this.#size;
  }

  get rotation() {
    return this.#rotation;
  }

  // tableau indiquant quelles sont les balles alignées avec leur couleur
  readonly aligned = $derived(this.position.map((v, i) =>
    v !== null && mod(i + this.#rotation, this.#size) === v
  ));
  // comme isValidRotation mais avec seconde conditition en moins 
  isValidRotation2 = $derived(count(this.aligned, x => x) === 1);
  // une rotation est valide si exactement une couleur est alignée et il y a une balle pour chaque couleur 
  isValidRotation = $derived(this.isValidRotation2 && this.position.every(v => v !== null)); 
  

  protected play({from, to}: Move): Position | null {
    if (from.kind === "panel" && to.kind === "wheel") {
      return this.position.with(to.id, from.id);
    } else if (from.kind === "wheel" && to.kind === "wheel") {
      return swap(this.position, from.id, to.id);
    } else if (from.kind === "wheel" && to.kind === "board") {
      return this.position.with(from.id, null);
    } else {
      return null;
    }
  }

  protected initialPosition = () => arrayOf(this.#size, null);
  protected isLevelFinished = () => false;
  protected onNewGame = () => this.#rotation = 0;

  check = async () => {
    this.lock(async () => {
      for (let i = 0; i < this.#size; i++) {
        if (!this.isValidRotation) {
          return;
        }
        this.#rotation += 1;
        await sleep(600);
      }
      await this.showVictory();
    });
  }

  setSize = (size: number) => this.newGame(() => this.#size = size);
  rotateLeft = () => this.#rotation -= 1;
  rotateRight = () => this.#rotation += 1;
}