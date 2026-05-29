import { type Constructor, CoreModel } from "./core.svelte";
import type { ISizeModel, SizeLimit } from "./types";

export function WithSize<Position, Move>() {
  return function <TBase extends Constructor<CoreModel<Position, Move>>>(Base: TBase) {
    abstract class C extends Base implements ISizeModel {
      #rows = $state(0);
      #columns = $state(0);
      #customSize = $state(false);

      constructor(...args: any[]) {
        super(...args);
      }

      get rows() {
        return this.#rows;
      }

      get columns() {
        return this.#columns;
      }

      get customSize() {
        return this.#customSize;
      }

      protected set rows(value: number) {
        this.#rows = value;
      }

      protected set columns(value: number) {
        this.#columns = value;
      }

      protected set customSize(value: boolean) {
        this.#customSize = value;
      } 

      get sizeLimit(): SizeLimit | null {
        return null;
      }

      initWithSize(rows: number, columns: number, customSize?: boolean) {
        this.#rows = rows;
        this.#columns = columns;
        if (customSize !== undefined) {
          this.#customSize = customSize;
        }
        this.newGame();
      }

      resize = (rows: number, columns: number, customSize?: boolean) => {
        let valid: boolean;
        if (!this.sizeLimit) {
          valid = true;
        } else {
          const {minRows, maxRows, minCols, maxCols} = this.sizeLimit;
          valid = rows >= minRows && rows <= maxRows && columns >= minCols && columns <= maxCols;
        }

        if (!valid) {
          return;
        }

        this.newGame(() => {
          this.#rows = rows;
          this.#columns = columns;
          if (customSize !== undefined) {
            this.#customSize = customSize;
          }
        });
      }
    }
    return C;
  }
}