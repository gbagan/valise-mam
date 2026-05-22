import { times } from "@gbagan/utils";

export function swap<A>(arr: readonly A[], i: number, j: number): A[] {
  const res = arr.slice();
  const tmp = res[i];
  res[i] = res[j];
  res[j] = tmp;
  return res; 
}

// renvoie les sous listes de taille k de [0, ... n-1]
export function sublists(n: number, k: number): number[][] {
  const res: number[][] = [];
  const current: number[] = [];

  function backtrack(start: number) {
    if (current.length === k) {
      res.push(current.slice());
      return;
    }
    for (let i = start; i < n; i++) {
      current.push(i);
      backtrack(i + 1);
      current.pop();
    }
  }

  backtrack(0);
  return res;
}

// renvoie un entier aléatoire entre start et end (end exclus)
export const random = (start: number, end: number) =>
  start + (end - start) * Math.random() | 0;

export function randomPick<A>(arr: readonly A[]): A | undefined {
  if (arr.length === 0) {
    return undefined;
  } else {
    return arr[Math.random() * arr.length | 0];
  }
}

// renvoie un nouveau tableau uniformément mélangé
export function shuffle<A>(arr: readonly A[]): A[] {
  const res = arr.slice();
  for (let i = res.length-1; i >= 0; i--) {
    const j = Math.random() * (i+1) | 0;
    const tmp = res[i];
    res[i] = res[j];
    res[j] = tmp;
  }
  return res;
}

export function allDistinct<A>(arr: readonly A[]): boolean {
  return new Set(arr).size === arr.length;
}

export function mod(x: number, y: number) {
  const z = x % y;
  return z < 0 ? z + y : z;
}

export const generate2 = <A>(n: number,  m: number, f: (i: number, j: number) => A) => 
  times(n * m, i => f(i / m | 0, i % m));

export function coords(cols: number, x: number): [number, number] {
  const row = x / cols | 0;
  const col = x % cols;
  return [row, col];
}

export function diffCoords(cols: number, x: number, y: number): [number, number] {
  const row1 = x / cols | 0;
  const col1 = x % cols;
  const row2 = y / cols | 0;
  const col2 = y % cols;
  return [row1 - row2, col1 - col2];
}

export function gridStyle(rows: number, columns: number, limit: number): string {
  const m = Math.max(limit, rows, columns);
  return `height:${100*rows/m}%;width:${100*columns/m}%;`
}

export function getPointerPosition(e: MouseEvent): {x: number, y: number} {
  const rect = (e.currentTarget as Element).getBoundingClientRect();
  const x = (e.clientX - rect.left) / rect.width;
  const y = (e.clientY - rect.top) / rect.height;
  return {x, y};
}