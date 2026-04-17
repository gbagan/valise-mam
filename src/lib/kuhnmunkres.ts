import { arrayOf } from "./util";

type Mat = readonly (readonly number[])[];

export function kuhnMunkres(costMatrix: Mat): number[] {
  const n = costMatrix.length;
  const u = arrayOf(n + 1, 0); // Potentiels des lignes
  const v = arrayOf(n + 1, 0); // Potentiels des colonnes
  const p = arrayOf(n + 1, 0); // p[j] = ligne associée à la colonne j
  const way = arrayOf(n + 1, 0);

  for (let i = 1; i <= n; i++) {
    p[0] = i;
    let j0 = 0;
    const minv = arrayOf(n + 1, Infinity);
    const used = arrayOf(n + 1, false);
    do {
      used[j0] = true;
      const i0 = p[j0];
      let delta = Infinity;
      let j1 = 0;
      for (let j = 1; j <= n; j++) {
        if (!used[j]) {
          const cur = costMatrix[i0 - 1][j - 1] - u[i0] - v[j];
          if (cur < minv[j]) {
            minv[j] = cur;
            way[j] = j0;
          }
          if (minv[j] < delta) {
            delta = minv[j];
            j1 = j;
          }
        }
      }
      for (let j = 0; j <= n; j++) {
        if (used[j]) {
          u[p[j]] += delta;
          v[j] -= delta;
        } else {
          minv[j] -= delta;
        }
      }
      j0 = j1;
    } while (p[j0] !== 0);

    do {
      const j1 = way[j0];
      p[j0] = p[j1];
      j0 = j1;
    } while (j0 !== 0);
  }

  const assignment = new Array(n);
  for (let j = 1; j <= n; j++) {
    if (p[j] > 0)
      assignment[p[j] - 1] = j - 1;
  }

  return assignment;
}