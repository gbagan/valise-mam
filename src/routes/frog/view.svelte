<script lang="ts">
  import type { IModel } from './types';
  import { tabulate, range } from '$lib/util';
  import Template from '$lib/components/Template.svelte';
  import * as I from '$lib/components/Icons';
  import Config from '$lib/components/Config.svelte';

  const { model }: { model: IModel } = $props();

  const reachable = $derived(tabulate(model.rows+1, i => model.canPlay(i)));

  type Cartesian = { x: number, y: number };
  type Polar = { radius: number, theta: number };

  const rotate = (theta: number) => `rotate(${theta * 180 / Math.PI}deg)`;
  
  function lineIntersection(m1: number, b1: number, m2: number, b2: number): Cartesian {
    const x = (b2 - b1) / (m1 - m2);
    return { x, y: m1 * x + b1 }
  }

  function polarToCartesian(p: Polar): Cartesian {
    return { x: p.radius * Math.cos(p.theta), y: p.radius * Math.sin(p.theta) }
  }

  function spiral(center: Cartesian, startRadius: number, radiusStep: number,
                startTheta: number, endTheta: number, thetaStep: number): string
  {
    const ps = [];
    for(let theta = startTheta; theta <= endTheta; theta += thetaStep) {
      const b = radiusStep / (2 * Math.PI);
      const r = startRadius + b * theta;
      const point = { x: center.x + r * Math.cos(theta), y: center.y + r * Math.sin(theta) };
      const slope = (b * Math.sin(theta) + r * Math.cos(theta)) / (b * Math.cos(theta) - r * Math.sin(theta));
      const intercept = -(slope * r * Math.cos(theta) - r * Math.sin(theta));
      ps.push({ point, slope, intercept });
    }
    let path = `M${ps[0].point.x} ${ps[0].point.y}`;
    for (let i = 0; i < ps.length - 1; i++) {
      const a = ps[i];
      const b = ps[i+1];
      const { x, y } = lineIntersection(a.slope, a.intercept, b.slope, b.intercept);
      path += `Q${x + center.x} ${y + center.y} ${b.point.x} ${b.point.y}`;
    }
    return path;
  }

  const spiralDescription = spiral({ x: 0, y: 0 }, 0, 61, 0, 37 / 6 * Math.PI, Math.PI / 6.0);

  const polarPoints = $derived.by(() => {
    const n = model.rows;
    return range(0, n+1).map(i => {
      const theta = Math.sqrt(i === n ? 21 : i * 20 / n) * 1.36 * Math.PI;
      const radius = 61 * theta / (2 * Math.PI);
      return {theta, radius};
    }).toReversed();
  });

  const cartesianPoints = $derived(polarPoints.map(polarToCartesian));
  const frogPoint = $derived(polarPoints[model.position]);

  const message = $derived(model.turnMessage());
  const winTitle = $derived(model.winTitleFor2Player());

  const onLilyClick = (e: MouseEvent, i: number) => {
    if (e.shiftKey) {
      model.toggleMarked(i);
    } else {
      model.playA(i)
    }
  }
</script>

{#snippet spiralPath()}
  <g>
    <path d={spiralDescription} fill="none" stroke="black" stroke-width="3"/>
    <line x1="153" y1="9" x2="207" y2="20" stroke="black" stroke-dasharray="5" stroke-width="6"/>
    <line x1="153" y1="7" x2="153" y2="39" stroke="black" stroke-width="3.0" />
    <line x1="207" y1="18" x2="207" y2="50" stroke="black" stroke-width="3.0" />
  </g>
{/snippet}


{#snippet lily(i: number, x: number, y: number, reachable: boolean, hidden: boolean)}
  <!-- svelte-ignore a11y_click_events_have_key_events -->
  <!-- svelte-ignore a11y_no_static_element_interactions -->
  <use
    href="#lily"
    class={["lily", {reachable, hidden}]}
    x={i === 0 ? x - 30 : x - 24}
    y={i === 0 ? y - 45 : y - 24}
    width={i === 0 ? 80 : 48}
    height={i === 0 ? 80 : 48}
    onclick={e => onLilyClick(e, i)}
    oncontextmenu={e => {
      e.preventDefault();
      model.toggleMarked(i);
    }}
  />
{/snippet}

{#snippet frog()}
  {@const {theta, radius} = frogPoint}
  <g
    class="frog-container"
    style:transform="{rotate(theta)} translate({radius}px, 0px) {rotate(-theta)}"
  >
    <use
      href="#frog2"
      x="-20"
      y="-30"
      width="40"
      height="40"
      class={["frog", {goal: model.position === 0}]}
    />
  </g>
{/snippet}

{#snippet board()}
  <div class="ui-board board"
    style:width="80vmin" style:height="80vmin">
    <svg viewBox="-190 -200 400 400">
      {@render spiralPath()}
      {#each cartesianPoints as p, i}
        {@render lily(i, p.x, p.y, false, false)}
        {@render lily(i, p.x, p.y, true, !reachable[i] || model.locked)}
        {#if model.help}
          <text x={p.x} y={p.y} class="index">{model.rows - i}</text>
        {/if}
        {#if model.marked[i] && i !== model.position}
          <use href="#frog2" x={p.x-20} y={p.y-20} width="20" height="20" class="frog marked" />
        {/if}
      {/each}
      {@render frog()}
    </svg>
    <span class="turn-message">{message}</span>
  </div>
{/snippet}

{#snippet config()}
  <Config title="La grenouille">
    <I.MultiSelectGroup
      title="Déplacements autorisés"
      values={[1, 2, 3, 4, 5]}
      selected={model.moves}
      disabled={model.locked}
      setter={model.movesSetter}
    />
    <I.TwoPlayers {model} />
    <I.Group title="Options">
      <I.Help {model} />
      <I.Undo {model} />
      <I.Redo {model} />
      <I.Reset {model} />
      <I.Rules {model} />
    </I.Group>
  </Config>
{/snippet}

{#snippet rules()}
  Le jeu de la <strong>grenouille</strong> est un jeu à <strong>deux joueurs</strong>.<br/>
  A chaque tour, un joueur peut <strong>avancer</strong> la grenouille d'un nombre de cases parmi ceux
  indiqués dans <strong>Déplacements autorisés</strong>.<br/>
  Le premier joueur à atteindre le <strong>nénuphar final</strong> a gagné.<br/>
  Pour éviter une situation bloquante, un joueur peut se déplacer vers le nénuphar
  final en utilisant <strong>moins</strong> de déplacements que ce qui lui est autorisé.<br/>
  Tu peux placer des indices sur les nénuphars avec un <strong>clic droit</strong> ou
  <strong>shift + clic gauche</strong>.
{/snippet}

<Template {model} {board} {config} {rules} {winTitle} />

<style>
  .board {
    width: 100%;
    height: 100%;
  }

  .lily {
    fill: url(#lilyg);

    &.reachable {
      transition: opacity 0.3s linear 0.5s;
      fill: url(#lilyh);
    }

    &.reachable.hidden {
      transition: opacity 0.3s linear;
      opacity: 0;
    }
  }

  .index {
    fill: blue;
    font-size: 20;
    text-anchor: middle;
    pointer-events: none;
  }

  .frog-container {
    transition: transform linear 0.6s;
  }

  .frog {
    fill: #bcd35f;
    pointer-events: none;
    &.marked {
      opacity: 0.6;
      fill: #bc835f;
    }
    &.goal {
      animation: frog-jump 4s infinite;
    }
  }

  @keyframes frog-jump {
    0% { transform: translate(0, 0); }
    70% { transform: translate(0, 0); }
    75% { transform: translate(0, -30px); }
    80% { transform: translate(0, 0); }
    85% { transform: translate(0, -30px); }
    90% { transform: translate(0, -0); }
    95% { transform: translate(0, -30px); }
    100% { transform: translate(0, 0); }
  }

  .turn-message {
    position: absolute;
    bottom: 0.5em;
    left: 0.5em;
    color: blue;
    font-size: 1.3em;
  }
</style>