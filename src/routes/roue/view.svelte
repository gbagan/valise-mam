<script lang="ts">
  import type { IModel, Location } from './types';
  import { arrayOf, range } from '@gbagan/utils';
  import Template from '$lib/components/Template.svelte';
  import * as I from '$lib/components/Icons';
  import Config from '$lib/components/Config.svelte';
  import DndBoard from '$lib/components/DndBoard.svelte';
  import DndItem from '$lib/components/DndItem.svelte';

  let {model}: {model: IModel} = $props();
  let dragged = $state<Location | null>(null);

  let draggedColor: number | null = $derived.by(() => {
    if (dragged === null || dragged.kind === "board") {
      return null;
    } else if (dragged.kind === "panel") {
      return dragged.id;
    } else {
      return model.position[dragged.id];
    }
  });

  let usedColors: boolean[] = $derived.by(() => {
    const used = arrayOf(model.size, false);
    for (const c of model.position) {
        if (c !== null) {
            used[c] = true;
        }
    }
    return used;
  })

  const colors = [
    "var(--blue-600)",
    "var(--red-500)",
    "var(--green-500)",
    "var(--purple-500)",
    "var(--yellow-400",
    "var(--slate-700)",
    "var(--cyan-400)",
    "var(--gray-400)"
  ];

  function polarToCartesian(centerX: number, centerY: number, radius: number, angle: number): [number, number] {
    return [ centerX + radius * Math.cos(angle), centerY + radius * Math.sin(angle) ];
  }

  function pizza(cx: number, cy: number, radius: number, startAngle: number, endAngle: number) {
    const [sx, sy] = polarToCartesian(cx, cy, radius, startAngle);
    const [ex, ey] = polarToCartesian(cx, cy, radius, endAngle);
    return `M${cx} ${cy}L${ex} ${ey}A${radius} ${radius} 0 0 0 ${sx} ${sy}L${cx} ${cy}`;
  }

  function pizza2(cx: number, cy: number, innerRadius: number, outerRadius: number,
    startAngle: number, endAngle: number) 
  {
    const [isx, isy] = polarToCartesian(cx, cy, innerRadius, startAngle);
    const [iex, iey] = polarToCartesian(cx, cy, innerRadius, endAngle);
    const [osx, osy] = polarToCartesian(cx, cy, outerRadius, startAngle);
    const [oex, oey] = polarToCartesian(cx, cy, outerRadius, endAngle);
    return `M${isx} ${isy}L${osx} ${osy}A${outerRadius} ${outerRadius} 0 0 1 ${oex} ${oey}`
      + `L${iex} ${iey}A${innerRadius} ${innerRadius} 0 0 0 ${isx} ${isy}`;
  }

  // svelte-ignore state_referenced_locally
  model.newGame()
</script>

{#snippet board2(_: null, _dragged: boolean, _droppable: boolean,
   onpointerdown?: (e: PointerEvent) => void, onpointerup?: (e: PointerEvent) => void)
}
  <!-- svelte-ignore a11y_no_static_element_interactions -->
  <rect x="-100" y="-120" width="100%" height="100%" fill="transparent"
    {onpointerdown} {onpointerup}
  />
{/snippet}

{#snippet wheelPart([i, aligned]: [number, boolean], _dragged: boolean, droppable: boolean,
   onpointerdown?: (e: PointerEvent) => void, onpointerup?: (e: PointerEvent) => void)
}
  <!-- svelte-ignore a11y_no_static_element_interactions -->
  <path
    d={pizza2(0, 0, 50, 80, 2 * Math.PI * (i - 0.5) / model.size, 2 * Math.PI * (i + 0.5) / model.size)}
    class={["wheel-part", {droppable}]}
    fill={!aligned ? "var(--orange-300)" : model.isValidRotation2 ? "var(--green-300)" : "var(--red-200)"}
    {onpointerdown} {onpointerup}
  />
{/snippet}

{#snippet disk([color, x, y]: [number, number, number], dragged2: boolean, _droppable: boolean,
   onpointerdown?: (e: PointerEvent) => void, onpointerup?: (e: PointerEvent) => void)
}
  <!-- svelte-ignore a11y_no_static_element_interactions -->
  <circle cx={x} cy={y} r="8"
    {onpointerdown}
    {onpointerup}
    fill={colors[color]}
    stroke="var(--main-color)"
    opacity={dragged2 ? "20%" : "100%"}
    pointer-events={dragged ? "none" : ""}
  />
{/snippet}

{#snippet draggedDisk(style: string)}
  <circle r="8"
    cx="-100"
    cy="-120"
    fill={colors[draggedColor!]}
    stroke="var(--main-color)"
    pointer-events="none"
    style={style}
  />
{/snippet}

{#snippet board()}
  <div class="ui-board board">
    <DndBoard
      viewBox="-100 -120 200 260"
      bind:dragged={dragged}
      draggedElement={draggedDisk} 
    >
      <DndItem bind:model={model}
        id={{kind: "board"}}
        argument={null}
        bind:dragged={dragged}
        droppable={true}
        render={board2}
      />
      {#each range(0, model.size) as color}
        {#if !usedColors[color]}  
          <DndItem bind:model={model}
            id={{kind: "panel", id: color}}
            argument={[color, -65 + 130 * color / (model.size - 1), -100]}
            bind:dragged={dragged}
            draggable={true}
            render={disk}
          />
        {/if}
      {/each}
      {#each colors.slice(0, model.size) as color, i}
        <path
          d={pizza(0, 0, 50, 2 * Math.PI * (i - 0.5) / model.size, 2 * Math.PI * (i + 0.5) / model.size)}
          fill={color}
          stroke="var(--main-color)"
          pointer-events="none"
        />
      {/each}
      <g class="outer-wheel" style:transform="rotate({360 * model.rotation / model.size}deg)">
       {#each model.aligned as align, i (i)}
          <DndItem bind:model={model}
            id={{kind: "wheel", id: i}}
            argument={[i, align]}
            bind:dragged={dragged}
            droppable={true}
            render={wheelPart}
          />  
        {/each}
        {#each model.position as color, i}
          {#if color !== null}
            {@const x = 64 * Math.cos(2 * i * Math.PI / model.size)}
            {@const y = 64 * Math.sin(2 * i * Math.PI / model.size)}
            <DndItem bind:model={model}
              id={{kind: "wheel", id: i}}
              argument={[color, x, y]}
              bind:dragged={dragged}
              draggable={true}
              droppable={true}
              render={disk}
            />
          {/if}
        {/each}
      </g>
    </DndBoard>
    <button
      class="ui-button btn-left"
      disabled={model.locked}
      onclick={model.rotateLeft}
    >↶</button>
    <button
      class="ui-button btn-right"
      disabled={model.locked}
      onclick={model.rotateRight}
    >↷</button>
    <button
      class="ui-button btn-validate"
      disabled={model.locked || !model.isValidRotation}
      onclick={model.check}
    >Valider</button>
    <div class="valid-rotation">
      {#if model.isValidRotation}
        <span class="valid">✓</span>
      {:else}
        <span class="invalid">✗</span>
      {/if}
    </div>
  </div>
{/snippet}

{#snippet config()}
  <Config title="Roue des couleurs">
    <I.SelectGroup
      title="Nombre de couleurs"
      values={[4, 5, 6, 7, 8]}
      selected={model.size}
      disabled={model.locked}
      setter={model.setSize}
    />
    <I.Group title="Options">
      <I.Reset bind:model={model} />
      <I.Rules bind:model={model} />
    </I.Group>
  </Config>
{/snippet}

{#snippet rules()}
  Le but du jeu <strong>Roue des couleurs</strong> est de poser une <strong>bille</strong> sur chaque emplacement de la roue
  et effectuer un <strong>tour complet</strong> de la roue en respectant la condition suivante:<br/>
  à chaque moment durant la rotation de la roue, <strong>exactement</strong> une bille a sa couleur
  qui correspond avec la couleur de la roue.<br/>
  Tu peux tester en faisant varier le nombre de couleurs de la roue mais également
  en faisant varier le nombre de couleurs que tu utilises.
{/snippet}

<Template bind:model={model} {board} {config} {rules} />

<style>
  .board {
    height: 82.5vmin;
    width: 75vmin;
    position: relative;
  }

  .btn-left {
    width: 3rem;
    height: 3rem;
    font-size: 2rem;
    padding: 0;
    position: absolute;
    left: 1rem;
    top: 0;
  }

  .btn-right {
    width: 3rem;
    height: 3rem;
    font-size: 2rem;
    padding: 0;
    position: absolute;
    right: 1rem;
    top: 0;
  }

  .btn-validate {
    font-size: 2rem;
    padding: 0;
    position: absolute;
    right: 0.5rem;
    bottom: 0.5rem;
  }

  .outer-wheel {
    transition: all linear 0.5s;
  }

  .wheel-part {
    stroke: var(--main-color);
    &.droppable:hover {
      stroke: green;
      stroke-width: 3;
      stroke-dasharray: 6;
    }
  }

  .valid-rotation {
    position: absolute;
    font-size: 4em;
    font-weight: bold;
    right: 0.5em;
    bottom: 1.2em;

    .valid {
        color: var(--green-500);
    }
    .invalid {
        color: var(--red-500);
    }
  }
</style>