<script lang="ts">
  import type { IModel, Position } from './types';
  import { gridStyle } from '$lib/util';
  import Template from '$lib/components/Template.svelte';
  import * as I from '$lib/components/Icons';
  import Config from '$lib/components/Config.svelte';
  import DndBoard from '$lib/components/DndBoard.svelte';
  import DndItem from '$lib/components/DndItem.svelte';
  import { onMount } from 'svelte';
  

  const {model}: {model: IModel} = $props();
  let dragged: number | null = $state(null);

  let winTitle = $derived.by(() => {
    const score = model.score();
    const s = score > 1 ? "s" : "";
    return `${score} case${s} restante${s}`;
  });

  onMount(() => {
    model.loadRecords();
  });
</script>

{#snippet peg([i, val]: [number, number], dragged: boolean, droppable: boolean,
  onpointerdown?: (e: PointerEvent) => void, onpointerup?: (e: PointerEvent) => void)
}
  {@const row = i / model.columns | 0}
  {@const col = i % model.columns}
  <g style:transform="translate({25+50*col}px,{25+50*row}px)">
    <!-- svelte-ignore a11y_no_static_element_interactions -->
    <rect
      class={["peg", {dragged, droppable}]}
      fill="rgb(255 {255 * (1 - Math.sqrt(val / (model.rows * model.columns)))} 0)"
      filter="drop-shadow({val/2}px {val/2}px 0.5px #656565)"
      {onpointerdown} {onpointerup}
    />
    <text y="3" class="peg-text">{val}</text>
  </g>
{/snippet}

{#snippet draggedElement(style: string)}
  <rect x="-15" y="-15" width="30" height="30" class="cursor" {style}/> 
{/snippet}

{#snippet board()}
  <div class="board-container">
    <div class="ui-board" style={gridStyle(model.rows, model.columns, 3)}>
      <DndBoard
        viewBox="0 0 {50 * model.columns} {50 * model.rows}"
        bind:dragged={dragged}
        {draggedElement}
      >
        {#each model.position as val, i}
          {#if val !== 0}
            <DndItem {model} bind:dragged={dragged}
              id={i}
              argument={[i, val]}
              draggable={true}
              droppable={true}
              render={peg}
            />
         {/if}
        {/each}
      </DndBoard>
    </div>
    
  </div>
{/snippet}

{#snippet config()}
  <Config title="Jeu d'acquisition">
    <I.SizesGroup {model}
      values={[[2, 2], [4, 4], [5, 5], [6, 6]]}
      customSize={true}
    />
    <I.Group title="Options">
      <I.Undo {model} />
      <I.Redo {model} />
      <I.Reset {model} />
      <I.Rules {model} />
    </I.Group>
    <I.BestScore {model} />
  </Config>
{/snippet}

{#snippet bestScore(position: Position)}
  <div class="bestscore-container">
    <div class="ui-board" style={gridStyle(model.rows, model.columns, 3)}>
      <svg viewBox="0 0 {50 * model.columns} {50 * model.rows}">
        {#each position as val, i}
          {#if val !== 0}
            {@render peg([i, val], false, false)}
          {/if}
        {/each}
      </svg>
    </div>
  </div>
{/snippet}

{#snippet rules()}
  À chaque tour de ce jeu, tu peux déplacer une pile de jetons vers une case adjacente
  qui contient au moins autant de jetons.<br/>
  Le but est de finir la partie avec le moins de cases contenant des piles de jetons.
{/snippet}

<Template {model} {board} {config} {rules} {bestScore} {winTitle} />

<style>
  .board-container {
    width: 75vmin;
    height: 75vmin;
    display: flex;
    align-items: center;
    justify-content: center;
  }

  .peg {
    cursor: pointer;
    x: -18px;
    y: -18px;
    width: 36px;
    height: 36px;
    rx: 5px;
    ry: 5px;

    &.dragged {
      opacity: 0.3;
    }

    &.droppable {
      stroke: green;
      stroke-width: 2;
      &:hover {
          stroke: red;
      }
    }
  }

  .peg-text {
    font: bold 20px sans-serif;
    fill: rgba(0, 0, 0, 0.65);
    pointer-events: none;
    text-anchor: middle;
    dominant-baseline: middle;
  }

  .cursor {
    pointer-events: none;
    fill: orange;
    opacity: 0.7;
  }

  .bestscore-container {
    width: 60vmin;
    height: 60vmin;
    display: flex;
    align-items: center;
    justify-content: center;
  }
</style>