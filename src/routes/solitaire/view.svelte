<script lang="ts">
  import { Board, type IModel, type Position } from './types';
  import { gridStyle } from '$lib/util';
  import Template from '$lib/components/Template.svelte';
  import Icon from '$lib/components/icons/Icon.svelte';
  import * as I from '$lib/components/Icons';
  import Config from '$lib/components/Config.svelte';
  import DndBoard from '$lib/components/DndBoard.svelte';
  import DndItem from '$lib/components/DndItem.svelte';
  import { onMount } from 'svelte';

  let {model}: {model: IModel} = $props();
  let dragged: number | null = $state(null);

  let winTitle = $derived.by(() => {
    const score = model.score();
    const s = score > 1 ? "s" : "";
    return `${score} pièce${s} restante${s}`;
  });

  function itemTransform(i: number): string {
    const row = i / model.columns | 0;
    const col = i % model.columns;
    if (model.boardType === Board.Circle) {
      const x = 125 + Math.sin(2 * Math.PI * i / model.rows) * 90;
      const y = 125 + Math.cos(2 * Math.PI * i / model.rows) * 90;
      return `translate(${x}px,${y}px)`;
    } else {
      return `translate(${50*col+25}px,${50*row+25}px)`;
    }
  }

  function tricolor(i: number): string {
    switch ((i % model.columns + model.help2 + (i / model.columns | 0)) % 3) {
      case 0: return "red";
      case 1: return "blue";
      default: return "green";
    }
  }

  onMount(() => {
    model.loadRecords();
  });
</script>


{#snippet hole(i: number, _dragged: boolean, droppable: boolean,
  onpointerdown?: (e: PointerEvent) => void, onpointerup?: (e: PointerEvent) => void)
}
  {#if model.help2 > 0 && model.boardType !== Board.Circle}
    <rect
      x="-25.0"
      y="-25.0"
      width="50"
      height="50"
      fill={tricolor(i)}
      style:transform={itemTransform(i)}
    />
  {/if}
  <!-- svelte-ignore a11y_no_static_element_interactions -->
  <circle
    r="17"
    class={["hole", {droppable}]}
    style:transform={itemTransform(i)}
    onpointerdown={onpointerdown}
    onpointerup={onpointerup}
  />
{/snippet}

{#snippet peg(i: number, dragged: boolean, _droppable: boolean,
  onpointerdown?: (e: PointerEvent) => void, onpointerup?: (e: PointerEvent) => void)
}
  <!-- svelte-ignore a11y_no_static_element_interactions -->
  <circle
    r="20"
    class={["peg", {dragged}]}
    style:transform={itemTransform(i)}
    onpointerdown={onpointerdown}
    onpointerup={onpointerup}
  />
{/snippet}

{#snippet draggedElement(style: string)}
  <circle r="20" width="30" height="30" class="cursor" {style}/> 
{/snippet}
    
{#snippet genericBoard(position: readonly boolean[], interactive: boolean)}
  <div
    class="ui-board"
    style={
      model.boardType === Board.Circle 
      ? "width:100%;height:100%;" 
      : gridStyle(model.rows, model.columns, 5)
    }
  >
    <DndBoard
      viewBox={
        model.boardType === Board.Circle 
        ? "0 0 250 250"
        : `0 0 ${50 * model.columns} ${50 * model.rows}`
      }
      bind:dragged={dragged} {draggedElement}
    >
      {#if model.boardType === Board.Circle}
        <circle cx="125" cy="125" r="90" class="circle" />
      {/if}
      {#each model.holes as hasHole, i}
        {#if hasHole}
          <DndItem bind:model={model} bind:dragged={dragged}
            id={i}
            argument={i}
            droppable={interactive}
            render={hole}
          />
        {/if}
      {/each}
      {#each position as hasPeg, i}
        {#if hasPeg}
          <DndItem bind:model={model} bind:dragged={dragged}
            id={i}
            argument={i}
            draggable={interactive}
            render={peg}
          />
        {/if}
      {/each}
    </DndBoard>
  </div>
{/snippet}

{#snippet board()}
  <div class="board-container">
    {@render genericBoard(model.position, true)}
  </div>
{/snippet}

{#snippet bestScore(position: Position)}
  <div class="scoredialog">
    {@render genericBoard(position, false)}
  </div>
{/snippet}

{#snippet config()}
  <Config title="Les reines">
    <I.SelectGroup
      title="Plateau"
      values={[Board.Circle, Board.Grid3, Board.Random, Board.English, Board.French]}
      text={["#circle", "3xN", "#shuffle", "#tea", "#bread"]}
      tooltip={["3xN", "Cercle", "Aléatoire", "Anglais", "Français"]}
      selected={model.boardType}
      setter={model.setBoard}
    />
    <I.Group title="Options">
      <Icon
        text="#help"
        tooltip="Aide"
        selected={model.help2 > 0}
        onclick={model.incrementHelp}
      />
      <I.Undo bind:model={model} />
      <I.Redo bind:model={model} />
      <I.Reset bind:model={model} />
      <I.Rules bind:model={model} />
    </I.Group>
    <I.BestScore bind:model={model} />
  </Config>
{/snippet}

{#snippet rules()}
  todo
{/snippet}

<Template bind:model={model} {board} {config} {rules} {winTitle} {bestScore} />

<style>
  .board-container {
    height: 75vmin;
    width: 75vmin;
    display: flex;
    align-items: center;
    justify-content: center;
  }

  .hole {
    fill: url(#soli-hole);

    &.droppable {
      stroke: green;
      stroke-width: 4;
    }
  }

  .peg {
    fill: url(#soli-peg);
    cursor: pointer;
    &.dragged {
      opacity: 0.3;
    }
  }

  .circle {
    stroke: grey;
    fill: transparent;
    stroke-width: 5;
  }

  .cursor {
    pointer-events: none;
    fill: url(#soli-peg);
  }

  .scoredialog {
    height: 60vmin;
    width: 60vmin;
    display: flex;
    align-items: center;
    justify-content: center;
  }
</style>