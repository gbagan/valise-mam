<script lang="ts">
  import type { IModel } from './types';
  import Template from '$lib/components/Template.svelte';
  import * as I from '$lib/components/Icons';
  import Config from '$lib/components/Config.svelte';
  import DndBoard from '$lib/components/DndBoard.svelte';
  import DndItem from '$lib/components/DndItem.svelte';


  const {model}: {model: IModel} = $props();
  let dragged: number | null = $state(null);

  const colors = [ "blue", "red", "green", "magenta", "orange", "gray", "cyan" ];

  let stepCount = $derived(model.historyLength());
  let winTitle = $derived(`Tu as gagné en ${stepCount} étapes`);
</script>

{#snippet tower(i: number)}
  <path
    d="M{i * 60 + 14} 99a3 3 0 0 1 0 -6h20a3 3 0 0 0 3 -3v-80a3 3 0 0 1 6 0v80 a3 3 0 0 0 3 3h20a3 3 0 0 1 0 6z"
    stroke="blue"
    stroke-width="0.5"
    fill="#d43"
  />
{/snippet}

{#snippet disk([x, y, disk, isTop]: [number, number, number, boolean],
    dragged: boolean, _droppable: boolean,
    onpointerdown?: (e: PointerEvent) => void,
    onpointerup?: (e: PointerEvent) => void,
)}
  <!-- svelte-ignore a11y_no_static_element_interactions -->
  <rect
    x={x - 25 + 2.5 * disk}
    y={y - 7}
    width={50 - 5 * disk}
    height="10"
    class={["disk", {dragged: dragged && isTop}]}
    fill={colors[disk]}
    {onpointerdown} {onpointerup}
  />
{/snippet}

{#snippet dropZone(column: number,
    _dragged: boolean, _droppable: boolean,
    onpointerdown?: (e: PointerEvent) => void,
    onpointerup?: (e: PointerEvent) => void,
)}
  <!-- svelte-ignore a11y_no_static_element_interactions -->
  <rect
    x={13+60*column} y="10" width="54" height="90" fill="transparent"
    {onpointerdown} {onpointerup}
  />
{/snippet}

{#snippet draggedElement(style: string)}
  <g {style} pointer-events="none">
    {@render disk([0, 0, model.position[dragged!].at(-1)!, false], false, false)}
  </g>
{/snippet}

{#snippet board()}
  <div class="ui-board board">
    <DndBoard viewBox="0 0 200 100" bind:dragged={dragged} {draggedElement}>
      {#each model.position as disks, column}
        {@render tower(column)}
        <DndItem {model} bind:dragged={dragged}
          id={column}
          argument={column}
          render={dropZone}
          droppable={true}
        />
        {#each disks as d, i}
          {@const isTop = i === disks.length - 1}
          <DndItem {model} bind:dragged={dragged}
            id={column}
            argument={[40+60*column, 90-10*i, d, isTop]}
            render={disk}
            draggable={isTop}
          />
        {/each}
      {/each}
    </DndBoard>
    <div class="moves">{stepCount} coup{stepCount > 1 ? "s" : ""}</div>
  </div>
{/snippet}

{#snippet config()}
  <Config title="Tours de Hanoi">
    <I.SelectGroup
      title="Nombre de disques"
      values={[4, 5, 6, 7, 8]}
      selected={model.diskCount}
      setter={model.setDiskCount}
    />
    <I.Group title="Options">
      <I.Undo {model} />
      <I.Redo {model} />
      <I.Reset {model} />
      <I.Rules {model} />
    </I.Group>
  </Config>
{/snippet}

{#snippet rules()}
  Le but des <strong>Tours de Hanoi</strong> est de déplacer tous les <strong>disques</strong> sur la tour
  de <strong>droite</strong> avec les contraintes suivantes:
  <ul>
    <li>tu peux déplacer un <strong>seul disque</strong> disque à la fois;</li>
    <li>tu ne peux pas déplacer un disque sur un disque <strong>plus petit</strong> que lui.</li>
  </ul>
{/snippet}

<Template {model} {board} {config} {rules} {winTitle} />

<style>
  .board {
    position: relative;
    width: 100vmin;
    height: 50vmin;
  }

  .disk {
    stroke: black;
    rx: 5px;
    ry: 5px;
    &.dragged {
        opacity: 0;
    }
  }

  .moves {
    position: absolute;
    top: 0;
    left: 0.5em;
    color: blue;
    font-size: 1.5rem;
  }
</style>