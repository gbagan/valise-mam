<script lang="ts">
  import { type IModel, type Move, SoapMode } from './types';
  import { tabulate2, gridStyle } from '$lib/util';
  import PointerTracker from '$lib/components/PointerTracker.svelte';
  import Template from '$lib/components/Template.svelte';
  import * as I from '$lib/components/Icons';
  import Config from '$lib/components/Config.svelte';

  type Props = { model: IModel }
  const {model}: Props = $props();

  let moveWhenHover: Move | null = $state(null);

  function isInside(row: number, col: number): boolean {
    const {left, right, top, bottom} = model.position;
    return col >= left && col < right && row >= top && row < bottom;
  }

  const grid = $derived(tabulate2(model.rows, model.columns, (row, col) => [row, col]));
  const pmoves = $derived(model.possibleMoves());

  let cutLine = $derived.by(() => {
    if (moveWhenHover === null) {
      return null;
    }
    const {left, right, top, bottom} = model.position;
    const [dir, i] = moveWhenHover;
    if (dir === "left" || dir === "right") {
      return { x1: i, y1: top, x2: i, y2: bottom };
    } else {
      return { x1: left, y1: i, x2: right, y2: i }; 
    }
  });

  let message = $derived(model.soap === null ? "Place le savon" : model.turnMessage());
  let winTitle = $derived(model.winTitleFor2Player());
</script>

{#snippet cutter(row: number, col: number, move: Move)}
  <!-- svelte-ignore a11y_click_events_have_key_events -->
  <!-- svelte-ignore a11y_no_static_element_interactions -->
  <circle
    cx={50 * col}
    cy={50 * row}
    r="7"
    class="cutter"
    onclick={() => {moveWhenHover = null; model.playA(move)}}
    onpointerenter={() => moveWhenHover = move}
    onpointerleave={() => moveWhenHover = null}
  />
{/snippet}

{#snippet pointer(x: number, y: number)}
  {#if model.soap === null}
    <use
      href="#skull"
      class="pointer"
      style:transform="translate({x*100}%,{y*100}%)"
      x="-20"
      y="-20"
      width="26"
      height="26"
    />
  {/if}
{/snippet}

{#snippet board()}
  {@const {left, right, top, bottom} = model.position}
  <div class="board-container">
    <div class="turn-message">{message}</div>
    <div class="ui-board" style={gridStyle(model.rows, model.columns, 3)}>
      <PointerTracker {pointer} viewBox="-7 -7 {50 * model.columns + 14} {50 * model.rows + 14}">
        {#each grid as [row, col]}
          <!-- svelte-ignore a11y_click_events_have_key_events -->
          <!-- svelte-ignore a11y_no_static_element_interactions -->
          <rect
            class={["square", {
              soap: model.soap !== null && model.soap[0] === row && model.soap[1] === col,
              hidden: !isInside(row, col)
            }]}
            style:transform="translate({50*col}px, {50*row}px)"
            onclick={() => model.putSoap([row, col])}
          />
        {/each}
        {#each pmoves as [dir, i]}
          {#if dir === "left" || dir === "right"}
            {@render cutter(top, i, [dir, i])}
            {@render cutter(bottom, i, [dir, i])}
          {:else}
            {@render cutter(i, left, [dir, i])}
            {@render cutter(i, right, [dir, i])}
          {/if}
        {/each}

        {#if model.soap !== null}
          {@const [row, col] = model.soap}
          <use
            href="#skull"
            x={50*col+12}
            y={50*row+12}
            width="26"
            height="26"
            class="skull"
          />
        {/if}
        {#if cutLine}
          {@const {x1, x2, y1, y2} = cutLine}
          <line x1={50*x1} x2={50*x2} y1={50*y1} y2={50*y2} class="cut-line" />
        {/if}
      </PointerTracker>
    </div>
  </div>
{/snippet}

{#snippet config()}
  <Config title="Chocolat">
    <I.SizesGroup {model} values={[[6, 7]]} customSize={true} />
    <I.SelectGroup
      title="Emplacement du savon"
      values={[SoapMode.Corner, SoapMode.Border, SoapMode.Standard, SoapMode.Custom]}
      text={["#choc-mode0", "#choc-mode1", "#choc-mode2", "#customize"]}
      tooltip={["Dans le coin", "Sur le bord", "N'importe où", "Personnalisé"]}
      selected={model.soapMode}
      disabled={model.locked}
      setter={model.setSoapMode}
    />
    <I.TwoPlayers {model} />
    <I.Group title="Options">
      <I.Undo {model} />
      <I.Redo {model} />
      <I.Reset {model} />
      <I.Rules {model} />
    </I.Group>
  </Config>
{/snippet}

{#snippet rules()}
  <strong>Chocolat</strong> est un jeu à <strong>deux joueurs</strong>.<br/>
  A chaque tour, un joueur <strong>coupe</strong> la barre de chocolat en deux,
  <strong>verticalement</strong> ou <strong>horizontalement</strong> et conserve celle
  qui contient le <strong>carré empoisonné</strong>.<br/>
  Lorsqu'il ne reste que le carré empoisonné, le joueur qui doit jouer a <strong>perdu</strong>.
{/snippet}

<Template {model} {board} {config} {rules} {winTitle} />

<style>
  .board-container {
    height: 75vmin;
    width: 75vmin;
    display: flex;
    align-items: center;
    justify-content: center;
    position: relative;
  }

  .turn-message {
    position: absolute;
    color: blue;
    font-weight: bold;
    font-size: 1.5rem;
    top: -2rem;
    left: 0;
  }

  .square {
    fill: #b37b47;
    stroke: black;
    stroke-width: 3;
    x: 7px;
    y: 7px;
    width: 36px;
    height: 36px;

    &.soap {
      fill: cyan;
    }

    &.hidden {
      opacity: 0;
    }
  }

  .skull {
    fill: #20AF20;
  }

  .cutter {
    fill: blue;
  }

  .cut-line {
    stroke: red;
    stroke-width: 2;
    stroke-dasharray: 10;
    pointer-events: none;
    opacity: 0.6;
  }

  .pointer {
    opacity: 0.6;
    pointer-events: none;
}
</style>