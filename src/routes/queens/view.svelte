<script lang="ts">
  import { piecesList, type IModel, type Piece, type Position } from "./types";
  import { arrayOf, range } from "@gbagan/utils";
  import { coords, gridStyle } from "$lib/util";
  import PointerTracker from "$lib/components/PointerTracker.svelte";
  import Template from "$lib/components/Template.svelte";
  import * as I from "$lib/components/Icons";
  import Config from "$lib/components/Config.svelte";
  import Dialog from "$lib/components/Dialog.svelte";
  import { onMount } from "svelte";

  const angles = [ 45, 90, 135, 0, 0, 180, -45, -90, -135 ];

  let {model}: {model: IModel} = $props();

  function tooltip(piece: Piece): string {
    switch (piece) {
      case "Q": return "Reine";
      case "K": return "Roi";
      case "R": return "Tour";
      case "B": return "Fou";
      case "N": return "Cavalier";
      default: return "Pièce personnalisée";
    }
  }

  let hoveredSquare: number | null = $state(null);

  // ensemble des cases attaquées par la case survolée par le pointeur de la souris
  const attackedByHovered: readonly boolean[] = $derived(
    hoveredSquare === null
    ? arrayOf(model.rows * model.columns, false)
    : model.attackedBy(model.selectedPiece, hoveredSquare)
  );

  onMount(() => {
    model.loadRecords();
  });
</script>

{#snippet pieceSelector()}
  <div class="pieceselector">
    {#each model.allowedPieces as piece}
      <I.Icon
        text="#piece-{piece}"
        selected={piece === model.selectedPiece}
        onclick={() => model.setSelectedPiece(piece)}
      />
    {/each}
  </div>
{/snippet}

{#snippet lines(rows: number, columns: number)}
  {#each range(1, columns) as i}
    <line y1="0" y2={50*rows} x1={i*50} x2={i*50} class="line" />
  {/each}
  {#each range(1, rows) as i}
    <line x1="0" x2={50*columns} y1={i*50} y2={i*50} class="line" />
  {/each}
{/snippet}

{#snippet pointer(x: number, y: number)}
  <use
    class="piece"
    href="#piece-{model.selectedPiece}"
    x={-20}
    y={-20}
    width="40"
    height="40"
    style:transform="translate({100*x}%, {100*y}%)"
  />
{/snippet}

{#snippet board2()}
  <div class="board">
    <div class="ui-board board" style={gridStyle(model.rows, model.columns, 5)}>
      <PointerTracker {pointer} viewBox="0 0 {50*model.columns} {50*model.rows}">
        {#each model.position as piece, i}
          {@const x = 50 * (i % model.columns)}  
          {@const y = 50 * (i / model.columns | 0)}  
          <!-- svelte-ignore a11y_click_events_have_key_events -->
          <!-- svelte-ignore a11y_no_static_element_interactions -->
          <rect {x} {y}
            class={["square", {
              attacked: i === hoveredSquare || attackedByHovered[i],
              capturable: model.help && (piece !== null || model.capturable[i])
            }]}
            onclick={() => model.playA(i)}
            onpointerenter={() => hoveredSquare = i}
            onpointerleave={() => hoveredSquare = null}
          />
          {#if piece !== null}
            <use
              class={["piece", {capturable: model.capturable[i]}]}
              href="#piece-{piece}"
              x={x + 5}
              y={y + 5}
              width="40"
              height="40"
            />
          {/if}
        {/each}
        {@render lines(model.rows, model.columns)}
      </PointerTracker>
    </div>
  </div>
{/snippet}

{#snippet board()}
  <div class="board-container">
    {@render pieceSelector()}
    {@render board2()}
  </div>
{/snippet}

{#snippet config()}
  <Config title="Les reines">
    <I.SizesGroup bind:model={model}
      values={[[4, 4], [5, 5], [7, 7], [8, 8]]}
      customSize={true}
    />
    <I.Group title="Pièces disponibles">
      {#each piecesList as piece}
        <I.Icon
          text="#piece-{piece}"
          selected={model.allowedPieces.includes(piece)}
          tooltip={tooltip(piece)}
          onclick={() => model.changeAllowedPieces(piece)}
        />
    {/each}
    </I.Group>
    <I.Group title="Options">
      <I.Icon
        text="#customize"
        selected={model.allowedPieces[0] === "custom"}
        tooltip="Crée ta propre pièce"
        onclick={model.customize}
      />
      <I.Icon
        text="#piece-mix"
        tooltip="Mode mixte"
        selected={model.multiPieces}
        onclick={model.toggleMultiPieces}
      />
      <I.Help bind:model={model} />
      <I.Reset bind:model={model} />
      <I.Rules bind:model={model} />
    </I.Group>
    <I.BestScore bind:model={model} />
  </Config>
{/snippet}

{#snippet rules()}
  Place le <strong>plus de pièces</strong> possible sur ta grille sans qu'aucune ne soit
  <strong>menacée</strong> par une autre pièce.<br/>
  Tu peux choisir de jouer avec différentes pièces comme celles du <strong>jeu d'échecs.</strong><br/>
  Le mode <strong>mixte</strong> permet de jouer avec plusieurs pièces différentes.<br/>
  Tu peux jouer avec une pièce <strong>personnalisée</strong> si tu le souhaites.
{/snippet}

{#snippet bestScore(position: Position)}
  <div class="bestscore-container">
    <div class="ui-board" style={gridStyle(model.rows, model.columns, 5)}>
      <svg viewBox="0 0 {50*model.columns} {50*model.rows}">
        {#each position as piece, i}
          {@const x = 50 * (i % model.columns)}  
          {@const y = 50 * (i / model.columns | 0)}
          <rect {x} {y} class="square" />
          {#if piece !== null}
            <use
              class="piece"
              href="#piece-{piece}"
              x={x + 5}
              y={y + 5}
              width="40"
              height="40"
            />
          {/if}
        {/each}
        {@render lines(model.rows, model.columns)}
      </svg>
    </div>
  </div>
{/snippet}

{#snippet custom()}
  <Dialog title="Personnalise ta pièce" onOk={model.closeDialog}>
    <div class="custompiece-container">
      <div class="ui-board custompiece-grid">
        <svg viewBox="0 0 250 250">
          {#each model.customLocalMoves as attacked, i}
            {@const [row, col] = coords(5, i)}
            <!-- svelte-ignore a11y_click_events_have_key_events -->
            <!-- svelte-ignore a11y_no_static_element_interactions -->
            <rect
              x={50*col} y={50*row}
              class={["square", {attacked}]}
              onclick={() => model.toggleCustomLocalMove(i)}
            />
          {/each}
          <use
            class="piece"
            href="#piece-custom"
            x="105"
            y="105"
            width="40"
            height="40"
          />
          {@render lines(5, 5)}
        </svg>
      </div>
      <div class="custompiece-directions">
        {#each model.customDirections as selected, i}
          <I.Icon
            text={i === 4 ? "" : "#arrow"}
            {selected}
            style="transform:rotate({angles[i]}deg)"
            onclick={() => model.toggleCustomDirection(i)}
          />
        {/each}
      </div>
    </div>
  </Dialog>
{/snippet}

<Template bind:model={model} {board} {config} {rules} {bestScore} {custom} />

<style>
  .board-container {
    display: flex;
    align-items: stretch;
    justify-content: center;
    flex-direction: column;
    gap: 1rem;
  }
  
  .board {
    width: 75vmin;
    height: 75vmin;
    display: flex;
    align-items: center;
    justify-content: center;
  }
 
  .line {
    stroke: #bdc3c7;
    stroke-width: 0.5;
    pointer-events: none;
  }

  .piece {
    pointer-events: none;
    &.capturable {
      fill: red;
      color: red;
    }
  }

  .square {
    fill: white;
    width: 50px;
    height: 50px;
    &.attacked {
      fill: #e6e6e6
    }
    &.capturable {
      fill: rgb(224, 123, 162);
    }
    &.capturable.attacked {
      fill: rgb(200, 90, 140);
    }
  }

  .pieceselector {
    display: flex;
    align-items: center;
    justify-content: center;
    margin-bottom: 0.5rem;
    gap: 1rem;
  }

  .bestscore-container {
    width: 60vmin;
    height: 60vmin;
  }

  .custompiece-grid {
    width: 50vmin;
    height: 50vmin;
  }

  .custompiece-container {
    display: flex;
    align-items: start;
    justify-content: center;
    gap: 1rem;
  }

  .custompiece-directions {
    display: grid;
    grid-template-columns: repeat(3, 1fr);
  }
</style>