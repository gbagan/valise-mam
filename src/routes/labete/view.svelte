<script lang="ts">
  import { type IModel, Mode, BeastType, type Position } from './types';
  import { coords, gridStyle, getPointerPosition } from '$lib/util';
  import Template from '$lib/components/Template.svelte';
  import * as I from '$lib/components/Icons';
  import Config from '$lib/components/Config.svelte';
  import Dialog from '$lib/components/Dialog.svelte';
  import { onMount } from 'svelte';

  type Props = { model: IModel }
  let {model}: Props = $props();
  let currentPosition: {x: number, y: number} | null = $state(null);
  let startingPosition: {x: number, y: number} | null = $state(null);
  let startingSquare : number | null = $state(null);

  let winTitle = $derived(`Record: ${model.score()} pièges`);

  const colors = [ "#5aa02c", "blue", "red", "yellow", "magenta", "cyan", "orange", "darkgreen", "grey" ];

  function handleKeydown(e: KeyboardEvent) {
    if (e.code === "ArrowLeft") {
      model.decrementSelectedColor();
    } else if (e.code === "ArrowRight") {
      model.incrementSelectedColor();
    }
  }

  function handlePointerdown(e: PointerEvent, i: number) {
    if (e.shiftKey) {
      (e.currentTarget as Element)?.releasePointerCapture(e.pointerId);
      startingSquare = i;
    }
  }

  function finishZone(i: number) {
    if (startingSquare !== null) {
      model.fillZone(startingSquare, i);
    }
    startingSquare = null;
    startingPosition = null;
  }

  onMount(() => {
    model.loadRecords();
  });
</script>

{#snippet zone()}
  {#if startingPosition && currentPosition}
    {@const { x: x1, y: y1 } = startingPosition}
    {@const { x: x2, y: y2 } = currentPosition}
    <rect
      x="{100*Math.min(x1, x2)}%"
      y="{100*Math.min(y1, y2)}%"
      width="{100*Math.abs(x2 - x1)}%"
      height="{100*Math.abs(y2 - y1)}%"
      class="zone"
      fill={colors[model.selectedColor]}
    />
  #{/if}
{/snippet}

{#snippet pointerTrap()}
  {#if !startingPosition && currentPosition}
    {@const {x, y} = currentPosition}
    <use
      href="#trap"
      x="-20"
      y="-20"
      width="40"
      height="40"
      class="pointer"
      style:transform="translate({100*x}%, {100*y}%)"
    />
  {/if}
{/snippet}

{#snippet board()}
  <div class="container">
    <div class="ui-board" style={gridStyle(model.rows, model.columns, 5)}>
      <!-- svelte-ignore a11y_no_static_element_interactions -->
      <svg
        viewBox="0 0 {50*model.columns} {50*model.rows}"  
        onpointerdown={e => { if(e.shiftKey) startingPosition = getPointerPosition(e) }}   
        onpointermove={e => currentPosition = getPointerPosition(e)}
        onpointerleave={() => startingPosition = currentPosition = null}
      >
        {#each model.squareColors as color, i}
          {@const [row, col] = coords(model.columns, i)}
          <use href="#grass" x={50*col} y={50*row} width="50" height="50" fill={colors[color]} />  
          <!-- svelte-ignore a11y_click_events_have_key_events -->
          <!-- svelte-ignore a11y_no_static_element_interactions -->
          <rect x={50*col} y={50*row} width="50" height="50"
            class="square-borders"
            onpointerdown={e => handlePointerdown(e, i)}
            onpointerup={() => finishZone(i)}
            onclick={e => { if (!e.shiftKey) model.playA(i) }}
          />
        {/each}
        {#each model.position as hasTrap, i}
          {@const [row, col] = coords(model.columns, i)}
          {#if hasTrap}
            <use href="#trap" x={50*col+5} y={50*row+5} width="40" height="40" pointer-events="none" />
          {/if}
        {/each}
        {#each model.nonTrappedBeast as hasBeast, i}
          {@const [row, col] = coords(model.columns, i)}
          <use
            href="#paw" x={50*col+5} y={50*row+5} width="40" height="40"
            class={["beast", {visible: model.help && hasBeast}]}
          />
        {/each}
        {@render zone()}
        {@render pointerTrap()}
      </svg>
    </div>
    <div class="color" style:background-color={colors[model.selectedColor]}></div>
  </div>
{/snippet}

{#snippet config()}
  <Config title="La bête">
    <I.SelectGroup
      title="Forme de la bête"
      values={[BeastType.Type1, BeastType.Type2, BeastType.Type3, BeastType.Type4, BeastType.Custom]}
      text={["#beast1", "#beast2", "#beast3", "#beast23", "#customize"]}
      selected={model.beastType}
      setter={i => model.newGame(() => model.setBeastType(i))}
    />
    <I.SelectGroup
      title="Type de la grille"
      values={[Mode.Standard, Mode.Cylinder, Mode.Torus]}
      text={["#grid-normal", "#grid-cylinder", "#grid-torus"]}
      tooltip={["Normale", "Cylindrique", "Torique"]}
      selected={model.mode}
      setter={model.setMode}
    />
    <I.SizesGroup bind:model={model}
      values={[[3,3], [5,5], [6,6]]}
      customSize={true}
    />
    <I.Group title="Options">
      <I.Help bind:model={model} interaction="press" />
      <I.Reset bind:model={model} />
      <I.Rules bind:model={model} />
    </I.Group>
    <I.BestScore bind:model={model} />
  </Config>
{/snippet}

{#snippet bestScore(position: Position)}
  <div class="bestscore-container">
    <div class="ui-board" style={gridStyle(model.rows, model.columns, 5)}>
      <svg viewBox="0 0 {50*model.columns} {50*model.rows}">
        {#each position as hasTrap, i}
          {@const [row, col] = coords(model.columns, i)}
          <use href="#grass" x={50*col} y={50*row} width="50" height="50" fill={colors[0]} />  
          <rect x={50*col} y={50*row} width="50" height="50" class="square-borders" />
          {#if hasTrap}
            <use href="#trap" x={50*col+5} y={50*row+5} width="40" height="40" />
          {/if}
        {/each}
      </svg>
    </div>
  </div>
{/snippet}

{#snippet custom()}
  <Dialog
    title="Personnalise ta bête"
    onOk={model.closeDialog}
  >
    <div class="custombeast-container">
      <svg viewBox="0 0 250 250">
        {#each model.customBeastGrid as beast, i}
          {@const [row, col] = coords(5, i)}
          <use href="#grass" x={50*col} y={50*row} width="50" height="50" fill={colors[0]} />  
          <!-- svelte-ignore a11y_click_events_have_key_events -->
          <!-- svelte-ignore a11y_no_static_element_interactions -->
          <rect
            x={50*col} y={50*row} width="50" height="50"
            class="square-borders"
            onclick={() => model.flipCustomBeast(i)}
          />
          <use
            href="#paw" x={50*col+5} y={50*row+5} width="40" height="40"
            class={["beast", {visible: beast}]}
          />
        {/each}
      </svg>
    </div>
  </Dialog>
{/snippet}


{#snippet rules()}
  Place le moins de <strong>pièges</strong> possible pour empêcher la <strong>bête</strong> d'abîmer ta belle pelouse !<br/>
  Tu peux choisir de jouer avec des bêtes de différentes formes comme celles prédéfinies dans
  <strong>Forme de la bête</strong>.<br/>
  Dans le dernier choix, la bête peut prendre l'une ou l'autre des formes indiquées.<br/>
  Le plateau de jeu peut prendre une <strong>grille</strong>, un <strong>cylindre</strong>
  ou un <strong>tore</strong>.<br/>
  <br/>
  Pour t'aider, tu peux <strong>colorier</strong> les zones en la sélectionnant à la souris
  et en maintenant <strong>shift</strong> enfoncé.<br/>
  Tu peux changer de couleur à l'aide des flèches gauche et droite du clavier.
{/snippet}

<svelte:window on:keydown={handleKeydown} />
<Template bind:model={model} {board} {config} {rules} {winTitle} {bestScore} {custom} />

<style>
  .container {
    position: relative;
    width: 75vmin;
    height: 75vmin;
    display: flex;
    align-items: center;
    justify-content: center;
  }
  
  .color {
    position: absolute;
    left: 10rem;
    top: -4em;
    display: inline-block;
    width: 3.5em;
    height: 2.5em;
  }

  .beast {
    opacity: 0;
    transition: opacity 0.3s ease-in;
    pointer-events: none;

    &.visible {
      opacity: 0.6;
      transition: none;
    }
  }

  .square-borders {
    stroke: black;
    stroke-width: 0.5;
    fill: transparent;
  }

  .pointer {
    opacity: 0.7;
    pointer-events: none;

    &.overtrap {
      opacity: 0.3;
    }
  }

  .zone {
    stroke: black;
    pointer-events: none;
    opacity: 0.4;
  }

  .custombeast-container {
    width: 50vmin;
    height: 50vmin;
  }

  .bestscore-container {
    width: 60vmin;
    height: 60vmin;
    display: flex;
    align-items: center;
    justify-content: center;
  }
</style>