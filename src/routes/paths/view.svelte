<script lang="ts">
  import { type IModel, Mode } from './types';
  import { arrayOf } from "@gbagan/utils";
  import { gridStyle } from '$lib/util';
  import Template from '$lib/components/Template.svelte';
  import PointerTracker from '$lib/components/PointerTracker.svelte';
  import * as I from '$lib/components/Icons';
  import Config from '$lib/components/Config.svelte';

  const {model}: {model: IModel} = $props();
  let levelFinished = $derived(model.isLevelFinished());

  let grid = $derived.by(() => {
    const g = arrayOf(model.rows * model.columns, false);
    for (const i of model.position) {
      g[i] = true;
    }
    return g;
  })

  let pathDescription = $derived(
    model.position
      .map((p, i) => {
        const x = 50 + 100 * (p % model.columns);
        const y =  50 + 100 * (p / model.columns | 0);
        return `${i === 0 ? 'M' : 'L'}${x} ${y}`
      }).join("")
  );
</script>

{#snippet square(x: number, y: number, darken: boolean, trap: boolean, door: boolean, onclick: () => void)}
  <g class={{darken}} style:transform="translate({x}px, {y}px)">
    <!-- svelte-ignore a11y_click_events_have_key_events -->
    <!-- svelte-ignore a11y_no_static_element_interactions -->
    <use href="#paths-background" width="100" height="100"
      onclick={onclick}
    />
    {#if door}
      <use href="#paths-door" width="100" height="100" />
    {/if}
    <use href="#paths-trap" width="100" height="100" class={["trap", {visible: trap && !door}]} />
  </g>
{/snippet}

{#snippet hero(position: number)}
  {@const row = position / model.columns | 0}
  {@const col = position % model.columns}
  <use href="#meeplehat" width="80" height="80" x="10" y="10" class="hero"
    style:transform="translate({col * 100}px, {row * 100}px)"
  />
{/snippet}

{#snippet pointer(x: number, y: number)}
  {#if model.position.length === 0}
    <use
      href="#meeplehat"
      class="pointer"
      x="-40"
      y="-40"
      width="80"
      height="80"
      style:transform="translate({100*x}%, {100*y}%)"
    />
  {:else if model.exit === null}
    <use
      href="#paths-door"
      class="pointer"
      x="-50"
      y="-50.0"
      width="100"
      height="100"
      style:transform="translate({100*x}%, {100*y}%)"
    />
  {/if}
{/snippet}

{#snippet board()}
  <div class="board-container">
    <div class="ui-board" style={gridStyle(model.rows, model.columns, 5)}>
      <PointerTracker {pointer} viewBox="0 0 {100*model.columns} {100*model.rows}">
        {#each grid as trapped, index}
          {@const row = index / model.columns | 0}
          {@const col = index % model.columns}
          {@render square(
            col * 100,
            row * 100,
            model.help && (row + col) % 2 === 0,
            trapped && index !== model.position.at(-1) && !levelFinished,
            index === model.exit,
            () => model.selectSquare(index)
          )}
        {/each}
        <path
          d={pathDescription}
          class={["path", {animate: levelFinished}]}
          stroke-dasharray={!levelFinished ? "0" : 100 * model.position.length}
          stroke-dashoffset={!levelFinished ? "0" : 100 * model.position.length}
        />
        {#if model.position.length > 0}
          {@render hero(model.position.at(-1)!)}
        {/if}
      </PointerTracker>
   </div>
  </div>
{/snippet}

{#snippet config()}
  <Config title="Chemins">
    <I.SelectGroup
      title="Mode de jeu"
      values={[Mode.Mode1, Mode.Mode2]}
      text={["#paths-mode0", "#paths-mode1"]}
      tooltip={["Mode 1", "Mode 2"]}
      selected={model.mode}
      setter={model.setMode}
    />

    <I.SizesGroup {model}
      values={[[4, 6], [5, 5], [3, 8]]}
      customSize={true}
    />

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
  <p>
    Après moultes péripéties dans le temple maudit de Berge, le professeur <strong>Hamilton Jones</strong>
    se retrouve dans la dernière salle.<br/>
    Pour sortir de celle-ci, il doit s'enfuir par une porte au-dessous de lui.<br/>
    Celle-ci ne peut être ouverte qu'en marchant sur chacune des dalles dans la salle.
  </p>
  <p>
    Malheureusement, ces dalles sont piégées, le piège se déclenchant peu de temps après avoir marché dessus.<br/>
    Donc, Hamilton ne <strong>ne peut pas remarcher</strong> sur une dalle sur laquelle il a déjà été.<br/>
    N'ayant plus l'aisance de sa jeunesse, Hamilton ne peut se déplacer que d'une <strong>dalle à la fois</strong>
    et ne peut le faire en <strong>diagonale</strong>.
  </p>
  <p>
    Trouve un parcours pour résoudre l'énigme. Ca semble facile ?
    Mais, cela est-il possible pour toutes les tailles de grille ?
  </p>
  <p>
      Tu remarqueras qu'il n'y a <strong>pas</strong> toujours de solution.<br/>
      Dans le deuxième mode de jeu, tu peux choisir la <strong>position de départ</strong> d'Hamilton ainsi que celle de la
      <strong>porte</strong>.<br/>
    Trouve des critères sur les positions d'Hamilton et de la porte pour qu'une solution soit possible
  </p>
{/snippet}

<Template {model} {board} {config} {rules} />

<style>
  .board-container {
    height: 75vmin;
    width: 75vmin;
    display: flex;
    align-items: center;
    justify-content: center;
  }
  
  .darken {
    filter: url(#darken);
  }

  .path {
    stroke-width: 8;
    stroke: blue;
    fill: none;

    &.animate {
      animation: drawline 4s linear forwards infinite;
    }
  }

  .trap{
    opacity: 0;
    pointer-events: none;

    &.visible {
      opacity: 1;
      transition: all 100ms linear 500ms;
    }
  }

  .pointer {
    opacity: 0.6;
    pointer-events: none;
  }

  .hero {
    transition: all 500ms linear;
    pointer-events: none;
  }

  @keyframes drawline {
    70%, 80% {
      stroke-dashoffset: 0;
    }
  }
</style>