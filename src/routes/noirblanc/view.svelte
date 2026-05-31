<script lang="ts">
  import type { IModel, Mode } from './types';
  import { gridStyle } from '$lib/util';
  import Template from '$lib/components/Template.svelte';
  import * as I from '$lib/components/Icons';
  import Config from '$lib/components/Config.svelte';
  import { onMount } from 'svelte';

  const {model}: {model: IModel} = $props();

  const levelTexts = ["3x3", "4x4", "2x10", "3x10", "5x5", "NxM", "#lo-rand"];
  const levelText = (i: number, unblocked: boolean) => unblocked ? levelTexts[i] : "#locked";

  const levelTooltips: (string | null)[] = [
    null, null, null, null, null,
    "Dimensions personnalisées", "Grille aléatoire"
  ];
  const levelTooltip = (i: number, unblocked: boolean) => unblocked ? levelTooltips[i] : "Difficulté non débloquée";

  onMount(() => model.loadRecords());
</script>

{#snippet square(light: boolean, cross: boolean, style: string, onclick: () => void)}
  <!-- svelte-ignore a11y_click_events_have_key_events -->
  <!-- svelte-ignore a11y_no_static_element_interactions -->
  <div class="square" {style} {onclick}>
    <div class={["square-inner", {light}]}>
      <div class="blanc">
        {#if cross}
          <svg class="cross"><use href="#cross"/></svg>
        {/if}
      </div>
      <div class="noir">
        {#if cross}
          <svg class="cross"><use href="#cross"/></svg>
        {/if}
      </div>
    </div>
  </div>
{/snippet}

{#snippet board()}
  {@const rows = model.rows}
  {@const cols = model.columns}  
  <div class="board-container">
    <div class="ui-board" style={gridStyle(model.rows, model.columns, 4)}>
      {#each model.position.light as light, i}
        {@const row = i / model.columns | 0}
        {@const col = i % model.columns}
        {@const style = `width:${86/cols}%;height:${86/rows}%;left:${(100*col+7)/cols}%;top:${(100*row+7)/rows}%;`}
        {@render square(light, model.help && model.position.played[i], style, () => (console.log(model), model.playA(i)))}
      {/each}
    </div>
  </div>
{/snippet}

{#snippet config()}
  <Config title="Tout noir, tout blanc">
    <I.SelectGroup
      title="Mode de jeu"
      values={[0, 1, 2, 3] as Mode[]}
      text={i => `#lo-mode${i+1}`}
      selected={model.mode}
      setter={i => model.changeMode(i)}
    />
    <I.SelectGroup
      title="Difficulté"
      values={[0, 1, 2, 3, 4, 5, 6]}
      text={i => levelText(i, i <= model.maxLevels[model.mode])}
      tooltip={i => levelTooltip(i, i <= model.maxLevels[model.mode])}
      disabled={i => i > model.maxLevels[model.mode]}
      selected={model.level}
      setter={model.changeLevel}
    />

    <I.Group title="Options">
      <I.Help {model} />
      <I.Reset {model} />
      <I.Rules {model} />
    </I.Group>
  </Config>
{/snippet}

{#snippet rules()}
  Le but de <strong>Tout noir, tout blanc</strong> est de <strong>retourner</strong> pour que toutes soient face noire.<br/>
  La difficulté est que lorsque tu retournes une tuile, les tuiles <strong>adjacentes</strong> sont également retournées.<br/>
  Ce jeu possède différents <strong>niveaux débloqués</strong> au fur et à mesure ainsi que d'autres modes de jeu.
  Selon le mode choisi, les règles pour retourner les tuiles changent.
{/snippet}

<Template {model} {board} {config} {rules} />

<style>
  .board-container {
    width: 75vmin;
    height: 75vmin;
    display: flex;
    align-items: center;
    justify-content: center;
  }

  .square {
    position: absolute;
    overflow: hidden;
    perspective: 1000px;
  }

  .square-inner {
    position: relative;
    width: 100%;
    height: 100%;
    transition: transform 0.8s;
    transform-style: preserve-3d;
    &.light {
      transform: rotateY(180deg);
    }
}
  
  .noir, .blanc {
    position: absolute;
    width: 100%;
    height: 100%;
    backface-visibility: hidden;
    border: 0.2rem solid #656565;
    border-radius: 12%;
  }
  
  .noir {
    background-color: #333;
  }
  
  .blanc {
    background-color: #dfdfdf;
    transform: rotateY(180deg);
  }

  .cross {
    position: absolute;
    width: 100%;
    height: 100%;
    opacity: 0.7;
    stroke: blue;
  }
</style>