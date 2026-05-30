<script lang="ts">
  import Template from '$lib/components/Template.svelte';
  import * as I from '$lib/components/Icons';
  import Config from '$lib/components/Config.svelte';
  import { type IModel } from './types';

  type Props = { model: IModel }
  let { model=$bindable() }: Props = $props();
  
  let hoverCell: number | null = $state(null);
  let levelFinished = $derived(model.isLevelFinished());

  const colors = [
    "var(--green-500)",
    "var(--yellow-400)",
    "var(--red-500)",
    "var(--purple-500)",
    "var(--blue-600)"
  ];

  function translateCell(i: number) {
    const x = 50 + 35 * Math.cos(2 *  i * Math.PI / model.size);
    const y = 45 + 35 * Math.sin(2 *  i * Math.PI / model.size);
    return `translate(${x}px, ${y}px)`;
  }
</script>

{#snippet cell(i: number, color: number)}
  <!-- svelte-ignore a11y_click_events_have_key_events -->
  <!-- svelte-ignore a11y_no_static_element_interactions -->
  <circle
    r="7.5"
    class={["cell", {finished: levelFinished}]}
    stroke={hoverCell !== null && model.inRange(i, hoverCell) ? "var(--green-600)" : "var(--main-color)"}
    fill={levelFinished ? "" : colors[color]}
    style:transform={translateCell(i)}
    onclick={() => model.playA(i)}
    onpointerenter={() => hoverCell = i}
    onpointerleave={() => hoverCell = null}
  />
{/snippet}

{#snippet colorCycle()}
  {#each colors.slice(0, model.colorCount) as color, i}
    <circle
      cx={95 + 15 * (i - model.colorCount)}
      cy="95"
      r="3"
      fill={color}
    />
    <path
      d="M0 2H4V0l3 3l-3 3v-2h-4Z"
      fill="black"
      style:transform="translate({99 + 15 * (i - model.colorCount)}px, 92px)"
    />
  {/each}
  <circle cx="95" cy="95" r="3" fill={colors[0]} />
{/snippet}

{#snippet board()}
  <div class="ui-board board">
    <svg viewBox="0 0 100 100">
      {#each model.position as color, i}
        {@render cell(i, color)}
      {/each}
      {@render colorCycle()}
    </svg>
  </div>
{/snippet}

{#snippet config()}
  <Config title="Jeux tricolores">
    <I.SelectGroup
      title="Nombre de feux"
      values={[4, 5, 6, 7, 8, 9, 10, 11, 12, 13]}
      selected={model.size}
      setter={model.setSize}
    />
    <I.SelectGroup
      title="Nombre de couleurs"
      values={[2, 3, 4, 5]}
      selected={model.colorCount}
      setter={model.setColorCount}
    />
    <I.SelectGroup
      title="Portée"
      values={[1, 2, 3]}
      selected={model.range}
      setter={model.setRange}
    />
    <I.Group title="Options">
      <I.Undo bind:model={model} />
      <I.Redo bind:model={model} />
      <I.Reset bind:model={model} />
      <I.Icon
        text="#shuffle"
        tooltip="Mélanger"
        selected={model.shuffle}
        onclick={model.toggleShuffle}
      />
      <I.Rules bind:model={model} />  
    </I.Group>
  </Config>
{/snippet}

{#snippet rules()}
  <strong>Feux tricolores</strong> est une variante de <strong>Tout noir ou tout blanc</strong> mais
  avec plusieurs <strong>couleurs</strong>.<br/>
  Lorsque tu <strong>cliques</strong> un feu, celui-ci change de couleur ainsi que tous les jetons proches
  jusqu'à la distance choisie dans <strong>Portée</strong>.<br/>
  Le but est que tous les jetons soient de couleur <strong>verte</strong>.
{/snippet}

<Template bind:model={model} {board} {config} {rules} />

<style>
  .board {
    height: 80vmin;
    width: 80vmin;
  }

  .cell {
    transition: fill 0.3s linear;
    &.finished {
      animation: 2s infinite tricolorHola;
    }
  }

  @keyframes tricolorHola {
    0%  { fill: var(--green-500); }
    20% { fill: var(--yellow-400); }
    40% { fill: var(--red-500); }
    60% { fill: var(--purple-500); }
    80% { fill: var(--blue-600); }
    100%  { fill: var(--green-500); }
  }
</style>