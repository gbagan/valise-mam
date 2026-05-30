<script lang="ts">
  import Template from '$lib/components/Template.svelte';
  import * as I from '$lib/components/Icons';
  import Config from '$lib/components/Config.svelte';
  import type { IModel } from './types';

  type Props = { model: IModel }
  const {model}: Props = $props();

  const levelFinished = $derived(model.isLevelFinished());
  
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

  const transformPeg = (position: number) => {
    const mid = position / 2 | 0;
    const angle = 2 * mid * Math.PI / model.baseCount;
    const x = 0.42 + 0.35 * Math.cos(angle) + 0.1 * (position % 2);
    const y = 0.46 + 0.35 * Math.sin(angle);
    return `translate(${100*x}%, ${100*y}%)`;
  }

  const transformBase = (i: number) => {
    const angle = 2 * i * Math.PI / model.baseCount;
    const x = 0.5 + 0.35 * Math.cos(angle);
    const y = 0.5 + 0.35 * Math.sin(angle);
    return `translate(${100*x}%, ${100*y}%) rotate(45deg)`;
  }
</script>

{#snippet board()}
  <div class="ui-board board">
    <svg viewBox="0 0 100 100">
      {#each colors.slice(0, model.baseCount) as color, i}
        <rect
          class="base"
          stroke={color}
          style:transform={transformBase(i)}          
        />
      {/each}

      {#each model.position as pos, peg}
        {#if peg !== model.missingPeg}
          <g
            class="player"
            style:transform={transformPeg(pos)}
          >
            <!-- svelte-ignore a11y_click_events_have_key_events -->
            <!-- svelte-ignore a11y_no_static_element_interactions -->
            <use
              href="#meeple"
              width="7"
              height="7"
              fill={colors[peg / 2 | 0]}
              onclick={() => model.playA(peg)}
              style:cursor={model.canPlay(peg) ? "pointer" : "not-allowed"}
              style:animation-delay="{1000 + 2000 * peg / model.baseCount}ms"
              class={{animate: levelFinished}} 
            />
          </g>
        {/if}
      {/each}
    </svg>
  </div>
{/snippet}

{#snippet config()}
  <Config title="Baseball multicolore">
    <I.SelectGroup
      title="Nombre de bases"
      values={[4, 5, 6, 7, 8]}
      selected={model.baseCount}
      setter={model.setBaseCount}
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
  Le but de <strong>Baseball multicolore</strong> est d'amener chaque <strong>jeton</strong> dans sa
  <strong>base</strong> (celle qui a la même couleur que le jeton).<br/>
  Pour cela, tu peux déplacer un jeton vers une base <strong>adjacente</strong>
  si celle-ci possède un emplacement <strong>libre</strong>.<br/>
  Pour déplacer un jeton, il te suffit de <strong>cliquer</strong> dessus.
{/snippet}

<Template {model} {board} {config} {rules} />

<style>
  .board {
    height: 80vmin;
    width: 80vmin;
    background-color: var(--green-300);
  }

  .base {
    stroke-width: 1.2;
    fill: white;
    x: -10px;
    y: -10px;
    width: 20px;
    height: 20px;
  }

  .player {
    transition: transform 0.5s linear;
  }

  .animate {
    animation: hola 4s linear infinite;
  }

  @keyframes hola {
    0%  { transform: translate(0, 0); }
    10% { transform: translate(0, -5%); }
    20%, 100% {transform: translate(0, 0); }
  }
</style>