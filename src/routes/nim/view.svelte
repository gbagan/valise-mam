<script lang="ts">
  import { range } from '@gbagan/utils';
  import { Turn } from '$lib/model/types';
  import Template from '$lib/components/Template.svelte';
  import * as I from '$lib/components/Icons';
  import Config from '$lib/components/Config.svelte';
  import { type IModel } from './types';

  type Props = { model: IModel }
  let {model}: Props = $props();

  const turnMessage = $derived(
    model.isLevelFinished() 
    ? "Partie finie"
    : model.turn === Turn.Player1
    ? "Tour du joueur bleu"
    : "Tour du joueur rouge"
  );

  const winTitle = $derived(model.winTitleFor2Player());
</script>

{#snippet row(i: number)}
  <rect class="row {model.length === 5? "row-5" : "row-10"}" y={10+19*i}/>
{/snippet}

{#snippet square(i: number, j: number)}
  <!-- svelte-ignore a11y_click_events_have_key_events -->
  <!-- svelte-ignore a11y_no_static_element_interactions -->
  <rect
    class="square"
    style:transform="translate({(model.length === 5 ? 30 : 5) + 10 * j}px, {15 + 19 * i}px) rotate(45deg)"
    style:cursor={model.canPlay({pile:i, pos: j}) ? "pointer" : "not-allowed"}
    onclick={() => model.playA({pile: i, pos: j})}
  />
{/snippet}

{#snippet peg(i: number, player: number, j: number)}
  <use
    href="#meeple"
    width="8"
    height="8"
    class="player"
    fill={player === 0 ? "blue" : "red"}
    style:transform="translate({ (model.length === 5 ? 26 : 1) + 10 * j}px, {11 + 19 * i}px)"
  />
{/snippet}

{#snippet board()}
  <div class="ui-board board">
    <svg viewBox="0 0 100 100">
      {#each range(0, model.pileCount) as i}
        {@render row(i)}
        {#each range(0, model.length) as j}
          {@render square(i, j)}
        {/each}
      {/each}
      
      {#each model.position as [p1, p2], i}
        {@render peg(i, 0, p1)}
        {@render peg(i, 1, p2)}
      {/each}
    </svg>
    <span class="turn-message">{turnMessage}</span>
  </div>
{/snippet}

{#snippet config()}
  <Config title="Bloque moi si tu peux">
    <I.SelectGroup 
      title="Taille des rangées"
      values={[ 1, 2, 3, 4, 5 ]}
      selected={model.pileCount}
      disabled={model.locked}
      setter={model.setPileCount}
    />
    <I.SelectGroup 
      title="Taille des rangées"
      values={[ 10, 5 ]}
      selected={model.length}
      disabled={model.locked}
      setter={model.setLength}
    />
    <I.TwoPlayers bind:model={model} />
    <I.Group title="Options">
      <I.Undo bind:model={model} />
      <I.Redo bind:model={model} />
      <I.Reset bind:model={model} />
      <I.Rules bind:model={model} />
    </I.Group>
  </Config>
{/snippet}

{#snippet rules()}
  Le but de <strong>Bloque moi</strong> si tu peux est d'acculer chacun des jetons de l'adversaire au bord du plateau de telle
  façon qu'il ne puisse <strong>plus en déplacer</strong>.<br/>
  À chaque tour, tu peux déplacer un de tes jetons vers la <strong>gauche</strong>
  ou vers la <strong>droite</strong> d'autant de cases que tu veux mais tu
  ne peux pas <strong>sauter</strong> par-dessus un jeton adverse.<br/>
  Tu es obligé de déplacer un jeton d'au moins une case, tu ne peux pas <strong>passer ton tour</strong>.<br/>
  Tu gagnes la partie si ton adversaire n'a aucun mouvement possible.
{/snippet}

<Template bind:model={model} {board} {config} {rules} {winTitle} />

<style>
  .board {
    height: 80vmin;
    width: 80vmin;
    position: relative;
    background-color: lightblue;
  }

  .square {
    x: -2.5px;
    y: -2.5px;
    width: 5px;
    height: 5px;
    fill: grey;
  }

  .row {
    height: 10px;
    fill: snow;
  }

  .row-5 {
    x: 25px;
    width: 50px;
  }

  .row-10 {
    width: 100px;
  }

  .player {
    cursor: not-allowed;
    transition: all 0.5s linear;
  }

  .turn-message {
    position: absolute;
    left: 0;
    top: 0;
    color: blue;
    font-size: 1.5em;
    font-weight: bold;
  }
</style>