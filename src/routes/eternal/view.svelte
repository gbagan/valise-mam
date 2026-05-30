<script lang="ts">
  import { GraphKind, Phase, Rules, type IModel } from "./types";
  import { getPointerPosition } from "$lib/util";
  import Template from "$lib/components/Template.svelte";
  import * as I from '$lib/components/Icons';
  import Config from '$lib/components/Config.svelte';
  import GraphEditor from "$lib/components/GraphEditor.svelte";

  const {model}: {model: IModel} = $props();

  let draggedGuard: number | null = $state(null);
  let pointerPosition: {x: number, y: number} | null = $state(null);

  type Coords = {x: number, y: number};
  const translateGuard = ({ x, y }: Coords) => `translate(${100*x}%,${100*y}%)`;

  function arrowPath(x1: number, y1: number, x2: number, y2: number) {
    const arrowSize = 6;
    const dx = x2 - x1;
    const dy = y2 - y1;
    const len = Math.sqrt(dx * dx + dy * dy);
    const angle2 = Math.acos(dx / len);
    const angle = dy >= 0 ? 2 * Math.PI - angle2 : angle2;
    const x3 = x2 + arrowSize * Math.sin(angle - Math.PI / 3);
    const y3 = y2 + arrowSize * Math.cos(angle - Math.PI / 3);
    const x4 = x2 + arrowSize * Math.sin(angle - 2 * Math.PI / 3);
    const y4 = y2 + arrowSize * Math.cos(angle - 2 * Math.PI / 3);
    return `M${x2} ${y2}L${x3} ${y3}L${x4} ${y4}z`;
  }

  function setPointer(e: PointerEvent) {
    if (draggedGuard === null) return;
    pointerPosition = getPointerPosition(e);
  }

  function startDrag(e: PointerEvent, i: number) {
    if (model.position.guards.includes(i) && model.position.attacked !== null)
    {
      (e.currentTarget as Element)?.releasePointerCapture(e.pointerId);
      draggedGuard = i;
    }
  }

  function dropGuard(to: number | null) {
    if (draggedGuard !== null) {
      model.moveGuard(draggedGuard, to);
      draggedGuard = null;
    }
  }

  const winTitle = "L'attaquant gagne";
</script>

{#snippet arrow(x1: number, y1: number, x2: number, y2: number)}
  <line {x1} {y1} {x2} {y2} class="line2" />
  <path d={arrowPath(x1, y1, x2, y2)} fill="red" />
{/snippet}

{#snippet cursor(x: number, y: number)}
  <use
    href="#roman"
    width="6"
    height="12"
    x="-3"
    y="-6"
    pointer-events="none"
    style:transform="translate({100*x}px, {100*y}px)"
  />
{/snippet}

{#snippet board()}
  <div class="ui-board board">
    <!-- svelte-ignore a11y_no_static_element_interactions -->
    <svg
      viewBox="0 0 100 100"
      onpointerdown={setPointer}
      onpointermove={setPointer}
      onpointerup={() => dropGuard(null)}
      onpointerleave={() => draggedGuard = null}
    >
      {#each model.graph.edges as [u, v]}
        {@const {x1, x2, y1, y2} = model.graph.getCoordsOfEdge(u, v)}
        <line x1={100*x1} x2={100*x2} y1={100*y1} y2={100*y2} class="line1" />
      {/each}
      {#if model.rules === Rules.ManyGuards}
        {#each model.nextMove as to, i}
          {@const from = model.position.guards[i]}
          {#if from !== to}
            {@const {x1, y1, x2, y2} = model.graph.getCoordsOfEdge(from, to)}
            {@render arrow(100*x1, 100*y1, 100*x2, 100*y2)}
          {/if}
        {/each}
      {/if}
      {#each model.graph.vertices as {x, y}}
        <circle cx={100*x} cy={100*y} r="3" fill="blue" />
      {/each}
      {#each model.position.guards as guard}
        <use
          href="#roman"
          width="6"
          height="12"
          x="-3"
          y="-6"
          class={["guard", {nomove: model.phase === Phase.Preparation}]}
          style:transform={translateGuard(model.graph.vertices[guard])}
        />
      {/each}
      {#if model.position.attacked !== null}
        <use
          href="#eternal-attack"
          width="8"
          height="8"
          x="-4"
          y="-4"
          style:transform={translateGuard(model.graph.vertices[model.position.attacked])}
          pointer-events="none"
        />
      {/if}
      {#each model.graph.vertices as pos, i}
        <!-- svelte-ignore a11y_click_events_have_key_events -->
        <!-- svelte-ignore a11y_no_static_element_interactions -->
        <rect
          width="10"
          height="10"
          x="-5"
          y="-5"
          fill="transparent"
          style:transform={translateGuard(pos)}
          onclick={() => model.selectVertex(i)}
          onpointerdown={e => startDrag(e, i)}
          onpointerup={() => dropGuard(i)}
          class={{sel: model.phase === Phase.Preparation
                  || model.position.attacked !== null && model.position.guards.includes(i)
                  || model.position.attacked === null && !model.position.guards.includes(i)
                  // todo
                }}
        />
      {/each}
      {#if draggedGuard !== null && pointerPosition !== null}
        {@render cursor(pointerPosition.x, pointerPosition.y)}
      {/if}
    </svg>
    <span class="info">
      {#if model.levelFinished}
        Le sommet attaqué ne peut être défendu
      {:else if model.phase === Phase.Preparation}
        Choisis la position initiale des gardes
      {:else if model.position.attacked !== null}
        Déplace un garde vers le sommet attaqué
      {:else}
        Choisis un sommet à attaquer
      {/if}
    </span>
    <button
      class="ui-button ui-button-primary validate"
      disabled={model.guardCount === 0 || model.phase === Phase.Game 
                && (model.rules === Rules.OneGuard)}
      onclick={model.validate}
    >Valider</button>
  </div>
{/snippet}

{#snippet config()}
  <Config title="Domination éternelle">
    <I.SelectGroup
      title="Type de graphe"
      values={[GraphKind.Path, GraphKind.Cycle, GraphKind.Biclique, GraphKind.Grid]}
      selected={model.graphKind}
      text={["#graph-path", "#graph-cycle", "#graph-biclique", "#graph-grid"]}
      disabled={model.locked}
      tooltip={["Chemin", "Cycle", "Biclique", "Grille"]}
      setter={i => model.newGame(() => model.setGraphKind(i))}
    >
      <I.Icon
        text="#customize"
        tooltip="Créé ton propre graphe"
        selected={model.graphKind === GraphKind.Custom}
        disabled={model.locked}
        onclick={model.customize}
      />
    </I.SelectGroup>
    <I.SelectGroup
      title="Règles"
      values={[Rules.OneGuard, Rules.ManyGuards]}
      selected={model.rules}
      text={["1", "∞"]}
      disabled={model.locked}
      tooltip={["Un seul garde peut se déplacer", "Plusieurs gardes peuvent se déplacer"]}
      setter={model.setRules}
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

{#snippet custom()}
  <GraphEditor onOk={model.acceptCustomGraph} />
{/snippet}

{#snippet rules()}
  <strong>Domination Eternelle</strong> est un jeu à deux joueurs: un <strong>attaquant</strong>
   et un <strong>défenseur</strong>.<br/>
  Au début de la partie, le défenseur choisit des sommets sur lesquels poser des <strong>gardes</strong>.<br/>
  Ensuite, à chaque tour, l'attaquant choisit d'attaquer un sommet puis le défenseur doit <strong>déplacer</strong>
  un de ses gardes vers le sommet attaqué à la condition que celui soit <strong>adjacent</strong> au garde.<br/>
  Si le défenseur ne peut pas déplacer de garde, il perd la partie.<br/>
  La partie peut ne pas avoir de fin. Le but est de déterminer le nombre minimum
  de gardes pour défendre infiniment toute attaque.<br/>
  Dans une variante, le défenseur peut déplacer <strong>plusieurs gardes</strong> à chaque tour.
{/snippet}

<Template {model} {board} {config} {custom} {rules} {winTitle} />

<style>
  .board {
    width: 75vmin;
    height: 75vmin;
  }
  
  .line1 {
    stroke: grey;
    stroke-dasharray: 3,1;
  }

  .line2 {
    stroke: red;
    stroke-width: 2;
  }

  .guard {
    transition: transform 0.5s linear;
    pointer-events: none;
    &.nomove {
        transition: none;
    }
  }

  .sel {
    cursor: pointer;
  }

  .validate {
    position: absolute;
    bottom: 1rem;
    right: 1rem;
    font-size: 1.7rem;
    padding: 0;
  }

  .info {
    position: absolute;
    bottom: 1rem;
    left: 1rem;
    font-size: 1.5rem;
    color: var(--emph-color);
    padding: 0;
  }
</style>