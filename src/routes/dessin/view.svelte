<script lang="ts">
  import GraphEditor from "$lib/components/GraphEditor.svelte";
  import * as I from '$lib/components/Icons';
  import Config from '$lib/components/Config.svelte';
  import Template from "$lib/components/Template.svelte";
  import PointerTracker from "$lib/components/PointerTracker.svelte";
  import { onMount } from "svelte";
  import graphs from "./graphs";
  import type { IModel, Position } from "./types";

  const {model}: {model: IModel} = $props();

  // pour l'animation quand le niveau est fini
  let counter = $state(0);

  onMount(() => {
    model.loadRecords();
  });

  let levelFinished = $derived(model.isLevelFinished());
  let winTitle = $derived(`Tu as réussi en ${model.raiseCount} levé${model.raiseCount > 1 ? "s" : ""}`);

  const colors = [ "red", "green", "magenta", "orange", "gray", "cyan", "black", "blue" ];

  type PathDescription = {path: string, length: number, start: number};

  let pathDescriptions: PathDescription[] = $derived.by(() => {
    let paths = [];
    let path = "";
    let length = 0;
    let start = 0;
    let pred: [x: number, y: number] | null = null;
    for (const move of model.position) {
      if (move === "raise") {
        length += 0.5;
        paths.push({path, length, start});
        path = "";
        start += length;
        length = 0;
        pred = null;
      } else {
        const {x, y} = model.graph.vertices[move];
        path += `${path === "" ? 'M' : 'L'} ${100*x} ${100*y}`;
        if (pred) {
          let [x2, y2] = pred;
          length += Math.sqrt((x2 - x) ** 2 + (y2 - y) ** 2);
        }
        pred = [x, y];
      }
    }
    if (path !== "") {
      length += 2;
      paths.push({path, length, start});
    }
    return paths;
  })
</script>

{#snippet currentLine(x2: number, y2: number)}
  {#if !levelFinished && typeof model.position.at(-1) === "number"}
    {@const {x, y} =  model.graph.vertices[model.position.at(-1) as number] }
    <line
      x1={x*100}
      y1={y*100}
      x2={x2*100}
      y2={y2*100}
      class="line-to-pointer"
      stroke={colors[model.raiseCount] ?? "red"}
    />
  {/if}
{/snippet}

{#snippet board()}
  <!-- svelte-ignore a11y_no_static_element_interactions -->
  <div class="ui-board board" oncontextmenu={e => { e.preventDefault(); model.playA("raise")}}>
    <PointerTracker viewBox="0 0 100 100" pointer={currentLine}>
      {#each model.graph.edges as [u, v]}
        {@const {x1, x2, y1, y2} = model.graph.getCoordsOfEdge(u, v)}
        <line
          x1={x1*100}
          x2={x2*100}
          y1={y1*100}
          y2={y2*100}
          class="line1"
        />
      {/each}
      {#each pathDescriptions as {path, length, start}, i (`${i},${counter}`)}
        <path
          d={path}
          class={["line2", {animate: levelFinished}]}
          stroke-dasharray={!levelFinished ? "0" : 100 * length}
          stroke-dashoffset={!levelFinished ? "0" : 100 * length}
          style:animation-duration="{length}s"
          style:animation-delay="{start}s"
          stroke={colors[i] ?? "red"}
          onanimationend={i < pathDescriptions.length - 1 ? null : () => counter += 1}
        />
      {/each}
      {#if !levelFinished}
        {#each model.graph.vertices as {x, y}, i}
          <!-- svelte-ignore a11y_click_events_have_key_events -->
          <!-- svelte-ignore a11y_no_static_element_interactions -->
          <circle
            cx={x*100} cy={y*100}
            r="3"
            stroke={i === model.position.at(-1) ? "red" : "blue"}
            fill="blue"
            onclick={() => model.playA(i)}
          />
        {/each}
      {/if}
    </PointerTracker>
    <span class="title">{model.graph.title}</span>
    <span class="raise-info">{model.raiseCount} levé{model.raiseCount > 1 ? "s" : ""} de crayon</span>
    <button
      class="ui-button ui-button-primary raise"
      disabled={levelFinished || typeof model.position.at(-1) !== "number"}
      onclick={() => model.playA("raise")}
    > Lever le crayon
    </button>
  </div>
{/snippet}

{#snippet config()}
  <Config title="Dessin">
    <I.SelectGroup
      title="Niveau"
      values={[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]}
      selected={model.graphIndex}
      text={i => "" + (i as number + 1)}
      tooltip={i => graphs[i as number].title}
      setter={model.setGraphIndex}
    >
      <I.Icon
        text="#customize"
        tooltip="Créé ton propre graphe"
        selected={model.graphIndex === "custom"}
        onclick={model.openCustomizeDialog}
      />
    </I.SelectGroup>
    <I.Group title="Options">
      <I.Undo {model} />
      <I.Redo {model} />
      <I.Reset {model} />
      <I.Rules {model} />
    </I.Group>
    <I.BestScore {model} />
  </Config>
{/snippet}

{#snippet bestScore(position: Position)}
  <div class="bestscore">
    <svg viewBox="0 0 100 100">
      {#each model.edgesOf(position) as [u, v], i}
        {@const {x1, x2, y1, y2} = model.graph.getCoordsOfEdge(u, v)}
        <line
          x1={x1*100}
          x2={x2*100}
          y1={y1*100}
          y2={y2*100}
          class="line2"
          stroke="red"
        />
        <text x={50*(x1+x2)} y={50*(y1+y2)} class="edge-no">{i+1}</text>
      {/each}
    </svg>
  </div>
{/snippet}

{#snippet custom()}
  <GraphEditor onOk={model.acceptCustomGraph} />
{/snippet}

{#snippet rules()}
  Le but du jeu est de dessiner le motif indiqué en pointillé en levant le moins souvent possible le crayon.<br/>
  Pour lever le crayon, tu peux cliquer sur le bouton prévu pour ou utiliser le clic droit.
{/snippet}

<Template {model} {board} {config} {rules} {bestScore} {custom} {winTitle} />

<style>
  .board {
    width: 75vmin;
    height: 75vmin;
  }

  .bestscore {
    width: 66vmin;
    height: 66vmin;
  }

  .line1 {
    stroke: grey;
    stroke-dasharray: 3,1;
  }

  .line2 {
    stroke-width: 1.5;
    fill: transparent;
    stroke-linejoin: round;
    &.animate {
      animation: drawline 6s linear forwards;
    }
  }

  .line-to-pointer {
    stroke-width: 1.5;
    pointer-events: none;
    opacity: 0.6;
  }

  .raise {
    position: absolute;
    bottom: 1rem;
    right: 1rem;
    font-size: 1.7rem;
    padding: 0;
  }

  .title {
    position: absolute;
    top: 1rem;
    left: 1rem;
    font-size: 1.5rem;
    color: green;
    font-weight: bold;
  }

  .raise-info {
    position: absolute;
    top: 1rem;
    right: 1rem;
    font-size: 1.5rem;
    color: blue;
    font-weight: bold;
  }

  .edge-no {
    fill: blue;
    font-weight: bold;
    font-size: 6px;
    text-anchor: middle;
    dominant-baseline: middle;
  }

  @keyframes drawline {
    to {
      stroke-dashoffset: 0;
    }
  }
</style>