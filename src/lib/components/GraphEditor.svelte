<script lang="ts">
  import { type IGraph, MutableGraph, type Coords } from "$lib/graph.svelte";
    import { getPointerPosition } from "$lib/util";
  import Dialog from "./Dialog.svelte";
  import Icon from "./icons/Icon.svelte";

  type Mode = "vertex" | "addedge" | "delete";

  interface Props {
    onOk: (graph: IGraph) => void;
  }

  let {onOk}: Props = $props();

  let graph = $state(new MutableGraph());
  let mode: Mode = $state("vertex");
  let selectedVertex: number | null = $state(null);
  let currentPosition: Coords | null = $state.raw(null);

  const onclick = (e: MouseEvent) => {
    if (mode === "vertex") {
      graph.addVertex(getPointerPosition(e));
    }
  }

  const onpointermove = (e: PointerEvent) => {
    if (selectedVertex === null) return;
    const {x, y} = getPointerPosition(e);
    if (mode === "vertex" && selectedVertex !== null) {
      graph.moveVertex(selectedVertex, {x, y});
    } else if (mode === "addedge") {
      currentPosition = {x, y};
    }
  }

  function dropOrLeave() {
    if (mode === "addedge") {
      selectedVertex = null;
      currentPosition = null;
    }
  }

  function onVertexClick(e: MouseEvent, i: number) {
    if (mode === "vertex") {
      e.stopPropagation();
    } else if (mode === "delete") {
      graph.removeVertex(i);
    }
  }

  function onVertexPointerUp(i: number) {
    if (mode === "vertex") {
      selectedVertex = null;
      currentPosition = null;
    } else if (mode === "addedge" && selectedVertex !== null) {
      graph.addEdge(selectedVertex, i);
      selectedVertex = null;
    }
  }

  function selectVertex(e: MouseEvent, i: number) {
    e.stopPropagation();
    if (mode === "addedge" || mode === "vertex") {
      selectedVertex = i;
    }
  }

  function onEdgeClick(u: number, v: number) {
    if (mode === "delete") {
      graph.removeEdge(u, v);
    }
  }

</script>

<Dialog title="Créé ton graphe" onOk={() => onOk(graph)}>
  <div class="container">
    <div class="board">
      <!-- svelte-ignore a11y_click_events_have_key_events -->
      <!-- svelte-ignore a11y_no_static_element_interactions -->
      <svg
        viewBox="0 0 100 100"
        class="svg {mode}"
        onpointerup={dropOrLeave}
        {onclick}
        {onpointermove}
        onpointerleave={dropOrLeave}
      >
        {#each graph.edges as [u, v]}
          {@const {x1, y1, x2, y2} = graph.getCoordsOfEdge(u, v)}
          <line
            x1={100*x1} y1={100*y1} x2={100*x2} y2={100*y2}
            class="edge"
            onclick={() => onEdgeClick(u, v)}
          />
        {/each}
        {#each graph.vertices as {x, y}, i}
          <circle
            cx={100*x} cy={100*y}
            r="3"
            class={["vertex", {selected: selectedVertex === i}]}
            onclick={e => onVertexClick(e, i)}
            onpointerdown={e => selectVertex(e, i)}
            onpointerup={() => onVertexPointerUp(i)}
          />
        {/each}
        {#if selectedVertex !== null && currentPosition !== null}
          {@const {x, y} = graph.vertices[selectedVertex]}
          <line x1={100*x} y1={100*y} x2={100*currentPosition.x} y2={100*currentPosition.y} class="line-to-pointer" />
        {/if}
        S.line
      </svg>
    </div>
    <div class="buttons">
      <Icon text="#vertex" selected={mode === "vertex"} tooltip="Ajoute ou déplace un sommet"
        onclick={() => mode = "vertex"}
      />
      <Icon text="#edge" selected={mode === "addedge"} tooltip="Connecte deux sommets"
        onclick={() => mode = "addedge"}
      />
      <Icon text="#trash" selected={mode === "delete"} tooltip="Retire un sommet ou une arête"
        onclick={() => mode = "delete"}
      />
      <Icon text="#clear" tooltip="Efface tout le graphe"
        onclick={() => graph.clear()}
      />
    </div>

  </div>
</Dialog>

<style>
  .container {
    display: flex;
    align-items: center;
    justify-content: center;
    gap: 1rem;
  }

  .board {
    width: 75vmin;
    height: 75vmin;
    box-shadow: var(--centered-shadow);
  }

  .vertex {
    fill: var(--blue-600);
    stroke: var(--blue-600);
    &.selected {
      stroke: var(--red-500);
    }
  }

  .delete > .vertex:hover {
    fill: var(--gray-500);
    stroke: var(--gray-500);
  }

  .edge {
    stroke: var(--red-500);
    stroke-width: 1.5;
  }

  .delete .edge:hover {
    stroke: var(--gray-500);
  }

  .buttons {
    display: grid;
    grid-template-columns: repeat(1, 1fr);
    grid-gap: 1rem;
  }

  .line-to-pointer {
    stroke: var(--red-500);
    stroke-width: 1.5;
    pointer-events: none;
    opacity: 0.6;
  }
</style>