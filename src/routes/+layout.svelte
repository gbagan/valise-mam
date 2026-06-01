<script lang="ts">
  import "../app.css";
  import { page } from '$app/state';
  import { resolve } from '$app/paths';
  import type { Snippet } from "svelte";

  type Props = { children: Snippet }
  const { children }: Props = $props();
  let isHome = $derived(page.route.id === '/');
</script>

<div class={["layout", isHome ? "valise" : "game"]}>
  {#if !isHome}
    <a class="valise-link" href={resolve('/')} aria-label="valise">
      <svg style="width: 100%; height: 100%">
        <use href="#valise"/>
      </svg>
    </a>
  {/if}
  {@render children()}
</div>
<style>
  .layout {
    height: 100vh;
    width: 100vw;
    transform: scale(1);
    &.game {
      animation: linear 1s gameenter;
    }
    &.valise {
      animation: linear 1s valiseenter;
    }
  }

  @keyframes gameenter {
    from {
      transform: scale(0.05); 
    }
    to {
      transform: scale(1);
    }
  }

  @keyframes valiseenter {
    from {
      transform: translate(-50%, -50%) scale(0.1);
    }
    to {
      transform: scale(1);
    }
  }

  .valise-link {
    position: absolute;
    left: 0;
    width: calc(192vmin / 25);
    height: calc(169vmin / 25);
    z-index: 10;
  }
</style>