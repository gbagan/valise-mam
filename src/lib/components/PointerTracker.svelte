<script lang="ts">
  import { getPointerPosition } from "$lib/util";
  import type { Snippet } from "svelte";

  interface Props {
    viewBox: string;
    class?: string;
    style?: string;
    children: Snippet;
    pointer: Snippet<[x: number, y: number]>;
  }

  let { viewBox, class: className, style, children, pointer }: Props = $props();
  
  let position: {x: number, y: number} | null = $state(null);
</script>

<!-- svelte-ignore a11y_no_static_element_interactions -->
<svg
  {viewBox} class={className} {style}
  onpointermove={e => position = getPointerPosition(e)}
  onpointerleave={() => position = null}
>
  {@render children()}
  {#if position}
    {@render pointer(position.x, position.y)}
  {/if}
</svg>