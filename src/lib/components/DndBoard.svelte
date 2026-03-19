<script lang="ts" generics="Drag">
  import { getPointerPosition } from '$lib/util';
  import type { Snippet } from 'svelte';

  interface Props {
    viewBox: string;
    class?: string;
    style?: string;
    dragged: Drag | null;
    children: Snippet;
    draggedElement: Snippet<[style: string]>;
  }
  
  let { viewBox, class: className, style, children, draggedElement, dragged=$bindable() }: Props = $props();
  let position: {x: number, y: number} | null = $state(null);

  const onpointermove = (e: PointerEvent) => {
    if (dragged === null) return;
    position = getPointerPosition(e);
  }

</script>

<!-- svelte-ignore a11y_no_static_element_interactions -->
<svg
  {viewBox} class={className} {style}
  onpointerdown={onpointermove}
  onpointermove={onpointermove}
  onpointerup={() => dragged = null}
  onpointerleave={() => dragged = null}
>
  {@render children()}
  {#if dragged !== null && position}
    {@render draggedElement(`transform:translate(${100*position.x}%,${100*position.y}%);`)}
  {/if}
</svg>