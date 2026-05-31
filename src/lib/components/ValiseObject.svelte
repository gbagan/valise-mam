<script lang="ts">
  import { resolve } from '$app/paths';

  interface Props {
    symbol: string;
    drag?: boolean;
    link?: string;
    help: string;
    x: number;
    y: number;
    width: number;
    height: number;
    rectX?: string;
    rectY?: string;
    rectWidth?: string;
    rectHeight?: string;
    style?: string;
    svg: SVGElement;
    setHelp: (s: string | null) => void;
  }

  const {symbol, link, drag, help, x, y, width, height,
        rectX, rectY, rectWidth, rectHeight, style, svg, setHelp}: Props = $props();

  let x2: number | null = $state(null);
  let y2: number | null = $state(null);

  let isDragging = false;

  const onpointerdown = () => {
    if (!drag) {
        return;
    }
    isDragging = true;
    svg.addEventListener("pointermove", onpointermove);
    svg.addEventListener("pointerup", onpointerup);
    svg.addEventListener("pointerleave", onpointerup);
  };

  const onpointermove = (e: PointerEvent) => {
    if (!isDragging) return;
    const rect = svg.getBoundingClientRect();
    x2 = 100 * (e.clientX - rect.left) / rect.width - width / 16.5;
    y2 = 100 * (e.clientY - rect.top) / rect.height - height / 13.8;
  };

  const onpointerup = () => {
    isDragging = false;
    svg.removeEventListener("pointermove", onpointermove);
    svg.removeEventListener("pointerup", onpointerup);
    svg.removeEventListener("pointerleave", onpointerup);
  };

</script>

<g style:transform="translate({x2 ?? x / 8.5}%, {y2 ?? y / 6.9}%)">
  <g {style}>
    <!-- svelte-ignore a11y_no_static_element_interactions -->
    <svg
      class={["object", {draggable: drag}]}
      onpointerenter={() => setHelp(help)}
      onpointerleave={() => setHelp(null)}
      {width} {height} {onpointerdown}
    >
      <use href="#{symbol}" class="symbol" />
      {#if link}
        <a href={resolve(`/${link}` as any)} aria-label={link}>
          <rect
            x={rectX ?? "0"}
            y={rectY ?? "0"}
            width={rectWidth ?? "100%"}
            height={rectHeight ?? "100%"}
            class="link"
          />
        </a>
      {:else}
        <rect
          x={rectX ?? "0"}
          y={rectY ?? "0"}
          width={rectWidth ?? "100%"}
          height={rectHeight ?? "100%"}
          class="link"
        />
      {/if}
    </svg>
  </g>
</g>

<style>
  .object {
    pointer-events: inherit;
    &.draggable {
        cursor: grab;
    }
  }

  .symbol {
    pointer-events: none;
  }

  .link {
    cursor: pointer;
    fill: transparent;
  }
</style>