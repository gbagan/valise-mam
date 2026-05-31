<script lang="ts">
  interface Props {
    text?: string;
    selected?: boolean;
    tooltip?: string | null;
    round?: boolean;
    large?: boolean;
    hidden?: boolean;
    disabled?: boolean;
    style?: string;
    onclick?: ((e: MouseEvent) => void) | null;
    onpointerdown?: ((e: PointerEvent) => void) | null;
    onpointerup?: ((e: PointerEvent) => void) | null;
    onpointerleave?: ((e: PointerEvent) => void) | null;
  }
  
  const { text, selected, tooltip, round, large, hidden, disabled, style,
    onclick, onpointerdown, onpointerup, onpointerleave }: Props = $props();
</script>

<button
  class={["icon", {selected, round, large, hidden}]}
  {disabled}
  {onclick} {onpointerdown} {onpointerup} {onpointerleave}
  oncontextmenu={e => e.preventDefault()}
>
  {#if text && text.startsWith("#")}
    <svg class="icon-symbol" {style}>
      <use href={text} />
    </svg>
  {:else if text}
    <span class="icon-text">{text}</span>
  {/if}

  {#if tooltip}
    <span class="tooltip">{tooltip}</span>
  {/if}
</button>

<style>
  svg {
    fill: var(--main-color);
  }

  .icon {
    line-height: 1.15;
    color: var(--main-color);

    position: relative;
    width: 3.6rem;
    height: 3.6rem;
    border: thin solid var(--gray-500);
    display: inline-flex;
    align-items : center;
    justify-content: center;
    cursor: pointer;
    border-radius: 0.7rem;
    opacity: 1;
    transition: opacity 0.3s ease;
    padding: 0.3rem;
    background-color: transparent;
    outline: 0;

    &.round {
      display: flex;
      border-radius: 50%;
    }

    &.selected {
      border: 4px solid var(--blue-600);
    }

    &:disabled {
      cursor: not-allowed;
      .icon-text,.icon-symbol {
        opacity: 0.5;
      }
    }

    &.hidden {
      pointer-events: none;
      opacity: 0;
    }
  }

  .icon-text {
    text-align: center;
    font-size: 1.5rem;
    font-weight: bold;
  }

  .tooltip {
    font-weight: normal;
    font-size: 0.9rem;
    font-family: var(--handwritten);
    background-color: var(--tooltip-bg);
    color: #fff;
    text-align: center;
    padding: 0.7rem 0.35rem;
    border-radius: 0.4rem;
    width: 9rem;
    position: absolute;
    z-index: 1;
    bottom: 125%;
    left: 50%;
    margin-left: -4.5rem;
    pointer-events: none;
    opacity: 0;
    transition: opacity 0.3s ease-in 0.1s;

    &::after {
      content: "";
      position: absolute;
      top: 100%;
      left: 50%;
      margin-left: -0.35rem;
      border-width: 0.35rem;
      border-style: solid;
      border-color: var(--tooltip-bg) transparent transparent transparent;
    }
  }

  .icon:hover .tooltip {
    opacity: 0.8;
  }
</style>