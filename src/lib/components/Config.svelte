<script lang="ts">
  import type { Snippet } from "svelte";

  interface Props {
    title: string,
    children: Snippet,
  }
  
  let { title, children }: Props = $props();
</script>

<div class="card">
  <div class="head">
      <div class="title">{title}</div>
  </div>
  <div class="body">{@render children()}</div>
</div>

<style>
  .card {
    padding: 0;
    border-radius: 2px;
    box-shadow: var(--centered-shadow);
    background-color: white;
  }

  .head{
    display: flex;
    align-items: center;
    justify-content: center;
    border-bottom: 2px solid #e8e8e8;
    padding: 0.5rem;
  }

  .title {
    font-size: 2rem;
    text-overflow: ellipsis;
    overflow: hidden;
    white-space: nowrap;
    color: rgba(0, 0, 0, 0.85);
    font-weight: 500;
    flex: 1;
  }

  .body {
    padding: 0.5rem;
  }

  @media (orientation: portrait) {
    .body {
      display: grid;
      grid-template-columns: repeat(2, 1fr);
      grid-gap: 0rem 4rem;
    }
  }
</style>