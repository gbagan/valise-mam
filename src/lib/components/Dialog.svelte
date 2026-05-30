<script lang="ts">
  import type { Snippet } from "svelte";

  interface Props {
    title: string;
    onOk?: () => void;
    onCancel?: () => void;
    children: Snippet;
  }
  
  let { title, onOk, onCancel, children }: Props = $props();
</script>

<div class="container">
  <div class="dialog">
    <div class="head">
      <div class="title">{title}</div>
    </div>
    <div class="body">
      {@render children()}
    </div>
    <div class="buttons">
      {#if onCancel}
        <button class="ui-button" onclick={onCancel}>Annuler</button>
      {/if}
      {#if onOk}
        <button class="ui-button" onclick={onOk}>Ok</button>
      {/if}
    </div>
  </div>
</div>

<style>
.container {
    position: absolute;
    left: 0;
    top: 0;
    width: 100%;
    height: 100%;
    display: flex;
    align-items: center;
    justify-content: center;
    background-color: rgba(127, 140, 141, 0.7);
    z-index: 1000;
}    

.dialog {
    padding: 0;
    background: white;
    border-radius: 2px;
    border: 1px solid #e8e8e8;
    display: block;
}

.head {
    border-bottom: 1px solid #e8e8e8;
    padding: 1rem;
    border-radius: 2px 2px 0 0;
    margin-bottom: -1px;
    min-height: 2rem;
}

.title {
    font-size: 2rem;
    color: rgba(0, 0, 0, 0.85);
    font-weight: 500;
    display: inline-block;
    flex: 1;
}

.body {
    padding: 1.5em;
    margin-bottom: -1px;
    border-bottom: 1px solid #e8e8e8;
}

.buttons {
    padding: 1em;
    text-align: right;

    & > button + button {
        margin-left: 0.5em;
    }
}
</style>