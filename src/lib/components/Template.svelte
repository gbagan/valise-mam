<script lang="ts" generics="Position, Move">
  import { type ICoreModel, Dialog, isScoreModel, isSizeModel } from '$lib/model/types';
  import CDialog from '$lib/components/Dialog.svelte';
  import IncDecGrid from '$lib/components/IncDecGrid.svelte';
  import { confetti } from '$lib/confetti';
  import type { Snippet } from 'svelte';

  interface Props {
    model: ICoreModel<Position, Move>;
    board: Snippet;
    config: Snippet;
    rules: Snippet;
    bestScore?: Snippet<[Position]>;
    custom?: Snippet;
    winTitle?: string;
  }

  const { board, config, rules, bestScore, custom, winTitle,
          model=$bindable()}: Props = $props();

  $inspect(model.dialog);
</script>

{#snippet winPanel(title: string, visible: boolean)}
  {#if visible}
    <div class="win-container">
      <div class="confetti-container">
        <div use:confetti={{stageHeight: "100vh", stageWidth: "100vw"}}></div>
      </div>
      <div class="win">{title}</div>
    </div>
  {/if}
{/snippet}

<div class="main-container">
  {#if isSizeModel(model) && model.sizeLimit}
    <IncDecGrid
      rows={model.rows}
      columns={model.columns}
      showRowButtons={model.customSize && model.sizeLimit.minRows < model.sizeLimit.maxRows}
      showColButtons={model.customSize && model.sizeLimit.minCols < model.sizeLimit.maxCols}
      locked={model.locked}
      resize={model.resize}
    >
      {@render board()}
    </IncDecGrid>
  {:else}
    {@render board()}
  {/if}
  {@render config()}
  {@render winPanel(winTitle || "GAGNÉ", model.isVictoryShown)}
  {#if model.dialog === Dialog.Rules}
    <CDialog title="Règles du jeu" onOk={model.closeDialog}>
      <div class="rules">
        {@render rules()}
      </div>
    </CDialog>
  {:else if model.dialog === Dialog.Customize}
    {@render custom?.()}
  {:else if model.dialog === Dialog.Score && isScoreModel(model)}
    <CDialog title="Meilleur score" onOk={model.closeDialog}>
      {@const position = model.bestPosition()! }
      {@render bestScore?.(position)}
    </CDialog>
  {:else if model.dialog === Dialog.NewGame}
    <CDialog
      title="Nouvelle Partie"
      onOk={() => model.newGame()}
      onCancel={model.closeDialog}
    >
      Tu es sur le point de créer une nouvelle partie. Ta partie en cours sera perdue. Es-tu sûr(e)?
    </CDialog>
  {/if}
</div>

<style>
  .main-container {
    position: fixed;
    display: flex;
    width: 100%;
    height: 100%;
    justify-content: space-around;
    align-items: center;
    background: linear-gradient(to right, #cfd9df, #e2ebf0);
  }

  @media screen and (orientation: portrait) {
    .main-container {
      flex-direction: column;
    }
  }

  .win-container {
    position: absolute;
    display: flex;
    align-items: center;
    justify-content: center; 
    pointer-events: none;
    z-index: 500;
  }

  .win {
    background: white;
    white-space: nowrap;
    border-radius: 1rem;
    border: thin solid red;
    font-size: 4rem;
    padding: 2rem;
    color: red;
    display: block;
    opacity: 0;
    animation: win-animation 2.5s ease-in-out forwards;
  }

  .confetti-container {
    position: fixed;
    top: 15vh;
    left: 0;
    width: 100%;
    height: 100%;
    pointer-events: none;
    display: flex;
    justify-content: center;
    z-index: 400;
  }

  .rules {
    width: 46rem;
    font-family: 'Shantell Sans';
    font-size: 1.25rem;
    line-height: 1.75rem;
  
    & :global(strong) {
      color: blue;
      font-weight: bold;
    }
  }


  @keyframes win-animation {
    0% {
      opacity: 0;
    }
    25% {
      opacity: 1;
    }
    75% {
      opacity: 1;
    }
    100% {
      opacity: 0;
    }
  }

</style>