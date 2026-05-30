<script lang="ts" generics="Position,Move">
  import type { ICoreModel, ISizeModel } from '$lib/model/types';
  import Icon from "./Icon.svelte";
  import IconGroup from "./IconGroup.svelte";

  interface Props {
    model: ICoreModel<Position, Move> & ISizeModel;
    values: readonly (readonly [number, number])[];
    customSize?: boolean;
  }
  
  const {model=$bindable(), values, customSize }: Props = $props();
</script>

<IconGroup title="Dimensions de la grille">
  {#each values as [rows, cols]}
    <Icon
      text="{rows}x{cols}"
      selected={!model.customSize && rows === model.rows && cols === model.columns}
      onclick={() => model.resize(rows, cols, false)}
    />
  {/each}
  {#if customSize}
    <Icon
      text="NxM"
      tooltip="Taille personalisée"
      selected={model.customSize}
      onclick={() => model.resize(model.rows, model.columns, true)}
    />
  {/if}
</IconGroup>