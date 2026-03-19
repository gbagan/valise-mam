<script lang="ts">
  import View from './view.svelte';
  import Model from './model.svelte';

  const model = new Model();
</script>

<View {model} />