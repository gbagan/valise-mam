<script lang="ts">
  import { onMount } from 'svelte';
  import ValiseObject from '$lib/components/ValiseObject.svelte';

  let isOpen = $state(false);
  let help: string | null = $state(null);
  let helpVisible = $state(false);

  function setHelp(s: string | null) {
    if (s === null) {
      helpVisible = false;
    } else {
      help = s;
      helpVisible = true;
    }
  }

  let svg: SVGElement = $state()!;

  onMount(() => setTimeout(() => isOpen = true, 1500));
</script>

{#snippet valise()}
  <svg bind:this={svg} viewBox="0 0 825 690">
    <use href="#valise" class="valise-close" /> 
    <g class="valise-open">
      <use href="#openvalise"/>
      <ValiseObject {svg} {setHelp}
        symbol="tile"
        link="tiling"
        help="Jeu: carrelage"
        x={280} y={400} width={120} height={60}
      />
      <ValiseObject {svg} {setHelp}
        symbol="chocolate"
        link="chocolat"
        help="Jeu: chocolat"
        x={200} y={200} width={60} height={60}
        style="transform:rotate(40deg);"  
      />
      <ValiseObject {svg} {setHelp}
        symbol="pen"
        link="dessin"
        help="Jeu: dessin"
        x={610} y={400} width={60} height={60}
      />
      <ValiseObject {svg} {setHelp}
        symbol="knight"
        link="queens"
        help="Jeu: les 8 reines"
        x={461} y={380} width={24} height={48}
        style="transform:rotate(40deg);"  
      />
      <ValiseObject {svg} {setHelp}
        symbol="paw"
        link="labete"
        help="Jeu: la bête"
        x={300} y={180} width={40} height={40}
        style="transform:rotate(30deg);opacity:0.5;"
      />
      <ValiseObject {svg} {setHelp}
        symbol="stack"
        link="jetons"
        help="Jeu: acquisition"
        x={350} y={500} width={50} height={50}
      />
      <ValiseObject {svg} {setHelp}
        symbol="block2"
        link="nim"
        help="Jeu: bloque moi si tu peux"
        x={380} y={120} width={40} height={40}
      />
      <ValiseObject {svg} {setHelp}
        symbol="tricolor2"
        link="baseball"
        help="Jeu: baseball multicolore"
        x={350} y={330} width={90} height={60}
        
      />
      <ValiseObject {svg} {setHelp}
        symbol="eternal-attack"
        link="eternal"
        help="Jeu: domination éternelle"
        x={260} y={125} width={40} height={40}
      />
      <ValiseObject {svg} {setHelp}
        symbol="traffic"
        link="tricolor"
        help="Jeu: feu tricolore"
        x={250} y={280} width={64} height={64}
      />
      <ValiseObject {svg} {setHelp}
        symbol="frog2"
        link="frog"
        help="Jeu: la grenouille"
        x={549} y={320} width={40} height={40}
        style="fill:#bcd35f;"
      />
      <ValiseObject {svg} {setHelp}
        symbol="flowerpot"
        help="Quelque chose se cache derrière ce pot"
        drag={true}
        x={533} y={300} width={64} height={64}
      />
      <ValiseObject {svg} {setHelp}
        symbol="bulbon"
        link="noirblanc"
        help="Jeu: tout noir, tout blanc"
        x={477} y={280} width={48} height={48}
      />
      <ValiseObject {svg} {setHelp}
        symbol="race"
        link="paths"
        help="Jeu: chemins"
        x={450} y={445} width={64} height={64}
        style="transform:rotate(40deg);"
      />
      <ValiseObject {svg} {setHelp}
        symbol="quiet"
        link="sansmot"
        help="Preuves sans mot"
        x={180} y={130} width={50} height={50}
      />
      <ValiseObject {svg} {setHelp}
        symbol="soli-peg2"
        link="solitaire"
        help="Jeu: solitaire"
        x={492} y={350} width={40} height={40}
      />
      <ValiseObject {svg} {setHelp}
        symbol="hanoibot"
        link="hanoi"
        help="Jeu: tours de Hanoi"
        x={500} y={430} width={75} height={51}
        rectX="30%" rectY="20%" rectWidth="40%" rectHeight="40%"
      />
      <ValiseObject {svg} {setHelp}
        symbol="hanoitop"
        help="Quelque chose se cache sous cette tour"
        drag={true}
        x={507} y={409} width={60} height={57}
      />
      <ValiseObject {svg} {setHelp}
        symbol="wheel"
        link="roue"
        help="Jeu: roue des couleurs"
        x={400} y={205} width={50} height={50}
        style="transform:scale(1,0.8);"
      />
    </g>
  </svg>
{/snippet}

<div class={["main-container", {open: isOpen}]}>
  <div class="logo">
    <svg style:width="100%" style:height="100%">
      <use href="#logo" />
    </svg>
  </div>
  <div class="container">
    {@render valise()}
    <div class={["help", {visible: helpVisible}]}>
      {help}
    </div>
  </div>
</div>

<style>
  .main-container {
    height: 100vh;
    width: 100vw;
  }

  .container {
    height: calc(69vmin * 1.1);
    width: calc(82.5vmin * 1.1);
    position: relative;
  }

  .valise-open {
    opacity: 0;
  }

  .valise-close {
    width: 100%;
    height: 100%;
    opacity: 1;
  }

  .logo {
    height: calc(18vmin * 1.1);
    width: calc(82.5vmin * 1.1);
    opacity: 0;
  }

  .main-container {
    display: flex;
    align-items: center;
    justify-content: center;
    flex-direction: column;
  }

  .main-container.open  {
    & .valise-open, & .logo {
      opacity: 1;
      transition: opacity 0.5s ease-in 0.5s;
    }

    .valise-close {
      opacity: 0;
      transition: opacity 0.5s ease-out;
    }
  }

  .help {
    position: absolute;
    left: 67%;
    top: 18%;
    font-size: 1.5em;
    display: inline;
    border: thick solid blue;
    padding: 0.5em;
    border-radius: 0.5em;
    opacity: 0;
    transition: opacity 0.5s ease;
    pointer-events: none;

    &.visible {
      opacity: 1;
    }
  }
</style>