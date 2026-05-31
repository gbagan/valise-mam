<script lang="ts">
  import { range, sleep } from "@gbagan/utils";
  import { Page } from "./types";

  type AnimationStep = [number, string, number];

  let anim: Record<string, number> = $state({});
  let locked = $state(false);
  let page: Page = $state(Page.Pythagore);

  async function runAnim(animation: AnimationStep[]) {
    if (locked) {
      return;
    }
    locked = true;
    anim = {};
    for (const [d, key, step] of animation) {
        await sleep(d);
        anim[key] = step;
    }
    locked = false;
  }

  function setPage(p: Page) {
    if (!locked) {
      page = p;
    }
  }

  // besoin d'un transform par défault pour empécher un bug sous safari
  const defaultStyle = "transform:translate(0px, 0px);";

  const compStyle = (tx: number, ty: number, _rotation: number, duration: number) =>
    `transform: translate(${tx}px, ${ty}px);`
    + `transition: transform linear ${duration}ms;`;
  

  const pythagoreSteps: AnimationStep[] = [
    [500, "a", 1],
    [200, "a", 2],
    [600, "b", 1],
    [200, "b", 2],
    [600, "c", 1],
    [200, "c", 2],
    [600, "d", 1],
    [200, "d", 2],
    [600, "e", 1],
  ];

  const pythagoreStyles: Record<string, string[]> = {
    a: [ "opacity: 0;", defaultStyle, compStyle(400, -100, 0, 600) ],
    b: [ "opacity: 0;", defaultStyle, compStyle(600, 0, 0, 600) ],
    c: [ "opacity: 0;", defaultStyle, compStyle(400, 0, 0, 600) ],
    d: [ "opacity: 0;", defaultStyle, compStyle(300, 200, 0, 600) ],
    e: [ "opacity: 0;", ""]
  }

  const carollSteps: AnimationStep[] = [
    [700, "a", 1],
    [700, "b", 1],
    [700, "c", 1],
    [700, "d", 1],
    [600, "e", 1],
  ];

  const carollStyles: Record<string, string[]> = {
    a: [ defaultStyle, compStyle(300, 150, 0, 600) ],
    b: [ defaultStyle, compStyle(550, 50, 0, 600) ],
    c: [ defaultStyle, compStyle(700, 0, 0, 600) ],
    d: [ defaultStyle, compStyle(950, -100, 0, 600) ],
    e: [ "opacity: 0;", "" ],
  }
</script>

{#snippet pythagoreAnimation()}
  {@const f = (x: string) => pythagoreStyles[x][anim[x] ?? 0]}
  <svg class="pytha-svg2" viewBox="-1 -1 702 302">
    <path
      d="M 0 300 h 300 v -300 h -300 Z L 100 100 M 0 100 h 300 l -200 -100 v 300"
      fill="transparent"
      stroke="#000"
    />
    <path
      d="M 400 300 h 300 v -300 h -300 Z M 400 200 l 200 100 l 100 -200 l -200 -100 l -100 200"
      fill="transparent"
      stroke="#000"
    />
    <path d="M 0 300 v -200 h 100 Z" fill="var(--blue-600)" stroke="#000" style={f("a")} />
    <path d="M 0 300 h 100 v -200 Z" fill="var(--yellow-300)" stroke="#000" style={f("b")} />
    <path d="M 100 0 h 200 v 100 Z" fill="var(--green-500)" stroke="#000" style={f("c")} />
    <path d="M 100 0 v 100 h 200 Z" fill="var(--red-500)" stroke="#000" style={f("d")} />
    <path d="M 0 300 v -200 h 100 Z" fill="var(--blue-600)" stroke="#000" style={f("e")} />
    <path d="M 0 300 h 100 v -200 Z" fill="var(--yellow-300)" stroke="#000" style={f("e")} />
    <path d="M 100 0 h 200 v 100 Z" fill="var(--green-500)" stroke="#000" style={f("e")} />
    <path d="M 100 0 v 100 h 200 0 Z" fill="var(--red-500)" stroke="#000" style={f("e")} />
    <text x="5" y="55" font-size="20" style={f("e")}>a</text>
    <text x="46" y="16" font-size="20" style={f("e")}>a</text>
    <text x="105" y="210" font-size="20" style={f("e")}>b</text>
    <text x="198" y="120" font-size="20" style={f("e")}>b</text>
    <text x="460" y="98" font-size="20" style={f("e")}>c</text>
    <text x="595" y="80" font-size="20" style={f("e")}>c</text>
  </svg>
{/snippet}

{#snippet pythagore()}
  <h2 class="h2">Que raconte le théorème de Pythagore?</h2>
  <p class="center">
    <svg class="pytha-svg" viewBox="0 -100 200 250">
      <path d="M 50 50 h 100 v 100 h -100 Z" fill="var(--yellow-300)" stroke="var(--slate-700)" />
      <path d="M 0 0 h 50 v 50 h -50 Z" fill="var(--yellow-300)" stroke="var(--slate-700)" />
      <path d="M 50 0 l 100 50 l 50 -100 l -100 -50 Z" fill="var(--green-500)" stroke="var(--slate-700)" />
      <text x="90" y="105" font-size="35">a²</text>
      <text x="18" y="35" font-size="35">b²</text>
      <text x="110" y="-10" font-size="35">c²</text>
    </svg>
  </p>
  <h2 class="h2">Preuve sans mot due à un auteur chinois inconnu qui vivait vers 200 avant J.-C.</h2>
  <p class="center">{@render pythagoreAnimation()}</p>
  <button class="anim-btn" onclick={() => runAnim(pythagoreSteps)}>Lancer l'animation</button>
{/snippet}


{#snippet carollAnimation()}
  {@const f = (x: string) => carollStyles[x][anim[x] ?? 0]}
  <svg class="caroll-svg" viewBox="-10 -10 1370 270">
    <path d="M 400 100 h 250 v -100 Z" fill="var(--orange-300)" style={f("a")} />
    <path d="M 400 200 h 150 v -50 h 100 v -50 h -250 Z" fill="var(--red-500)" style={f("b")} />
    <path d="M 400 250 h 250 v -100 h -100 v 50 h -150 Z" fill="var(--blue-600)" style={f("c")} />
    <path d="M 0 250 h 400 v -150 Z" fill="var(--green-500)" style={f("d")} />
    <path d="M 400 100 h 250 v -100 Z" fill="var(--orange-300)" style={f("e")} />
    <path d="M 400 200 h 150 v -50 h 100 v -50 h -250 Z" fill="var(--red-500)" style={f("e")} />
    <path d="M 400 250 h 250 v -100 h -100 v 50 h -150 Z" fill="var(--blue-600)" style={f("e")} />
    <path d="M 0 250 h 400 v -150 Z" fill="var(--green-500)" style={f("e")} />
    {#each range(0, 28) as i}
      <line x1={50 * i} y1={-10} x2={50 * i} y2={260} class="grid" />
    {/each}
    {#each range(0, 6) as i}
      <line x1="-10" y1={50 * i} x2="1360" y2={50 * i} class="grid" />
    {/each}
  </svg>
{/snippet}

{#snippet caroll()}
  <h2 class="h2">Où est passé le carré manquant ?</h2>
  <p class="center">{@render carollAnimation()}</p>
  <button class="anim-btn" onclick={() => runAnim(carollSteps)}>Lancer l'animation</button>
{/snippet}

<div class="main">
  <div class="menu">
    <button class="link" onclick={() => setPage(Page.Pythagore)}>1</button>
    <button class="link" onclick={() => setPage(Page.Caroll)}>2</button>
  </div>

  <div>
    <h1 class="title">Preuves sans mot</h1>
    {#if page === Page.Pythagore}
      {@render pythagore()}
    {:else if page === Page.Caroll}
      {@render caroll()}
    {/if}
  </div>
</div>

<style>
  .main {
    position: relative;
  }

  .menu {
    position: absolute;
    right: 0;
    top: 0;
    font-size: 1.5rem;
  }

  .center {
    text-align: center;
  }

  .link {
    width: 4rem;
    height: 4rem;
    border: solid var(--blue-500) thick;
    text-align: center;
    background-color: var(--blue-100);
  }

  .title {
    text-align: center;
    color: var(--blue-600);
  }

  .h2 {
    color: var(--green-600);
    margin: 1rem 0;
  }

  .pytha-svg {
    display: inline-block;
    width: 20vmin;
  }

  .pytha-svg2 {
    display: inline-block;
    width: 84vmin;
  }

  .caroll-svg {
    display: inline-block;
    width: 90vw;
  }

  .grid {
    stroke: var(--slate-700);
    stroke-width: 1;
    stroke-dasharray: 10;
  }

  .anim-btn {
    font-family: var(--handwritten);
    text-align: center;
    width: 100%;
    display: inline;
    font-size: 1.5rem;
    border: 0;
    background-color: transparent;
    color: var(--emph-color);
    text-decoration: underline;
    cursor: pointer
  }
</style>