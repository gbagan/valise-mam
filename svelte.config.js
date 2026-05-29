import adapter from '@sveltejs/adapter-static';
import { vitePreprocess } from '@sveltejs/vite-plugin-svelte';


/** @type {import('@sveltejs/kit').Config} */
const config = {
	// Consult https://svelte.dev/docs/kit/integrations
	// for more information about preprocessors
	base: "./",
	preprocess: vitePreprocess(),
	kit: {
		adapter: adapter({
			pages: 'build',
			assets: 'build',
		}),
		prerender: {
  			entries: ['*']
		},
		output: {
			bundleStrategy: 'single'
		},
		paths: {
      		base: process.env.BASE_PATH  ?? "",
		},
	}
};

export default config;