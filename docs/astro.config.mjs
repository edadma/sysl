// @ts-check
import { defineConfig } from 'astro/config';
import starlight from '@astrojs/starlight';

// https://astro.build/config
export default defineConfig({
	integrations: [
		starlight({
			title: 'SysL Documentation',
			sidebar: [
				{
					label: 'Getting Started',
					items: [
						{ label: 'Overview', slug: 'guides/overview' },
						{ label: 'Repository Map', slug: 'guides/repository-map' },
					],
				},
				{
					label: 'Language Reference',
					autogenerate: { directory: 'reference' },
				},
				{
					label: 'Implementation',
					autogenerate: { directory: 'implementation' },
				},
			],
		}),
	],
});
