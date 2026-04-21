// @ts-check
import { defineConfig } from 'astro/config';
import starlight from '@astrojs/starlight';
import syslGrammar from './src/grammars/sysl.tmLanguage.json';

// https://astro.build/config
export default defineConfig({
	integrations: [
		starlight({
			title: 'SysL Documentation',
			expressiveCode: {
				shiki: {
					langs: [
						{
							...syslGrammar,
							aliases: ['lsysl'],
						},
					],
				},
			},
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
