// @ts-check
import { defineConfig } from 'astro/config';
import starlight from '@astrojs/starlight';
import syslGrammar from './src/grammars/sysl.tmLanguage.json';

// https://astro.build/config
export default defineConfig({
	site: 'https://sysl.sh',
	integrations: [
		starlight({
			title: 'Sysl',
			description:
				'A systems programming language with value types, ref-counted refs, and raw pointers — from kernels to compilers to the metal underneath them.',
			logo: {
				src: './public/favicon.svg',
				alt: 'Sysl',
			},
			favicon: '/favicon.svg',
			social: [
				{
					icon: 'github',
					label: 'GitHub',
					href: 'https://github.com/edadma/trisc',
				},
			],
			editLink: {
				baseUrl: 'https://github.com/edadma/trisc/edit/dev/sysl/docs/',
			},
			lastUpdated: true,
			expressiveCode: {
				themes: ['starlight-dark', 'starlight-light'],
				shiki: {
					langs: [
						{
							...syslGrammar,
							aliases: ['lsysl'],
						},
					],
				},
				styleOverrides: {
					borderRadius: '0.5rem',
					codeFontFamily:
						"ui-monospace, SFMono-Regular, 'SF Mono', Menlo, Consolas, 'Liberation Mono', monospace",
				},
			},
			customCss: ['./src/styles/sysl.css'],
			components: {},
			sidebar: [
				{
					label: 'Get started',
					items: [
						{ label: 'Introduction', slug: 'guides/introduction' },
						{ label: 'Installation', slug: 'guides/installation' },
						{ label: 'Hello, Sysl', slug: 'guides/hello-sysl' },
						{ label: 'Tour of the language', slug: 'guides/tour' },
						{ label: 'Systems programming', slug: 'guides/systems-programming' },
					],
				},
				{
					label: 'Language reference',
					items: [
						{ label: 'Reference overview', slug: 'reference/scope' },
						{ label: 'Types', slug: 'reference/types' },
						{ label: 'Expressions', slug: 'reference/expressions' },
						{
							label: 'Statements and control flow',
							slug: 'reference/statements-and-control-flow',
						},
						{ label: 'Functions', slug: 'reference/functions' },
						{
							label: 'Arrays, slices, and pointers',
							slug: 'reference/arrays-slices-pointers',
						},
						{ label: 'Strings', slug: 'reference/strings' },
						{ label: 'Generics', slug: 'reference/generics' },
						{
							label: 'Traits and operators',
							slug: 'reference/traits-and-operators',
						},
						{ label: 'Modules and imports', slug: 'reference/modules-and-imports' },
						{ label: 'Builtins and runtime', slug: 'reference/builtins-and-runtime' },
						{
							label: 'Attributes and testing',
							slug: 'reference/attributes-and-testing',
						},
						{ label: 'Literate Sysl', slug: 'reference/literate-sysl' },
					],
				},
				{
					label: 'Standard library',
					items: [
						{ label: 'Overview', slug: 'stdlib/overview' },
						{ label: 'std.strings', slug: 'stdlib/strings' },
						{ label: 'std.regex', slug: 'stdlib/regex' },
						{ label: 'std.crypto', slug: 'stdlib/crypto' },
						{ label: 'std.io and std.bufio', slug: 'stdlib/io' },
						{
							label: 'Containers, sorting, slices',
							slug: 'stdlib/containers',
						},
					],
				},
				{
					label: 'Compiler internals',
					items: [
						{ label: 'Overview', slug: 'implementation/overview' },
						{ label: 'Pipeline', slug: 'implementation/pipeline' },
						{
							label: 'Analyzer architecture',
							slug: 'implementation/analyzer-architecture',
						},
						{ label: 'Source map', slug: 'implementation/source-map' },
						{ label: 'Testing strategy', slug: 'implementation/testing' },
					],
				},
			],
		}),
	],
});
