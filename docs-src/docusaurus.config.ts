import type { Config } from '@docusaurus/types';
import type * as Preset from '@docusaurus/preset-classic';

const config: Config = {
  title: 'Nidus',
  tagline: 'Modular application & microservice framework for Delphi',
  favicon: 'img/favicon.svg',

  // Build output directory is set via CLI (`npm run build` → `docusaurus build --out-dir ../docs`)
  // so it stays compatible with Docusaurus 3 schema validation (top-level `outDir` is not allowed).

  url: 'https://moderndelphiworks.github.io',
  baseUrl: '/Nidus/',
  organizationName: 'ModernDelphiWorks',
  projectName: 'Nidus',

  onBrokenLinks: 'throw',
  onBrokenMarkdownLinks: 'warn',

  i18n: {
    defaultLocale: 'en',
    locales: ['en'],
  },

  presets: [
    [
      'classic',
      {
        docs: {
          sidebarPath: './sidebars.ts',
          routeBasePath: '/',
          editUrl: undefined,
        },
        blog: false,
        theme: {
          customCss: './src/css/custom.css',
        },
      } satisfies Preset.Options,
    ],
  ],

  themeConfig: {
    navbar: {
      title: 'Nidus',
      items: [
        {
          type: 'docSidebar',
          sidebarId: 'docsSidebar',
          position: 'left',
          label: 'Home',
        },
        {
          type: 'dropdown',
          label: 'Projects',
          position: 'left',
          items: [{ to: '/nidus/', label: 'Nidus' }],
        },
      ],
    },
    footer: {
      style: 'dark',
      copyright: `© ${new Date().getFullYear()} Nidus — ModernDelphiWorks.`,
    },
    prism: {
      additionalLanguages: ['bash', 'json', 'powershell'],
    },
  } satisfies Preset.ThemeConfig,
};

export default config;
