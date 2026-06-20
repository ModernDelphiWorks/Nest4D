import type { SidebarsConfig } from '@docusaurus/plugin-content-docs';

const sidebars: SidebarsConfig = {
  docsSidebar: [
    {
      type: 'doc',
      id: 'intro',
      label: 'Documentation portal',
    },
    {
      type: 'category',
      label: 'Projects',
      items: [{ type: 'link', label: 'Nidus', href: '/nidus/' }],
    },
  ],
  nidusSidebar: [
    {
      type: 'category',
      label: 'Nidus',
      link: { type: 'doc', id: 'nidus/index' },
      items: [
        'nidus/introduction',
        {
          type: 'category',
          label: 'Getting Started',
          items: [
            'nidus/getting-started/installation',
            'nidus/getting-started/quickstart',
          ],
        },
        {
          type: 'category',
          label: 'Guides',
          items: [
            'nidus/guides/modular-architecture',
            'nidus/guides/dependency-injection',
            'nidus/guides/validation-pipes',
            'nidus/guides/security-guards',
            'nidus/guides/object-pooling',
            'nidus/guides/module-caching',
            'nidus/guides/rpc-microservices',
            'nidus/guides/message-bus',
            'nidus/guides/horse-integration',
          ],
        },
        {
          type: 'category',
          label: 'Reference',
          items: [
            'nidus/reference/api',
            'nidus/reference/decorators',
          ],
        },
        'nidus/troubleshooting/common-errors',
      ],
    },
  ],
};

export default sidebars;
