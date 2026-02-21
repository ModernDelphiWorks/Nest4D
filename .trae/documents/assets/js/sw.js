// Service Worker para o site do Nest4D
// Otimização de performance e cache offline

const CACHE_NAME = 'nest4d-site-v1.0.0';
const STATIC_CACHE = 'nest4d-static-v1.0.0';
const DYNAMIC_CACHE = 'nest4d-dynamic-v1.0.0';
const IMAGE_CACHE = 'nest4d-images-v1.0.0';

// Arquivos para cache estático (apenas arquivos que existem)
const STATIC_FILES = [
  '/',
  '/index.html',
  '/assets/css/main.css',
  '/assets/js/main.js',
  '/assets/images/nest4d_logo.png',
  '/assets/images/nest4d_fluxo.png',
  'https://cdnjs.cloudflare.com/ajax/libs/prism/1.29.0/themes/prism-tomorrow.min.css'
];

// URLs que devem ser sempre buscadas da rede
const NETWORK_FIRST = [
  '/api/',
  '/docs/',
  '/examples/'
];

// URLs para cache de imagens
const IMAGE_EXTENSIONS = ['.jpg', '.jpeg', '.png', '.gif', '.webp', '.svg', '.ico'];

// Instalação do Service Worker
self.addEventListener('install', event => {
  console.log('🔧 Service Worker: Instalando...');
  
  event.waitUntil(
    Promise.all([
      // Cache estático
      caches.open(STATIC_CACHE).then(cache => {
        console.log('📦 Service Worker: Cacheando arquivos estáticos');
        return cache.addAll(STATIC_FILES);
      }),
      
      // Pular waiting para ativar imediatamente
      self.skipWaiting()
    ])
  );
});

// Ativação do Service Worker
self.addEventListener('activate', event => {
  console.log('✅ Service Worker: Ativando...');
  
  event.waitUntil(
    Promise.all([
      // Limpar caches antigos
      caches.keys().then(cacheNames => {
        return Promise.all(
          cacheNames.map(cacheName => {
            if (cacheName !== STATIC_CACHE && 
                cacheName !== DYNAMIC_CACHE && 
                cacheName !== IMAGE_CACHE) {
              console.log('🗑️ Service Worker: Removendo cache antigo:', cacheName);
              return caches.delete(cacheName);
            }
          })
        );
      }),
      
      // Tomar controle de todas as abas
      self.clients.claim()
    ])
  );
});

// Interceptação de requisições
self.addEventListener('fetch', event => {
  const { request } = event;
  const url = new URL(request.url);
  
  // Ignorar requisições não-HTTP
  if (!request.url.startsWith('http')) {
    return;
  }
  
  // Estratégia baseada no tipo de recurso
  if (isStaticFile(request.url)) {
    // Cache First para arquivos estáticos
    event.respondWith(cacheFirst(request, STATIC_CACHE));
  } else if (isImageFile(request.url)) {
    // Cache First para imagens
    event.respondWith(cacheFirst(request, IMAGE_CACHE));
  } else if (isNetworkFirst(request.url)) {
    // Network First para APIs e conteúdo dinâmico
    event.respondWith(networkFirst(request, DYNAMIC_CACHE));
  } else {
    // Stale While Revalidate para outros recursos
    event.respondWith(staleWhileRevalidate(request, DYNAMIC_CACHE));
  }
});

// Estratégia Cache First
async function cacheFirst(request, cacheName) {
  try {
    const cache = await caches.open(cacheName);
    const cachedResponse = await cache.match(request);
    
    if (cachedResponse) {
      console.log('📦 Cache Hit:', request.url);
      return cachedResponse;
    }
    
    console.log('🌐 Cache Miss, buscando da rede:', request.url);
    const networkResponse = await fetch(request);
    
    if (networkResponse.ok) {
      cache.put(request, networkResponse.clone());
    }
    
    return networkResponse;
  } catch (error) {
    console.error('❌ Erro em cacheFirst:', error);
    return new Response('Recurso não disponível offline', {
      status: 503,
      statusText: 'Service Unavailable'
    });
  }
}

// Estratégia Network First
async function networkFirst(request, cacheName) {
  try {
    console.log('🌐 Network First:', request.url);
    const networkResponse = await fetch(request);
    
    if (networkResponse.ok) {
      const cache = await caches.open(cacheName);
      cache.put(request, networkResponse.clone());
    }
    
    return networkResponse;
  } catch (error) {
    console.log('📦 Network falhou, tentando cache:', request.url);
    const cache = await caches.open(cacheName);
    const cachedResponse = await cache.match(request);
    
    if (cachedResponse) {
      return cachedResponse;
    }
    
    console.error('❌ Erro em networkFirst:', error);
    return new Response('Conteúdo não disponível offline', {
      status: 503,
      statusText: 'Service Unavailable'
    });
  }
}

// Estratégia Stale While Revalidate
async function staleWhileRevalidate(request, cacheName) {
  try {
    const cache = await caches.open(cacheName);
    const cachedResponse = await cache.match(request);
    
    // Buscar da rede em background
    const networkPromise = fetch(request).then(networkResponse => {
      if (networkResponse.ok) {
        cache.put(request, networkResponse.clone());
      }
      return networkResponse;
    }).catch(error => {
      console.warn('⚠️ Falha na revalidação:', request.url, error);
    });
    
    // Retornar cache imediatamente se disponível
    if (cachedResponse) {
      console.log('📦 Stale cache:', request.url);
      return cachedResponse;
    }
    
    // Aguardar rede se não há cache
    console.log('🌐 Aguardando rede:', request.url);
    return await networkPromise;
  } catch (error) {
    console.error('❌ Erro em staleWhileRevalidate:', error);
    return new Response('Recurso não disponível', {
      status: 503,
      statusText: 'Service Unavailable'
    });
  }
}

// Verificar se é arquivo estático
function isStaticFile(url) {
  return STATIC_FILES.some(file => url.includes(file)) ||
         url.includes('.css') ||
         url.includes('.js') ||
         url.includes('.woff') ||
         url.includes('.woff2') ||
         url.includes('.ttf');
}

// Verificar se é arquivo de imagem
function isImageFile(url) {
  return IMAGE_EXTENSIONS.some(ext => url.includes(ext));
}

// Verificar se deve usar Network First
function isNetworkFirst(url) {
  return NETWORK_FIRST.some(path => url.includes(path));
}

// Limpeza periódica de cache
self.addEventListener('message', event => {
  if (event.data && event.data.type === 'CLEAN_CACHE') {
    cleanOldCache();
  }
  
  if (event.data && event.data.type === 'SKIP_WAITING') {
    self.skipWaiting();
  }
});

// Função para limpar cache antigo
async function cleanOldCache() {
  try {
    const cacheNames = await caches.keys();
    const oldCaches = cacheNames.filter(name => 
      name !== STATIC_CACHE && 
      name !== DYNAMIC_CACHE && 
      name !== IMAGE_CACHE
    );
    
    await Promise.all(
      oldCaches.map(cacheName => {
        console.log('🗑️ Limpando cache antigo:', cacheName);
        return caches.delete(cacheName);
      })
    );
    
    console.log('✅ Limpeza de cache concluída');
  } catch (error) {
    console.error('❌ Erro na limpeza de cache:', error);
  }
}

// Background Sync para requisições offline
self.addEventListener('sync', event => {
  if (event.tag === 'background-sync') {
    console.log('🔄 Background Sync executado');
    event.waitUntil(doBackgroundSync());
  }
});

async function doBackgroundSync() {
  try {
    // Implementar sincronização de dados offline
    console.log('🔄 Sincronizando dados offline...');
    
    // Exemplo: enviar dados armazenados localmente
    const offlineData = await getOfflineData();
    if (offlineData.length > 0) {
      await syncOfflineData(offlineData);
    }
  } catch (error) {
    console.error('❌ Erro no background sync:', error);
  }
}

async function getOfflineData() {
  // Implementar recuperação de dados offline
  return [];
}

async function syncOfflineData(data) {
  // Implementar sincronização de dados
  console.log('📤 Sincronizando:', data.length, 'itens');
}

// Push Notifications
self.addEventListener('push', event => {
  if (!event.data) {
    return;
  }
  
  const data = event.data.json();
  const options = {
    body: data.body || 'Nova atualização disponível!',
    icon: '/assets/images/nest4d_logo.png',
    badge: '/assets/images/nest4d_logo.png',
    vibrate: [200, 100, 200],
    data: {
      url: data.url || '/',
      timestamp: Date.now()
    },
    actions: [
      {
        action: 'open',
        title: 'Abrir',
        icon: '/assets/images/nest4d_logo.png'
      },
      {
        action: 'close',
        title: 'Fechar'
      }
    ]
  };
  
  event.waitUntil(
    self.registration.showNotification(data.title || 'Nest4D', options)
  );
});

// Clique em notificação
self.addEventListener('notificationclick', event => {
  event.notification.close();
  
  if (event.action === 'open' || !event.action) {
    const url = event.notification.data.url || '/';
    
    event.waitUntil(
      clients.matchAll({ type: 'window' }).then(clientList => {
        // Verificar se já existe uma aba aberta
        for (const client of clientList) {
          if (client.url === url && 'focus' in client) {
            return client.focus();
          }
        }
        
        // Abrir nova aba
        if (clients.openWindow) {
          return clients.openWindow(url);
        }
      })
    );
  }
});

// Monitoramento de performance
self.addEventListener('fetch', event => {
  const startTime = performance.now();
  
  event.respondWith(
    handleRequest(event.request).then(response => {
      const endTime = performance.now();
      const duration = endTime - startTime;
      
      // Log performance para requisições lentas
      if (duration > 1000) {
        console.warn('🐌 Requisição lenta:', event.request.url, `${duration.toFixed(2)}ms`);
      }
      
      return response;
    })
  );
});

async function handleRequest(request) {
  // Implementar lógica de requisição baseada no tipo
  const url = new URL(request.url);
  
  if (isStaticFile(request.url)) {
    return cacheFirst(request, STATIC_CACHE);
  } else if (isImageFile(request.url)) {
    return cacheFirst(request, IMAGE_CACHE);
  } else if (isNetworkFirst(request.url)) {
    return networkFirst(request, DYNAMIC_CACHE);
  } else {
    return staleWhileRevalidate(request, DYNAMIC_CACHE);
  }
}

// Relatório de erro
self.addEventListener('error', event => {
  console.error('❌ Service Worker Error:', event.error);
  
  // Enviar relatório de erro (implementar conforme necessário)
  reportError({
    message: event.error.message,
    stack: event.error.stack,
    timestamp: new Date().toISOString(),
    userAgent: navigator.userAgent
  });
});

function reportError(errorData) {
  // Implementar envio de relatório de erro
  console.log('📊 Relatório de erro:', errorData);
}

console.log('🚀 Nest4D Service Worker carregado!');