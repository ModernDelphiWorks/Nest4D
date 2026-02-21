# Guia de Uso do Sistema de Resiliência - Nest4D + Horse

Este guia explica como usar o sistema de resiliência integrado ao framework Nest4D com Horse.

## Configuração Inicial

### 1. Habilitando Resiliência

```pascal
uses
  nest4d.horse;

begin
  // Configuração básica com resiliência habilitada
  Nest4D_Horse(
    TNest4DHorseConfig.Default
      .EnableResilience(True)           // Habilita resiliência
      .ResilienceEndpoint('/resilience') // Endpoint para métricas
      .ResilienceConfig('Default')       // Configuração padrão
  );
end;
```

### 2. Configurações Disponíveis

#### Default (Padrão)
- Retry: 3 tentativas, delay de 1 segundo
- Circuit Breaker: 5 falhas em 60 segundos
- Timeout: 30 segundos

#### HighAvailability (Alta Disponibilidade)
- Retry: 5 tentativas, delay de 500ms
- Circuit Breaker: 3 falhas em 30 segundos
- Timeout: 15 segundos

#### FastFail (Falha Rápida)
- Retry: 1 tentativa, delay de 100ms
- Circuit Breaker: 2 falhas em 10 segundos
- Timeout: 5 segundos

#### Bulkhead (Isolamento)
- Retry: 2 tentativas, delay de 2 segundos
- Circuit Breaker: 10 falhas em 120 segundos
- Timeout: 60 segundos

```pascal
// Exemplo com configuração de alta disponibilidade
Nest4D_Horse(
  TNest4DHorseConfig.Default
    .EnableResilience(True)
    .ResilienceConfig('HighAvailability')
);
```

## Usando Atributos de Resiliência

### 1. Atributo [Retry]

Aplica política de retry automático em caso de falha:

```pascal
[Get('/api/data')]
[Retry(3, 1000)] // 3 tentativas, 1000ms de delay
function GetData(const ARequest: IRouteRequest): TReturnPair;
begin
  // Sua lógica aqui
  // Em caso de exceção, será tentado novamente automaticamente
end;
```

#### Retry Avançado com Backoff Exponencial

```pascal
[Get('/api/advanced')]
[Retry(5, 1000, 'exponential', 2.0, 0.1)] 
// 5 tentativas, delay inicial 1s, backoff exponencial, multiplicador 2.0, jitter 0.1
function GetAdvancedData(const ARequest: IRouteRequest): TReturnPair;
```

### 2. Atributo [CircuitBreaker]

Protege contra falhas em cascata:

```pascal
[Get('/api/external')]
[CircuitBreaker(5, 60000, 30000)] 
// 5 falhas, janela de 60s, timeout de 30s
function CallExternalService(const ARequest: IRouteRequest): TReturnPair;
begin
  // Chamada para serviço externo
  // Circuit breaker abrirá após 5 falhas consecutivas
end;
```

### 3. Atributo [Fallback]

Executa método alternativo em caso de falha:

```pascal
[Get('/api/cached')]
[Fallback('GetCachedData')]
function GetLiveData(const ARequest: IRouteRequest): TReturnPair;
begin
  // Tenta obter dados em tempo real
end;

// Método de fallback
function GetCachedData(const ARequest: IRouteRequest): TReturnPair;
begin
  // Retorna dados em cache
  Result := TResponse.Ok('Dados em cache');
end;
```

### 4. Combinando Atributos

Você pode combinar múltiplos atributos para máxima resiliência:

```pascal
[Get('/api/critical')]
[Retry(2, 500)]                    // Retry primeiro
[CircuitBreaker(3, 30000, 15000)]  // Circuit breaker como proteção
[Fallback('CriticalFallback')]     // Fallback como último recurso
function CriticalOperation(const ARequest: IRouteRequest): TReturnPair;
begin
  // Operação crítica
end;
```

## Monitoramento e Métricas

### Endpoint de Métricas

O sistema expõe métricas em tempo real no endpoint configurado (padrão: `/resilience`):

```bash
GET http://localhost:9000/resilience
```

Resposta JSON:
```json
{
  "totalRequests": 1250,
  "successfulRequests": 1100,
  "failedRequests": 150,
  "retriedRequests": 75,
  "circuitBreakerTrips": 5,
  "fallbackExecutions": 25,
  "averageResponseTime": 245.67,
  "activeRequests": 3,
  "timestamp": "2024-01-15T10:30:45.123Z"
}
```

### Métricas Disponíveis

- **totalRequests**: Total de requisições processadas
- **successfulRequests**: Requisições bem-sucedidas
- **failedRequests**: Requisições que falharam
- **retriedRequests**: Requisições que foram tentadas novamente
- **circuitBreakerTrips**: Vezes que o circuit breaker abriu
- **fallbackExecutions**: Execuções de métodos de fallback
- **averageResponseTime**: Tempo médio de resposta (ms)
- **activeRequests**: Requisições ativas no momento

## Exemplos Práticos

### Exemplo 1: API de Pagamento

```pascal
[Controller('/api/payment')]
TPaymentController = class(TInterfacedObject, IController)
public
  [Post('/process')]
  [Retry(3, 2000)]                   // 3 tentativas, 2s de delay
  [CircuitBreaker(5, 60000, 30000)]  // Proteção contra falhas
  [Fallback('ProcessPaymentOffline')] // Fallback para processamento offline
  function ProcessPayment(const ARequest: IRouteRequest): TReturnPair;
end;
```

### Exemplo 2: Cache com Fallback

```pascal
[Controller('/api/products')]
TProductController = class(TInterfacedObject, IController)
public
  [Get('/list')]
  [Retry(2, 1000)]           // Retry rápido
  [Fallback('GetCachedProducts')] // Fallback para cache
  function GetProducts(const ARequest: IRouteRequest): TReturnPair;
  
private
  function GetCachedProducts(const ARequest: IRouteRequest): TReturnPair;
end;
```

### Exemplo 3: Serviço Crítico

```pascal
[Controller('/api/critical')]
TCriticalController = class(TInterfacedObject, IController)
public
  [Get('/health-check')]
  [Retry(1, 100)]                // Falha rápida
  [CircuitBreaker(2, 10000, 5000)] // Circuit breaker agressivo
  function HealthCheck(const ARequest: IRouteRequest): TReturnPair;
end;
```

## Boas Práticas

### 1. Configuração por Ambiente

```pascal
// Desenvolvimento
Nest4D_Horse(
  TNest4DHorseConfig.Default
    .ResilienceConfig('FastFail') // Falha rápida para debug
);

// Produção
Nest4D_Horse(
  TNest4DHorseConfig.Default
    .ResilienceConfig('HighAvailability') // Máxima disponibilidade
);
```

### 2. Timeouts Apropriados

- **APIs rápidas**: 1-5 segundos
- **Operações normais**: 10-30 segundos
- **Operações pesadas**: 60+ segundos

### 3. Estratégias de Retry

- **Operações idempotentes**: Use retry liberalmente
- **Operações críticas**: Use retry conservador
- **Operações de escrita**: Cuidado com retry

### 4. Circuit Breaker

- **Serviços externos**: Sempre use circuit breaker
- **Operações custosas**: Configure limites baixos
- **APIs críticas**: Configure timeouts curtos

### 5. Fallback

- **Sempre tenha um plano B**: Cache, dados padrão, serviço alternativo
- **Fallback deve ser rápido**: Evite operações custosas
- **Monitore execuções**: Muitos fallbacks indicam problemas

## Troubleshooting

### Problema: Muitos Retries
**Solução**: Reduza o número de tentativas ou aumente o delay

### Problema: Circuit Breaker Abrindo Frequentemente
**Solução**: Ajuste o threshold ou a janela de tempo

### Problema: Fallback Executando Sempre
**Solução**: Verifique a lógica principal e logs de erro

### Problema: Performance Degradada
**Solução**: Monitore métricas e ajuste timeouts

## Integração com Logging

O sistema de resiliência se integra automaticamente com o sistema de logging do Nest4D:

```pascal
// Logs automáticos incluem:
// - Tentativas de retry
// - Abertura/fechamento do circuit breaker
// - Execuções de fallback
// - Métricas de performance
```

Para mais informações, consulte a documentação completa do Nest4D.