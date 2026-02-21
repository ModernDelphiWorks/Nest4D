# Nest4D Horse Enhanced Middleware

Este documento explica como usar o middleware Horse aprimorado do Nest4D com sistema de monitoramento integrado.

## Visão Geral

O middleware Horse foi aprimorado para incluir:
- **Logging estruturado** com contexto de requisição
- **Métricas de performance** e contadores
- **Health checks** automáticos
- **Interceptadores** para tratamento de requisições
- **Rastreamento de requisições** com Request ID
- **Detecção de operações lentas**

## Configuração

### Configuração Básica (Padrão)

```pascal
uses
  Horse,
  nest4d.horse;

begin
  // Usa configuração padrão
  THorse.Use(Nest4D_Horse(TAppModule));
  THorse.Listen(9000);
end.
```

### Configuração Personalizada

```pascal
uses
  Horse,
  nest4d.horse;

var
  LConfig: TNest4DHorseConfig;
begin
  // Configura o sistema de monitoramento
  LConfig := TNest4DHorseConfig.Default;
  LConfig.EnableMetrics := True;
  LConfig.EnableLogging := True;
  LConfig.EnableHealthChecks := True;
  LConfig.EnableInterceptors := True;
  LConfig.LogLevel := llInfo;
  LConfig.MetricsEndpoint := '/metrics';
  LConfig.HealthEndpoint := '/health';
  LConfig.SlowRequestThresholdMs := 1000; // 1 segundo
  
  // Aplica configuração personalizada (endpoints configurados automaticamente)
  THorse.Use(Nest4D_Horse(TAppModule, LConfig));
  
  THorse.Listen(9000);
end.
```

## Configurações Disponíveis

### TNest4DHorseConfig

| Propriedade | Tipo | Padrão | Descrição |
|-------------|------|--------|----------|
| `EnableMetrics` | Boolean | True | Habilita coleta de métricas |
| `EnableLogging` | Boolean | True | Habilita logging estruturado |
| `EnableHealthChecks` | Boolean | True | Habilita health checks |
| `EnableInterceptors` | Boolean | True | Habilita interceptadores |
| `LogLevel` | TLogLevel | llInfo | Nível mínimo de log |
| `MetricsEndpoint` | String | '/metrics' | Endpoint para métricas |
| `HealthEndpoint` | String | '/health' | Endpoint para health check |
| `SlowRequestThresholdMs` | Integer | 1000 | Limite para requisições lentas (ms) |

## Funcionalidades

### 1. Logging Estruturado

Todas as requisições são automaticamente logadas com:
- Request ID único
- Método HTTP e caminho
- Duração da requisição
- Status code da resposta
- Informações de erro (quando aplicável)

```
[INFO] Request completed successfully
{
  "requestId": "abc123def456",
  "method": "GET",
  "path": "/api/users",
  "statusCode": 200,
  "duration": 45
}
```

### 2. Métricas Automáticas

O sistema coleta automaticamente:
- `http_requests_total` - Total de requisições
- `http_request_duration_ms` - Duração das requisições
- `http_responses_{status}_total` - Respostas por status code
- `http_errors_total` - Total de erros
- `http_slow_requests_total` - Requisições lentas

### 3. Health Checks

Endpoint `/health` retorna:
```json
{
  "status": "healthy",
  "timestamp": "2024-01-15T10:30:00.000Z",
  "details": "All systems operational"
}
```

Status possíveis:
- `healthy` (200) - Sistema funcionando normalmente
- `degraded` (200) - Sistema funcionando com limitações
- `unhealthy` (503) - Sistema com problemas

### 4. Request ID

Cada requisição recebe um ID único:
- Adicionado automaticamente ao header `X-Request-ID`
- Incluído em todos os logs
- Útil para rastreamento e debugging

### 5. Tratamento de Erros Aprimorado

Respostas de erro incluem informações estruturadas:
```json
{
  "error": "BadRequest",
  "message": "Invalid parameter",
  "statusCode": 400,
  "requestId": "abc123def456",
  "timestamp": "2024-01-15T10:30:00.000Z"
}
```

## Endpoints de Monitoramento

### Health Check
```
GET /health
```

Retorna o status de saúde da aplicação.

### Métricas
```
GET /metrics
```

Retorna métricas da aplicação em formato texto (compatível com Prometheus).

## Exemplo Completo

Veja o arquivo `Examples/enhanced_horse_example.pas` para um exemplo completo de uso.

## Integração com Sistemas Externos

### Prometheus

As métricas são compatíveis com Prometheus. Configure o scraping:

```yaml
scrape_configs:
  - job_name: 'nest4d-app'
    static_configs:
      - targets: ['localhost:9000']
    metrics_path: '/metrics'
    scrape_interval: 15s
```

### Kubernetes

Para deployments em Kubernetes, configure health checks:

```yaml
apiVersion: v1
kind: Pod
spec:
  containers:
  - name: nest4d-app
    livenessProbe:
      httpGet:
        path: /health
        port: 9000
      initialDelaySeconds: 30
      periodSeconds: 10
    readinessProbe:
      httpGet:
        path: /health
        port: 9000
      initialDelaySeconds: 5
      periodSeconds: 5
```

## Troubleshooting

### Problema: Métricas não aparecem
**Solução**: Verifique se `EnableMetrics` está `True` e se o endpoint `/metrics` está configurado.

### Problema: Logs não aparecem
**Solução**: Verifique se `EnableLogging` está `True` e se o `LogLevel` está adequado.

### Problema: Health check retorna erro
**Solução**: Verifique se `EnableHealthChecks` está `True` e se não há problemas de dependências.

### Problema: Request ID não aparece
**Solução**: Verifique se o middleware está sendo aplicado antes de outros middlewares.

## Considerações de Performance

- O sistema de monitoramento adiciona overhead mínimo (~1-2ms por requisição)
- Métricas são coletadas em memória e exportadas sob demanda
- Logs são escritos de forma assíncrona quando possível
- Health checks são executados de forma otimizada

## Migração do Middleware Anterior

Se você estava usando o middleware Horse anterior:

1. **Sem configuração personalizada**: Nenhuma mudança necessária
2. **Com configuração personalizada**: Use a estrutura `TNest4DHorseConfig`

O middleware é totalmente compatível com versões anteriores e agora oferece configuração automática dos endpoints de monitoramento.