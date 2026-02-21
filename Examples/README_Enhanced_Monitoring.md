# Nest4D Enhanced Monitoring System

Este documento explica como usar o sistema de monitoramento aprimorado do Nest4D, que inclui métricas, logging estruturado, interceptadores, health checks e tratamento avançado de erros.

## Visão Geral

O sistema de monitoramento aprimorado do Nest4D fornece:

- **Métricas**: Coleta e exportação de métricas de aplicação
- **Logging Estruturado**: Sistema de logging com contexto e múltiplos appenders
- **Interceptadores**: Middleware para capturar e processar requisições/respostas
- **Health Checks**: Verificações de saúde da aplicação e dependências
- **Tratamento de Erros**: Captura e formatação automática de exceções

## Componentes Principais

### 1. Sistema de Métricas (`nest4d.metrics.pas`)

#### Tipos de Métricas

```pascal
TMetricType = (mtCounter, mtGauge, mtHistogram, mtSummary);
```

#### Uso Básico

```pascal
var
  LMetrics: IMetricsCollector;
  LTags: TDictionary<String, String>;
begin
  LMetrics := GetMetricsCollector;
  
  LTags := TDictionary<String, String>.Create;
  try
    LTags.Add('endpoint', '/api/users');
    LTags.Add('method', 'GET');
    
    // Incrementar contador
    LMetrics.IncrementCounter('http_requests_total', LTags);
    
    // Definir gauge
    LMetrics.SetGauge('active_connections', 42, LTags);
    
    // Registrar duração
    LMetrics.RecordDuration('request_duration_ms', 150.5, LTags);
  finally
    LTags.Free;
  end;
end;
```

#### Exportadores Disponíveis

- **Console**: Exibe métricas no console
- **JSON**: Exporta métricas em formato JSON
- **Prometheus**: Exporta métricas no formato Prometheus (planejado)

### 2. Sistema de Logging (`nest4d.logging.pas`)

#### Níveis de Log

```pascal
TLogLevel = (llTrace, llDebug, llInfo, llWarn, llError, llFatal);
```

#### Uso Básico

```pascal
var
  LLogger: IStructuredLogger;
  LContext: TLogContext;
begin
  LLogger := GetLogger;
  
  LContext := CreateLogContext('req-123');
  LContext.Route := '/api/users';
  LContext.Method := 'GET';
  LContext.UserID := 'user-456';
  
  LLogger.Info('Processing user request', LContext);
  
  try
    // Código da aplicação
  except
    on E: Exception do
      LLogger.Error('Failed to process request', LContext, E);
  end;
end;
```

#### Appenders Disponíveis

- **Console**: Log colorido no console
- **File**: Log em arquivo texto
- **JSON**: Log estruturado em arquivo JSON

### 3. Sistema de Interceptadores (`nest4d.interceptor.pas`)

#### Tipos de Interceptadores

- **Request Interceptor**: Processa requisições antes do handler
- **Response Interceptor**: Processa respostas após o handler
- **Exception Interceptor**: Processa exceções

#### Interceptador Customizado

```pascal
type
  TMyCustomInterceptor = class(TInterfacedObject, IRequestInterceptor)
  public
    function Intercept(ARequest: THorseRequest; AResponse: THorseResponse; ANext: TProc): Boolean;
  end;

function TMyCustomInterceptor.Intercept(ARequest: THorseRequest; AResponse: THorseResponse; ANext: TProc): Boolean;
begin
  Result := True;
  
  // Validação customizada
  if ARequest.Headers['X-API-Key'] = '' then
  begin
    AResponse.Status(401).Send('{"error":"API Key required"}');
    Result := False;
    Exit;
  end;
  
  // Continua o processamento
  if Assigned(ANext) then
    ANext();
end;
```

### 4. Sistema de Health Checks (`nest4d.health.pas`)

#### Status de Saúde

```pascal
THealthStatus = (hsHealthy, hsDegraded, hsUnhealthy);
```

#### Health Check Customizado

```pascal
type
  TMyHealthCheck = class(TInterfacedObject, IHealthCheck)
  public
    function GetName: String;
    function Check: THealthCheckResult;
    function GetTimeout: Integer;
  end;

function TMyHealthCheck.Check: THealthCheckResult;
begin
  Result := THealthCheckResult.Create('my_service', hsHealthy);
  
  try
    // Verificação customizada
    if MyServiceIsRunning then
    begin
      Result.Status := hsHealthy;
      Result.Message := 'Service is running';
    end
    else
    begin
      Result.Status := hsUnhealthy;
      Result.Message := 'Service is down';
    end;
  except
    on E: Exception do
    begin
      Result.Status := hsUnhealthy;
      Result.Message := E.Message;
    end;
  end;
end;
```

### 5. Middleware Enhanced (`nest4d.horse.pas`)

#### Configuração

```pascal
var
  LApp: THorse;
begin
  LApp := THorse.Create;
  
  // Configura o middleware enhanced (tudo automático)
  LApp.Use(Nest4D_Horse);
  
  // Suas rotas
  LApp.Get('/api/users', GetUsers);
  
  LApp.Listen(9000);
end;
```

## Endpoints de Monitoramento

O middleware automaticamente expõe os seguintes endpoints:

### Health Checks

- `GET /health` - Status geral de saúde
- `GET /health/detailed` - Detalhes de todos os health checks
- `GET /health/ready` - Readiness probe (para Kubernetes)
- `GET /health/live` - Liveness probe (para Kubernetes)

### Métricas

- `GET /metrics` - Métricas em formato JSON
- `GET /metrics/prometheus` - Métricas em formato Prometheus

## Exemplo Completo

Veja o arquivo `enhanced_monitoring_example.pas` para um exemplo completo de uso.

### Executando o Exemplo

1. Compile o projeto
2. Execute o binário
3. Acesse os endpoints:
   - `http://localhost:9000/api/users` - Endpoint de exemplo
   - `http://localhost:9000/health` - Health check
   - `http://localhost:9000/metrics` - Métricas

### Testando Funcionalidades

```bash
# Requisição normal
curl http://localhost:9000/api/users

# Criar usuário
curl -X POST http://localhost:9000/api/users \
  -H "Content-Type: application/json" \
  -d '{"name":"João","email":"joao@example.com"}'

# Endpoint lento (para testar métricas de performance)
curl http://localhost:9000/api/slow

# Endpoint com erro (para testar tratamento de exceções)
curl http://localhost:9000/api/error

# Endpoint admin (para testar interceptador de segurança)
curl http://localhost:9000/api/admin/dashboard

# Com autorização
curl -H "Authorization: Bearer token" http://localhost:9000/api/admin/dashboard

# Health check
curl http://localhost:9000/health

# Métricas
curl http://localhost:9000/metrics
```

## Configuração de Produção

### Logging

```pascal
// Configurar appenders para produção
var
  LLogger: TStructuredLogger;
begin
  LLogger := TStructuredLogger.Create;
  
  // Apenas arquivo em produção (sem console)
  LLogger.AddAppender(TFileLogAppender.Create('logs/app.log'));
  LLogger.AddAppender(TJSONLogAppender.Create('logs/app.json'));
  
  // Nível de log mais restritivo
  LLogger.SetLogLevel(llWarn);
end;
```

### Métricas

```pascal
// Configurar exportadores para produção
var
  LMetrics: TMetricsCollector;
begin
  LMetrics := TMetricsCollector.Create;
  
  // Apenas JSON em produção (sem console)
  LMetrics.AddExporter(TJSONMetricsExporter.Create('metrics/metrics.json'));
end;
```

### Health Checks

```pascal
// Registrar health checks essenciais
var
  LHealthService: IHealthService;
begin
  LHealthService := GetHealthService;
  
  // Banco de dados
  LHealthService.RegisterHealthCheck(
    TDatabaseHealthCheck.Create('database', 'connection_string'));
  
  // Cache Redis
  LHealthService.RegisterHealthCheck(
    TRedisHealthCheck.Create('redis', 'redis://localhost:6379'));
  
  // API externa
  LHealthService.RegisterHealthCheck(
    THttpEndpointHealthCheck.Create('external_api', 'https://api.external.com/health'));
end;
```

## Integração com Kubernetes

```yaml
apiVersion: v1
kind: Pod
spec:
  containers:
  - name: nest4d-app
    image: my-nest4d-app:latest
    ports:
    - containerPort: 9000
    livenessProbe:
      httpGet:
        path: /health/live
        port: 9000
      initialDelaySeconds: 30
      periodSeconds: 10
    readinessProbe:
      httpGet:
        path: /health/ready
        port: 9000
      initialDelaySeconds: 5
      periodSeconds: 5
```

## Integração com Prometheus

```yaml
# prometheus.yml
scrape_configs:
  - job_name: 'nest4d-app'
    static_configs:
      - targets: ['localhost:9000']
    metrics_path: '/metrics/prometheus'
    scrape_interval: 15s
```

## Troubleshooting

### Logs não aparecem

1. Verifique se o nível de log está correto
2. Verifique se os appenders estão configurados
3. Verifique permissões de escrita nos diretórios de log

### Métricas não são coletadas

1. Verifique se `EnableMetrics` está `True`
2. Verifique se o `MetricsCollector` está inicializado
3. Verifique se os exportadores estão configurados

### Health checks falham

1. Verifique se as dependências estão disponíveis
2. Verifique timeouts dos health checks
3. Verifique logs para detalhes dos erros

## Próximos Passos

- [ ] Implementar exportador Prometheus nativo
- [ ] Adicionar métricas de JVM/runtime
- [ ] Implementar distributed tracing
- [ ] Adicionar alerting baseado em métricas
- [ ] Implementar rate limiting com métricas