# Exemplo: Sistema de Resiliência

Este exemplo demonstra como implementar padrões de resiliência no Nest4D, incluindo Circuit Breaker, Retry Policy, Fallback e Bulkhead.

## 📋 Configuração de Resiliência

### 1. Arquivo de Configuração (config.json)

```json
{
  "resilience": {
    "default": {
      "retry": {
        "maxAttempts": 3,
        "backoffType": "exponential",
        "baseDelay": 1000,
        "maxDelay": 10000,
        "jitter": true
      },
      "circuitBreaker": {
        "failureThreshold": 5,
        "recoveryTimeout": 30000,
        "halfOpenMaxCalls": 3
      },
      "fallback": {
        "enabled": true,
        "strategy": "cache"
      }
    },
    "highAvailability": {
      "retry": {
        "maxAttempts": 5,
        "backoffType": "exponential",
        "baseDelay": 500,
        "maxDelay": 5000,
        "jitter": true
      },
      "circuitBreaker": {
        "failureThreshold": 3,
        "recoveryTimeout": 15000,
        "halfOpenMaxCalls": 5
      },
      "fallback": {
        "enabled": true,
        "strategy": "alternative_service"
      }
    },
    "fastFail": {
      "retry": {
        "maxAttempts": 1,
        "backoffType": "fixed",
        "baseDelay": 100
      },
      "circuitBreaker": {
        "failureThreshold": 2,
        "recoveryTimeout": 5000,
        "halfOpenMaxCalls": 1
      },
      "fallback": {
        "enabled": false
      }
    }
  },
  "monitoring": {
    "metrics": {
      "enabled": true,
      "endpoint": "/metrics"
    },
    "health": {
      "enabled": true,
      "endpoint": "/health"
    }
  }
}
```

### 2. Service com Resiliência

```pascal
// UserService.pas
unit UserService;

interface

uses
  System.SysUtils,
  System.JSON,
  System.Net.HttpClient,
  nest4d.resilience.async,
  nest4d.cache.async,
  nest4d.logging.async;

type
  TUser = record
    ID: Integer;
    Name: string;
    Email: string;
    CreatedAt: TDateTime;
  end;

  IUserService = interface
    ['{B8F5E2A1-4C3D-4E5F-8A9B-1C2D3E4F5A6B}']
    function GetUser(UserID: Integer): TUser;
    function GetUsers: TArray<TUser>;
    function CreateUser(const Name, Email: string): TUser;
    function UpdateUser(UserID: Integer; const Name, Email: string): TUser;
    function DeleteUser(UserID: Integer): Boolean;
  end;

  [Retry(MaxAttempts: 3, BackoffType: 'exponential')]
  [CircuitBreaker(FailureThreshold: 5, RecoveryTimeout: 30000)]
  [Fallback(Strategy: 'cache')]
  TUserService = class(TInterfacedObject, IUserService)
  private
    FHttpClient: THTTPClient;
    FCache: ICacheService;
    FLogger: ILogger;
    FBaseURL: string;
    
    function CallExternalAPI(const Endpoint: string; const Method: string = 'GET'; 
                           const Body: string = ''): string;
    function ParseUser(const JSON: string): TUser;
    function ParseUsers(const JSON: string): TArray<TUser>;
  public
    constructor Create(Cache: ICacheService; Logger: ILogger);
    destructor Destroy; override;
    
    // Interface methods
    function GetUser(UserID: Integer): TUser;
    function GetUsers: TArray<TUser>;
    function CreateUser(const Name, Email: string): TUser;
    function UpdateUser(UserID: Integer; const Name, Email: string): TUser;
    function DeleteUser(UserID: Integer): Boolean;
  end;

implementation

constructor TUserService.Create(Cache: ICacheService; Logger: ILogger);
begin
  inherited Create;
  FHttpClient := THTTPClient.Create;
  FCache := Cache;
  FLogger := Logger;
  FBaseURL := 'https://api.external-service.com/users';
end;

destructor TUserService.Destroy;
begin
  FHttpClient.Free;
  inherited;
end;

function TUserService.CallExternalAPI(const Endpoint: string; const Method: string; 
                                    const Body: string): string;
var
  Response: IHTTPResponse;
  URL: string;
begin
  URL := FBaseURL + Endpoint;
  
  FLogger.Info('Calling external API: ' + Method + ' ' + URL);
  
  try
    case Method.ToUpper of
      'GET':
        Response := FHttpClient.Get(URL);
      'POST':
        Response := FHttpClient.Post(URL, TStringStream.Create(Body), nil, 
                                   [TNetHeader.Create('Content-Type', 'application/json')]);
      'PUT':
        Response := FHttpClient.Put(URL, TStringStream.Create(Body), nil,
                                  [TNetHeader.Create('Content-Type', 'application/json')]);
      'DELETE':
        Response := FHttpClient.Delete(URL);
    else
      raise Exception.Create('Unsupported HTTP method: ' + Method);
    end;
    
    if Response.StatusCode = 200 then
    begin
      Result := Response.ContentAsString;
      FLogger.Info('API call successful');
    end
    else
    begin
      FLogger.Error(Format('API call failed with status %d: %s', 
                          [Response.StatusCode, Response.StatusText]));
      raise Exception.CreateFmt('API Error: %d - %s', 
                               [Response.StatusCode, Response.StatusText]);
    end;
    
  except
    on E: Exception do
    begin
      FLogger.Error('API call exception: ' + E.Message);
      raise;
    end;
  end;
end;

function TUserService.ParseUser(const JSON: string): TUser;
var
  JSONObj: TJSONObject;
begin
  JSONObj := TJSONObject.ParseJSONValue(JSON) as TJSONObject;
  try
    if Assigned(JSONObj) then
    begin
      Result.ID := JSONObj.GetValue<Integer>('id');
      Result.Name := JSONObj.GetValue<string>('name');
      Result.Email := JSONObj.GetValue<string>('email');
      Result.CreatedAt := ISO8601ToDate(JSONObj.GetValue<string>('created_at'));
    end
    else
      raise Exception.Create('Invalid JSON format');
  finally
    JSONObj.Free;
  end;
end;

function TUserService.ParseUsers(const JSON: string): TArray<TUser>;
var
  JSONArray: TJSONArray;
  I: Integer;
begin
  JSONArray := TJSONObject.ParseJSONValue(JSON) as TJSONArray;
  try
    if Assigned(JSONArray) then
    begin
      SetLength(Result, JSONArray.Count);
      for I := 0 to JSONArray.Count - 1 do
      begin
        Result[I] := ParseUser((JSONArray.Items[I] as TJSONObject).ToString);
      end;
    end
    else
      raise Exception.Create('Invalid JSON array format');
  finally
    JSONArray.Free;
  end;
end;

function TUserService.GetUser(UserID: Integer): TUser;
var
  CacheKey: string;
  CachedData: string;
  APIResponse: string;
begin
  CacheKey := Format('user_%d', [UserID]);
  
  // Tentar buscar do cache primeiro (fallback strategy)
  if FCache.TryGet(CacheKey, CachedData) then
  begin
    FLogger.Info('User found in cache: ' + IntToStr(UserID));
    Result := ParseUser(CachedData);
    Exit;
  end;
  
  try
    // Chamar API externa (com resiliência automática)
    APIResponse := CallExternalAPI('/' + IntToStr(UserID));
    Result := ParseUser(APIResponse);
    
    // Armazenar no cache para futuras consultas
    FCache.Put(CacheKey, APIResponse, 300); // 5 minutos TTL
    
  except
    on E: Exception do
    begin
      FLogger.Error(Format('Failed to get user %d: %s', [UserID, E.Message]));
      
      // Fallback: retornar dados em cache mesmo expirados (se existirem)
      if FCache.TryGet(CacheKey, CachedData, True) then // ignoreExpiry = True
      begin
        FLogger.Warning('Returning stale cache data for user: ' + IntToStr(UserID));
        Result := ParseUser(CachedData);
      end
      else
      begin
        // Último recurso: dados padrão
        Result.ID := UserID;
        Result.Name := 'User Not Available';
        Result.Email := 'unavailable@service.com';
        Result.CreatedAt := Now;
        FLogger.Warning('Returning default user data for: ' + IntToStr(UserID));
      end;
    end;
  end;
end;

function TUserService.GetUsers: TArray<TUser>;
var
  CacheKey: string;
  CachedData: string;
  APIResponse: string;
begin
  CacheKey := 'users_list';
  
  // Verificar cache
  if FCache.TryGet(CacheKey, CachedData) then
  begin
    FLogger.Info('Users list found in cache');
    Result := ParseUsers(CachedData);
    Exit;
  end;
  
  try
    // Chamar API
    APIResponse := CallExternalAPI('');
    Result := ParseUsers(APIResponse);
    
    // Cache por 2 minutos
    FCache.Put(CacheKey, APIResponse, 120);
    
  except
    on E: Exception do
    begin
      FLogger.Error('Failed to get users list: ' + E.Message);
      
      // Fallback: cache expirado
      if FCache.TryGet(CacheKey, CachedData, True) then
      begin
        FLogger.Warning('Returning stale users list from cache');
        Result := ParseUsers(CachedData);
      end
      else
      begin
        // Lista vazia como último recurso
        SetLength(Result, 0);
        FLogger.Warning('Returning empty users list');
      end;
    end;
  end;
end;

function TUserService.CreateUser(const Name, Email: string): TUser;
var
  RequestBody: TJSONObject;
  APIResponse: string;
begin
  RequestBody := TJSONObject.Create;
  try
    RequestBody.AddPair('name', Name);
    RequestBody.AddPair('email', Email);
    
    APIResponse := CallExternalAPI('', 'POST', RequestBody.ToString);
    Result := ParseUser(APIResponse);
    
    // Invalidar cache da lista de usuários
    FCache.Remove('users_list');
    
    FLogger.Info(Format('User created successfully: %s (%s)', [Name, Email]));
    
  finally
    RequestBody.Free;
  end;
end;

function TUserService.UpdateUser(UserID: Integer; const Name, Email: string): TUser;
var
  RequestBody: TJSONObject;
  APIResponse: string;
  CacheKey: string;
begin
  RequestBody := TJSONObject.Create;
  try
    RequestBody.AddPair('name', Name);
    RequestBody.AddPair('email', Email);
    
    APIResponse := CallExternalAPI('/' + IntToStr(UserID), 'PUT', RequestBody.ToString);
    Result := ParseUser(APIResponse);
    
    // Atualizar cache do usuário específico
    CacheKey := Format('user_%d', [UserID]);
    FCache.Put(CacheKey, APIResponse, 300);
    
    // Invalidar cache da lista
    FCache.Remove('users_list');
    
    FLogger.Info(Format('User %d updated successfully', [UserID]));
    
  finally
    RequestBody.Free;
  end;
end;

function TUserService.DeleteUser(UserID: Integer): Boolean;
var
  CacheKey: string;
begin
  try
    CallExternalAPI('/' + IntToStr(UserID), 'DELETE');
    
    // Remover do cache
    CacheKey := Format('user_%d', [UserID]);
    FCache.Remove(CacheKey);
    FCache.Remove('users_list');
    
    FLogger.Info(Format('User %d deleted successfully', [UserID]));
    Result := True;
    
  except
    on E: Exception do
    begin
      FLogger.Error(Format('Failed to delete user %d: %s', [UserID, E.Message]));
      Result := False;
    end;
  end;
end;

end.
```

### 3. Controller com Resiliência

```pascal
// UserController.pas
unit UserController;

interface

uses
  System.SysUtils,
  System.JSON,
  Horse,
  nest4d.core.async,
  nest4d.resilience.async,
  UserService;

type
  [Controller('/api/users')]
  TUserController = class
  private
    FUserService: IUserService;
  public
    constructor Create(UserService: IUserService);
    
    [Get('/')]
    [Resilience('highAvailability')] // Usar perfil de alta disponibilidade
    procedure GetUsers(Req: THorseRequest; Res: THorseResponse);
    
    [Get('/:id')]
    [Resilience('default')] // Usar perfil padrão
    procedure GetUser(Req: THorseRequest; Res: THorseResponse);
    
    [Post('/')]
    [Resilience('fastFail')] // Falhar rápido para operações de escrita
    procedure CreateUser(Req: THorseRequest; Res: THorseResponse);
    
    [Put('/:id')]
    [Resilience('fastFail')]
    procedure UpdateUser(Req: THorseRequest; Res: THorseResponse);
    
    [Delete('/:id')]
    [Resilience('fastFail')]
    procedure DeleteUser(Req: THorseRequest; Res: THorseResponse);
  end;

implementation

constructor TUserController.Create(UserService: IUserService);
begin
  inherited Create;
  FUserService := UserService;
end;

procedure TUserController.GetUsers(Req: THorseRequest; Res: THorseResponse);
var
  Users: TArray<TUser>;
  JSONArray: TJSONArray;
  JSONUser: TJSONObject;
  User: TUser;
begin
  try
    Users := FUserService.GetUsers;
    
    JSONArray := TJSONArray.Create;
    try
      for User in Users do
      begin
        JSONUser := TJSONObject.Create;
        JSONUser.AddPair('id', User.ID);
        JSONUser.AddPair('name', User.Name);
        JSONUser.AddPair('email', User.Email);
        JSONUser.AddPair('created_at', DateToISO8601(User.CreatedAt));
        JSONArray.AddElement(JSONUser);
      end;
      
      Res.Send(JSONArray.ToString);
    finally
      JSONArray.Free;
    end;
    
  except
    on E: Exception do
    begin
      Res.Status(500).Send(Format('{"error": "Failed to get users", "message": "%s"}', 
                                 [E.Message]));
    end;
  end;
end;

procedure TUserController.GetUser(Req: THorseRequest; Res: THorseResponse);
var
  UserID: Integer;
  User: TUser;
  JSONUser: TJSONObject;
begin
  try
    UserID := StrToInt(Req.Params['id']);
    User := FUserService.GetUser(UserID);
    
    JSONUser := TJSONObject.Create;
    try
      JSONUser.AddPair('id', User.ID);
      JSONUser.AddPair('name', User.Name);
      JSONUser.AddPair('email', User.Email);
      JSONUser.AddPair('created_at', DateToISO8601(User.CreatedAt));
      
      Res.Send(JSONUser.ToString);
    finally
      JSONUser.Free;
    end;
    
  except
    on E: EConvertError do
      Res.Status(400).Send('{"error": "Invalid user ID"}');
    on E: Exception do
      Res.Status(500).Send(Format('{"error": "Failed to get user", "message": "%s"}', 
                                 [E.Message]));
  end;
end;

procedure TUserController.CreateUser(Req: THorseRequest; Res: THorseResponse);
var
  RequestBody: TJSONObject;
  Name, Email: string;
  User: TUser;
  JSONUser: TJSONObject;
begin
  try
    RequestBody := TJSONObject.ParseJSONValue(Req.Body) as TJSONObject;
    if not Assigned(RequestBody) then
    begin
      Res.Status(400).Send('{"error": "Invalid JSON body"}');
      Exit;
    end;
    
    try
      Name := RequestBody.GetValue<string>('name');
      Email := RequestBody.GetValue<string>('email');
      
      if (Name.Trim = '') or (Email.Trim = '') then
      begin
        Res.Status(400).Send('{"error": "Name and email are required"}');
        Exit;
      end;
      
      User := FUserService.CreateUser(Name, Email);
      
      JSONUser := TJSONObject.Create;
      try
        JSONUser.AddPair('id', User.ID);
        JSONUser.AddPair('name', User.Name);
        JSONUser.AddPair('email', User.Email);
        JSONUser.AddPair('created_at', DateToISO8601(User.CreatedAt));
        
        Res.Status(201).Send(JSONUser.ToString);
      finally
        JSONUser.Free;
      end;
      
    finally
      RequestBody.Free;
    end;
    
  except
    on E: Exception do
      Res.Status(500).Send(Format('{"error": "Failed to create user", "message": "%s"}', 
                                 [E.Message]));
  end;
end;

procedure TUserController.UpdateUser(Req: THorseRequest; Res: THorseResponse);
var
  UserID: Integer;
  RequestBody: TJSONObject;
  Name, Email: string;
  User: TUser;
  JSONUser: TJSONObject;
begin
  try
    UserID := StrToInt(Req.Params['id']);
    
    RequestBody := TJSONObject.ParseJSONValue(Req.Body) as TJSONObject;
    if not Assigned(RequestBody) then
    begin
      Res.Status(400).Send('{"error": "Invalid JSON body"}');
      Exit;
    end;
    
    try
      Name := RequestBody.GetValue<string>('name');
      Email := RequestBody.GetValue<string>('email');
      
      User := FUserService.UpdateUser(UserID, Name, Email);
      
      JSONUser := TJSONObject.Create;
      try
        JSONUser.AddPair('id', User.ID);
        JSONUser.AddPair('name', User.Name);
        JSONUser.AddPair('email', User.Email);
        JSONUser.AddPair('created_at', DateToISO8601(User.CreatedAt));
        
        Res.Send(JSONUser.ToString);
      finally
        JSONUser.Free;
      end;
      
    finally
      RequestBody.Free;
    end;
    
  except
    on E: EConvertError do
      Res.Status(400).Send('{"error": "Invalid user ID"}');
    on E: Exception do
      Res.Status(500).Send(Format('{"error": "Failed to update user", "message": "%s"}', 
                                 [E.Message]));
  end;
end;

procedure TUserController.DeleteUser(Req: THorseRequest; Res: THorseResponse);
var
  UserID: Integer;
begin
  try
    UserID := StrToInt(Req.Params['id']);
    
    if FUserService.DeleteUser(UserID) then
      Res.Status(204).Send('')
    else
      Res.Status(500).Send('{"error": "Failed to delete user"}');
      
  except
    on E: EConvertError do
      Res.Status(400).Send('{"error": "Invalid user ID"}');
    on E: Exception do
      Res.Status(500).Send(Format('{"error": "Failed to delete user", "message": "%s"}', 
                                 [E.Message]));
  end;
end;

end.
```

### 4. Programa Principal com Monitoramento

```pascal
program ResilientWebServer;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.JSON,
  nest4d.core.async,
  nest4d.horse.async,
  nest4d.resilience.async,
  nest4d.cache.async,
  nest4d.logging.async,
  nest4d.metrics.async,
  nest4d.health.async,
  nest4d.config,
  UserService,
  UserController;

var
  Config: TNest4DConfig;
  UserService: IUserService;
  UserController: TUserController;

begin
  try
    WriteLn('=== Nest4D Resilient Server ===');
    WriteLn('');
    
    // Carregar configuração
    Config := TNest4DConfig.Create;
    Config.LoadFromFile('config.json');
    
    // Configurar e iniciar servidor
    TNest4D
      .Create
      
      // Configurar resiliência
      .UseResilience(Config)
      
      // Configurar cache
      .UseCache
      
      // Configurar logging
      .UseLogging
      
      // Configurar métricas
      .UseMetrics
      
      // Configurar health checks
      .UseHealthChecks
      
      // Registrar serviços
      .RegisterSingleton<IUserService, TUserService>
      
      // Registrar controllers
      .RegisterController<TUserController>
      
      // Endpoints de monitoramento
      .Get('/metrics', 
        procedure(Req: THorseRequest; Res: THorseResponse)
        var
          Metrics: TJSONObject;
        begin
          Metrics := TNest4DMetrics.GetMetrics;
          try
            Res.Send(Metrics.ToString);
          finally
            Metrics.Free;
          end;
        end)
      
      .Get('/health', 
        procedure(Req: THorseRequest; Res: THorseResponse)
        var
          Health: TJSONObject;
        begin
          Health := TNest4DHealth.GetHealthStatus;
          try
            if Health.GetValue<string>('status') = 'healthy' then
              Res.Send(Health.ToString)
            else
              Res.Status(503).Send(Health.ToString);
          finally
            Health.Free;
          end;
        end)
      
      .Get('/resilience', 
        procedure(Req: THorseRequest; Res: THorseResponse)
        var
          ResilienceStats: TJSONObject;
        begin
          ResilienceStats := TNest4DResilience.GetStatistics;
          try
            Res.Send(ResilienceStats.ToString);
          finally
            ResilienceStats.Free;
          end;
        end)
      
      // Iniciar servidor
      .Listen(8080);
    
    WriteLn('🚀 Servidor resiliente rodando em http://localhost:8080');
    WriteLn('');
    WriteLn('📊 Endpoints de monitoramento:');
    WriteLn('  GET /health      - Status de saúde do sistema');
    WriteLn('  GET /metrics     - Métricas de performance');
    WriteLn('  GET /resilience  - Estatísticas de resiliência');
    WriteLn('');
    WriteLn('👥 API de usuários:');
    WriteLn('  GET    /api/users     - Listar usuários (High Availability)');
    WriteLn('  GET    /api/users/:id - Buscar usuário (Default)');
    WriteLn('  POST   /api/users     - Criar usuário (Fast Fail)');
    WriteLn('  PUT    /api/users/:id - Atualizar usuário (Fast Fail)');
    WriteLn('  DELETE /api/users/:id - Deletar usuário (Fast Fail)');
    WriteLn('');
    WriteLn('🔧 Perfis de resiliência configurados:');
    WriteLn('  • Default: 3 tentativas, circuit breaker, fallback cache');
    WriteLn('  • High Availability: 5 tentativas, recuperação rápida');
    WriteLn('  • Fast Fail: 1 tentativa, falha rápida');
    WriteLn('');
    WriteLn('Pressione ENTER para parar o servidor...');
    ReadLn;
    
  except
    on E: Exception do
    begin
      WriteLn('❌ Erro ao iniciar servidor: ' + E.Message);
      WriteLn('Pressione ENTER para sair...');
      ReadLn;
    end;
  end;
end.
```

## 🧪 Testando a Resiliência

### 1. Teste de Circuit Breaker

```bash
# Simular falhas consecutivas para ativar o circuit breaker
for i in {1..10}; do
  curl -w "Status: %{http_code}\n" http://localhost:8080/api/users/999
  sleep 1
done

# Verificar status do circuit breaker
curl http://localhost:8080/resilience
```

### 2. Teste de Retry Policy

```bash
# Monitorar logs durante falhas temporárias
curl -v http://localhost:8080/api/users/1

# Verificar métricas de retry
curl http://localhost:8080/metrics | jq '.resilience.retry'
```

### 3. Teste de Fallback

```bash
# Primeiro, popular o cache
curl http://localhost:8080/api/users/1

# Simular falha do serviço externo e verificar fallback
# (desconectar rede ou parar serviço externo)
curl http://localhost:8080/api/users/1
```

### 4. Monitoramento em Tempo Real

```bash
# Health check
watch -n 2 'curl -s http://localhost:8080/health | jq .'

# Métricas
watch -n 5 'curl -s http://localhost:8080/metrics | jq .resilience'

# Estatísticas de resiliência
watch -n 3 'curl -s http://localhost:8080/resilience | jq .'
```

## 📊 Métricas de Resiliência

### Exemplo de Resposta `/metrics`

```json
{
  "timestamp": "2024-01-15T10:30:45Z",
  "resilience": {
    "retry": {
      "total_attempts": 156,
      "successful_retries": 23,
      "failed_retries": 8,
      "success_rate": 0.95
    },
    "circuit_breaker": {
      "state": "closed",
      "failure_count": 2,
      "success_count": 145,
      "last_failure": "2024-01-15T10:25:30Z",
      "next_attempt": null
    },
    "fallback": {
      "cache_hits": 89,
      "cache_misses": 12,
      "fallback_executions": 5
    }
  },
  "performance": {
    "avg_response_time": 245,
    "p95_response_time": 890,
    "p99_response_time": 1250,
    "requests_per_second": 45.2
  }
}
```

### Exemplo de Resposta `/resilience`

```json
{
  "profiles": {
    "default": {
      "circuit_breaker": {
        "state": "closed",
        "failure_threshold": 5,
        "current_failures": 1,
        "recovery_timeout": 30000,
        "last_state_change": "2024-01-15T10:20:15Z"
      },
      "retry": {
        "max_attempts": 3,
        "total_executions": 89,
        "retry_executions": 12,
        "success_rate": 0.93
      }
    },
    "highAvailability": {
      "circuit_breaker": {
        "state": "closed",
        "failure_threshold": 3,
        "current_failures": 0,
        "recovery_timeout": 15000
      },
      "retry": {
        "max_attempts": 5,
        "total_executions": 45,
        "retry_executions": 8,
        "success_rate": 0.98
      }
    },
    "fastFail": {
      "circuit_breaker": {
        "state": "closed",
        "failure_threshold": 2,
        "current_failures": 0,
        "recovery_timeout": 5000
      },
      "retry": {
        "max_attempts": 1,
        "total_executions": 23,
        "retry_executions": 0,
        "success_rate": 0.87
      }
    }
  }
}
```

## 🚀 Próximos Passos

1. **Configurar Alertas**: Integre com sistemas de monitoramento
2. **Implementar Bulkhead**: Isole recursos críticos
3. **Adicionar Rate Limiting**: Proteja contra sobrecarga
4. **Configurar Distributed Tracing**: Rastreie requisições entre serviços
5. **Implementar Chaos Engineering**: Teste a resiliência em produção

## 📚 Recursos Relacionados

- [Exemplo Básico](./basic-server.md)
- [Exemplo com Plugins](./plugins.md)
- [Exemplo com Microserviços](./microservices.md)
- [Guia de Arquitetura](../architecture.md)

---

Este exemplo demonstra como o Nest4D torna a implementação de padrões de resiliência simples e declarativa, permitindo que você construa sistemas robustos e tolerantes a falhas.