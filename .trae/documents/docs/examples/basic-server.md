# Exemplo: Servidor Básico

Este exemplo demonstra como criar um servidor web básico com Nest4D, incluindo rotas simples, middleware e tratamento de erros.

## 📋 Código Completo

### 1. Programa Principal

```pascal
program BasicWebServer;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.JSON,
  nest4d.core.async,
  nest4d.horse.async,
  nest4d.interceptor.async,
  nest4d.logging.async;

begin
  try
    WriteLn('=== Nest4D Basic Server ===');
    WriteLn('');
    
    // Configurar e iniciar servidor
    TNest4D
      .Create
      // Middleware de logging
      .UseInterceptor<TLoggingInterceptor>
      
      // Rota básica
      .Get('/api/hello', 
        procedure(Req: THorseRequest; Res: THorseResponse)
        begin
          Res.Send('{"message": "Hello from Nest4D!", "timestamp": "' + 
                   DateTimeToStr(Now) + '"}');
        end)
      
      // Rota com parâmetros
      .Get('/api/hello/:name', 
        procedure(Req: THorseRequest; Res: THorseResponse)
        var
          Name: string;
          Response: TJSONObject;
        begin
          Name := Req.Params['name'];
          
          Response := TJSONObject.Create;
          try
            Response.AddPair('message', 'Hello, ' + Name + '!');
            Response.AddPair('timestamp', DateTimeToStr(Now));
            Response.AddPair('user', Name);
            
            Res.Send(Response.ToString);
          finally
            Response.Free;
          end;
        end)
      
      // Rota POST com body
      .Post('/api/echo', 
        procedure(Req: THorseRequest; Res: THorseResponse)
        var
          Body: TJSONObject;
          Response: TJSONObject;
        begin
          try
            Body := TJSONObject.ParseJSONValue(Req.Body) as TJSONObject;
            if Assigned(Body) then
            begin
              Response := TJSONObject.Create;
              try
                Response.AddPair('echo', Body.Clone as TJSONValue);
                Response.AddPair('received_at', DateTimeToStr(Now));
                Response.AddPair('method', 'POST');
                
                Res.Send(Response.ToString);
              finally
                Response.Free;
              end;
            end
            else
            begin
              Res.Status(400).Send('{"error": "Invalid JSON body"}');
            end;
          finally
            Body.Free;
          end;
        end)
      
      // Rota de status
      .Get('/api/status', 
        procedure(Req: THorseRequest; Res: THorseResponse)
        var
          Status: TJSONObject;
        begin
          Status := TJSONObject.Create;
          try
            Status.AddPair('status', 'online');
            Status.AddPair('server', 'Nest4D Basic Server');
            Status.AddPair('version', '1.0.0');
            Status.AddPair('uptime', DateTimeToStr(Now));
            Status.AddPair('memory_usage', IntToStr(GetHeapStatus.TotalAllocated));
            
            Res.Send(Status.ToString);
          finally
            Status.Free;
          end;
        end)
      
      // Middleware de tratamento de erro global
      .Use(procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
        begin
          try
            Next();
          except
            on E: Exception do
            begin
              WriteLn('Erro capturado: ' + E.Message);
              Res.Status(500).Send('{"error": "Internal server error", "message": "' + 
                                   E.Message + '"}');
            end;
          end;
        end)
      
      // Iniciar servidor na porta 8080
      .Listen(8080);
    
    WriteLn('🚀 Servidor rodando em http://localhost:8080');
    WriteLn('');
    WriteLn('📡 Endpoints disponíveis:');
    WriteLn('  GET    /api/hello');
    WriteLn('  GET    /api/hello/:name');
    WriteLn('  POST   /api/echo');
    WriteLn('  GET    /api/status');
    WriteLn('');
    WriteLn('💡 Exemplos de uso:');
    WriteLn('  curl http://localhost:8080/api/hello');
    WriteLn('  curl http://localhost:8080/api/hello/João');
    WriteLn('  curl -X POST -H "Content-Type: application/json" -d "{\"name\":\"test\"}" http://localhost:8080/api/echo');
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

## 🧪 Testando o Servidor

### 1. Teste com cURL

```bash
# Teste básico
curl http://localhost:8080/api/hello

# Teste com parâmetro
curl http://localhost:8080/api/hello/Maria

# Teste POST com JSON
curl -X POST \
  -H "Content-Type: application/json" \
  -d '{"name":"João", "age":30}' \
  http://localhost:8080/api/echo

# Verificar status do servidor
curl http://localhost:8080/api/status
```

### 2. Respostas Esperadas

**GET /api/hello**
```json
{
  "message": "Hello from Nest4D!",
  "timestamp": "2024-01-15 10:30:45"
}
```

**GET /api/hello/Maria**
```json
{
  "message": "Hello, Maria!",
  "timestamp": "2024-01-15 10:31:20",
  "user": "Maria"
}
```

**POST /api/echo**
```json
{
  "echo": {
    "name": "João",
    "age": 30
  },
  "received_at": "2024-01-15 10:32:10",
  "method": "POST"
}
```

**GET /api/status**
```json
{
  "status": "online",
  "server": "Nest4D Basic Server",
  "version": "1.0.0",
  "uptime": "2024-01-15 10:30:00",
  "memory_usage": "2048576"
}
```

## 🔧 Versão com Interceptors Customizados

### 1. Interceptor de CORS

```pascal
// CorsInterceptor.pas
unit CorsInterceptor;

interface

uses
  nest4d.interceptor.async,
  Horse;

type
  TCorsInterceptor = class(TInterceptor)
  public
    procedure Before(Req: THorseRequest; Res: THorseResponse); override;
  end;

implementation

procedure TCorsInterceptor.Before(Req: THorseRequest; Res: THorseResponse);
begin
  // Configurar headers CORS
  Res.AddHeader('Access-Control-Allow-Origin', '*');
  Res.AddHeader('Access-Control-Allow-Methods', 'GET, POST, PUT, DELETE, OPTIONS');
  Res.AddHeader('Access-Control-Allow-Headers', 'Content-Type, Authorization');
  
  // Responder a requisições OPTIONS
  if Req.MethodType = mtOptions then
  begin
    Res.Status(200).Send('');
    Exit;
  end;
end;

end.
```

### 2. Interceptor de Rate Limiting

```pascal
// RateLimitInterceptor.pas
unit RateLimitInterceptor;

interface

uses
  nest4d.interceptor.async,
  Horse,
  System.SysUtils,
  System.Generics.Collections,
  System.DateUtils;

type
  TRateLimitInfo = record
    RequestCount: Integer;
    WindowStart: TDateTime;
  end;

  TRateLimitInterceptor = class(TInterceptor)
  private
    FClientRequests: TDictionary<string, TRateLimitInfo>;
    FMaxRequests: Integer;
    FWindowMinutes: Integer;
  public
    constructor Create(MaxRequests: Integer = 100; WindowMinutes: Integer = 1);
    destructor Destroy; override;
    procedure Before(Req: THorseRequest; Res: THorseResponse); override;
  end;

implementation

constructor TRateLimitInterceptor.Create(MaxRequests: Integer; WindowMinutes: Integer);
begin
  inherited Create;
  FClientRequests := TDictionary<string, TRateLimitInfo>.Create;
  FMaxRequests := MaxRequests;
  FWindowMinutes := WindowMinutes;
end;

destructor TRateLimitInterceptor.Destroy;
begin
  FClientRequests.Free;
  inherited;
end;

procedure TRateLimitInterceptor.Before(Req: THorseRequest; Res: THorseResponse);
var
  ClientIP: string;
  RateInfo: TRateLimitInfo;
  Now: TDateTime;
begin
  ClientIP := Req.RawWebRequest.RemoteAddr;
  Now := System.SysUtils.Now;
  
  if FClientRequests.TryGetValue(ClientIP, RateInfo) then
  begin
    // Verificar se a janela de tempo expirou
    if MinutesBetween(Now, RateInfo.WindowStart) >= FWindowMinutes then
    begin
      // Reset da janela
      RateInfo.RequestCount := 1;
      RateInfo.WindowStart := Now;
    end
    else
    begin
      // Incrementar contador
      Inc(RateInfo.RequestCount);
      
      // Verificar limite
      if RateInfo.RequestCount > FMaxRequests then
      begin
        Res.Status(429).Send('{"error": "Rate limit exceeded", "retry_after": 60}');
        Exit;
      end;
    end;
  end
  else
  begin
    // Primeiro request do cliente
    RateInfo.RequestCount := 1;
    RateInfo.WindowStart := Now;
  end;
  
  FClientRequests.AddOrSetValue(ClientIP, RateInfo);
  
  // Adicionar headers informativos
  Res.AddHeader('X-RateLimit-Limit', IntToStr(FMaxRequests));
  Res.AddHeader('X-RateLimit-Remaining', IntToStr(FMaxRequests - RateInfo.RequestCount));
  Res.AddHeader('X-RateLimit-Reset', IntToStr(Trunc(IncMinute(RateInfo.WindowStart, FWindowMinutes))));
end;

end.
```

### 3. Servidor com Interceptors Customizados

```pascal
program AdvancedBasicServer;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  nest4d.core.async,
  nest4d.horse.async,
  nest4d.interceptor.async,
  nest4d.logging.async,
  CorsInterceptor,
  RateLimitInterceptor;

begin
  try
    TNest4D
      .Create
      // Interceptors na ordem de execução
      .UseInterceptor<TCorsInterceptor>
      .UseInterceptor<TRateLimitInterceptor>
      .UseInterceptor<TLoggingInterceptor>
      
      // Rotas da API
      .Get('/api/hello', 
        procedure(Req: THorseRequest; Res: THorseResponse)
        begin
          Res.Send('{"message": "Hello with CORS and Rate Limiting!"}');
        end)
      
      .Listen(8080);
    
    WriteLn('🚀 Servidor avançado rodando em http://localhost:8080');
    WriteLn('✅ CORS habilitado');
    WriteLn('⏱️  Rate limiting: 100 req/min por IP');
    WriteLn('📝 Logging habilitado');
    WriteLn('');
    WriteLn('Pressione ENTER para parar...');
    ReadLn;
    
  except
    on E: Exception do
      WriteLn('Erro: ' + E.Message);
  end;
end.
```

## 📊 Monitoramento Básico

### 1. Endpoint de Health Check

```pascal
// Adicionar ao servidor principal
.Get('/health', 
  procedure(Req: THorseRequest; Res: THorseResponse)
  var
    Health: TJSONObject;
    Checks: TJSONArray;
    Check: TJSONObject;
  begin
    Health := TJSONObject.Create;
    Checks := TJSONArray.Create;
    
    try
      // Check básico do servidor
      Check := TJSONObject.Create;
      Check.AddPair('name', 'server');
      Check.AddPair('status', 'healthy');
      Check.AddPair('timestamp', DateTimeToStr(Now));
      Checks.AddElement(Check);
      
      // Check de memória
      Check := TJSONObject.Create;
      Check.AddPair('name', 'memory');
      Check.AddPair('status', 'healthy');
      Check.AddPair('usage_bytes', IntToStr(GetHeapStatus.TotalAllocated));
      Checks.AddElement(Check);
      
      Health.AddPair('status', 'healthy');
      Health.AddPair('timestamp', DateTimeToStr(Now));
      Health.AddPair('checks', Checks);
      
      Res.Send(Health.ToString);
    finally
      Health.Free;
    end;
  end)
```

## 🚀 Próximos Passos

1. **Adicionar Banco de Dados**: Integre com FireDAC ou outro componente de dados
2. **Implementar Autenticação**: Use JWT ou OAuth2
3. **Adicionar Validação**: Implemente validação de entrada
4. **Configurar Logging**: Use arquivos de log estruturados
5. **Deploy**: Configure para produção

## 📚 Recursos Relacionados

- [Exemplo com Resiliência](./resilience.md)
- [Exemplo com Plugins](./plugins.md)
- [Exemplo com Microserviços](./microservices.md)
- [Guia de Arquitetura](../architecture.md)

---

Este exemplo fornece uma base sólida para construir APIs REST com Nest4D, incluindo middleware customizado, tratamento de erros e monitoramento básico.