{
             Nest4D - Development Framework for Delphi

                   Copyright (c) 2023, Isaque Pinheiro
                          All rights reserved.

                    GNU Lesser General Public License
                      Versão 3, 29 de junho de 2007
}

{
  @abstract(Nest4D Framework - Enhanced Monitoring Example)
  @created(01 Mai 2023)
  @author(Isaque Pinheiro <isaquesp@gmail.com>)
  @homepage(https://www.isaquepinheiro.com.br)
  @documentation(https://nest4d-en.docs-br.com)
}

program enhanced_monitoring_example;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Classes,
  System.JSON,
  System.Generics.Collections,
  Horse,
  Horse.Jhonson,
  Horse.CORS,
  Nest4D.metrics,
  Nest4D.logging,
  Nest4D.interceptor,
  Nest4D.health,
  Nest4D.horse,
  Nest4D.exception;

type
  // Exemplo de health check customizado
  TRedisHealthCheck = class(TInterfacedObject, IHealthCheck)
  private
    FName: String;
    FConnectionString: String;
    FTimeout: Integer;
  public
    constructor Create(const AName, AConnectionString: String; ATimeout: Integer = 3000);
    function GetName: String;
    function Check: THealthCheckResult;
    function GetTimeout: Integer;
  end;
  
  // Exemplo de interceptador customizado
  TCustomBusinessInterceptor = class(TInterfacedObject, IRequestInterceptor)
  private
    FLogger: IStructuredLogger;
    FMetricsCollector: IMetricsCollector;
  public
    constructor Create;
    function Intercept(ARequest: THorseRequest; AResponse: THorseResponse; ANext: TProc): Boolean;
  end;

// Implementação do health check customizado
constructor TRedisHealthCheck.Create(const AName, AConnectionString: String; ATimeout: Integer = 3000);
begin
  inherited Create;
  FName := AName;
  FConnectionString := AConnectionString;
  FTimeout := ATimeout;
end;

function TRedisHealthCheck.GetName: String;
begin
  Result := FName;
end;

function TRedisHealthCheck.GetTimeout: Integer;
begin
  Result := FTimeout;
end;

function TRedisHealthCheck.Check: THealthCheckResult;
begin
  Result := THealthCheckResult.Create(FName, hsHealthy);
  
  try
    // Simula verificação de conexão Redis
    Sleep(Random(200) + 50);
    
    if Random(10) = 0 then // 10% de chance de falha
    begin
      Result.Status := hsUnhealthy;
      Result.Message := 'Redis connection failed';
    end
    else
    begin
      Result.Status := hsHealthy;
      Result.Message := 'Redis connection successful';
      Result.Data.Add('connection_string', FConnectionString);
      Result.Data.Add('ping_time_ms', IntToStr(Random(50) + 10));
    end;
  except
    on E: Exception do
    begin
      Result.Status := hsUnhealthy;
      Result.Message := Format('Redis health check failed: %s', [E.Message]);
    end;
  end;
end;

// Implementação do interceptador customizado
constructor TCustomBusinessInterceptor.Create;
begin
  inherited Create;
  FLogger := GetLogger;
  FMetricsCollector := GetMetricsCollector;
end;

function TCustomBusinessInterceptor.Intercept(ARequest: THorseRequest; AResponse: THorseResponse; ANext: TProc): Boolean;
var
  LContext: TLogContext;
  LTags: TDictionary<String, String>;
begin
  Result := True;
  
  LContext := CreateLogContext(ARequest.Headers['X-Request-ID']);
  LContext.Route := ARequest.RawWebRequest.PathInfo;
  LContext.Method := ARequest.MethodType.ToString;
  
  // Exemplo de validação de negócio
  if ARequest.RawWebRequest.PathInfo.StartsWith('/api/admin') then
  begin
    if ARequest.Headers['Authorization'] = '' then
    begin
      FLogger.Warn('Unauthorized access attempt to admin endpoint', LContext);
      
      if Assigned(FMetricsCollector) then
      begin
        LTags := TDictionary<String, String>.Create;
        try
          LTags.Add('endpoint', 'admin');
          LTags.Add('reason', 'missing_auth');
          FMetricsCollector.IncrementCounter('unauthorized_access_attempts', LTags);
        finally
          LTags.Free;
        end;
      end;
      
      AResponse.Status(401)
               .ContentType('application/json')
               .Send('{"error":"Unauthorized","message":"Authorization header required"}');
      Result := False;
      Exit;
    end;
  end;
  
  // Log de acesso a endpoints específicos
  if ARequest.RawWebRequest.PathInfo.StartsWith('/api/sensitive') then
  begin
    FLogger.Info('Access to sensitive endpoint', LContext);
    
    if Assigned(FMetricsCollector) then
    begin
      LTags := TDictionary<String, String>.Create;
      try
        LTags.Add('endpoint_type', 'sensitive');
        FMetricsCollector.IncrementCounter('sensitive_endpoint_access', LTags);
      finally
        LTags.Free;
      end;
    end;
  end;
  
  if Assigned(ANext) then
    ANext();
end;

// Endpoints de exemplo
procedure GetUsers(ARequest: THorseRequest; AResponse: THorseResponse; ANext: TProc);
var
  LUsers: TJSONArray;
  LUser: TJSONObject;
  LLogger: IStructuredLogger;
  LContext: TLogContext;
  LMetrics: IMetricsCollector;
  LTags: TDictionary<String, String>;
begin
  LLogger := Nest4D.logging.GetLogger;
  LMetrics := Nest4D.metrics.GetMetricsCollector;
  
  LContext := CreateLogContext(ARequest.Headers['X-Request-ID']);
  LContext.Route := '/api/users';
  LContext.Method := 'GET';
  
  try
    LLogger.Info('Fetching users list', LContext);
    
    // Simula busca no banco de dados
    Sleep(Random(100) + 50);
    
    LUsers := TJSONArray.Create;
    try
      for var I := 1 to 10 do
      begin
        LUser := TJSONObject.Create;
        LUser.AddPair('id', TJSONNumber.Create(I));
        LUser.AddPair('name', Format('User %d', [I]));
        LUser.AddPair('email', Format('user%d@example.com', [I]));
        LUsers.AddElement(LUser);
      end;
      
      // Métricas de negócio
      if Assigned(LMetrics) then
      begin
        LTags := TDictionary<String, String>.Create;
        try
          LTags.Add('operation', 'list_users');
          LMetrics.IncrementCounter('business_operations_total', LTags);
          LMetrics.SetGauge('users_returned', LUsers.Count, LTags);
        finally
          LTags.Free;
        end;
      end;
      
      AResponse.Status(200)
               .ContentType('application/json')
               .Send(LUsers.ToJSON);
               
      LLogger.Info(Format('Successfully returned %d users', [LUsers.Count]), LContext);
    finally
      LUsers.Free;
    end;
  except
    on E: Exception do
    begin
      LLogger.Error('Failed to fetch users', LContext, E);
      raise EBadRequestException.Create('Failed to fetch users');
    end;
  end;
end;

procedure CreateUser(ARequest: THorseRequest; AResponse: THorseResponse; ANext: TProc);
var
  LUserData: TJSONObject;
  LNewUser: TJSONObject;
  LLogger: IStructuredLogger;
  LContext: TLogContext;
  LMetrics: IMetricsCollector;
  LTags: TDictionary<String, String>;
begin
  LLogger := Nest4D.logging.GetLogger;
  LMetrics := Nest4D.metrics.GetMetricsCollector;
  
  LContext := CreateLogContext(ARequest.Headers['X-Request-ID']);
  LContext.Route := '/api/users';
  LContext.Method := 'POST';
  
  try
    LUserData := TJSONObject.ParseJSONValue(ARequest.Body) as TJSONObject;
    if not Assigned(LUserData) then
      raise EBadRequestException.Create('Invalid JSON data');
      
    try
      LLogger.Info('Creating new user', LContext);
      
      // Simula validação e criação no banco
      Sleep(Random(200) + 100);
      
      if Random(20) = 0 then // 5% de chance de falha
        raise Exception.Create('Database connection failed');
      
      LNewUser := TJSONObject.Create;
      try
        LNewUser.AddPair('id', TJSONNumber.Create(Random(1000) + 1));
        LNewUser.AddPair('name', LUserData.GetValue('name'));
        LNewUser.AddPair('email', LUserData.GetValue('email'));
        LNewUser.AddPair('created_at', FormatDateTime('yyyy-mm-dd"T"hh:nn:ss"Z"', Now));
        
        // Métricas de negócio
        if Assigned(LMetrics) then
        begin
          LTags := TDictionary<String, String>.Create;
          try
            LTags.Add('operation', 'create_user');
            LMetrics.IncrementCounter('business_operations_total', LTags);
            LMetrics.IncrementCounter('users_created_total', LTags);
          finally
            LTags.Free;
          end;
        end;
        
        AResponse.Status(201)
                 .ContentType('application/json')
                 .Send(LNewUser.ToJSON);
                 
        LLogger.Info('User created successfully', LContext);
      finally
        LNewUser.Free;
      end;
    finally
      LUserData.Free;
    end;
  except
    on E: EBadRequestException do
    begin
      LLogger.Warn(E.Message, LContext);
      raise;
    end;
    on E: Exception do
    begin
      LLogger.Error('Failed to create user', LContext, E);
      raise EBadRequestException.Create('Failed to create user');
    end;
  end;
end;

procedure SlowEndpoint(ARequest: THorseRequest; AResponse: THorseResponse; ANext: TProc);
var
  LLogger: IStructuredLogger;
  LContext: TLogContext;
begin
  LLogger := Nest4D.logging.GetLogger;
  
  LContext := CreateLogContext(ARequest.Headers['X-Request-ID']);
  LContext.Route := '/api/slow';
  LContext.Method := 'GET';
  
  LLogger.Info('Processing slow endpoint', LContext);
  
  // Simula processamento lento
  Sleep(2000 + Random(1000));
  
  AResponse.Status(200)
           .ContentType('application/json')
           .Send('{"message":"Slow operation completed","duration":"2-3 seconds"}');
end;

procedure ErrorEndpoint(ARequest: THorseRequest; AResponse: THorseResponse; ANext: TProc);
begin
  // Simula diferentes tipos de erro
  case Random(3) of
    0: raise EBadRequestException.Create('Invalid request parameters');
    1: raise EUnauthorizedException.Create('Access denied');
    2: raise Exception.Create('Unexpected server error');
  end;
end;

procedure AdminEndpoint(ARequest: THorseRequest; AResponse: THorseResponse; ANext: TProc);
begin
  AResponse.Status(200)
           .ContentType('application/json')
           .Send('{"message":"Admin access granted","data":"sensitive information"}');
end;

procedure ConfigureCustomHealthChecks;
var
  LHealthService: IHealthService;
begin
  LHealthService := Nest4D.health.GetHealthService;
  
  // Adiciona health checks customizados
  LHealthService.RegisterHealthCheck(TRedisHealthCheck.Create('redis', 'redis://localhost:6379'));
  LHealthService.RegisterHealthCheck(TDatabaseHealthCheck.Create('database', 'Server=localhost;Database=myapp'));
  LHealthService.RegisterHealthCheck(THttpEndpointHealthCheck.Create('external_api', 'https://api.example.com/health'));
  
  // Health check customizado com função lambda
  LHealthService.RegisterHealthCheck(
    TCustomHealthCheck.Create('custom_business_rule', 
      function: THealthCheckResult
      begin
        Result := THealthCheckResult.Create('custom_business_rule', hsHealthy);
        
        // Simula verificação de regra de negócio
        if Random(10) = 0 then
        begin
          Result.Status := hsDegraded;
          Result.Message := 'Business rule validation warning';
        end
        else
        begin
          Result.Message := 'All business rules are healthy';
        end;
        
        Result.Data.Add('last_check', FormatDateTime('yyyy-mm-dd hh:nn:ss', Now));
        Result.Data.Add('rule_version', '1.2.3');
      end));
end;

procedure ConfigureCustomInterceptors;
var
  LInterceptorManager: TInterceptorManager;
begin
  LInterceptorManager := Nest4D.interceptor.GetInterceptorManager;
  
  // Adiciona interceptador customizado
  LInterceptorManager.AddRequestInterceptor(TCustomBusinessInterceptor.Create);
end;

procedure ConfigureCustomMetrics;
var
  LMetrics: IMetricsCollector;
  LTags: TDictionary<String, String>;
begin
  LMetrics := Nest4D.metrics.GetMetricsCollector;
  
  if Assigned(LMetrics) then
  begin
    LTags := TDictionary<String, String>.Create;
    try
      LTags.Add('app_name', 'enhanced_monitoring_example');
      LTags.Add('version', '1.0.0');
      
      // Métricas iniciais da aplicação
      LMetrics.SetGauge('app_info', 1, LTags);
      LMetrics.SetGauge('app_start_time', DateTimeToUnix(Now), LTags);
    finally
      LTags.Free;
    end;
  end;
end;

var
  LApp: THorse;
  LLogger: IStructuredLogger;
  LContext: TLogContext;
  
begin
  try
    WriteLn('=== Nest4D Enhanced Monitoring Example ===');
    WriteLn('');
    
    LApp := THorse.Create;
    
    // Middlewares básicos do Horse
    LApp.Use(Jhonson());
    LApp.Use(CORS());
    
    // Configura o middleware enhanced do Nest4D
    LApp.Use(Nest4D_Horse);
    
    // Configura componentes customizados
    ConfigureCustomHealthChecks;
    ConfigureCustomInterceptors;
    ConfigureCustomMetrics;
    
    // Configura rotas da aplicação
    LApp.Get('/api/users', GetUsers);
    LApp.Post('/api/users', CreateUser);
    LApp.Get('/api/slow', SlowEndpoint);
    LApp.Get('/api/error', ErrorEndpoint);
    LApp.Get('/api/admin/dashboard', AdminEndpoint);
    LApp.Get('/api/sensitive/data', 
      procedure(ARequest: THorseRequest; AResponse: THorseResponse; ANext: TProc)
      begin
        AResponse.Status(200)
                 .ContentType('application/json')
                 .Send('{"sensitive_data":"This is sensitive information"}');
      end);
    
    // Log de inicialização
    LLogger := TNest4DEnhancedMiddleware.GetLogger;
    LContext := CreateLogContext;
    LLogger.Info('Enhanced monitoring example server starting', LContext);
    
    WriteLn('Server starting on port 9000...');
    WriteLn('');
    WriteLn('Available endpoints:');
    WriteLn('  GET  /api/users           - List users');
    WriteLn('  POST /api/users           - Create user');
    WriteLn('  GET  /api/slow            - Slow endpoint (2-3s)');
    WriteLn('  GET  /api/error           - Random error endpoint');
    WriteLn('  GET  /api/admin/dashboard - Admin endpoint (requires auth)');
    WriteLn('  GET  /api/sensitive/data  - Sensitive endpoint');
    WriteLn('');
    WriteLn('Monitoring endpoints:');
    WriteLn('  GET  /health              - Health check');
    WriteLn('  GET  /health/detailed     - Detailed health check');
    WriteLn('  GET  /health/ready        - Readiness probe');
    WriteLn('  GET  /health/live         - Liveness probe');
    WriteLn('  GET  /metrics             - Metrics (JSON)');
    WriteLn('  GET  /metrics/prometheus  - Metrics (Prometheus format)');
    WriteLn('');
    WriteLn('Logs are written to:');
    WriteLn('  - Console (structured)');
    WriteLn('  - logs\Nest4D.log (text format)');
    WriteLn('  - logs\Nest4D.json (JSON format)');
    WriteLn('');
    WriteLn('Press Ctrl+C to stop the server');
    WriteLn('');
    
    LApp.Listen(9000);
    
  except
    on E: Exception do
    begin
      WriteLn(Format('Error starting server: %s', [E.Message]));
      if Assigned(LLogger) then
        LLogger.Fatal('Failed to start server', LContext, E);
    end;
  end;
end.