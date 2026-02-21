unit Test.Nest4D.Resilience.Integration;

interface

uses
  TestFramework,
  System.SysUtils,
  System.Classes,
  System.DateUtils,
  System.Threading,
  System.SyncObjs,
  Nest4D.resilience.interfaces,
  Nest4D.resilience.retry,
  Nest4D.resilience.fallback,
  Nest4D.resilience.circuitbreaker,
  Nest4D.resilience.interceptor,
  Nest4D.resilience.horse;

type
  // Simulador de serviço externo com diferentes comportamentos
  TExternalServiceSimulator = class
  private
    FFailureRate: Double; // 0.0 a 1.0
    FLatency: Cardinal; // em milissegundos
    FIsDown: Boolean;
    FRequestCount: Integer;
    FLock: TCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;
    
    procedure SetFailureRate(const ARate: Double);
    procedure SetLatency(const ALatency: Cardinal);
    procedure SetServiceDown(const ADown: Boolean);
    function SimulateRequest: TRestResponse;
    function GetRequestCount: Integer;
    procedure Reset;
  end;

  // Adapter que usa o simulador
  TSimulatorFrameworkAdapter = class(TInterfacedObject, IRestFrameworkAdapter)
  private
    FSimulator: TExternalServiceSimulator;
  public
    constructor Create(ASimulator: TExternalServiceSimulator);
    
    function ExecuteRequest(const ARequest: TRestRequest): TRestResponse;
    function GetFrameworkName: string;
    function GetFrameworkVersion: string;
    function IsThreadSafe: Boolean;
  end;

  // Testes de cenários reais
  TTestRealWorldScenarios = class(TTestCase)
  private
    FInterceptor: IResilienceInterceptor;
    FSimulator: TExternalServiceSimulator;
    FAdapter: TSimulatorFrameworkAdapter;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestMicroserviceFailover;
    procedure TestDatabaseConnectionPool;
    procedure TestAPIRateLimiting;
    procedure TestNetworkPartition;
    procedure TestCascadingFailureProtection;
    procedure TestLoadBalancingWithFallback;
    procedure TestCircuitBreakerRecovery;
    procedure TestBulkheadIsolationScenario;
  end;

  // Testes de performance e stress
  TTestPerformanceStress = class(TTestCase)
  private
    FInterceptor: IResilienceInterceptor;
    FSimulator: TExternalServiceSimulator;
    FAdapter: TSimulatorFrameworkAdapter;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestHighThroughputScenario;
    procedure TestMemoryLeakPrevention;
    procedure TestConcurrentCircuitBreakers;
    procedure TestRetryStormPrevention;
    procedure TestMetricsAccuracy;
  end;

implementation

{ TExternalServiceSimulator }

constructor TExternalServiceSimulator.Create;
begin
  inherited;
  FLock := TCriticalSection.Create;
  FFailureRate := 0.0;
  FLatency := 100;
  FIsDown := False;
  FRequestCount := 0;
end;

destructor TExternalServiceSimulator.Destroy;
begin
  FLock.Free;
  inherited;
end;

procedure TExternalServiceSimulator.SetFailureRate(const ARate: Double);
begin
  FLock.Enter;
  try
    FFailureRate := ARate;
  finally
    FLock.Leave;
  end;
end;

procedure TExternalServiceSimulator.SetLatency(const ALatency: Cardinal);
begin
  FLock.Enter;
  try
    FLatency := ALatency;
  finally
    FLock.Leave;
  end;
end;

procedure TExternalServiceSimulator.SetServiceDown(const ADown: Boolean);
begin
  FLock.Enter;
  try
    FIsDown := ADown;
  finally
    FLock.Leave;
  end;
end;

function TExternalServiceSimulator.SimulateRequest: TRestResponse;
var
  ShouldFail: Boolean;
begin
  FLock.Enter;
  try
    Inc(FRequestCount);
    
    // Simular latência
    if FLatency > 0 then
      Sleep(FLatency);
    
    // Verificar se o serviço está down
    if FIsDown then
    begin
      raise Exception.Create('Service is down');
    end;
    
    // Simular falhas baseadas na taxa de falha
    ShouldFail := Random < FFailureRate;
    
    if ShouldFail then
    begin
      raise Exception.Create('Simulated service failure');
    end;
    
    // Resposta de sucesso
    Result.StatusCode := 200;
    Result.Content := Format('{"success":true,"request_id":"%d","timestamp":"%s"}', 
      [FRequestCount, FormatDateTime('yyyy-mm-dd hh:nn:ss', Now)]);
    Result.Success := True;
    SetLength(Result.Headers, 1);
    Result.Headers[0].Name := 'Content-Type';
    Result.Headers[0].Value := 'application/json';
  finally
    FLock.Leave;
  end;
end;

function TExternalServiceSimulator.GetRequestCount: Integer;
begin
  FLock.Enter;
  try
    Result := FRequestCount;
  finally
    FLock.Leave;
  end;
end;

procedure TExternalServiceSimulator.Reset;
begin
  FLock.Enter;
  try
    FRequestCount := 0;
    FFailureRate := 0.0;
    FLatency := 100;
    FIsDown := False;
  finally
    FLock.Leave;
  end;
end;

{ TSimulatorFrameworkAdapter }

constructor TSimulatorFrameworkAdapter.Create(ASimulator: TExternalServiceSimulator);
begin
  inherited Create;
  FSimulator := ASimulator;
end;

function TSimulatorFrameworkAdapter.ExecuteRequest(const ARequest: TRestRequest): TRestResponse;
begin
  Result := FSimulator.SimulateRequest;
end;

function TSimulatorFrameworkAdapter.GetFrameworkName: string;
begin
  Result := 'SimulatorFramework';
end;

function TSimulatorFrameworkAdapter.GetFrameworkVersion: string;
begin
  Result := '1.0.0';
end;

function TSimulatorFrameworkAdapter.IsThreadSafe: Boolean;
begin
  Result := True;
end;

{ TTestRealWorldScenarios }

procedure TTestRealWorldScenarios.SetUp;
begin
  inherited;
  FSimulator := TExternalServiceSimulator.Create;
  FAdapter := TSimulatorFrameworkAdapter.Create(FSimulator);
  FInterceptor := TResilienceInterceptor.Create(FAdapter);
end;

procedure TTestRealWorldScenarios.TearDown;
begin
  FInterceptor := nil;
  FAdapter.Free;
  FSimulator.Free;
  inherited;
end;

procedure TTestRealWorldScenarios.TestMicroserviceFailover;
var
  Config: IResilienceConfig;
  FallbackService: IFallbackService;
  Request: TRestRequest;
  Response: TRestResponse;
begin
  // Cenário: Microserviço principal falha, usar serviço de backup
  
  // Configurar fallback para simular serviço de backup
  FallbackService := TFallbackService.Create;
  FallbackService.SetFallbackType(ftServiceCall);
  FallbackService.SetFallbackCallback(
    function(const Req: TRestRequest): TRestResponse
    begin
      Result.StatusCode := 200;
      Result.Content := '{"source":"backup_service","data":"fallback_data"}';
      Result.Success := True;
    end
  );
  
  Config := TResilienceConfigFactory.CreateHighAvailability('/api/users');
  Config.SetFallbackService(FallbackService);
  FInterceptor.AddEndpointConfig('/api/users', Config);
  
  // Simular falha do serviço principal
  FSimulator.SetServiceDown(True);
  
  Request := TRestRequest.Create;
  Request.Endpoint := '/api/users';
  
  Response := FInterceptor.ExecuteWithResilience('/api/users', Request);
  
  CheckEquals(200, Response.StatusCode, 'Should use backup service');
  CheckTrue(Pos('backup_service', Response.Content) > 0, 'Should indicate backup service was used');
end;

procedure TTestRealWorldScenarios.TestDatabaseConnectionPool;
var
  Config: IResilienceConfig;
  Request: TRestRequest;
  Tasks: array[0..19] of ITask;
  SuccessCount: Integer;
  i: Integer;
begin
  // Cenário: Pool de conexões limitado com proteção contra esgotamento
  
  Config := TResilienceConfigFactory.CreateBulkhead('/api/database', 5); // Simular 5 conexões
  FInterceptor.AddEndpointConfig('/api/database', Config);
  
  // Simular latência de banco de dados
  FSimulator.SetLatency(200);
  
  Request := TRestRequest.Create;
  Request.Endpoint := '/api/database';
  
  SuccessCount := 0;
  
  // Executar muitas requisições concorrentes
  for i := 0 to 19 do
  begin
    Tasks[i] := TTask.Create(
      procedure
      var
        LocalRequest: TRestRequest;
        LocalResponse: TRestResponse;
      begin
        LocalRequest := TRestRequest.Create;
        LocalRequest.Endpoint := '/api/database';
        LocalResponse := FInterceptor.ExecuteWithResilience('/api/database', LocalRequest);
        if LocalResponse.Success then
          TInterlocked.Increment(SuccessCount);
      end
    );
    Tasks[i].Start;
  end;
  
  // Aguardar conclusão
  for i := 0 to 19 do
    Tasks[i].Wait;
  
  // Verificar se o pool foi respeitado
  CheckTrue(SuccessCount > 0, 'Some requests should succeed');
  CheckTrue(SuccessCount <= 15, 'Should respect connection pool limits'); // Permitir alguma margem
end;

procedure TTestRealWorldScenarios.TestAPIRateLimiting;
var
  Config: IResilienceConfig;
  Request: TRestRequest;
  Response: TRestResponse;
  i: Integer;
  RejectedCount: Integer;
begin
  // Cenário: API externa com rate limiting
  
  Config := TResilienceConfigFactory.CreateBulkhead('/api/external', 3); // Limite baixo
  Config.SetTimeout(1000); // Timeout rápido
  FInterceptor.AddEndpointConfig('/api/external', Config);
  
  // Simular latência para forçar concorrência
  FSimulator.SetLatency(500);
  
  Request := TRestRequest.Create;
  Request.Endpoint := '/api/external';
  
  RejectedCount := 0;
  
  // Executar requisições rapidamente
  for i := 1 to 10 do
  begin
    Response := FInterceptor.ExecuteWithResilience('/api/external', Request);
    if Response.StatusCode = 429 then // Too Many Requests
      Inc(RejectedCount);
  end;
  
  CheckTrue(RejectedCount > 0, 'Should reject some requests due to rate limiting');
end;

procedure TTestRealWorldScenarios.TestNetworkPartition;
var
  Config: IResilienceConfig;
  Request: TRestRequest;
  Response: TRestResponse;
  StartTime: TDateTime;
begin
  // Cenário: Partição de rede temporária
  
  Config := TResilienceConfigFactory.CreateDefault('/api/remote');
  FInterceptor.AddEndpointConfig('/api/remote', Config);
  
  Request := TRestRequest.Create;
  Request.Endpoint := '/api/remote';
  
  // Simular partição de rede (serviço down)
  FSimulator.SetServiceDown(True);
  
  StartTime := Now;
  
  try
    Response := FInterceptor.ExecuteWithResilience('/api/remote', Request);
    // Se chegou aqui, deve ter usado fallback
    CheckTrue(Response.StatusCode >= 200, 'Should handle network partition gracefully');
  except
    on E: Exception do
    begin
      // Verificar se falhou rapidamente (circuit breaker)
      var ElapsedTime := MilliSecondsBetween(Now, StartTime);
      CheckTrue(ElapsedTime < 10000, 'Should fail fast during network partition');
    end;
  end;
end;

procedure TTestRealWorldScenarios.TestCascadingFailureProtection;
var
  ConfigA, ConfigB, ConfigC: IResilienceConfig;
  Request: TRestRequest;
  i: Integer;
begin
  // Cenário: Proteção contra falhas em cascata
  
  // Configurar três serviços interdependentes
  ConfigA := TResilienceConfigFactory.CreateFastFail('/service-a');
  ConfigB := TResilienceConfigFactory.CreateFastFail('/service-b');
  ConfigC := TResilienceConfigFactory.CreateFastFail('/service-c');
  
  FInterceptor.AddEndpointConfig('/service-a', ConfigA);
  FInterceptor.AddEndpointConfig('/service-b', ConfigB);
  FInterceptor.AddEndpointConfig('/service-c', ConfigC);
  
  // Simular falha alta
  FSimulator.SetFailureRate(0.8);
  
  Request := TRestRequest.Create;
  
  // Executar requisições para todos os serviços
  for i := 1 to 10 do
  begin
    try
      Request.Endpoint := '/service-a';
      FInterceptor.ExecuteWithResilience('/service-a', Request);
    except
      // Ignorar falhas esperadas
    end;
    
    try
      Request.Endpoint := '/service-b';
      FInterceptor.ExecuteWithResilience('/service-b', Request);
    except
      // Ignorar falhas esperadas
    end;
    
    try
      Request.Endpoint := '/service-c';
      FInterceptor.ExecuteWithResilience('/service-c', Request);
    except
      // Ignorar falhas esperadas
    end;
  end;
  
  // Verificar se os circuit breakers estão protegendo
  var CBA := ConfigA.GetCircuitBreaker;
  var CBB := ConfigB.GetCircuitBreaker;
  var CBC := ConfigC.GetCircuitBreaker;
  
  CheckTrue((CBA.GetState = cbsOpen) or (CBB.GetState = cbsOpen) or (CBC.GetState = cbsOpen),
    'At least one circuit breaker should be open to prevent cascading failures');
end;

procedure TTestRealWorldScenarios.TestLoadBalancingWithFallback;
var
  Config: IResilienceConfig;
  FallbackService: IFallbackService;
  Request: TRestRequest;
  Response: TRestResponse;
  i: Integer;
  FallbackUsed: Boolean;
begin
  // Cenário: Load balancing com fallback para cache
  
  FallbackService := TFallbackService.Create;
  FallbackService.SetFallbackType(ftCachedResponse);
  
  // Adicionar resposta ao cache
  var CachedResponse: TRestResponse;
  CachedResponse.StatusCode := 200;
  CachedResponse.Content := '{"source":"cache","data":"cached_data"}';
  CachedResponse.Success := True;
  FallbackService.AddToCache('/api/data', CachedResponse);
  
  Config := TResilienceConfigFactory.CreateHighAvailability('/api/data');
  Config.SetFallbackService(FallbackService);
  FInterceptor.AddEndpointConfig('/api/data', Config);
  
  // Simular falha intermitente (50%)
  FSimulator.SetFailureRate(0.5);
  
  Request := TRestRequest.Create;
  Request.Endpoint := '/api/data';
  
  FallbackUsed := False;
  
  // Executar várias requisições
  for i := 1 to 10 do
  begin
    Response := FInterceptor.ExecuteWithResilience('/api/data', Request);
    if Pos('cache', Response.Content) > 0 then
      FallbackUsed := True;
  end;
  
  CheckTrue(FallbackUsed, 'Should use cached fallback when primary service fails');
end;

procedure TTestRealWorldScenarios.TestCircuitBreakerRecovery;
var
  Config: IResilienceConfig;
  Request: TRestRequest;
  Response: TRestResponse;
  CircuitBreaker: ICircuitBreaker;
  i: Integer;
begin
  // Cenário: Recuperação automática do circuit breaker
  
  Config := TResilienceConfigFactory.CreateDefault('/api/recovery');
  CircuitBreaker := Config.GetCircuitBreaker;
  FInterceptor.AddEndpointConfig('/api/recovery', Config);
  
  Request := TRestRequest.Create;
  Request.Endpoint := '/api/recovery';
  
  // Fase 1: Forçar abertura do circuit breaker
  FSimulator.SetServiceDown(True);
  
  for i := 1 to 5 do
  begin
    try
      FInterceptor.ExecuteWithResilience('/api/recovery', Request);
    except
      // Ignorar falhas esperadas
    end;
  end;
  
  CheckEquals(Ord(cbsOpen), Ord(CircuitBreaker.GetState), 'Circuit should be open');
  
  // Fase 2: Aguardar timeout de recuperação
  Sleep(6000); // Aguardar mais que o timeout padrão
  
  // Fase 3: Restaurar serviço e testar recuperação
  FSimulator.SetServiceDown(False);
  
  Response := FInterceptor.ExecuteWithResilience('/api/recovery', Request);
  CheckEquals(Ord(cbsHalfOpen), Ord(CircuitBreaker.GetState), 'Circuit should be half-open');
  
  // Fase 4: Mais requisições bem-sucedidas devem fechar o circuit
  FInterceptor.ExecuteWithResilience('/api/recovery', Request);
  CheckEquals(Ord(cbsClosed), Ord(CircuitBreaker.GetState), 'Circuit should recover to closed');
end;

procedure TTestRealWorldScenarios.TestBulkheadIsolationScenario;
var
  ConfigCritical, ConfigNonCritical: IResilienceConfig;
  RequestCritical, RequestNonCritical: TRestRequest;
  TasksCritical, TasksNonCritical: array[0..4] of ITask;
  CriticalResults, NonCriticalResults: array[0..4] of Boolean;
  i: Integer;
begin
  // Cenário: Isolamento de recursos críticos vs não-críticos
  
  // Serviço crítico com recursos dedicados
  ConfigCritical := TResilienceConfigFactory.CreateBulkhead('/api/critical', 10);
  
  // Serviço não-crítico com recursos limitados
  ConfigNonCritical := TResilienceConfigFactory.CreateBulkhead('/api/non-critical', 2);
  
  FInterceptor.AddEndpointConfig('/api/critical', ConfigCritical);
  FInterceptor.AddEndpointConfig('/api/non-critical', ConfigNonCritical);
  
  // Simular latência para forçar concorrência
  FSimulator.SetLatency(300);
  
  RequestCritical := TRestRequest.Create;
  RequestCritical.Endpoint := '/api/critical';
  
  RequestNonCritical := TRestRequest.Create;
  RequestNonCritical.Endpoint := '/api/non-critical';
  
  // Executar requisições concorrentes
  for i := 0 to 4 do
  begin
    TasksCritical[i] := TTask.Create(
      procedure
      var
        LocalRequest: TRestRequest;
        Response: TRestResponse;
      begin
        LocalRequest := TRestRequest.Create;
        LocalRequest.Endpoint := '/api/critical';
        Response := FInterceptor.ExecuteWithResilience('/api/critical', LocalRequest);
        CriticalResults[i] := Response.Success;
      end
    );
    
    TasksNonCritical[i] := TTask.Create(
      procedure
      var
        LocalRequest: TRestRequest;
        Response: TRestResponse;
      begin
        LocalRequest := TRestRequest.Create;
        LocalRequest.Endpoint := '/api/non-critical';
        Response := FInterceptor.ExecuteWithResilience('/api/non-critical', LocalRequest);
        NonCriticalResults[i] := Response.Success;
      end
    );
    
    TasksCritical[i].Start;
    TasksNonCritical[i].Start;
  end;
  
  // Aguardar conclusão
  for i := 0 to 4 do
  begin
    TasksCritical[i].Wait;
    TasksNonCritical[i].Wait;
  end;
  
  // Verificar isolamento
  var CriticalSuccessCount := 0;
  var NonCriticalSuccessCount := 0;
  
  for i := 0 to 4 do
  begin
    if CriticalResults[i] then Inc(CriticalSuccessCount);
    if NonCriticalResults[i] then Inc(NonCriticalSuccessCount);
  end;
  
  CheckEquals(5, CriticalSuccessCount, 'Critical service should not be affected by non-critical limits');
  CheckTrue(NonCriticalSuccessCount <= 2, 'Non-critical service should respect its bulkhead limits');
end;

{ TTestPerformanceStress }

procedure TTestPerformanceStress.SetUp;
begin
  inherited;
  FSimulator := TExternalServiceSimulator.Create;
  FAdapter := TSimulatorFrameworkAdapter.Create(FSimulator);
  FInterceptor := TResilienceInterceptor.Create(FAdapter);
end;

procedure TTestPerformanceStress.TearDown;
begin
  FInterceptor := nil;
  FAdapter.Free;
  FSimulator.Free;
  inherited;
end;

procedure TTestPerformanceStress.TestHighThroughputScenario;
var
  Config: IResilienceConfig;
  Request: TRestRequest;
  Tasks: array[0..99] of ITask;
  SuccessCount: Integer;
  StartTime: TDateTime;
  ElapsedTime: Integer;
  i: Integer;
begin
  // Teste de alta throughput
  
  Config := TResilienceConfigFactory.CreateHighAvailability('/api/high-throughput');
  FInterceptor.AddEndpointConfig('/api/high-throughput', Config);
  
  // Configurar para baixa latência
  FSimulator.SetLatency(10);
  
  Request := TRestRequest.Create;
  Request.Endpoint := '/api/high-throughput';
  
  SuccessCount := 0;
  StartTime := Now;
  
  // Executar 100 requisições concorrentes
  for i := 0 to 99 do
  begin
    Tasks[i] := TTask.Create(
      procedure
      var
        LocalRequest: TRestRequest;
        LocalResponse: TRestResponse;
      begin
        LocalRequest := TRestRequest.Create;
        LocalRequest.Endpoint := '/api/high-throughput';
        LocalResponse := FInterceptor.ExecuteWithResilience('/api/high-throughput', LocalRequest);
        if LocalResponse.Success then
          TInterlocked.Increment(SuccessCount);
      end
    );
    Tasks[i].Start;
  end;
  
  // Aguardar conclusão
  for i := 0 to 99 do
    Tasks[i].Wait;
  
  ElapsedTime := MilliSecondsBetween(Now, StartTime);
  
  CheckTrue(SuccessCount >= 95, 'Should handle high throughput with minimal failures');
  CheckTrue(ElapsedTime < 5000, 'Should complete high throughput test quickly');
end;

procedure TTestPerformanceStress.TestMemoryLeakPrevention;
var
  Config: IResilienceConfig;
  Request: TRestRequest;
  i: Integer;
  InitialMemory, FinalMemory: Cardinal;
begin
  // Teste de prevenção de vazamentos de memória
  
  Config := TResilienceConfigFactory.CreateDefault('/api/memory-test');
  FInterceptor.AddEndpointConfig('/api/memory-test', Config);
  
  Request := TRestRequest.Create;
  Request.Endpoint := '/api/memory-test';
  
  // Medir memória inicial
  InitialMemory := GetHeapStatus.TotalAllocated;
  
  // Executar muitas requisições
  for i := 1 to 1000 do
  begin
    try
      FInterceptor.ExecuteWithResilience('/api/memory-test', Request);
    except
      // Ignorar falhas para este teste
    end;
    
    // Forçar garbage collection periodicamente
    if i mod 100 = 0 then
    begin
      System.GC;
      Sleep(10);
    end;
  end;
  
  // Forçar limpeza final
  System.GC;
  Sleep(100);
  
  FinalMemory := GetHeapStatus.TotalAllocated;
  
  // Verificar se não há vazamento significativo (permitir 10% de crescimento)
  var MemoryGrowth := ((FinalMemory - InitialMemory) * 100) div InitialMemory;
  CheckTrue(MemoryGrowth < 10, Format('Memory growth should be minimal, but was %d%%', [MemoryGrowth]));
end;

procedure TTestPerformanceStress.TestConcurrentCircuitBreakers;
var
  Configs: array[0..9] of IResilienceConfig;
  Tasks: array[0..99] of ITask;
  Request: TRestRequest;
  i, j: Integer;
begin
  // Teste de múltiplos circuit breakers concorrentes
  
  // Criar 10 endpoints diferentes
  for i := 0 to 9 do
  begin
    Configs[i] := TResilienceConfigFactory.CreateFastFail(Format('/api/endpoint-%d', [i]));
    FInterceptor.AddEndpointConfig(Format('/api/endpoint-%d', [i]), Configs[i]);
  end;
  
  // Simular falhas altas
  FSimulator.SetFailureRate(0.7);
  
  Request := TRestRequest.Create;
  
  // Executar requisições concorrentes para todos os endpoints
  for i := 0 to 99 do
  begin
    Tasks[i] := TTask.Create(
      procedure
      var
        LocalRequest: TRestRequest;
        EndpointIndex: Integer;
      begin
        EndpointIndex := Random(10);
        LocalRequest := TRestRequest.Create;
        LocalRequest.Endpoint := Format('/api/endpoint-%d', [EndpointIndex]);
        try
          FInterceptor.ExecuteWithResilience(Format('/api/endpoint-%d', [EndpointIndex]), LocalRequest);
        except
          // Ignorar falhas esperadas
        end;
      end
    );
    Tasks[i].Start;
  end;
  
  // Aguardar conclusão
  for i := 0 to 99 do
    Tasks[i].Wait;
  
  // Verificar se pelo menos alguns circuit breakers abriram
  var OpenCircuits := 0;
  for i := 0 to 9 do
  begin
    if Configs[i].GetCircuitBreaker.GetState = cbsOpen then
      Inc(OpenCircuits);
  end;
  
  CheckTrue(OpenCircuits > 0, 'Some circuit breakers should be open due to failures');
end;

procedure TTestPerformanceStress.TestRetryStormPrevention;
var
  Config: IResilienceConfig;
  Request: TRestRequest;
  Tasks: array[0..49] of ITask;
  StartTime: TDateTime;
  ElapsedTime: Integer;
  i: Integer;
begin
  // Teste de prevenção de retry storm
  
  Config := TResilienceConfigFactory.CreateDefault('/api/retry-storm');
  FInterceptor.AddEndpointConfig('/api/retry-storm', Config);
  
  // Configurar para sempre falhar
  FSimulator.SetServiceDown(True);
  
  Request := TRestRequest.Create;
  Request.Endpoint := '/api/retry-storm';
  
  StartTime := Now;
  
  // Executar muitas requisições concorrentes que vão falhar
  for i := 0 to 49 do
  begin
    Tasks[i] := TTask.Create(
      procedure
      var
        LocalRequest: TRestRequest;
      begin
        LocalRequest := TRestRequest.Create;
        LocalRequest.Endpoint := '/api/retry-storm';
        try
          FInterceptor.ExecuteWithResilience('/api/retry-storm', LocalRequest);
        except
          // Ignorar falhas esperadas
        end;
      end
    );
    Tasks[i].Start;
  end;
  
  // Aguardar conclusão
  for i := 0 to 49 do
    Tasks[i].Wait;
  
  ElapsedTime := MilliSecondsBetween(Now, StartTime);
  
  // O circuit breaker deve prevenir retry storm, então deve falhar rapidamente
  CheckTrue(ElapsedTime < 10000, 'Should prevent retry storm and fail fast');
end;

procedure TTestPerformanceStress.TestMetricsAccuracy;
var
  Config: IResilienceConfig;
  Request: TRestRequest;
  Tasks: array[0..99] of ITask;
  Metrics: TResilienceMetrics;
  i: Integer;
begin
  // Teste de precisão das métricas sob carga
  
  Config := TResilienceConfigFactory.CreateDefault('/api/metrics-test');
  FInterceptor.AddEndpointConfig('/api/metrics-test', Config);
  
  // Configurar 50% de falhas
  FSimulator.SetFailureRate(0.5);
  
  Request := TRestRequest.Create;
  Request.Endpoint := '/api/metrics-test';
  
  // Executar 100 requisições concorrentes
  for i := 0 to 99 do
  begin
    Tasks[i] := TTask.Create(
      procedure
      var
        LocalRequest: TRestRequest;
      begin
        LocalRequest := TRestRequest.Create;
        LocalRequest.Endpoint := '/api/metrics-test';
        try
          FInterceptor.ExecuteWithResilience('/api/metrics-test', LocalRequest);
        except
          // Ignorar falhas esperadas
        end;
      end
    );
    Tasks[i].Start;
  end;
  
  // Aguardar conclusão
  for i := 0 to 99 do
    Tasks[i].Wait;
  
  Metrics := FInterceptor.GetMetrics('/api/metrics-test');
  
  // Verificar se as métricas são consistentes
  CheckEquals(100, Metrics.TotalRequests, 'Should track all requests accurately');
  CheckEquals(Metrics.SuccessfulRequests + Metrics.FailedRequests, Metrics.TotalRequests, 
    'Success + Failed should equal Total');
  CheckTrue(Metrics.AverageResponseTime > 0, 'Should track response time');
end;

initialization
  RegisterTest(TTestRealWorldScenarios.Suite);
  RegisterTest(TTestPerformanceStress.Suite);

end.