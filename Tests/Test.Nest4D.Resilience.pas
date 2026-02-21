unit Test.Nest4D.Resilience;

interface

uses
  TestFramework,
  System.SysUtils,
  System.Classes,
  System.DateUtils,
  System.Threading,
  Nest4D.resilience.interfaces,
  Nest4D.resilience.retry,
  Nest4D.resilience.fallback,
  Nest4D.resilience.circuitbreaker,
  Nest4D.resilience.interceptor,
  Nest4D.resilience.horse;

type
  // Mock Framework Adapter para testes
  TMockFrameworkAdapter = class(TInterfacedObject, IRestFrameworkAdapter)
  private
    FShouldFail: Boolean;
    FFailureCount: Integer;
    FCurrentFailures: Integer;
    FExecutionTime: Cardinal;
  public
    constructor Create(AShouldFail: Boolean = False; AFailureCount: Integer = 0; AExecutionTime: Cardinal = 100);
    
    // IRestFrameworkAdapter
    function ExecuteRequest(const ARequest: TRestRequest): TRestResponse;
    function GetFrameworkName: string;
    function GetFrameworkVersion: string;
    function IsThreadSafe: Boolean;
    
    // Métodos para controle dos testes
    procedure SetShouldFail(const AValue: Boolean);
    procedure SetFailureCount(const AValue: Integer);
    procedure ResetFailures;
  end;

  // Testes para TRetryPolicy
  TTestRetryPolicy = class(TTestCase)
  private
    FRetryPolicy: IRetryPolicy;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCreateDefault;
    procedure TestCreateAggressive;
    procedure TestCreateConservative;
    procedure TestFixedDelayStrategy;
    procedure TestLinearDelayStrategy;
    procedure TestExponentialDelayStrategy;
    procedure TestMaxAttemptsLimit;
    procedure TestJitterCalculation;
    procedure TestRetryCallback;
  end;

  // Testes para TCircuitBreaker
  TTestCircuitBreaker = class(TTestCase)
  private
    FCircuitBreaker: ICircuitBreaker;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCreateDefault;
    procedure TestCreateHighAvailability;
    procedure TestCreateFastFail;
    procedure TestClosedState;
    procedure TestOpenState;
    procedure TestHalfOpenState;
    procedure TestFailureThreshold;
    procedure TestRecoveryTimeout;
    procedure TestSuccessThreshold;
    procedure TestMetricsTracking;
    procedure TestThreadSafety;
  end;

  // Testes para TFallbackService
  TTestFallbackService = class(TTestCase)
  private
    FFallbackService: IFallbackService;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestStaticResponse;
    procedure TestCachedResponse;
    procedure TestServiceCall;
    procedure TestCustomCallback;
    procedure TestFallbackChain;
    procedure TestErrorHandling;
  end;

  // Testes para TResilienceInterceptor
  TTestResilienceInterceptor = class(TTestCase)
  private
    FInterceptor: IResilienceInterceptor;
    FMockAdapter: TMockFrameworkAdapter;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestExecuteWithResilience;
    procedure TestEndpointConfiguration;
    procedure TestDefaultConfiguration;
    procedure TestConcurrencyLimit;
    procedure TestTimeoutHandling;
    procedure TestMetricsCollection;
    procedure TestRetryWithFallback;
    procedure TestCircuitBreakerIntegration;
    procedure TestThreadSafety;
  end;

  // Testes para TResilienceConfigFactory
  TTestResilienceConfigFactory = class(TTestCase)
  published
    procedure TestCreateDefault;
    procedure TestCreateHighAvailability;
    procedure TestCreateFastFail;
    procedure TestCreateBulkhead;
    procedure TestConfigurationProperties;
  end;

  // Testes de integração
  TTestResilienceIntegration = class(TTestCase)
  private
    FInterceptor: IResilienceInterceptor;
    FMockAdapter: TMockFrameworkAdapter;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestNetworkFailureRecovery;
    procedure TestServiceOverload;
    procedure TestGracefulDegradation;
    procedure TestAutomaticRecovery;
    procedure TestCascadingFailures;
    procedure TestBulkheadIsolation;
  end;

implementation

{ TMockFrameworkAdapter }

constructor TMockFrameworkAdapter.Create(AShouldFail: Boolean; AFailureCount: Integer; AExecutionTime: Cardinal);
begin
  inherited Create;
  FShouldFail := AShouldFail;
  FFailureCount := AFailureCount;
  FCurrentFailures := 0;
  FExecutionTime := AExecutionTime;
end;

function TMockFrameworkAdapter.ExecuteRequest(const ARequest: TRestRequest): TRestResponse;
begin
  // Simular tempo de execução
  if FExecutionTime > 0 then
    Sleep(FExecutionTime);
  
  // Simular falhas
  if FShouldFail or (FCurrentFailures < FFailureCount) then
  begin
    Inc(FCurrentFailures);
    raise Exception.Create('Mock failure for testing');
  end;
  
  // Resposta de sucesso
  Result.StatusCode := 200;
  Result.Content := '{"status":"success","data":"mock response"}';
  Result.Success := True;
  SetLength(Result.Headers, 1);
  Result.Headers[0].Name := 'Content-Type';
  Result.Headers[0].Value := 'application/json';
end;

function TMockFrameworkAdapter.GetFrameworkName: string;
begin
  Result := 'MockFramework';
end;

function TMockFrameworkAdapter.GetFrameworkVersion: string;
begin
  Result := '1.0.0';
end;

function TMockFrameworkAdapter.IsThreadSafe: Boolean;
begin
  Result := True;
end;

procedure TMockFrameworkAdapter.SetShouldFail(const AValue: Boolean);
begin
  FShouldFail := AValue;
end;

procedure TMockFrameworkAdapter.SetFailureCount(const AValue: Integer);
begin
  FFailureCount := AValue;
  FCurrentFailures := 0;
end;

procedure TMockFrameworkAdapter.ResetFailures;
begin
  FCurrentFailures := 0;
end;

{ TTestRetryPolicy }

procedure TTestRetryPolicy.SetUp;
begin
  inherited;
  FRetryPolicy := TRetryPolicyFactory.CreateDefault;
end;

procedure TTestRetryPolicy.TearDown;
begin
  FRetryPolicy := nil;
  inherited;
end;

procedure TTestRetryPolicy.TestCreateDefault;
begin
  CheckNotNull(FRetryPolicy, 'RetryPolicy should not be nil');
  CheckEquals(3, FRetryPolicy.GetMaxAttempts, 'Default max attempts should be 3');
  CheckEquals(Ord(rsExponential), Ord(FRetryPolicy.GetStrategy), 'Default strategy should be exponential');
end;

procedure TTestRetryPolicy.TestCreateAggressive;
var
  Policy: IRetryPolicy;
begin
  Policy := TRetryPolicyFactory.CreateAggressive;
  CheckNotNull(Policy, 'Aggressive policy should not be nil');
  CheckTrue(Policy.GetMaxAttempts >= 5, 'Aggressive policy should have more attempts');
end;

procedure TTestRetryPolicy.TestCreateConservative;
var
  Policy: IRetryPolicy;
begin
  Policy := TRetryPolicyFactory.CreateConservative;
  CheckNotNull(Policy, 'Conservative policy should not be nil');
  CheckTrue(Policy.GetMaxAttempts <= 2, 'Conservative policy should have fewer attempts');
end;

procedure TTestRetryPolicy.TestFixedDelayStrategy;
var
  Policy: TRetryPolicy;
  Delay1, Delay2: Cardinal;
begin
  Policy := TRetryPolicy.Create;
  try
    Policy.SetStrategy(rsFixed);
    Policy.SetBaseDelay(1000);
    
    Delay1 := Policy.CalculateDelay(1);
    Delay2 := Policy.CalculateDelay(2);
    
    CheckEquals(1000, Delay1, 'Fixed delay should be constant');
    CheckEquals(1000, Delay2, 'Fixed delay should be constant');
  finally
    Policy.Free;
  end;
end;

procedure TTestRetryPolicy.TestLinearDelayStrategy;
var
  Policy: TRetryPolicy;
  Delay1, Delay2: Cardinal;
begin
  Policy := TRetryPolicy.Create;
  try
    Policy.SetStrategy(rsLinear);
    Policy.SetBaseDelay(1000);
    
    Delay1 := Policy.CalculateDelay(1);
    Delay2 := Policy.CalculateDelay(2);
    
    CheckEquals(1000, Delay1, 'First linear delay should be base delay');
    CheckEquals(2000, Delay2, 'Second linear delay should be 2x base delay');
  finally
    Policy.Free;
  end;
end;

procedure TTestRetryPolicy.TestExponentialDelayStrategy;
var
  Policy: TRetryPolicy;
  Delay1, Delay2, Delay3: Cardinal;
begin
  Policy := TRetryPolicy.Create;
  try
    Policy.SetStrategy(rsExponential);
    Policy.SetBaseDelay(1000);
    
    Delay1 := Policy.CalculateDelay(1);
    Delay2 := Policy.CalculateDelay(2);
    Delay3 := Policy.CalculateDelay(3);
    
    CheckEquals(1000, Delay1, 'First exponential delay should be base delay');
    CheckEquals(2000, Delay2, 'Second exponential delay should be 2x base delay');
    CheckEquals(4000, Delay3, 'Third exponential delay should be 4x base delay');
  finally
    Policy.Free;
  end;
end;

procedure TTestRetryPolicy.TestMaxAttemptsLimit;
var
  Policy: TRetryPolicy;
begin
  Policy := TRetryPolicy.Create;
  try
    Policy.SetMaxAttempts(3);
    CheckEquals(3, Policy.GetMaxAttempts, 'Max attempts should be set correctly');
  finally
    Policy.Free;
  end;
end;

procedure TTestRetryPolicy.TestJitterCalculation;
var
  Policy: TRetryPolicy;
  Delay1, Delay2: Cardinal;
begin
  Policy := TRetryPolicy.Create;
  try
    Policy.SetStrategy(rsFixed);
    Policy.SetBaseDelay(1000);
    Policy.SetJitter(True);
    
    Delay1 := Policy.CalculateDelay(1);
    Delay2 := Policy.CalculateDelay(1);
    
    // Com jitter, os delays devem ser diferentes
    CheckTrue(Delay1 <> Delay2, 'Jitter should make delays different');
    CheckTrue(Delay1 >= 500, 'Jitter delay should be at least 50% of base');
    CheckTrue(Delay1 <= 1500, 'Jitter delay should be at most 150% of base');
  finally
    Policy.Free;
  end;
end;

procedure TTestRetryPolicy.TestRetryCallback;
var
  Policy: TRetryPolicy;
  CallbackCalled: Boolean;
begin
  Policy := TRetryPolicy.Create;
  try
    CallbackCalled := False;
    Policy.SetRetryCallback(
      procedure(Attempt: Integer; const Error: Exception)
      begin
        CallbackCalled := True;
      end
    );
    
    // Simular chamada do callback
    if Assigned(Policy.GetRetryCallback) then
      Policy.GetRetryCallback(1, Exception.Create('Test'));
    
    CheckTrue(CallbackCalled, 'Retry callback should be called');
  finally
    Policy.Free;
  end;
end;

{ TTestCircuitBreaker }

procedure TTestCircuitBreaker.SetUp;
begin
  inherited;
  FCircuitBreaker := TCircuitBreakerFactory.CreateDefault('test-endpoint');
end;

procedure TTestCircuitBreaker.TearDown;
begin
  FCircuitBreaker := nil;
  inherited;
end;

procedure TTestCircuitBreaker.TestCreateDefault;
begin
  CheckNotNull(FCircuitBreaker, 'CircuitBreaker should not be nil');
  CheckEquals(Ord(cbsClosed), Ord(FCircuitBreaker.GetState), 'Initial state should be Closed');
end;

procedure TTestCircuitBreaker.TestCreateHighAvailability;
var
  CB: ICircuitBreaker;
begin
  CB := TCircuitBreakerFactory.CreateHighAvailability('test-ha');
  CheckNotNull(CB, 'High availability circuit breaker should not be nil');
end;

procedure TTestCircuitBreaker.TestCreateFastFail;
var
  CB: ICircuitBreaker;
begin
  CB := TCircuitBreakerFactory.CreateFastFail('test-ff');
  CheckNotNull(CB, 'Fast fail circuit breaker should not be nil');
end;

procedure TTestCircuitBreaker.TestClosedState;
begin
  CheckEquals(Ord(cbsClosed), Ord(FCircuitBreaker.GetState), 'Should start in Closed state');
  CheckTrue(FCircuitBreaker.CanExecute, 'Should allow execution in Closed state');
end;

procedure TTestCircuitBreaker.TestOpenState;
var
  i: Integer;
begin
  // Forçar abertura do circuit breaker com falhas
  for i := 1 to 10 do
    FCircuitBreaker.RecordFailure;
  
  CheckEquals(Ord(cbsOpen), Ord(FCircuitBreaker.GetState), 'Should be in Open state after failures');
  CheckFalse(FCircuitBreaker.CanExecute, 'Should not allow execution in Open state');
end;

procedure TTestCircuitBreaker.TestHalfOpenState;
var
  i: Integer;
  CB: TCircuitBreaker;
begin
  CB := TCircuitBreaker.Create('test', 3, 1000, 2);
  try
    // Forçar abertura
    for i := 1 to 5 do
      CB.RecordFailure;
    
    CheckEquals(Ord(cbsOpen), Ord(CB.GetState), 'Should be Open');
    
    // Aguardar timeout de recuperação
    Sleep(1100);
    
    // Primeira tentativa deve colocar em Half-Open
    CheckTrue(CB.CanExecute, 'Should allow first attempt after timeout');
    CheckEquals(Ord(cbsHalfOpen), Ord(CB.GetState), 'Should be in Half-Open state');
  finally
    CB.Free;
  end;
end;

procedure TTestCircuitBreaker.TestFailureThreshold;
var
  CB: TCircuitBreaker;
  i: Integer;
begin
  CB := TCircuitBreaker.Create('test', 3, 5000, 2);
  try
    // Registrar falhas até o limite
    for i := 1 to 2 do
    begin
      CB.RecordFailure;
      CheckEquals(Ord(cbsClosed), Ord(CB.GetState), 'Should remain Closed before threshold');
    end;
    
    // Falha que deve abrir o circuit
    CB.RecordFailure;
    CheckEquals(Ord(cbsOpen), Ord(CB.GetState), 'Should be Open after threshold');
  finally
    CB.Free;
  end;
end;

procedure TTestCircuitBreaker.TestRecoveryTimeout;
var
  CB: TCircuitBreaker;
  i: Integer;
begin
  CB := TCircuitBreaker.Create('test', 2, 500, 1);
  try
    // Forçar abertura
    for i := 1 to 3 do
      CB.RecordFailure;
    
    CheckFalse(CB.CanExecute, 'Should not execute immediately after opening');
    
    // Aguardar timeout
    Sleep(600);
    
    CheckTrue(CB.CanExecute, 'Should allow execution after recovery timeout');
  finally
    CB.Free;
  end;
end;

procedure TTestCircuitBreaker.TestSuccessThreshold;
var
  CB: TCircuitBreaker;
  i: Integer;
begin
  CB := TCircuitBreaker.Create('test', 2, 500, 2);
  try
    // Forçar abertura
    for i := 1 to 3 do
      CB.RecordFailure;
    
    // Aguardar e entrar em Half-Open
    Sleep(600);
    CheckTrue(CB.CanExecute, 'Should allow execution');
    
    // Registrar sucessos para fechar
    CB.RecordSuccess;
    CheckEquals(Ord(cbsHalfOpen), Ord(CB.GetState), 'Should remain Half-Open after first success');
    
    CB.RecordSuccess;
    CheckEquals(Ord(cbsClosed), Ord(CB.GetState), 'Should be Closed after success threshold');
  finally
    CB.Free;
  end;
end;

procedure TTestCircuitBreaker.TestMetricsTracking;
var
  Metrics: TCircuitBreakerMetrics;
begin
  FCircuitBreaker.RecordSuccess;
  FCircuitBreaker.RecordFailure;
  
  Metrics := FCircuitBreaker.GetMetrics;
  CheckEquals(1, Metrics.SuccessCount, 'Should track success count');
  CheckEquals(1, Metrics.FailureCount, 'Should track failure count');
  CheckEquals(2, Metrics.TotalRequests, 'Should track total requests');
end;

procedure TTestCircuitBreaker.TestThreadSafety;
var
  Tasks: array[0..9] of ITask;
  i: Integer;
begin
  // Executar operações concorrentes
  for i := 0 to 9 do
  begin
    Tasks[i] := TTask.Create(
      procedure
      var
        j: Integer;
      begin
        for j := 1 to 100 do
        begin
          if j mod 2 = 0 then
            FCircuitBreaker.RecordSuccess
          else
            FCircuitBreaker.RecordFailure;
        end;
      end
    );
    Tasks[i].Start;
  end;
  
  // Aguardar conclusão
  for i := 0 to 9 do
    Tasks[i].Wait;
  
  // Verificar se as métricas são consistentes
  var Metrics := FCircuitBreaker.GetMetrics;
  CheckEquals(1000, Metrics.TotalRequests, 'Should track all requests correctly');
end;

{ TTestFallbackService }

procedure TTestFallbackService.SetUp;
begin
  inherited;
  FFallbackService := TFallbackService.Create;
end;

procedure TTestFallbackService.TearDown;
begin
  FFallbackService := nil;
  inherited;
end;

procedure TTestFallbackService.TestStaticResponse;
var
  Response: TRestResponse;
begin
  FFallbackService.SetFallbackType(ftStaticResponse);
  FFallbackService.SetStaticResponse('{"fallback":true}');
  
  Response := FFallbackService.ExecuteFallback(TRestRequest.Create);
  CheckEquals(200, Response.StatusCode, 'Static fallback should return 200');
  CheckTrue(Pos('fallback', Response.Content) > 0, 'Should contain fallback content');
end;

procedure TTestFallbackService.TestCachedResponse;
var
  Request: TRestRequest;
  Response: TRestResponse;
begin
  Request := TRestRequest.Create;
  Request.Endpoint := '/test';
  
  FFallbackService.SetFallbackType(ftCachedResponse);
  
  // Primeiro, adicionar uma resposta ao cache
  Response.StatusCode := 200;
  Response.Content := '{"cached":true}';
  Response.Success := True;
  FFallbackService.AddToCache('/test', Response);
  
  // Executar fallback
  Response := FFallbackService.ExecuteFallback(Request);
  CheckEquals(200, Response.StatusCode, 'Cached fallback should return cached status');
  CheckTrue(Pos('cached', Response.Content) > 0, 'Should contain cached content');
end;

procedure TTestFallbackService.TestServiceCall;
var
  Response: TRestResponse;
  CallbackExecuted: Boolean;
begin
  CallbackExecuted := False;
  
  FFallbackService.SetFallbackType(ftServiceCall);
  FFallbackService.SetFallbackCallback(
    function(const Request: TRestRequest): TRestResponse
    begin
      CallbackExecuted := True;
      Result.StatusCode := 200;
      Result.Content := '{"service_fallback":true}';
      Result.Success := True;
    end
  );
  
  Response := FFallbackService.ExecuteFallback(TRestRequest.Create);
  CheckTrue(CallbackExecuted, 'Fallback callback should be executed');
  CheckEquals(200, Response.StatusCode, 'Service fallback should return callback result');
end;

procedure TTestFallbackService.TestCustomCallback;
var
  Response: TRestResponse;
  CustomExecuted: Boolean;
begin
  CustomExecuted := False;
  
  FFallbackService.SetFallbackType(ftCustom);
  FFallbackService.SetFallbackCallback(
    function(const Request: TRestRequest): TRestResponse
    begin
      CustomExecuted := True;
      Result.StatusCode := 202;
      Result.Content := '{"custom":true}';
      Result.Success := True;
    end
  );
  
  Response := FFallbackService.ExecuteFallback(TRestRequest.Create);
  CheckTrue(CustomExecuted, 'Custom callback should be executed');
  CheckEquals(202, Response.StatusCode, 'Custom fallback should return custom result');
end;

procedure TTestFallbackService.TestFallbackChain;
var
  Service1, Service2: IFallbackService;
  Response: TRestResponse;
begin
  // Criar cadeia de fallbacks
  Service1 := TFallbackService.Create;
  Service2 := TFallbackService.Create;
  
  Service1.SetFallbackType(ftServiceCall);
  Service1.SetFallbackCallback(
    function(const Request: TRestRequest): TRestResponse
    begin
      // Simular falha no primeiro fallback
      raise Exception.Create('First fallback failed');
    end
  );
  
  Service2.SetFallbackType(ftStaticResponse);
  Service2.SetStaticResponse('{"final_fallback":true}');
  
  Service1.SetNextFallback(Service2);
  
  Response := Service1.ExecuteFallback(TRestRequest.Create);
  CheckEquals(200, Response.StatusCode, 'Should use second fallback when first fails');
  CheckTrue(Pos('final_fallback', Response.Content) > 0, 'Should contain final fallback content');
end;

procedure TTestFallbackService.TestErrorHandling;
var
  Response: TRestResponse;
begin
  FFallbackService.SetFallbackType(ftServiceCall);
  FFallbackService.SetFallbackCallback(
    function(const Request: TRestRequest): TRestResponse
    begin
      raise Exception.Create('Fallback error');
    end
  );
  
  Response := FFallbackService.ExecuteFallback(TRestRequest.Create);
  CheckEquals(500, Response.StatusCode, 'Should return error status when fallback fails');
  CheckFalse(Response.Success, 'Should mark response as failed');
end;

{ TTestResilienceInterceptor }

procedure TTestResilienceInterceptor.SetUp;
begin
  inherited;
  FMockAdapter := TMockFrameworkAdapter.Create;
  FInterceptor := TResilienceInterceptor.Create(FMockAdapter);
end;

procedure TTestResilienceInterceptor.TearDown;
begin
  FInterceptor := nil;
  FMockAdapter := nil;
  inherited;
end;

procedure TTestResilienceInterceptor.TestExecuteWithResilience;
var
  Request: TRestRequest;
  Response: TRestResponse;
begin
  Request := TRestRequest.Create;
  Request.Endpoint := '/test';
  
  Response := FInterceptor.ExecuteWithResilience('/test', Request);
  CheckEquals(200, Response.StatusCode, 'Should execute successfully');
  CheckTrue(Response.Success, 'Response should be marked as successful');
end;

procedure TTestResilienceInterceptor.TestEndpointConfiguration;
var
  Config: IResilienceConfig;
begin
  Config := TResilienceConfigFactory.CreateHighAvailability('/api/test');
  FInterceptor.AddEndpointConfig('/api/test', Config);
  
  var RetrievedConfig := FInterceptor.GetEndpointConfig('/api/test');
  CheckNotNull(RetrievedConfig, 'Should retrieve endpoint configuration');
  CheckEquals('/api/test', RetrievedConfig.GetEndpoint, 'Should return correct endpoint');
end;

procedure TTestResilienceInterceptor.TestDefaultConfiguration;
var
  DefaultConfig: IResilienceConfig;
  RetrievedConfig: IResilienceConfig;
begin
  DefaultConfig := TResilienceConfigFactory.CreateFastFail('*');
  FInterceptor.SetDefaultConfig(DefaultConfig);
  
  RetrievedConfig := FInterceptor.GetEndpointConfig('/unknown/endpoint');
  CheckNotNull(RetrievedConfig, 'Should return default config for unknown endpoint');
end;

procedure TTestResilienceInterceptor.TestConcurrencyLimit;
var
  Config: IResilienceConfig;
  Request: TRestRequest;
  Tasks: array[0..4] of ITask;
  Results: array[0..4] of TRestResponse;
  i: Integer;
begin
  // Configurar limite de concorrência baixo
  Config := TResilienceConfigFactory.CreateBulkhead('/limited', 2);
  FInterceptor.AddEndpointConfig('/limited', Config);
  
  // Configurar mock para execução lenta
  FMockAdapter := TMockFrameworkAdapter.Create(False, 0, 500);
  FInterceptor := TResilienceInterceptor.Create(FMockAdapter);
  FInterceptor.AddEndpointConfig('/limited', Config);
  
  Request := TRestRequest.Create;
  Request.Endpoint := '/limited';
  
  // Executar múltiplas requisições concorrentes
  for i := 0 to 4 do
  begin
    Tasks[i] := TTask.Create(
      procedure
      var
        LocalRequest: TRestRequest;
      begin
        LocalRequest := TRestRequest.Create;
        LocalRequest.Endpoint := '/limited';
        Results[i] := FInterceptor.ExecuteWithResilience('/limited', LocalRequest);
      end
    );
    Tasks[i].Start;
  end;
  
  // Aguardar conclusão
  for i := 0 to 4 do
    Tasks[i].Wait;
  
  // Verificar se algumas requisições foram rejeitadas por limite de concorrência
  var RejectedCount := 0;
  for i := 0 to 4 do
  begin
    if Results[i].StatusCode = 429 then // Too Many Requests
      Inc(RejectedCount);
  end;
  
  CheckTrue(RejectedCount > 0, 'Some requests should be rejected due to concurrency limit');
end;

procedure TTestResilienceInterceptor.TestTimeoutHandling;
var
  Config: IResilienceConfig;
  Request: TRestRequest;
  Response: TRestResponse;
  StartTime: TDateTime;
  ExecutionTime: Integer;
begin
  // Configurar timeout baixo
  Config := TResilienceConfigFactory.CreateFastFail('/timeout');
  Config.SetTimeout(1000); // 1 segundo
  FInterceptor.AddEndpointConfig('/timeout', Config);
  
  // Configurar mock para execução lenta
  FMockAdapter := TMockFrameworkAdapter.Create(False, 0, 2000); // 2 segundos
  FInterceptor := TResilienceInterceptor.Create(FMockAdapter);
  FInterceptor.AddEndpointConfig('/timeout', Config);
  
  Request := TRestRequest.Create;
  Request.Endpoint := '/timeout';
  
  StartTime := Now;
  Response := FInterceptor.ExecuteWithResilience('/timeout', Request);
  ExecutionTime := MilliSecondsBetween(Now, StartTime);
  
  CheckTrue(ExecutionTime < 1500, 'Should timeout before mock execution completes');
  CheckEquals(408, Response.StatusCode, 'Should return timeout status');
end;

procedure TTestResilienceInterceptor.TestMetricsCollection;
var
  Request: TRestRequest;
  Metrics: TResilienceMetrics;
begin
  Request := TRestRequest.Create;
  Request.Endpoint := '/metrics';
  
  // Executar algumas requisições
  FInterceptor.ExecuteWithResilience('/metrics', Request);
  FInterceptor.ExecuteWithResilience('/metrics', Request);
  
  Metrics := FInterceptor.GetMetrics('/metrics');
  CheckEquals(2, Metrics.TotalRequests, 'Should track total requests');
  CheckEquals(2, Metrics.SuccessfulRequests, 'Should track successful requests');
  CheckEquals(0, Metrics.FailedRequests, 'Should track failed requests');
end;

procedure TTestResilienceInterceptor.TestRetryWithFallback;
var
  Config: IResilienceConfig;
  Request: TRestRequest;
  Response: TRestResponse;
begin
  // Configurar para falhar algumas vezes e depois usar fallback
  FMockAdapter.SetFailureCount(2);
  
  Config := TResilienceConfigFactory.CreateDefault('/retry-fallback');
  var FallbackService := TFallbackService.Create;
  FallbackService.SetFallbackType(ftStaticResponse);
  FallbackService.SetStaticResponse('{"fallback_used":true}');
  Config.SetFallbackService(FallbackService);
  
  FInterceptor.AddEndpointConfig('/retry-fallback', Config);
  
  Request := TRestRequest.Create;
  Request.Endpoint := '/retry-fallback';
  
  Response := FInterceptor.ExecuteWithResilience('/retry-fallback', Request);
  CheckEquals(200, Response.StatusCode, 'Should use fallback after retries fail');
  CheckTrue(Pos('fallback_used', Response.Content) > 0, 'Should contain fallback content');
end;

procedure TTestResilienceInterceptor.TestCircuitBreakerIntegration;
var
  Config: IResilienceConfig;
  Request: TRestRequest;
  Response: TRestResponse;
  i: Integer;
begin
  // Configurar para sempre falhar
  FMockAdapter.SetShouldFail(True);
  
  Config := TResilienceConfigFactory.CreateFastFail('/circuit');
  FInterceptor.AddEndpointConfig('/circuit', Config);
  
  Request := TRestRequest.Create;
  Request.Endpoint := '/circuit';
  
  // Executar várias requisições para abrir o circuit breaker
  for i := 1 to 5 do
  begin
    try
      Response := FInterceptor.ExecuteWithResilience('/circuit', Request);
    except
      // Ignorar exceções para este teste
    end;
  end;
  
  // Verificar se o circuit breaker está aberto
  var CircuitBreaker := Config.GetCircuitBreaker;
  CheckEquals(Ord(cbsOpen), Ord(CircuitBreaker.GetState), 'Circuit breaker should be open after failures');
end;

procedure TTestResilienceInterceptor.TestThreadSafety;
var
  Tasks: array[0..9] of ITask;
  Request: TRestRequest;
  i: Integer;
begin
  Request := TRestRequest.Create;
  Request.Endpoint := '/thread-test';
  
  // Executar requisições concorrentes
  for i := 0 to 9 do
  begin
    Tasks[i] := TTask.Create(
      procedure
      var
        LocalRequest: TRestRequest;
        j: Integer;
      begin
        for j := 1 to 10 do
        begin
          LocalRequest := TRestRequest.Create;
          LocalRequest.Endpoint := '/thread-test';
          FInterceptor.ExecuteWithResilience('/thread-test', LocalRequest);
        end;
      end
    );
    Tasks[i].Start;
  end;
  
  // Aguardar conclusão
  for i := 0 to 9 do
    Tasks[i].Wait;
  
  // Verificar métricas
  var Metrics := FInterceptor.GetMetrics('/thread-test');
  CheckEquals(100, Metrics.TotalRequests, 'Should handle all concurrent requests');
end;

{ TTestResilienceConfigFactory }

procedure TTestResilienceConfigFactory.TestCreateDefault;
var
  Config: IResilienceConfig;
begin
  Config := TResilienceConfigFactory.CreateDefault('/test');
  CheckNotNull(Config, 'Default config should not be nil');
  CheckEquals('/test', Config.GetEndpoint, 'Should set endpoint correctly');
  CheckNotNull(Config.GetRetryPolicy, 'Should have retry policy');
  CheckNotNull(Config.GetCircuitBreaker, 'Should have circuit breaker');
end;

procedure TTestResilienceConfigFactory.TestCreateHighAvailability;
var
  Config: IResilienceConfig;
begin
  Config := TResilienceConfigFactory.CreateHighAvailability('/ha-test');
  CheckNotNull(Config, 'High availability config should not be nil');
  CheckTrue(Config.GetTimeout > 30000, 'Should have longer timeout for HA');
  CheckTrue(Config.GetMaxConcurrentRequests > 100, 'Should allow more concurrent requests for HA');
end;

procedure TTestResilienceConfigFactory.TestCreateFastFail;
var
  Config: IResilienceConfig;
begin
  Config := TResilienceConfigFactory.CreateFastFail('/ff-test');
  CheckNotNull(Config, 'Fast fail config should not be nil');
  CheckTrue(Config.GetTimeout < 10000, 'Should have shorter timeout for fast fail');
  CheckNull(Config.GetFallbackService, 'Fast fail should not have fallback');
end;

procedure TTestResilienceConfigFactory.TestCreateBulkhead;
var
  Config: IResilienceConfig;
begin
  Config := TResilienceConfigFactory.CreateBulkhead('/bulk-test', 25);
  CheckNotNull(Config, 'Bulkhead config should not be nil');
  CheckEquals(25, Config.GetMaxConcurrentRequests, 'Should set custom concurrency limit');
end;

procedure TTestResilienceConfigFactory.TestConfigurationProperties;
var
  Config: IResilienceConfig;
begin
  Config := TResilienceConfigFactory.CreateDefault('/props-test');
  
  CheckTrue(Config.GetEnabled, 'Should be enabled by default');
  CheckTrue(Config.GetTimeout > 0, 'Should have positive timeout');
  CheckTrue(Config.GetMaxConcurrentRequests > 0, 'Should have positive concurrency limit');
end;

{ TTestResilienceIntegration }

procedure TTestResilienceIntegration.SetUp;
begin
  inherited;
  FMockAdapter := TMockFrameworkAdapter.Create;
  FInterceptor := TResilienceInterceptor.Create(FMockAdapter);
end;

procedure TTestResilienceIntegration.TearDown;
begin
  FInterceptor := nil;
  FMockAdapter := nil;
  inherited;
end;

procedure TTestResilienceIntegration.TestNetworkFailureRecovery;
var
  Config: IResilienceConfig;
  Request: TRestRequest;
  Response: TRestResponse;
begin
  // Simular falhas temporárias de rede
  FMockAdapter.SetFailureCount(2); // Falha nas primeiras 2 tentativas
  
  Config := TResilienceConfigFactory.CreateDefault('/network-test');
  FInterceptor.AddEndpointConfig('/network-test', Config);
  
  Request := TRestRequest.Create;
  Request.Endpoint := '/network-test';
  
  Response := FInterceptor.ExecuteWithResilience('/network-test', Request);
  CheckEquals(200, Response.StatusCode, 'Should recover from temporary network failures');
  CheckTrue(Response.Success, 'Should succeed after retries');
end;

procedure TTestResilienceIntegration.TestServiceOverload;
var
  Config: IResilienceConfig;
  Request: TRestRequest;
  Tasks: array[0..19] of ITask;
  SuccessCount: Integer;
  i: Integer;
begin
  // Configurar limite baixo para simular sobrecarga
  Config := TResilienceConfigFactory.CreateBulkhead('/overload-test', 5);
  FInterceptor.AddEndpointConfig('/overload-test', Config);
  
  Request := TRestRequest.Create;
  Request.Endpoint := '/overload-test';
  
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
        LocalRequest.Endpoint := '/overload-test';
        LocalResponse := FInterceptor.ExecuteWithResilience('/overload-test', LocalRequest);
        if LocalResponse.Success then
          TInterlocked.Increment(SuccessCount);
      end
    );
    Tasks[i].Start;
  end;
  
  // Aguardar conclusão
  for i := 0 to 19 do
    Tasks[i].Wait;
  
  // Verificar se o sistema lidou com a sobrecarga adequadamente
  CheckTrue(SuccessCount > 0, 'Some requests should succeed');
  CheckTrue(SuccessCount < 20, 'Some requests should be rejected due to overload protection');
end;

procedure TTestResilienceIntegration.TestGracefulDegradation;
var
  Config: IResilienceConfig;
  Request: TRestRequest;
  Response: TRestResponse;
  FallbackService: IFallbackService;
begin
  // Configurar para sempre falhar, mas com fallback
  FMockAdapter.SetShouldFail(True);
  
  FallbackService := TFallbackService.Create;
  FallbackService.SetFallbackType(ftStaticResponse);
  FallbackService.SetStaticResponse('{"degraded_service":true,"message":"Service temporarily unavailable"}')
  
  Config := TResilienceConfigFactory.CreateDefault('/degradation-test');
  Config.SetFallbackService(FallbackService);
  FInterceptor.AddEndpointConfig('/degradation-test', Config);
  
  Request := TRestRequest.Create;
  Request.Endpoint := '/degradation-test';
  
  Response := FInterceptor.ExecuteWithResilience('/degradation-test', Request);
  CheckEquals(200, Response.StatusCode, 'Should provide degraded service via fallback');
  CheckTrue(Pos('degraded_service', Response.Content) > 0, 'Should indicate degraded service');
end;

procedure TTestResilienceIntegration.TestAutomaticRecovery;
var
  Config: IResilienceConfig;
  Request: TRestRequest;
  Response: TRestResponse;
  CircuitBreaker: ICircuitBreaker;
  i: Integer;
begin
  Config := TResilienceConfigFactory.CreateDefault('/recovery-test');
  CircuitBreaker := Config.GetCircuitBreaker;
  FInterceptor.AddEndpointConfig('/recovery-test', Config);
  
  Request := TRestRequest.Create;
  Request.Endpoint := '/recovery-test';
  
  // Forçar abertura do circuit breaker
  FMockAdapter.SetShouldFail(True);
  for i := 1 to 5 do
  begin
    try
      FInterceptor.ExecuteWithResilience('/recovery-test', Request);
    except
      // Ignorar exceções
    end;
  end;
  
  CheckEquals(Ord(cbsOpen), Ord(CircuitBreaker.GetState), 'Circuit should be open');
  
  // Aguardar timeout de recuperação
  Sleep(6000); // Aguardar mais que o timeout padrão
  
  // Restaurar funcionamento normal
  FMockAdapter.SetShouldFail(False);
  
  // Executar requisição que deve colocar em half-open
  Response := FInterceptor.ExecuteWithResilience('/recovery-test', Request);
  CheckEquals(Ord(cbsHalfOpen), Ord(CircuitBreaker.GetState), 'Circuit should be half-open');
  
  // Mais algumas requisições bem-sucedidas devem fechar o circuit
  FInterceptor.ExecuteWithResilience('/recovery-test', Request);
  CheckEquals(Ord(cbsClosed), Ord(CircuitBreaker.GetState), 'Circuit should recover to closed');
end;

procedure TTestResilienceIntegration.TestCascadingFailures;
var
  Config1, Config2: IResilienceConfig;
  Request: TRestRequest;
  CB1, CB2: ICircuitBreaker;
  i: Integer;
begin
  // Configurar dois endpoints com circuit breakers
  Config1 := TResilienceConfigFactory.CreateFastFail('/service1');
  Config2 := TResilienceConfigFactory.CreateFastFail('/service2');
  
  FInterceptor.AddEndpointConfig('/service1', Config1);
  FInterceptor.AddEndpointConfig('/service2', Config2);
  
  CB1 := Config1.GetCircuitBreaker;
  CB2 := Config2.GetCircuitBreaker;
  
  // Simular falha em cascata
  FMockAdapter.SetShouldFail(True);
  
  Request := TRestRequest.Create;
  
  // Falhar service1
  Request.Endpoint := '/service1';
  for i := 1 to 5 do
  begin
    try
      FInterceptor.ExecuteWithResilience('/service1', Request);
    except
      // Ignorar exceções
    end;
  end;
  
  // Falhar service2
  Request.Endpoint := '/service2';
  for i := 1 to 5 do
  begin
    try
      FInterceptor.ExecuteWithResilience('/service2', Request);
    except
      // Ignorar exceções
    end;
  end;
  
  // Verificar se ambos os circuit breakers estão abertos
  CheckEquals(Ord(cbsOpen), Ord(CB1.GetState), 'Service1 circuit should be open');
  CheckEquals(Ord(cbsOpen), Ord(CB2.GetState), 'Service2 circuit should be open');
  
  // Verificar se as requisições são rejeitadas rapidamente
  CheckFalse(CB1.CanExecute, 'Service1 should reject requests');
  CheckFalse(CB2.CanExecute, 'Service2 should reject requests');
end;

procedure TTestResilienceIntegration.TestBulkheadIsolation;
var
  ConfigFast, ConfigSlow: IResilienceConfig;
  RequestFast, RequestSlow: TRestRequest;
  TasksFast, TasksSlow: array[0..4] of ITask;
  FastResults, SlowResults: array[0..4] of Boolean;
  i: Integer;
begin
  // Configurar dois endpoints com limites diferentes
  ConfigFast := TResilienceConfigFactory.CreateBulkhead('/fast-service', 10);
  ConfigSlow := TResilienceConfigFactory.CreateBulkhead('/slow-service', 2);
  
  FInterceptor.AddEndpointConfig('/fast-service', ConfigFast);
  FInterceptor.AddEndpointConfig('/slow-service', ConfigSlow);
  
  RequestFast := TRestRequest.Create;
  RequestFast.Endpoint := '/fast-service';
  
  RequestSlow := TRestRequest.Create;
  RequestSlow.Endpoint := '/slow-service';
  
  // Executar requisições concorrentes para ambos os serviços
  for i := 0 to 4 do
  begin
    TasksFast[i] := TTask.Create(
      procedure
      var
        LocalRequest: TRestRequest;
        Response: TRestResponse;
      begin
        LocalRequest := TRestRequest.Create;
        LocalRequest.Endpoint := '/fast-service';
        Response := FInterceptor.ExecuteWithResilience('/fast-service', LocalRequest);
        FastResults[i] := Response.Success;
      end
    );
    
    TasksSlow[i] := TTask.Create(
      procedure
      var
        LocalRequest: TRestRequest;
        Response: TRestResponse;
      begin
        LocalRequest := TRestRequest.Create;
        LocalRequest.Endpoint := '/slow-service';
        Response := FInterceptor.ExecuteWithResilience('/slow-service', LocalRequest);
        SlowResults[i] := Response.Success;
      end
    );
    
    TasksFast[i].Start;
    TasksSlow[i].Start;
  end;
  
  // Aguardar conclusão
  for i := 0 to 4 do
  begin
    TasksFast[i].Wait;
    TasksSlow[i].Wait;
  end;
  
  // Verificar isolamento - serviço rápido não deve ser afetado pelo lento
  var FastSuccessCount := 0;
  var SlowSuccessCount := 0;
  
  for i := 0 to 4 do
  begin
    if FastResults[i] then Inc(FastSuccessCount);
    if SlowResults[i] then Inc(SlowSuccessCount);
  end;
  
  CheckEquals(5, FastSuccessCount, 'Fast service should not be affected by slow service limits');
  CheckTrue(SlowSuccessCount <= 2, 'Slow service should respect its bulkhead limits');
end;

initialization
  RegisterTest(TTestRetryPolicy.Suite);
  RegisterTest(TTestCircuitBreaker.Suite);
  RegisterTest(TTestFallbackService.Suite);
  RegisterTest(TTestResilienceInterceptor.Suite);
  RegisterTest(TTestResilienceConfigFactory.Suite);
  RegisterTest(TTestResilienceIntegration.Suite);

end.