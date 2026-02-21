unit Nest4D.Resilience.Interceptor;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  System.DateUtils,
  Nest4D.Resilience.Interfaces,
  Nest4D.Resilience.Retry,
  Nest4D.Resilience.Fallback,
  Nest4D.Resilience.CircuitBreaker;

type
  // Configura??o de resili?ncia por endpoint
  TEndpointResilienceConfig = class(TInterfacedObject, IResilienceConfig)
  private
    FEndpoint: string;
    FRetryPolicy: IRetryPolicy;
    FFallbackService: IFallbackService;
    FCircuitBreaker: ICircuitBreaker;
    FEnabled: Boolean;
    FTimeout: Integer;
    FMaxConcurrentRequests: Integer;
  public
    constructor Create(const AEndpoint: string);
    destructor Destroy; override;

    // IResilienceConfig
    function GetEndpoint: string;
    function GetRetryPolicy: IRetryPolicy;
    function GetFallbackService: IFallbackService;
    function GetCircuitBreaker: ICircuitBreaker;
    function GetEnabled: Boolean;
    function GetTimeout: Integer;
    function GetMaxConcurrentRequests: Integer;

    procedure SetRetryPolicy(const APolicy: IRetryPolicy);
    procedure SetFallbackService(const AService: IFallbackService);
    procedure SetCircuitBreaker(const ABreaker: ICircuitBreaker);
    procedure SetEnabled(const AEnabled: Boolean);
    procedure SetTimeout(const ATimeout: Integer);
    procedure SetMaxConcurrentRequests(const AMax: Integer);

    property Endpoint: string read GetEndpoint;
    property RetryPolicy: IRetryPolicy read GetRetryPolicy write SetRetryPolicy;
    property FallbackService: IFallbackService read GetFallbackService write SetFallbackService;
    property CircuitBreaker: ICircuitBreaker read GetCircuitBreaker write SetCircuitBreaker;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property Timeout: Integer read GetTimeout write SetTimeout;
    property MaxConcurrentRequests: Integer read GetMaxConcurrentRequests write SetMaxConcurrentRequests;
  end;

  // Interceptor principal de resili?ncia
  TResilienceInterceptor = class(TInterfacedObject, IResilienceInterceptor)
  private
    FConfigs: TDictionary<string, IResilienceConfig>;
    FDefaultConfig: IResilienceConfig;
    FFrameworkAdapter: IRestFrameworkAdapter;
    FMetrics: TDictionary<string, TResilienceMetrics>;
    FCriticalSection: TCriticalSection;
    FActiveRequests: TDictionary<string, Integer>;

    function GetConfigForEndpoint(const AEndpoint: string): IResilienceConfig;
    function ShouldApplyResilience(const AEndpoint: string; const AConfig: IResilienceConfig): Boolean;
    procedure UpdateMetrics(const AEndpoint: string; const ASuccess: Boolean; const AExecutionTime: Cardinal);
    function CheckConcurrencyLimit(const AEndpoint: string; const AConfig: IResilienceConfig): Boolean;
    procedure IncrementActiveRequests(const AEndpoint: string);
    procedure DecrementActiveRequests(const AEndpoint: string);
  public
    constructor Create(const AFrameworkAdapter: IRestFrameworkAdapter);
    destructor Destroy; override;

    // IResilienceInterceptor
    function ExecuteWithResilience(const AEndpoint: string; const ARequest: TRestRequest): TRestResponse;
    procedure AddEndpointConfig(const AEndpoint: string; const AConfig: IResilienceConfig);
    procedure RemoveEndpointConfig(const AEndpoint: string);
    function GetEndpointConfig(const AEndpoint: string): IResilienceConfig;
    procedure SetDefaultConfig(const AConfig: IResilienceConfig);
    function GetMetrics(const AEndpoint: string): TResilienceMetrics;
    function GetAllMetrics: TArray<TResilienceMetrics>;
    procedure ResetMetrics(const AEndpoint: string = '');
  end;

  // Factory para criar configura??es pr?-definidas
  TResilienceConfigFactory = class
  public
    class function CreateDefault(const AEndpoint: string): IResilienceConfig;
    class function CreateHighAvailability(const AEndpoint: string): IResilienceConfig;
    class function CreateFastFail(const AEndpoint: string): IResilienceConfig;
    class function CreateBulkhead(const AEndpoint: string; const AMaxConcurrent: Integer): IResilienceConfig;
  end;

implementation

{ TEndpointResilienceConfig }

constructor TEndpointResilienceConfig.Create(const AEndpoint: string);
begin
  inherited Create;
  FEndpoint := AEndpoint;
  FEnabled := True;
  FTimeout := 30000; // 30 segundos
  FMaxConcurrentRequests := 100;
end;

destructor TEndpointResilienceConfig.Destroy;
begin
  FRetryPolicy := nil;
  FFallbackService := nil;
  FCircuitBreaker := nil;
  inherited;
end;

function TEndpointResilienceConfig.GetEndpoint: string;
begin
  Result := FEndpoint;
end;

function TEndpointResilienceConfig.GetRetryPolicy: IRetryPolicy;
begin
  Result := FRetryPolicy;
end;

function TEndpointResilienceConfig.GetFallbackService: IFallbackService;
begin
  Result := FFallbackService;
end;

function TEndpointResilienceConfig.GetCircuitBreaker: ICircuitBreaker;
begin
  Result := FCircuitBreaker;
end;

function TEndpointResilienceConfig.GetEnabled: Boolean;
begin
  Result := FEnabled;
end;

function TEndpointResilienceConfig.GetTimeout: Integer;
begin
  Result := FTimeout;
end;

function TEndpointResilienceConfig.GetMaxConcurrentRequests: Integer;
begin
  Result := FMaxConcurrentRequests;
end;

procedure TEndpointResilienceConfig.SetRetryPolicy(const APolicy: IRetryPolicy);
begin
  FRetryPolicy := APolicy;
end;

procedure TEndpointResilienceConfig.SetFallbackService(const AService: IFallbackService);
begin
  FFallbackService := AService;
end;

procedure TEndpointResilienceConfig.SetCircuitBreaker(const ABreaker: ICircuitBreaker);
begin
  FCircuitBreaker := ABreaker;
end;

procedure TEndpointResilienceConfig.SetEnabled(const AEnabled: Boolean);
begin
  FEnabled := AEnabled;
end;

procedure TEndpointResilienceConfig.SetTimeout(const ATimeout: Integer);
begin
  FTimeout := ATimeout;
end;

procedure TEndpointResilienceConfig.SetMaxConcurrentRequests(const AMax: Integer);
begin
  FMaxConcurrentRequests := AMax;
end;

{ TResilienceInterceptor }

constructor TResilienceInterceptor.Create(const AFrameworkAdapter: IRestFrameworkAdapter);
begin
  inherited Create;
  FFrameworkAdapter := AFrameworkAdapter;
  FConfigs := TDictionary<string, IResilienceConfig>.Create;
  FMetrics := TDictionary<string, TResilienceMetrics>.Create;
  FActiveRequests := TDictionary<string, Integer>.Create;
  FCriticalSection := TCriticalSection.Create;

  // Configura??o padr?o
  FDefaultConfig := TResilienceConfigFactory.CreateDefault('*');
end;

destructor TResilienceInterceptor.Destroy;
begin
  FCriticalSection.Enter;
  try
    FConfigs.Free;
    FMetrics.Free;
    FActiveRequests.Free;
  finally
    FCriticalSection.Leave;
    FCriticalSection.Free;
  end;
  inherited;
end;

function TResilienceInterceptor.GetConfigForEndpoint(const AEndpoint: string): IResilienceConfig;
begin
  FCriticalSection.Enter;
  try
    if not FConfigs.TryGetValue(AEndpoint, Result) then
      Result := FDefaultConfig;
  finally
    FCriticalSection.Leave;
  end;
end;

function TResilienceInterceptor.ShouldApplyResilience(const AEndpoint: string; const AConfig: IResilienceConfig): Boolean;
begin
  Result := (AConfig <> nil) and AConfig.GetEnabled;
end;

function TResilienceInterceptor.CheckConcurrencyLimit(const AEndpoint: string; const AConfig: IResilienceConfig): Boolean;
var
  ActiveCount: Integer;
begin
  FCriticalSection.Enter;
  try
    if not FActiveRequests.TryGetValue(AEndpoint, ActiveCount) then
      ActiveCount := 0;

    Result := ActiveCount < AConfig.GetMaxConcurrentRequests;
  finally
    FCriticalSection.Leave;
  end;
end;

procedure TResilienceInterceptor.IncrementActiveRequests(const AEndpoint: string);
var
  Count: Integer;
begin
  FCriticalSection.Enter;
  try
    if not FActiveRequests.TryGetValue(AEndpoint, Count) then
      Count := 0;
    FActiveRequests.AddOrSetValue(AEndpoint, Count + 1);
  finally
    FCriticalSection.Leave;
  end;
end;

procedure TResilienceInterceptor.DecrementActiveRequests(const AEndpoint: string);
var
  Count: Integer;
begin
  FCriticalSection.Enter;
  try
    if FActiveRequests.TryGetValue(AEndpoint, Count) and (Count > 0) then
      FActiveRequests.AddOrSetValue(AEndpoint, Count - 1);
  finally
    FCriticalSection.Leave;
  end;
end;

function TResilienceInterceptor.ExecuteWithResilience(const AEndpoint: string; const ARequest: TRestRequest): TRestResponse;
var
  Config: IResilienceConfig;
  StartTime: TDateTime;
  ExecutionTime: Cardinal;
  Success: Boolean;
  LastException: Exception;
  FallbackResponse: TFallbackResponse;
begin
  Config := GetConfigForEndpoint(AEndpoint);

  if not ShouldApplyResilience(AEndpoint, Config) then
  begin
    // Execu??o direta sem resili?ncia
    Result := FFrameworkAdapter.ExecuteRequest(ARequest);
    Exit;
  end;

  // Verificar limite de concorr?ncia
  if not CheckConcurrencyLimit(AEndpoint, Config) then
  begin
    Result.StatusCode := 503; // Service Unavailable
    Result.Content := 'Service temporarily overloaded';
    Result.Success := False;
    Exit;
  end;

  IncrementActiveRequests(AEndpoint);
  StartTime := Now;
  Success := False;
  LastException := nil;

  try
    // Verificar Circuit Breaker
    if (Config.GetCircuitBreaker <> nil) and not Config.GetCircuitBreaker.CanExecute then
    begin
      // Circuit Breaker aberto - tentar fallback
      if Config.GetFallbackService <> nil then
      begin
        FallbackResponse := Config.GetFallbackService.GetFallback(AEndpoint, ARequest);
        if FallbackResponse.Success then
        begin
          Result.StatusCode := 200;
          Result.Content := FallbackResponse.Content;
          Result.Success := True;
          Result.Headers := FallbackResponse.Headers;
          Success := True;
          Exit;
        end;
      end;

      Result.StatusCode := 503;
      Result.Content := 'Service temporarily unavailable (Circuit Breaker Open)';
      Result.Success := False;
      Exit;
    end;

    // Executar com retry policy
    if Config.GetRetryPolicy <> nil then
    begin
      Result := Config.GetRetryPolicy.Execute(
        function: TRestResponse
        begin
          Result := FFrameworkAdapter.ExecuteRequest(ARequest);
        end
      );
    end
    else
    begin
      Result := FFrameworkAdapter.ExecuteRequest(ARequest);
    end;

    Success := Result.Success;

    // Atualizar Circuit Breaker
    if Config.GetCircuitBreaker <> nil then
    begin
      if Success then
        Config.GetCircuitBreaker.RecordSuccess
      else
        Config.GetCircuitBreaker.RecordFailure;
    end;

    // Se falhou e temos fallback, tentar usar
    if not Success and (Config.GetFallbackService <> nil) then
    begin
      FallbackResponse := Config.GetFallbackService.GetFallback(AEndpoint, ARequest);
      if FallbackResponse.Success then
      begin
        Result.StatusCode := 200;
        Result.Content := FallbackResponse.Content;
        Result.Success := True;
        Result.Headers := FallbackResponse.Headers;
        Success := True;
      end;
    end;

  except
    on E: Exception do
    begin
      LastException := E;
      Success := False;

      // Atualizar Circuit Breaker em caso de exce??o
      if Config.GetCircuitBreaker <> nil then
        Config.GetCircuitBreaker.RecordFailure;

      // Tentar fallback em caso de exce??o
      if Config.GetFallbackService <> nil then
      begin
        try
          FallbackResponse := Config.GetFallbackService.GetFallback(AEndpoint, ARequest);
          if FallbackResponse.Success then
          begin
            Result.StatusCode := 200;
            Result.Content := FallbackResponse.Content;
            Result.Success := True;
            Result.Headers := FallbackResponse.Headers;
            Success := True;
          end
          else
          begin
            Result.StatusCode := 500;
            Result.Content := Format('Request failed: %s', [E.Message]);
            Result.Success := False;
          end;
        except
          Result.StatusCode := 500;
          Result.Content := Format('Request failed: %s', [E.Message]);
          Result.Success := False;
        end;
      end
      else
      begin
        Result.StatusCode := 500;
        Result.Content := Format('Request failed: %s', [E.Message]);
        Result.Success := False;
      end;
    end;
  end;

finally
  DecrementActiveRequests(AEndpoint);
  ExecutionTime := MilliSecondsBetween(Now, StartTime);
  UpdateMetrics(AEndpoint, Success, ExecutionTime);
end;
end;

procedure TResilienceInterceptor.UpdateMetrics(const AEndpoint: string; const ASuccess: Boolean; const AExecutionTime: Cardinal);
var
  Metrics: TResilienceMetrics;
begin
  FCriticalSection.Enter;
  try
    if not FMetrics.TryGetValue(AEndpoint, Metrics) then
    begin
      Metrics.Endpoint := AEndpoint;
      Metrics.TotalRequests := 0;
      Metrics.SuccessfulRequests := 0;
      Metrics.FailedRequests := 0;
      Metrics.AverageResponseTime := 0;
      Metrics.LastRequestTime := Now;
    end;

    Inc(Metrics.TotalRequests);
    if ASuccess then
      Inc(Metrics.SuccessfulRequests)
    else
      Inc(Metrics.FailedRequests);

    // Calcular m?dia m?vel do tempo de resposta
    Metrics.AverageResponseTime := ((Metrics.AverageResponseTime * (Metrics.TotalRequests - 1)) + AExecutionTime) div Metrics.TotalRequests;
    Metrics.LastRequestTime := Now;

    FMetrics.AddOrSetValue(AEndpoint, Metrics);
  finally
    FCriticalSection.Leave;
  end;
end;

procedure TResilienceInterceptor.AddEndpointConfig(const AEndpoint: string; const AConfig: IResilienceConfig);
begin
  FCriticalSection.Enter;
  try
    FConfigs.AddOrSetValue(AEndpoint, AConfig);
  finally
    FCriticalSection.Leave;
  end;
end;

procedure TResilienceInterceptor.RemoveEndpointConfig(const AEndpoint: string);
begin
  FCriticalSection.Enter;
  try
    FConfigs.Remove(AEndpoint);
  finally
    FCriticalSection.Leave;
  end;
end;

function TResilienceInterceptor.GetEndpointConfig(const AEndpoint: string): IResilienceConfig;
begin
  Result := GetConfigForEndpoint(AEndpoint);
end;

procedure TResilienceInterceptor.SetDefaultConfig(const AConfig: IResilienceConfig);
begin
  FDefaultConfig := AConfig;
end;

function TResilienceInterceptor.GetMetrics(const AEndpoint: string): TResilienceMetrics;
begin
  FCriticalSection.Enter;
  try
    if not FMetrics.TryGetValue(AEndpoint, Result) then
    begin
      Result.Endpoint := AEndpoint;
      Result.TotalRequests := 0;
      Result.SuccessfulRequests := 0;
      Result.FailedRequests := 0;
      Result.AverageResponseTime := 0;
      Result.LastRequestTime := 0;
    end;
  finally
    FCriticalSection.Leave;
  end;
end;

function TResilienceInterceptor.GetAllMetrics: TArray<TResilienceMetrics>;
var
  Metrics: TResilienceMetrics;
  List: TList<TResilienceMetrics>;
begin
  List := TList<TResilienceMetrics>.Create;
  try
    FCriticalSection.Enter;
    try
      for Metrics in FMetrics.Values do
        List.Add(Metrics);
    finally
      FCriticalSection.Leave;
    end;
    Result := List.ToArray;
  finally
    List.Free;
  end;
end;

procedure TResilienceInterceptor.ResetMetrics(const AEndpoint: string);
begin
  FCriticalSection.Enter;
  try
    if AEndpoint = '' then
      FMetrics.Clear
    else
      FMetrics.Remove(AEndpoint);
  finally
    FCriticalSection.Leave;
  end;
end;

{ TResilienceConfigFactory }

class function TResilienceConfigFactory.CreateDefault(const AEndpoint: string): IResilienceConfig;
var
  Config: TEndpointResilienceConfig;
begin
  Config := TEndpointResilienceConfig.Create(AEndpoint);
  Config.SetRetryPolicy(TRetryPolicyFactory.CreateDefault);
  Config.SetFallbackService(TFallbackService.Create);
  Config.SetCircuitBreaker(TCircuitBreakerFactory.CreateDefault(AEndpoint));
  Config.SetTimeout(30000);
  Config.SetMaxConcurrentRequests(100);
  Result := Config;
end;

class function TResilienceConfigFactory.CreateHighAvailability(const AEndpoint: string): IResilienceConfig;
var
  Config: TEndpointResilienceConfig;
begin
  Config := TEndpointResilienceConfig.Create(AEndpoint);
  Config.SetRetryPolicy(TRetryPolicyFactory.CreateAggressive);
  Config.SetFallbackService(TFallbackService.Create);
  Config.SetCircuitBreaker(TCircuitBreakerFactory.CreateHighAvailability(AEndpoint));
  Config.SetTimeout(60000);
  Config.SetMaxConcurrentRequests(200);
  Result := Config;
end;

class function TResilienceConfigFactory.CreateFastFail(const AEndpoint: string): IResilienceConfig;
var
  Config: TEndpointResilienceConfig;
begin
  Config := TEndpointResilienceConfig.Create(AEndpoint);
  Config.SetRetryPolicy(TRetryPolicyFactory.CreateConservative);
  Config.SetFallbackService(nil); // Sem fallback para fast fail
  Config.SetCircuitBreaker(TCircuitBreakerFactory.CreateFastFail(AEndpoint));
  Config.SetTimeout(5000);
  Config.SetMaxConcurrentRequests(50);
  Result := Config;
end;

class function TResilienceConfigFactory.CreateBulkhead(const AEndpoint: string; const AMaxConcurrent: Integer): IResilienceConfig;
var
  Config: TEndpointResilienceConfig;
begin
  Config := TEndpointResilienceConfig.Create(AEndpoint);
  Config.SetRetryPolicy(TRetryPolicyFactory.CreateDefault);
  Config.SetFallbackService(TFallbackService.Create);
  Config.SetCircuitBreaker(TCircuitBreakerFactory.CreateDefault(AEndpoint));
  Config.SetTimeout(30000);
  Config.SetMaxConcurrentRequests(AMaxConcurrent);
  Result := Config;
end;

end.

