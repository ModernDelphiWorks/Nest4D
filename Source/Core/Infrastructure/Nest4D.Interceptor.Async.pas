unit Nest4D.Interceptor.Async;

interface

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  System.DateUtils,
  System.Generics.Collections,
  System.Rtti,
  System.TypInfo,
  System.Variants,
  Nest4D.Interceptor,
  Nest4D.Async,
  Nest4D.Logging.Async,
  Nest4D.Metrics.Async,
  Nest4D.Logging,
  Nest4D.Health.Async;

type
  // Contexto de intercepta??o ass?ncrona
  TAsyncInterceptorContext = record
    RequestId: String;
    StartTime: TDateTime;
    EndTime: TDateTime;
    Duration: Double;
    Success: Boolean;
    ErrorMessage: String;
    Metadata: TDictionary<String, Variant>;

    constructor Create(const ARequestId: String);
    procedure Finish(ASuccess: Boolean; const AError: String = '');
    function GetDuration: Double;
  end;

  // Callback para interceptors ass?ncronos
  TAsyncInterceptorCallback = reference to procedure(const AContext: TAsyncInterceptorContext);
  TAsyncInterceptorErrorCallback = reference to procedure(const AContext: TAsyncInterceptorContext; const AException: Exception);

  // Interface base para interceptors ass?ncronos
  INest4DAsyncInterceptor = interface
    ['{F21261E0-C953-4243-97C7-D0C8AB3F89DB}']
    procedure BeforeAsync(const AContext: TAsyncInterceptorContext; const ACallback: TAsyncInterceptorCallback = nil);
    procedure AfterAsync(const AContext: TAsyncInterceptorContext; const ACallback: TAsyncInterceptorCallback = nil);
    procedure OnErrorAsync(const AContext: TAsyncInterceptorContext; const AException: Exception; const ACallback: TAsyncInterceptorErrorCallback = nil);

    function GetName: String;
    function GetPriority: Integer;
    function IsEnabled: Boolean;
    procedure SetEnabled(AEnabled: Boolean);
  end;

  // Interceptor base ass?ncrono
  TNest4DAsyncInterceptorBase = class(TInterfacedObject, INest4DAsyncInterceptor)
  private
    FName: String;
    FPriority: Integer;
    FEnabled: Boolean;
    FAsyncExecutor: TNest4DAsyncExecutor;

  protected
    procedure DoBeforeAsync(const AContext: TAsyncInterceptorContext; const ACallback: TAsyncInterceptorCallback); virtual;
    procedure DoAfterAsync(const AContext: TAsyncInterceptorContext; const ACallback: TAsyncInterceptorCallback); virtual;
    procedure DoOnErrorAsync(const AContext: TAsyncInterceptorContext; const AException: Exception; const ACallback: TAsyncInterceptorErrorCallback); virtual;

  public
    constructor Create(const AName: String; APriority: Integer = 100);
    destructor Destroy; override;

    procedure BeforeAsync(const AContext: TAsyncInterceptorContext; const ACallback: TAsyncInterceptorCallback = nil);
    procedure AfterAsync(const AContext: TAsyncInterceptorContext; const ACallback: TAsyncInterceptorCallback = nil);
    procedure OnErrorAsync(const AContext: TAsyncInterceptorContext; const AException: Exception; const ACallback: TAsyncInterceptorErrorCallback = nil);

    function GetName: String;
    function GetPriority: Integer;
    function IsEnabled: Boolean;
    procedure SetEnabled(AEnabled: Boolean);
  end;

  // Interceptor de logging ass?ncrono
  TNest4DAsyncLoggingInterceptor = class(TNest4DAsyncInterceptorBase)
  private
    FAsyncLogger: INest4DAsyncLogger;
    FLogLevel: TLogLevel;
    FIncludeRequestBody: Boolean;
    FIncludeResponseBody: Boolean;
    FMaxBodySize: Integer;

  protected
    procedure DoBeforeAsync(const AContext: TAsyncInterceptorContext; const ACallback: TAsyncInterceptorCallback); override;
    procedure DoAfterAsync(const AContext: TAsyncInterceptorContext; const ACallback: TAsyncInterceptorCallback); override;
    procedure DoOnErrorAsync(const AContext: TAsyncInterceptorContext; const AException: Exception; const ACallback: TAsyncInterceptorErrorCallback); override;

  public
    constructor Create(const AAsyncLogger: INest4DAsyncLogger; ALogLevel: TLogLevel = llInfo);

    procedure SetLogLevel(ALevel: TLogLevel);
    procedure SetBodyLogging(AIncludeRequest, AIncludeResponse: Boolean; AMaxSize: Integer = 1024);
  end;

  // Interceptor de m?tricas ass?ncrono
  TNest4DAsyncMetricsInterceptor = class(TNest4DAsyncInterceptorBase)
  private
    FAsyncMetrics: INest4DAsyncMetrics;
    FTrackTiming: Boolean;
    FTrackMemory: Boolean;
    FTrackConcurrency: Boolean;
    FConcurrentRequests: Integer;
    FConcurrencyLock: TCriticalSection;

  protected
    procedure DoBeforeAsync(const AContext: TAsyncInterceptorContext; const ACallback: TAsyncInterceptorCallback); override;
    procedure DoAfterAsync(const AContext: TAsyncInterceptorContext; const ACallback: TAsyncInterceptorCallback); override;
    procedure DoOnErrorAsync(const AContext: TAsyncInterceptorContext; const AException: Exception; const ACallback: TAsyncInterceptorErrorCallback); override;

  public
    constructor Create(const AAsyncMetrics: INest4DAsyncMetrics);
    destructor Destroy; override;

    procedure SetTrackingOptions(ATiming, AMemory, AConcurrency: Boolean);
    function GetConcurrentRequests: Integer;
  end;

  // Interceptor de health check ass?ncrono
  TNest4DAsyncHealthInterceptor = class(TNest4DAsyncInterceptorBase)
  private
    FAsyncHealth: INest4DAsyncHealth;
    FCheckInterval: Integer;
    FLastCheck: TDateTime;
    FHealthStatus: String;
    FHealthLock: TCriticalSection;

  protected
    procedure DoBeforeAsync(const AContext: TAsyncInterceptorContext; const ACallback: TAsyncInterceptorCallback); override;
    procedure DoAfterAsync(const AContext: TAsyncInterceptorContext; const ACallback: TAsyncInterceptorCallback); override;

    procedure UpdateHealthStatus;
    function ShouldCheckHealth: Boolean;

  public
    constructor Create(const AAsyncHealth: INest4DAsyncHealth; ACheckInterval: Integer = 60);
    destructor Destroy; override;

    procedure SetCheckInterval(AIntervalSeconds: Integer);
    function GetHealthStatus: String;
    procedure ForceHealthCheck;
  end;

  // Interceptor de resili?ncia ass?ncrono
  TNest4DAsyncResilienceInterceptor = class(TNest4DAsyncInterceptorBase)
  private
    FRetryCount: Integer;
    FRetryDelay: Integer;
    FCircuitBreakerThreshold: Integer;
    FCircuitBreakerTimeout: Integer;
    FFailureCount: Integer;
    FLastFailure: TDateTime;
    FCircuitOpen: Boolean;
    FResilienceLock: TCriticalSection;

  protected
    procedure DoBeforeAsync(const AContext: TAsyncInterceptorContext; const ACallback: TAsyncInterceptorCallback); override;
    procedure DoAfterAsync(const AContext: TAsyncInterceptorContext; const ACallback: TAsyncInterceptorCallback); override;
    procedure DoOnErrorAsync(const AContext: TAsyncInterceptorContext; const AException: Exception; const ACallback: TAsyncInterceptorErrorCallback); override;

    function ShouldRetry(const AContext: TAsyncInterceptorContext; AAttempt: Integer): Boolean;
    procedure UpdateCircuitBreaker(ASuccess: Boolean);
    function IsCircuitOpen: Boolean;

  public
    constructor Create(ARetryCount: Integer = 3; ARetryDelay: Integer = 1000; ACircuitThreshold: Integer = 5; ACircuitTimeout: Integer = 30000);
    destructor Destroy; override;

    procedure SetRetryPolicy(ACount: Integer; ADelay: Integer);
    procedure SetCircuitBreaker(AThreshold: Integer; ATimeout: Integer);
    function GetCircuitStatus: String;
    procedure ResetCircuit;
  end;

  // Gerenciador de interceptors ass?ncronos
  TNest4DAsyncInterceptorManager = class
  private
    FInterceptors: TList<INest4DAsyncInterceptor>;
    FLock: TReaderWriterLockSlim;
    FAsyncExecutor: TNest4DAsyncExecutor;

    procedure SortInterceptorsByPriority;

  public
    constructor Create;
    destructor Destroy; override;

    procedure AddInterceptor(const AInterceptor: INest4DAsyncInterceptor);
    procedure RemoveInterceptor(const AInterceptor: INest4DAsyncInterceptor);
    procedure RemoveInterceptorByName(const AName: String);
    function GetInterceptor(const AName: String): INest4DAsyncInterceptor;
    procedure ClearInterceptors;

    procedure ExecuteBeforeAsync(const AContext: TAsyncInterceptorContext; const ACallback: TAsyncInterceptorCallback = nil);
    procedure ExecuteAfterAsync(const AContext: TAsyncInterceptorContext; const ACallback: TAsyncInterceptorCallback = nil);
    procedure ExecuteOnErrorAsync(const AContext: TAsyncInterceptorContext; const AException: Exception; const ACallback: TAsyncInterceptorErrorCallback = nil);

    function GetInterceptorCount: Integer;
    function GetInterceptorNames: TArray<String>;
    procedure EnableAll;
    procedure DisableAll;
  end;

  // Pipeline de interceptors ass?ncronos
  TNest4DAsyncInterceptorPipeline = class
  private
    FManager: TNest4DAsyncInterceptorManager;
    FAsyncExecutor: TNest4DAsyncExecutor;
    FPipelineId: String;
    FStartTime: TDateTime;
    FMetrics: TDictionary<String, Variant>;

  public
    constructor Create(const AManager: TNest4DAsyncInterceptorManager; const APipelineId: String = '');
    destructor Destroy; override;

    procedure ExecuteAsync(
      const ARequestId: String;
      const AExecuteProc: TProc;
      const AOnSuccess: TAsyncInterceptorCallback = nil;
      const AOnError: TAsyncInterceptorErrorCallback = nil
    );

    function GetPipelineId: String;
    function GetMetrics: TDictionary<String, Variant>;
    procedure AddMetric(const AKey: String; const AValue: Variant);
  end;

// Fun??es utilit?rias
function GetAsyncInterceptorManager: TNest4DAsyncInterceptorManager;
function CreateAsyncLoggingInterceptor(const AAsyncLogger: INest4DAsyncLogger; ALogLevel: TLogLevel = llInfo): TNest4DAsyncLoggingInterceptor;
function CreateAsyncMetricsInterceptor(const AAsyncMetrics: INest4DAsyncMetrics): TNest4DAsyncMetricsInterceptor;
function CreateAsyncHealthInterceptor(const AAsyncHealth: INest4DAsyncHealth; ACheckInterval: Integer = 60): TNest4DAsyncHealthInterceptor;
function CreateAsyncResilienceInterceptor(ARetryCount: Integer = 3; ARetryDelay: Integer = 1000): TNest4DAsyncResilienceInterceptor;

implementation

uses
  Winapi.Windows,
  System.Math;

var
  GAsyncInterceptorManager: TNest4DAsyncInterceptorManager;
  GAsyncInterceptorLock: TCriticalSection;

function GetAsyncInterceptorManager: TNest4DAsyncInterceptorManager;
begin
  if not Assigned(GAsyncInterceptorManager) then
  begin
    if not Assigned(GAsyncInterceptorLock) then
      GAsyncInterceptorLock := TCriticalSection.Create;

    GAsyncInterceptorLock.Enter;
    try
      if not Assigned(GAsyncInterceptorManager) then
        GAsyncInterceptorManager := TNest4DAsyncInterceptorManager.Create;
    finally
      GAsyncInterceptorLock.Leave;
    end;
  end;
  Result := GAsyncInterceptorManager;
end;

function CreateAsyncLoggingInterceptor(const AAsyncLogger: INest4DAsyncLogger; ALogLevel: TLogLevel = llInfo): TNest4DAsyncLoggingInterceptor;
begin
  Result := TNest4DAsyncLoggingInterceptor.Create(AAsyncLogger, ALogLevel);
end;

function CreateAsyncMetricsInterceptor(const AAsyncMetrics: INest4DAsyncMetrics): TNest4DAsyncMetricsInterceptor;
begin
  Result := TNest4DAsyncMetricsInterceptor.Create(AAsyncMetrics);
end;

function CreateAsyncHealthInterceptor(const AAsyncHealth: INest4DAsyncHealth; ACheckInterval: Integer = 60): TNest4DAsyncHealthInterceptor;
begin
  Result := TNest4DAsyncHealthInterceptor.Create(AAsyncHealth, ACheckInterval);
end;

function CreateAsyncResilienceInterceptor(ARetryCount: Integer = 3; ARetryDelay: Integer = 1000): TNest4DAsyncResilienceInterceptor;
begin
  Result := TNest4DAsyncResilienceInterceptor.Create(ARetryCount, ARetryDelay);
end;

{ TAsyncInterceptorContext }

constructor TAsyncInterceptorContext.Create(const ARequestId: String);
begin
  RequestId := ARequestId;
  StartTime := Now;
  EndTime := 0;
  Duration := 0;
  Success := False;
  ErrorMessage := '';
  Metadata := TDictionary<String, Variant>.Create;
end;

procedure TAsyncInterceptorContext.Finish(ASuccess: Boolean; const AError: String = '');
begin
  EndTime := Now;
  Duration := MilliSecondsBetween(EndTime, StartTime);
  Success := ASuccess;
  ErrorMessage := AError;
end;

function TAsyncInterceptorContext.GetDuration: Double;
begin
  if EndTime > 0 then
    Result := Duration
  else
    Result := MilliSecondsBetween(Now, StartTime);
end;

{ TNest4DAsyncInterceptorBase }

constructor TNest4DAsyncInterceptorBase.Create(const AName: String; APriority: Integer = 100);
begin
  inherited Create;
  FName := AName;
  FPriority := APriority;
  FEnabled := True;
  FAsyncExecutor := TNest4DAsyncExecutor.GetInstance;
end;

destructor TNest4DAsyncInterceptorBase.Destroy;
begin
  inherited Destroy;
end;

procedure TNest4DAsyncInterceptorBase.BeforeAsync(const AContext: TAsyncInterceptorContext; const ACallback: TAsyncInterceptorCallback = nil);
begin
  if not FEnabled then
  begin
    if Assigned(ACallback) then
      ACallback(AContext);
    Exit;
  end;

  FAsyncExecutor.ExecuteAsync(
    procedure
    begin
      try
        DoBeforeAsync(AContext, ACallback);
      except
        on E: Exception do
        begin
          // Log error silently and continue
          if Assigned(ACallback) then
            ACallback(AContext);
        end;
      end;
    end);
end;

procedure TNest4DAsyncInterceptorBase.AfterAsync(const AContext: TAsyncInterceptorContext; const ACallback: TAsyncInterceptorCallback = nil);
begin
  if not FEnabled then
  begin
    if Assigned(ACallback) then
      ACallback(AContext);
    Exit;
  end;

  FAsyncExecutor.ExecuteAsync(
    procedure
    begin
      try
        DoAfterAsync(AContext, ACallback);
      except
        on E: Exception do
        begin
          if Assigned(ACallback) then
            ACallback(AContext);
        end;
      end;
    end);
end;

procedure TNest4DAsyncInterceptorBase.OnErrorAsync(const AContext: TAsyncInterceptorContext; const AException: Exception; const ACallback: TAsyncInterceptorErrorCallback = nil);
begin
  if not FEnabled then
  begin
    if Assigned(ACallback) then
      ACallback(AContext, AException);
    Exit;
  end;

  FAsyncExecutor.ExecuteAsync(
    procedure
    begin
      try
        DoOnErrorAsync(AContext, AException, ACallback);
      except
        on E: Exception do
        begin
          if Assigned(ACallback) then
            ACallback(AContext, AException);
        end;
      end;
    end);
end;

procedure TNest4DAsyncInterceptorBase.DoBeforeAsync(const AContext: TAsyncInterceptorContext; const ACallback: TAsyncInterceptorCallback);
begin
  // Implementa??o padr?o - apenas chama callback
  if Assigned(ACallback) then
    ACallback(AContext);
end;

procedure TNest4DAsyncInterceptorBase.DoAfterAsync(const AContext: TAsyncInterceptorContext; const ACallback: TAsyncInterceptorCallback);
begin
  // Implementa??o padr?o - apenas chama callback
  if Assigned(ACallback) then
    ACallback(AContext);
end;

procedure TNest4DAsyncInterceptorBase.DoOnErrorAsync(const AContext: TAsyncInterceptorContext; const AException: Exception; const ACallback: TAsyncInterceptorErrorCallback);
begin
  // Implementa??o padr?o - apenas chama callback
  if Assigned(ACallback) then
    ACallback(AContext, AException);
end;

function TNest4DAsyncInterceptorBase.GetName: String;
begin
  Result := FName;
end;

function TNest4DAsyncInterceptorBase.GetPriority: Integer;
begin
  Result := FPriority;
end;

function TNest4DAsyncInterceptorBase.IsEnabled: Boolean;
begin
  Result := FEnabled;
end;

procedure TNest4DAsyncInterceptorBase.SetEnabled(AEnabled: Boolean);
begin
  FEnabled := AEnabled;
end;

{ TNest4DAsyncLoggingInterceptor }

constructor TNest4DAsyncLoggingInterceptor.Create(const AAsyncLogger: INest4DAsyncLogger; ALogLevel: TLogLevel = llInfo);
begin
  inherited Create('AsyncLogging', 200);
  FAsyncLogger := AAsyncLogger;
  FLogLevel := ALogLevel;
  FIncludeRequestBody := False;
  FIncludeResponseBody := False;
  FMaxBodySize := 1024;
end;

procedure TNest4DAsyncLoggingInterceptor.DoBeforeAsync(const AContext: TAsyncInterceptorContext; const ACallback: TAsyncInterceptorCallback);
begin
  FAsyncLogger.InfoAsync(
    Format('Request started: %s', [AContext.RequestId]),
    procedure
    begin
      if Assigned(ACallback) then
        ACallback(AContext);
    end);
end;

procedure TNest4DAsyncLoggingInterceptor.DoAfterAsync(const AContext: TAsyncInterceptorContext; const ACallback: TAsyncInterceptorCallback);
begin
  FAsyncLogger.InfoAsync(
    Format('Request completed: %s (Duration: %.2fms, Success: %s)',
      [AContext.RequestId, AContext.GetDuration, BoolToStr(AContext.Success, True)]),
    procedure
    begin
      if Assigned(ACallback) then
        ACallback(AContext);
    end);
end;

procedure TNest4DAsyncLoggingInterceptor.DoOnErrorAsync(const AContext: TAsyncInterceptorContext; const AException: Exception; const ACallback: TAsyncInterceptorErrorCallback);
begin
  FAsyncLogger.ErrorAsync(
    Format('Request failed: %s - %s', [AContext.RequestId, AException.Message]),
    procedure
    begin
      if Assigned(ACallback) then
        ACallback(AContext, AException);
    end);
end;

procedure TNest4DAsyncLoggingInterceptor.SetLogLevel(ALevel: TLogLevel);
begin
  FLogLevel := ALevel;
end;

procedure TNest4DAsyncLoggingInterceptor.SetBodyLogging(AIncludeRequest, AIncludeResponse: Boolean; AMaxSize: Integer = 1024);
begin
  FIncludeRequestBody := AIncludeRequest;
  FIncludeResponseBody := AIncludeResponse;
  FMaxBodySize := AMaxSize;
end;

{ TNest4DAsyncMetricsInterceptor }

constructor TNest4DAsyncMetricsInterceptor.Create(const AAsyncMetrics: INest4DAsyncMetrics);
begin
  inherited Create('AsyncMetrics', 150);
  FAsyncMetrics := AAsyncMetrics;
  FTrackTiming := True;
  FTrackMemory := True;
  FTrackConcurrency := True;
  FConcurrentRequests := 0;
  FConcurrencyLock := TCriticalSection.Create;
end;

destructor TNest4DAsyncMetricsInterceptor.Destroy;
begin
  FConcurrencyLock.Free;
  inherited Destroy;
end;

procedure TNest4DAsyncMetricsInterceptor.DoBeforeAsync(const AContext: TAsyncInterceptorContext; const ACallback: TAsyncInterceptorCallback);
begin
  if FTrackConcurrency then
  begin
    FConcurrencyLock.Enter;
    try
      Inc(FConcurrentRequests);
      FAsyncMetrics.GaugeAsync('concurrent_requests', FConcurrentRequests);
    finally
      FConcurrencyLock.Leave;
    end;
  end;

  FAsyncMetrics.CounterAsync('requests_started', 1,
    procedure
    begin
      if Assigned(ACallback) then
        ACallback(AContext);
    end);
end;

procedure TNest4DAsyncMetricsInterceptor.DoAfterAsync(const AContext: TAsyncInterceptorContext; const ACallback: TAsyncInterceptorCallback);
begin
  if FTrackTiming then
    FAsyncMetrics.HistogramAsync('request_duration_ms', AContext.GetDuration);

  if FTrackConcurrency then
  begin
    FConcurrencyLock.Enter;
    try
      Dec(FConcurrentRequests);
      FAsyncMetrics.GaugeAsync('concurrent_requests', FConcurrentRequests);
    finally
      FConcurrencyLock.Leave;
    end;
  end;

  if AContext.Success then
    FAsyncMetrics.CounterAsync('requests_success', 1)
  else
    FAsyncMetrics.CounterAsync('requests_failed', 1);

  FAsyncMetrics.CounterAsync('requests_completed', 1,
    procedure
    begin
      if Assigned(ACallback) then
        ACallback(AContext);
    end);
end;

procedure TNest4DAsyncMetricsInterceptor.DoOnErrorAsync(const AContext: TAsyncInterceptorContext; const AException: Exception; const ACallback: TAsyncInterceptorErrorCallback);
begin
  FAsyncMetrics.CounterAsync('requests_error', 1);
  FAsyncMetrics.CounterAsync('errors_by_type.' + AException.ClassName, 1,
    procedure
    begin
      if Assigned(ACallback) then
        ACallback(AContext, AException);
    end);
end;

procedure TNest4DAsyncMetricsInterceptor.SetTrackingOptions(ATiming, AMemory, AConcurrency: Boolean);
begin
  FTrackTiming := ATiming;
  FTrackMemory := AMemory;
  FTrackConcurrency := AConcurrency;
end;

function TNest4DAsyncMetricsInterceptor.GetConcurrentRequests: Integer;
begin
  FConcurrencyLock.Enter;
  try
    Result := FConcurrentRequests;
  finally
    FConcurrencyLock.Leave;
  end;
end;

{ TNest4DAsyncHealthInterceptor }

constructor TNest4DAsyncHealthInterceptor.Create(const AAsyncHealth: INest4DAsyncHealth; ACheckInterval: Integer = 60);
begin
  inherited Create('AsyncHealth', 300);
  FAsyncHealth := AAsyncHealth;
  FCheckInterval := ACheckInterval;
  FLastCheck := 0;
  FHealthStatus := 'Unknown';
  FHealthLock := TCriticalSection.Create;
end;

destructor TNest4DAsyncHealthInterceptor.Destroy;
begin
  FHealthLock.Free;
  inherited Destroy;
end;

procedure TNest4DAsyncHealthInterceptor.DoBeforeAsync(const AContext: TAsyncInterceptorContext; const ACallback: TAsyncInterceptorCallback);
begin
  if ShouldCheckHealth then
    UpdateHealthStatus;

  if Assigned(ACallback) then
    ACallback(AContext);
end;

procedure TNest4DAsyncHealthInterceptor.DoAfterAsync(const AContext: TAsyncInterceptorContext; const ACallback: TAsyncInterceptorCallback);
begin
  // Health check ap?s requisi??o se necess?rio
  if ShouldCheckHealth then
    UpdateHealthStatus;

  if Assigned(ACallback) then
    ACallback(AContext);
end;

procedure TNest4DAsyncHealthInterceptor.UpdateHealthStatus;
begin
  FAsyncHealth.CheckAllAsync(
    procedure(const AResults: TArray<TAsyncHealthResult>)
    var
      LOverallHealth: String;
      LResult: TAsyncHealthResult;
      LHealthy: Boolean;
    begin
      LHealthy := True;
      for LResult in AResults do
      begin
        if not LResult.IsHealthy then
        begin
          LHealthy := False;
          Break;
        end;
      end;

      if LHealthy then
        LOverallHealth := 'Healthy'
      else
        LOverallHealth := 'Unhealthy';

      FHealthLock.Enter;
      try
        FHealthStatus := LOverallHealth;
        FLastCheck := Now;
      finally
        FHealthLock.Leave;
      end;
    end);
end;

function TNest4DAsyncHealthInterceptor.ShouldCheckHealth: Boolean;
begin
  FHealthLock.Enter;
  try
    Result := (FLastCheck = 0) or (SecondsBetween(Now, FLastCheck) >= FCheckInterval);
  finally
    FHealthLock.Leave;
  end;
end;

procedure TNest4DAsyncHealthInterceptor.SetCheckInterval(AIntervalSeconds: Integer);
begin
  FCheckInterval := AIntervalSeconds;
end;

function TNest4DAsyncHealthInterceptor.GetHealthStatus: String;
begin
  FHealthLock.Enter;
  try
    Result := FHealthStatus;
  finally
    FHealthLock.Leave;
  end;
end;

procedure TNest4DAsyncHealthInterceptor.ForceHealthCheck;
begin
  FHealthLock.Enter;
  try
    FLastCheck := 0; // Force next check
  finally
    FHealthLock.Leave;
  end;
  UpdateHealthStatus;
end;

{ TNest4DAsyncResilienceInterceptor }

constructor TNest4DAsyncResilienceInterceptor.Create(ARetryCount: Integer = 3; ARetryDelay: Integer = 1000; ACircuitThreshold: Integer = 5; ACircuitTimeout: Integer = 30000);
begin
  inherited Create('AsyncResilience', 50);
  FRetryCount := ARetryCount;
  FRetryDelay := ARetryDelay;
  FCircuitBreakerThreshold := ACircuitThreshold;
  FCircuitBreakerTimeout := ACircuitTimeout;
  FFailureCount := 0;
  FLastFailure := 0;
  FCircuitOpen := False;
  FResilienceLock := TCriticalSection.Create;
end;

destructor TNest4DAsyncResilienceInterceptor.Destroy;
begin
  FResilienceLock.Free;
  inherited Destroy;
end;

procedure TNest4DAsyncResilienceInterceptor.DoBeforeAsync(const AContext: TAsyncInterceptorContext; const ACallback: TAsyncInterceptorCallback);
begin
  // Verifica circuit breaker
  if IsCircuitOpen then
  begin
    AContext.Metadata.Add('circuit_breaker_open', True);
    AContext.Finish(False, 'Circuit breaker is open');
  end;

  if Assigned(ACallback) then
    ACallback(AContext);
end;

procedure TNest4DAsyncResilienceInterceptor.DoAfterAsync(const AContext: TAsyncInterceptorContext; const ACallback: TAsyncInterceptorCallback);
begin
  UpdateCircuitBreaker(AContext.Success);

  if Assigned(ACallback) then
    ACallback(AContext);
end;

procedure TNest4DAsyncResilienceInterceptor.DoOnErrorAsync(const AContext: TAsyncInterceptorContext; const AException: Exception; const ACallback: TAsyncInterceptorErrorCallback);
begin
  UpdateCircuitBreaker(False);

  if Assigned(ACallback) then
    ACallback(AContext, AException);
end;

function TNest4DAsyncResilienceInterceptor.ShouldRetry(const AContext: TAsyncInterceptorContext; AAttempt: Integer): Boolean;
begin
  Result := (AAttempt < FRetryCount) and not IsCircuitOpen;
end;

procedure TNest4DAsyncResilienceInterceptor.UpdateCircuitBreaker(ASuccess: Boolean);
begin
  FResilienceLock.Enter;
  try
    if ASuccess then
    begin
      FFailureCount := 0;
      FCircuitOpen := False;
    end
    else
    begin
      Inc(FFailureCount);
      FLastFailure := Now;

      if FFailureCount >= FCircuitBreakerThreshold then
        FCircuitOpen := True;
    end;
  finally
    FResilienceLock.Leave;
  end;
end;

function TNest4DAsyncResilienceInterceptor.IsCircuitOpen: Boolean;
begin
  FResilienceLock.Enter;
  try
    if FCircuitOpen and (MilliSecondsBetween(Now, FLastFailure) >= FCircuitBreakerTimeout) then
    begin
      // Tenta fechar o circuit breaker
      FCircuitOpen := False;
      FFailureCount := 0;
    end;

    Result := FCircuitOpen;
  finally
    FResilienceLock.Leave;
  end;
end;

procedure TNest4DAsyncResilienceInterceptor.SetRetryPolicy(ACount: Integer; ADelay: Integer);
begin
  FRetryCount := ACount;
  FRetryDelay := ADelay;
end;

procedure TNest4DAsyncResilienceInterceptor.SetCircuitBreaker(AThreshold: Integer; ATimeout: Integer);
begin
  FCircuitBreakerThreshold := AThreshold;
  FCircuitBreakerTimeout := ATimeout;
end;

function TNest4DAsyncResilienceInterceptor.GetCircuitStatus: String;
begin
  FResilienceLock.Enter;
  try
    if FCircuitOpen then
      Result := Format('Open (Failures: %d, Last: %s)', [FFailureCount, DateTimeToStr(FLastFailure)])
    else
      Result := Format('Closed (Failures: %d)', [FFailureCount]);
  finally
    FResilienceLock.Leave;
  end;
end;

procedure TNest4DAsyncResilienceInterceptor.ResetCircuit;
begin
  FResilienceLock.Enter;
  try
    FCircuitOpen := False;
    FFailureCount := 0;
    FLastFailure := 0;
  finally
    FResilienceLock.Leave;
  end;
end;

{ TNest4DAsyncInterceptorManager }

constructor TNest4DAsyncInterceptorManager.Create;
begin
  inherited Create;
  FInterceptors := TList<INest4DAsyncInterceptor>.Create;
  FLock := TReaderWriterLockSlim.Create;
  FAsyncExecutor := TNest4DAsyncExecutor.GetInstance;
end;

destructor TNest4DAsyncInterceptorManager.Destroy;
begin
  FLock.Free;
  FInterceptors.Free;
  inherited Destroy;
end;

procedure TNest4DAsyncInterceptorManager.SortInterceptorsByPriority;
begin
  FInterceptors.Sort(TComparer<INest4DAsyncInterceptor>.Construct(
    function(const Left, Right: INest4DAsyncInterceptor): Integer
    begin
      Result := Left.GetPriority - Right.GetPriority;
    end));
end;

procedure TNest4DAsyncInterceptorManager.AddInterceptor(const AInterceptor: INest4DAsyncInterceptor);
begin
  FLock.BeginWrite;
  try
    FInterceptors.Add(AInterceptor);
    SortInterceptorsByPriority;
  finally
    FLock.EndWrite;
  end;
end;

procedure TNest4DAsyncInterceptorManager.RemoveInterceptor(const AInterceptor: INest4DAsyncInterceptor);
begin
  FLock.BeginWrite;
  try
    FInterceptors.Remove(AInterceptor);
  finally
    FLock.EndWrite;
  end;
end;

procedure TNest4DAsyncInterceptorManager.RemoveInterceptorByName(const AName: String);
var
  I: Integer;
begin
  FLock.BeginWrite;
  try
    for I := FInterceptors.Count - 1 downto 0 do
    begin
      if FInterceptors[I].GetName = AName then
        FInterceptors.Delete(I);
    end;
  finally
    FLock.EndWrite;
  end;
end;

function TNest4DAsyncInterceptorManager.GetInterceptor(const AName: String): INest4DAsyncInterceptor;
var
  LInterceptor: INest4DAsyncInterceptor;
begin
  Result := nil;

  FLock.BeginRead;
  try
    for LInterceptor in FInterceptors do
    begin
      if LInterceptor.GetName = AName then
      begin
        Result := LInterceptor;
        Break;
      end;
    end;
  finally
    FLock.EndRead;
  end;
end;

procedure TNest4DAsyncInterceptorManager.ClearInterceptors;
begin
  FLock.BeginWrite;
  try
    FInterceptors.Clear;
  finally
    FLock.EndWrite;
  end;
end;

procedure TNest4DAsyncInterceptorManager.ExecuteBeforeAsync(const AContext: TAsyncInterceptorContext; const ACallback: TAsyncInterceptorCallback = nil);
var
  LInterceptors: TArray<INest4DAsyncInterceptor>;
  I: Integer;
begin
  FLock.BeginRead;
  try
    LInterceptors := FInterceptors.ToArray;
  finally
    FLock.EndRead;
  end;

  // Executa interceptors em paralelo
  FAsyncExecutor.WaitAll(
    function: TArray<TNest4DPromise<Boolean>>
    var
      LPromises: TArray<TNest4DPromise<Boolean>>;
      LInterceptor: INest4DAsyncInterceptor;
    begin
      SetLength(LPromises, Length(LInterceptors));

      for I := 0 to High(LInterceptors) do
      begin
        LInterceptor := LInterceptors[I];
        LPromises[I] := FAsyncExecutor.ExecuteAsync<Boolean>(
          function: Boolean
          begin
            try
              LInterceptor.BeforeAsync(AContext);
              Result := True;
            except
              Result := False;
            end;
          end);
      end;

      Result := LPromises;
    end(),
    procedure(const AResults: TArray<Boolean>)
    begin
      if Assigned(ACallback) then
        ACallback(AContext);
    end);
end;

procedure TNest4DAsyncInterceptorManager.ExecuteAfterAsync(const AContext: TAsyncInterceptorContext; const ACallback: TAsyncInterceptorCallback = nil);
var
  LInterceptors: TArray<INest4DAsyncInterceptor>;
  I: Integer;
begin
  FLock.BeginRead;
  try
    LInterceptors := FInterceptors.ToArray;
  finally
    FLock.EndRead;
  end;

  FAsyncExecutor.WaitAll(
    function: TArray<TNest4DPromise<Boolean>>
    var
      LPromises: TArray<TNest4DPromise<Boolean>>;
      LInterceptor: INest4DAsyncInterceptor;
    begin
      SetLength(LPromises, Length(LInterceptors));

      for I := 0 to High(LInterceptors) do
      begin
        LInterceptor := LInterceptors[I];
        LPromises[I] := FAsyncExecutor.ExecuteAsync<Boolean>(
          function: Boolean
          begin
            try
              LInterceptor.AfterAsync(AContext);
              Result := True;
            except
              Result := False;
            end;
          end);
      end;

      Result := LPromises;
    end(),
    procedure(const AResults: TArray<Boolean>)
    begin
      if Assigned(ACallback) then
        ACallback(AContext);
    end);
end;

procedure TNest4DAsyncInterceptorManager.ExecuteOnErrorAsync(const AContext: TAsyncInterceptorContext; const AException: Exception; const ACallback: TAsyncInterceptorErrorCallback = nil);
var
  LInterceptors: TArray<INest4DAsyncInterceptor>;
  I: Integer;
begin
  FLock.BeginRead;
  try
    LInterceptors := FInterceptors.ToArray;
  finally
    FLock.EndRead;
  end;

  FAsyncExecutor.WaitAll(
    function: TArray<TNest4DPromise<Boolean>>
    var
      LPromises: TArray<TNest4DPromise<Boolean>>;
      LInterceptor: INest4DAsyncInterceptor;
    begin
      SetLength(LPromises, Length(LInterceptors));

      for I := 0 to High(LInterceptors) do
      begin
        LInterceptor := LInterceptors[I];
        LPromises[I] := FAsyncExecutor.ExecuteAsync<Boolean>(
          function: Boolean
          begin
            try
              LInterceptor.OnErrorAsync(AContext, AException);
              Result := True;
            except
              Result := False;
            end;
          end);
      end;

      Result := LPromises;
    end(),
    procedure(const AResults: TArray<Boolean>)
    begin
      if Assigned(ACallback) then
        ACallback(AContext, AException);
    end);
end;

function TNest4DAsyncInterceptorManager.GetInterceptorCount: Integer;
begin
  FLock.BeginRead;
  try
    Result := FInterceptors.Count;
  finally
    FLock.EndRead;
  end;
end;

function TNest4DAsyncInterceptorManager.GetInterceptorNames: TArray<String>;
var
  LInterceptor: INest4DAsyncInterceptor;
  I: Integer;
begin
  FLock.BeginRead;
  try
    SetLength(Result, FInterceptors.Count);
    for I := 0 to FInterceptors.Count - 1 do
      Result[I] := FInterceptors[I].GetName;
  finally
    FLock.EndRead;
  end;
end;

procedure TNest4DAsyncInterceptorManager.EnableAll;
var
  LInterceptor: INest4DAsyncInterceptor;
begin
  FLock.BeginRead;
  try
    for LInterceptor in FInterceptors do
      LInterceptor.SetEnabled(True);
  finally
    FLock.EndRead;
  end;
end;

procedure TNest4DAsyncInterceptorManager.DisableAll;
var
  LInterceptor: INest4DAsyncInterceptor;
begin
  FLock.BeginRead;
  try
    for LInterceptor in FInterceptors do
      LInterceptor.SetEnabled(False);
  finally
    FLock.EndRead;
  end;
end;

{ TNest4DAsyncInterceptorPipeline }

constructor TNest4DAsyncInterceptorPipeline.Create(const AManager: TNest4DAsyncInterceptorManager; const APipelineId: String = '');
begin
  inherited Create;
  FManager := AManager;
  FAsyncExecutor := TNest4DAsyncExecutor.GetInstance;
  FPipelineId := IfThen(APipelineId <> '', APipelineId, TGuid.NewGuid.ToString);
  FStartTime := Now;
  FMetrics := TDictionary<String, Variant>.Create;
end;

destructor TNest4DAsyncInterceptorPipeline.Destroy;
begin
  FMetrics.Free;
  inherited Destroy;
end;

procedure TNest4DAsyncInterceptorPipeline.ExecuteAsync(
  const ARequestId: String;
  const AExecuteProc: TProc;
  const AOnSuccess: TAsyncInterceptorCallback = nil;
  const AOnError: TAsyncInterceptorErrorCallback = nil
);
var
  LContext: TAsyncInterceptorContext;
begin
  LContext := TAsyncInterceptorContext.Create(ARequestId);

  // Executa Before interceptors
  FManager.ExecuteBeforeAsync(LContext,
    procedure(const ABeforeContext: TAsyncInterceptorContext)
    begin
      // Executa a opera??o principal
      FAsyncExecutor.ExecuteAsync(
        procedure
        begin
          try
            AExecuteProc();
            LContext.Finish(True);

            // Executa After interceptors
            FManager.ExecuteAfterAsync(LContext,
              procedure(const AAfterContext: TAsyncInterceptorContext)
              begin
                if Assigned(AOnSuccess) then
                  AOnSuccess(LContext);
              end);
          except
            on E: Exception do
            begin
              LContext.Finish(False, E.Message);

              // Executa Error interceptors
              FManager.ExecuteOnErrorAsync(LContext, E,
                procedure(const AErrorContext: TAsyncInterceptorContext; const AException: Exception)
                begin
                  if Assigned(AOnError) then
                    AOnError(LContext, E);
                end);
            end;
          end;
        end);
    end);
end;

function TNest4DAsyncInterceptorPipeline.GetPipelineId: String;
begin
  Result := FPipelineId;
end;

function TNest4DAsyncInterceptorPipeline.GetMetrics: TDictionary<String, Variant>;
begin
  Result := FMetrics;
end;

procedure TNest4DAsyncInterceptorPipeline.AddMetric(const AKey: String; const AValue: Variant);
begin
  FMetrics.AddOrSetValue(AKey, AValue);
end;

initialization

finalization
  if Assigned(GAsyncInterceptorLock) then
  begin
    GAsyncInterceptorLock.Free;
    GAsyncInterceptorLock := nil;
  end;
  GAsyncInterceptorManager := nil;

end.

