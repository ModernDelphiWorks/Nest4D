unit Nest4D.Resilience.CircuitBreaker;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  Nest4D.Resilience.Interfaces,
  Nest4D.Request.Data;

type
  // Implementa??o do Circuit Breaker
  TCircuitBreaker = class(TInterfacedObject, ICircuitBreaker)
  private
    FState: TCircuitBreakerState;
    FFailureThreshold: Integer;
    FSuccessThreshold: Integer;
    FTimeoutMs: Cardinal;
    FLastFailureTime: TDateTime;
    FFailureCount: Integer;
    FSuccessCount: Integer;
    FTotalRequests: Integer;
    FTotalFailures: Integer;
    FTotalSuccesses: Integer;
    FLastStateChange: TDateTime;
    FCriticalSection: TRTLCriticalSection;
    FOnStateChange: TCircuitBreakerStateChangeEvent;
    FName: string;
    FMetrics: TCircuitBreakerMetrics;

    procedure InitializeCriticalSection;
    procedure FinalizeCriticalSection;
    procedure Lock;
    procedure Unlock;

    procedure ChangeState(const ANewState: TCircuitBreakerState);
    function ShouldAttemptReset: Boolean;
    procedure UpdateMetrics(const ASuccess: Boolean; const AExecutionTime: Cardinal);
    procedure ResetCounters;
    function GetFailureRate: Double;

  public
    constructor Create(const AName: string; const AFailureThreshold: Integer = 5;
      const ASuccessThreshold: Integer = 3; const ATimeoutMs: Cardinal = 60000);
    destructor Destroy; override;

    // ICircuitBreaker implementation
    function CanExecute: Boolean;
    procedure RecordSuccess(const AExecutionTime: Cardinal = 0);
    procedure RecordFailure(const AException: Exception = nil);
    function GetState: TCircuitBreakerState;
    function GetMetrics: TCircuitBreakerMetrics;
    procedure Reset;

    // M?todos adicionais
    procedure SetFailureThreshold(const AThreshold: Integer);
    procedure SetSuccessThreshold(const AThreshold: Integer);
    procedure SetTimeout(const ATimeoutMs: Cardinal);
    procedure SetStateChangeCallback(const ACallback: TCircuitBreakerStateChangeEvent);

    function GetStateAsString: string;
    function GetDetailedStats: string;
    function IsHealthy: Boolean;

    property Name: string read FName;
    property State: TCircuitBreakerState read GetState;
    property FailureThreshold: Integer read FFailureThreshold write SetFailureThreshold;
    property SuccessThreshold: Integer read FSuccessThreshold write SetSuccessThreshold;
    property TimeoutMs: Cardinal read FTimeoutMs write SetTimeout;
    property OnStateChange: TCircuitBreakerStateChangeEvent read FOnStateChange write SetStateChangeCallback;
  end;

  // Gerenciador de m?ltiplos Circuit Breakers
  TCircuitBreakerManager = class
  private
    FCircuitBreakers: TDictionary<string, ICircuitBreaker>;
    FCriticalSection: TRTLCriticalSection;
    FDefaultFailureThreshold: Integer;
    FDefaultSuccessThreshold: Integer;
    FDefaultTimeoutMs: Cardinal;

    procedure InitializeCriticalSection;
    procedure FinalizeCriticalSection;
    procedure Lock;
    procedure Unlock;

  public
    constructor Create;
    destructor Destroy; override;

    function GetOrCreateCircuitBreaker(const AName: string): ICircuitBreaker; overload;
    function GetOrCreateCircuitBreaker(const AName: string; const AFailureThreshold: Integer;
      const ASuccessThreshold: Integer; const ATimeoutMs: Cardinal): ICircuitBreaker; overload;

    function GetCircuitBreaker(const AName: string): ICircuitBreaker;
    procedure RemoveCircuitBreaker(const AName: string);

    function GetAllCircuitBreakers: TArray<ICircuitBreaker>;
    function GetCircuitBreakerNames: TArray<string>;

    procedure ResetAll;
    function GetOverallHealth: Boolean;
    function GetManagerStats: string;

    // Configura??es padr?o
    procedure SetDefaultFailureThreshold(const AThreshold: Integer);
    procedure SetDefaultSuccessThreshold(const AThreshold: Integer);
    procedure SetDefaultTimeout(const ATimeoutMs: Cardinal);

    property DefaultFailureThreshold: Integer read FDefaultFailureThreshold write SetDefaultFailureThreshold;
    property DefaultSuccessThreshold: Integer read FDefaultSuccessThreshold write SetDefaultSuccessThreshold;
    property DefaultTimeoutMs: Cardinal read FDefaultTimeoutMs write SetDefaultTimeout;
  end;

  // Factory para criar Circuit Breakers pr?-configurados
  TCircuitBreakerFactory = class
  public
    class function CreateDefault(const AName: string): ICircuitBreaker;
    class function CreateSensitive(const AName: string): ICircuitBreaker;
    class function CreateTolerant(const AName: string): ICircuitBreaker;
    class function CreateCustom(const AName: string; const AFailureThreshold: Integer;
      const ASuccessThreshold: Integer; const ATimeoutMs: Cardinal): ICircuitBreaker;
  end;

implementation

uses
  DateUtils,
  Math,
  Nest4D.Logging;

{ TCircuitBreaker }

constructor TCircuitBreaker.Create(const AName: string; const AFailureThreshold: Integer;
  const ASuccessThreshold: Integer; const ATimeoutMs: Cardinal);
begin
  inherited Create;

  FName := AName;
  FState := cbsClosed;
  FFailureThreshold := AFailureThreshold;
  FSuccessThreshold := ASuccessThreshold;
  FTimeoutMs := ATimeoutMs;

  FLastFailureTime := 0;
  FFailureCount := 0;
  FSuccessCount := 0;
  FTotalRequests := 0;
  FTotalFailures := 0;
  FTotalSuccesses := 0;
  FLastStateChange := Now;

  InitializeCriticalSection;

  // Inicializa m?tricas
  FMetrics.State := FState;
  FMetrics.FailureCount := 0;
  FMetrics.SuccessCount := 0;
  FMetrics.TotalRequests := 0;
  FMetrics.FailureRate := 0.0;
  FMetrics.LastFailureTime := 0;
  FMetrics.LastStateChange := FLastStateChange;
  FMetrics.AverageResponseTime := 0;

  TNest4DLogger.Info(Format('Circuit Breaker criado: %s (Falhas: %d, Sucessos: %d, Timeout: %dms)',
    [FName, FFailureThreshold, FSuccessThreshold, FTimeoutMs]));
end;

destructor TCircuitBreaker.Destroy;
begin
  Lock;
  try
    TNest4DLogger.Info(Format('Circuit Breaker destru?do: %s', [FName]));
  finally
    Unlock;
    FinalizeCriticalSection;
  end;

  inherited Destroy;
end;

procedure TCircuitBreaker.InitializeCriticalSection;
begin
  InitializeCriticalSection(FCriticalSection);
end;

procedure TCircuitBreaker.FinalizeCriticalSection;
begin
  DeleteCriticalSection(FCriticalSection);
end;

procedure TCircuitBreaker.Lock;
begin
  EnterCriticalSection(FCriticalSection);
end;

procedure TCircuitBreaker.Unlock;
begin
  LeaveCriticalSection(FCriticalSection);
end;

function TCircuitBreaker.CanExecute: Boolean;
begin
  Lock;
  try
    case FState of
      cbsClosed:
        Result := True;

      cbsOpen:
        begin
          if ShouldAttemptReset then
          begin
            ChangeState(cbsHalfOpen);
            Result := True;
          end
          else
            Result := False;
        end;

      cbsHalfOpen:
        Result := True;

    else
      Result := False;
    end;

    if Result then
      Inc(FTotalRequests);

  finally
    Unlock;
  end;
end;

procedure TCircuitBreaker.RecordSuccess(const AExecutionTime: Cardinal);
begin
  Lock;
  try
    Inc(FTotalSuccesses);
    UpdateMetrics(True, AExecutionTime);

    case FState of
      cbsClosed:
        begin
          // Reset failure count on success
          FFailureCount := 0;
        end;

      cbsHalfOpen:
        begin
          Inc(FSuccessCount);

          if FSuccessCount >= FSuccessThreshold then
          begin
            ChangeState(cbsClosed);
            ResetCounters;
          end;
        end;
    end;

    TNest4DLogger.Debug(Format('Circuit Breaker %s: Sucesso registrado (Tempo: %dms)', [FName, AExecutionTime]));
  finally
    Unlock;
  end;
end;

procedure TCircuitBreaker.RecordFailure(const AException: Exception);
var
  LExceptionMsg: string;
begin
  Lock;
  try
    Inc(FTotalFailures);
    FLastFailureTime := Now;
    UpdateMetrics(False, 0);

    LExceptionMsg := '';
    if Assigned(AException) then
      LExceptionMsg := AException.Message;

    case FState of
      cbsClosed:
        begin
          Inc(FFailureCount);

          if FFailureCount >= FFailureThreshold then
          begin
            ChangeState(cbsOpen);
          end;
        end;

      cbsHalfOpen:
        begin
          ChangeState(cbsOpen);
          ResetCounters;
        end;
    end;

    TNest4DLogger.Warn(Format('Circuit Breaker %s: Falha registrada - %s', [FName, LExceptionMsg]));
  finally
    Unlock;
  end;
end;

function TCircuitBreaker.GetState: TCircuitBreakerState;
begin
  Lock;
  try
    Result := FState;
  finally
    Unlock;
  end;
end;

function TCircuitBreaker.GetMetrics: TCircuitBreakerMetrics;
begin
  Lock;
  try
    FMetrics.State := FState;
    FMetrics.FailureCount := FFailureCount;
    FMetrics.SuccessCount := FSuccessCount;
    FMetrics.TotalRequests := FTotalRequests;
    FMetrics.FailureRate := GetFailureRate;
    FMetrics.LastFailureTime := FLastFailureTime;
    FMetrics.LastStateChange := FLastStateChange;

    Result := FMetrics;
  finally
    Unlock;
  end;
end;

procedure TCircuitBreaker.Reset;
begin
  Lock;
  try
    ChangeState(cbsClosed);
    ResetCounters;

    FLastFailureTime := 0;
    FTotalRequests := 0;
    FTotalFailures := 0;
    FTotalSuccesses := 0;

    TNest4DLogger.Info(Format('Circuit Breaker %s: Reset manual executado', [FName]));
  finally
    Unlock;
  end;
end;

procedure TCircuitBreaker.ChangeState(const ANewState: TCircuitBreakerState);
var
  LOldState: TCircuitBreakerState;
begin
  LOldState := FState;
  FState := ANewState;
  FLastStateChange := Now;

  TNest4DLogger.Info(Format('Circuit Breaker %s: Estado alterado de %s para %s',
    [FName, GetStateAsString(LOldState), GetStateAsString(ANewState)]));

  // Chama callback se definido
  if Assigned(FOnStateChange) then
  begin
    try
      FOnStateChange(Self, LOldState, ANewState);
    except
      on E: Exception do
        TNest4DLogger.Error(Format('Erro no callback de mudan?a de estado do Circuit Breaker %s: %s',
          [FName, E.Message]));
    end;
  end;
end;

function TCircuitBreaker.ShouldAttemptReset: Boolean;
begin
  Result := (FLastFailureTime > 0) and
            (MilliSecondsBetween(Now, FLastFailureTime) >= FTimeoutMs);
end;

procedure TCircuitBreaker.UpdateMetrics(const ASuccess: Boolean; const AExecutionTime: Cardinal);
begin
  // Atualiza tempo m?dio de resposta (m?dia m?vel simples)
  if ASuccess and (AExecutionTime > 0) then
  begin
    if FMetrics.AverageResponseTime = 0 then
      FMetrics.AverageResponseTime := AExecutionTime
    else
      FMetrics.AverageResponseTime := Round((FMetrics.AverageResponseTime + AExecutionTime) / 2);
  end;
end;

procedure TCircuitBreaker.ResetCounters;
begin
  FFailureCount := 0;
  FSuccessCount := 0;
end;

function TCircuitBreaker.GetFailureRate: Double;
begin
  if FTotalRequests = 0 then
    Result := 0.0
  else
    Result := (FTotalFailures / FTotalRequests) * 100.0;
end;

function TCircuitBreaker.GetStateAsString: string;
begin
  Result := GetStateAsString(FState);
end;

function TCircuitBreaker.GetStateAsString(const AState: TCircuitBreakerState): string;
begin
  case AState of
    cbsClosed: Result := 'Closed';
    cbsOpen: Result := 'Open';
    cbsHalfOpen: Result := 'Half-Open';
  else
    Result := 'Unknown';
  end;
end;

function TCircuitBreaker.GetDetailedStats: string;
var
  LStats: TStringBuilder;
  LUptime: string;
begin
  Lock;
  try
    LStats := TStringBuilder.Create;
    try
      LUptime := FormatDateTime('hh:nn:ss', Now - FLastStateChange);

      LStats.AppendFormat('Circuit Breaker: %s%s', [FName, sLineBreak]);
      LStats.AppendFormat('  Estado: %s (h? %s)%s', [GetStateAsString, LUptime, sLineBreak]);
      LStats.AppendFormat('  Configura??o:%s', [sLineBreak]);
      LStats.AppendFormat('    - Limite de falhas: %d%s', [FFailureThreshold, sLineBreak]);
      LStats.AppendFormat('    - Limite de sucessos: %d%s', [FSuccessThreshold, sLineBreak]);
      LStats.AppendFormat('    - Timeout: %d ms%s', [FTimeoutMs, sLineBreak]);
      LStats.AppendFormat('  Estat?sticas:%s', [sLineBreak]);
      LStats.AppendFormat('    - Total de requisi??es: %d%s', [FTotalRequests, sLineBreak]);
      LStats.AppendFormat('    - Total de sucessos: %d%s', [FTotalSuccesses, sLineBreak]);
      LStats.AppendFormat('    - Total de falhas: %d%s', [FTotalFailures, sLineBreak]);
      LStats.AppendFormat('    - Taxa de falhas: %.2f%%%s', [GetFailureRate, sLineBreak]);
      LStats.AppendFormat('    - Tempo m?dio de resposta: %d ms%s', [FMetrics.AverageResponseTime, sLineBreak]);

      if FLastFailureTime > 0 then
        LStats.AppendFormat('    - ?ltima falha: %s%s', [DateTimeToStr(FLastFailureTime), sLineBreak]);

      Result := LStats.ToString;
    finally
      LStats.Free;
    end;
  finally
    Unlock;
  end;
end;

function TCircuitBreaker.IsHealthy: Boolean;
begin
  Lock;
  try
    Result := (FState = cbsClosed) and (GetFailureRate < 50.0);
  finally
    Unlock;
  end;
end;

procedure TCircuitBreaker.SetFailureThreshold(const AThreshold: Integer);
begin
  if AThreshold > 0 then
  begin
    Lock;
    try
      FFailureThreshold := AThreshold;
      TNest4DLogger.Info(Format('Circuit Breaker %s: Limite de falhas alterado para %d', [FName, AThreshold]));
    finally
      Unlock;
    end;
  end;
end;

procedure TCircuitBreaker.SetSuccessThreshold(const AThreshold: Integer);
begin
  if AThreshold > 0 then
  begin
    Lock;
    try
      FSuccessThreshold := AThreshold;
      TNest4DLogger.Info(Format('Circuit Breaker %s: Limite de sucessos alterado para %d', [FName, AThreshold]));
    finally
      Unlock;
    end;
  end;
end;

procedure TCircuitBreaker.SetTimeout(const ATimeoutMs: Cardinal);
begin
  if ATimeoutMs > 0 then
  begin
    Lock;
    try
      FTimeoutMs := ATimeoutMs;
      TNest4DLogger.Info(Format('Circuit Breaker %s: Timeout alterado para %d ms', [FName, ATimeoutMs]));
    finally
      Unlock;
    end;
  end;
end;

procedure TCircuitBreaker.SetStateChangeCallback(const ACallback: TCircuitBreakerStateChangeEvent);
begin
  Lock;
  try
    FOnStateChange := ACallback;
  finally
    Unlock;
  end;
end;

{ TCircuitBreakerManager }

constructor TCircuitBreakerManager.Create;
begin
  inherited Create;

  FCircuitBreakers := TDictionary<string, ICircuitBreaker>.Create;
  InitializeCriticalSection;

  // Configura??es padr?o
  FDefaultFailureThreshold := 5;
  FDefaultSuccessThreshold := 3;
  FDefaultTimeoutMs := 60000;

  TNest4DLogger.Info('Circuit Breaker Manager criado');
end;

destructor TCircuitBreakerManager.Destroy;
begin
  Lock;
  try
    FCircuitBreakers.Clear;
    FCircuitBreakers.Free;

    TNest4DLogger.Info('Circuit Breaker Manager destru?do');
  finally
    Unlock;
    FinalizeCriticalSection;
  end;

  inherited Destroy;
end;

procedure TCircuitBreakerManager.InitializeCriticalSection;
begin
  InitializeCriticalSection(FCriticalSection);
end;

procedure TCircuitBreakerManager.FinalizeCriticalSection;
begin
  DeleteCriticalSection(FCriticalSection);
end;

procedure TCircuitBreakerManager.Lock;
begin
  EnterCriticalSection(FCriticalSection);
end;

procedure TCircuitBreakerManager.Unlock;
begin
  LeaveCriticalSection(FCriticalSection);
end;

function TCircuitBreakerManager.GetOrCreateCircuitBreaker(const AName: string): ICircuitBreaker;
begin
  Result := GetOrCreateCircuitBreaker(AName, FDefaultFailureThreshold,
    FDefaultSuccessThreshold, FDefaultTimeoutMs);
end;

function TCircuitBreakerManager.GetOrCreateCircuitBreaker(const AName: string;
  const AFailureThreshold: Integer; const ASuccessThreshold: Integer;
  const ATimeoutMs: Cardinal): ICircuitBreaker;
var
  LKey: string;
begin
  Lock;
  try
    LKey := LowerCase(Trim(AName));

    if not FCircuitBreakers.TryGetValue(LKey, Result) then
    begin
      Result := TCircuitBreaker.Create(AName, AFailureThreshold, ASuccessThreshold, ATimeoutMs);
      FCircuitBreakers.Add(LKey, Result);

      TNest4DLogger.Info(Format('Novo Circuit Breaker criado pelo manager: %s', [AName]));
    end;
  finally
    Unlock;
  end;
end;

function TCircuitBreakerManager.GetCircuitBreaker(const AName: string): ICircuitBreaker;
var
  LKey: string;
begin
  Lock;
  try
    LKey := LowerCase(Trim(AName));

    if not FCircuitBreakers.TryGetValue(LKey, Result) then
      Result := nil;
  finally
    Unlock;
  end;
end;

procedure TCircuitBreakerManager.RemoveCircuitBreaker(const AName: string);
var
  LKey: string;
begin
  Lock;
  try
    LKey := LowerCase(Trim(AName));

    if FCircuitBreakers.Remove(LKey) then
      TNest4DLogger.Info(Format('Circuit Breaker removido: %s', [AName]));
  finally
    Unlock;
  end;
end;

function TCircuitBreakerManager.GetAllCircuitBreakers: TArray<ICircuitBreaker>;
var
  LList: TList<ICircuitBreaker>;
  LCircuitBreaker: ICircuitBreaker;
begin
  Lock;
  try
    LList := TList<ICircuitBreaker>.Create;
    try
      for LCircuitBreaker in FCircuitBreakers.Values do
        LList.Add(LCircuitBreaker);

      Result := LList.ToArray;
    finally
      LList.Free;
    end;
  finally
    Unlock;
  end;
end;

function TCircuitBreakerManager.GetCircuitBreakerNames: TArray<string>;
var
  LList: TList<string>;
  LName: string;
begin
  Lock;
  try
    LList := TList<string>.Create;
    try
      for LName in FCircuitBreakers.Keys do
        LList.Add(LName);

      Result := LList.ToArray;
    finally
      LList.Free;
    end;
  finally
    Unlock;
  end;
end;

procedure TCircuitBreakerManager.ResetAll;
var
  LCircuitBreaker: ICircuitBreaker;
begin
  Lock;
  try
    for LCircuitBreaker in FCircuitBreakers.Values do
      LCircuitBreaker.Reset;

    TNest4DLogger.Info('Todos os Circuit Breakers foram resetados');
  finally
    Unlock;
  end;
end;

function TCircuitBreakerManager.GetOverallHealth: Boolean;
var
  LCircuitBreaker: ICircuitBreaker;
  LCircuitBreakerImpl: TCircuitBreaker;
begin
  Lock;
  try
    Result := True;

    for LCircuitBreaker in FCircuitBreakers.Values do
    begin
      if LCircuitBreaker.QueryInterface(TCircuitBreaker, LCircuitBreakerImpl) = S_OK then
      begin
        if not LCircuitBreakerImpl.IsHealthy then
        begin
          Result := False;
          Break;
        end;
      end;
    end;
  finally
    Unlock;
  end;
end;

function TCircuitBreakerManager.GetManagerStats: string;
var
  LStats: TStringBuilder;
  LCircuitBreakers: TArray<ICircuitBreaker>;
  LCircuitBreaker: ICircuitBreaker;
  LCircuitBreakerImpl: TCircuitBreaker;
  LHealthyCount, LUnhealthyCount: Integer;
begin
  LStats := TStringBuilder.Create;
  try
    LCircuitBreakers := GetAllCircuitBreakers;
    LHealthyCount := 0;
    LUnhealthyCount := 0;

    for LCircuitBreaker in LCircuitBreakers do
    begin
      if LCircuitBreaker.QueryInterface(TCircuitBreaker, LCircuitBreakerImpl) = S_OK then
      begin
        if LCircuitBreakerImpl.IsHealthy then
          Inc(LHealthyCount)
        else
          Inc(LUnhealthyCount);
      end;
    end;

    LStats.AppendFormat('Circuit Breaker Manager Stats:%s', [sLineBreak]);
    LStats.AppendFormat('  Total de Circuit Breakers: %d%s', [Length(LCircuitBreakers), sLineBreak]);
    LStats.AppendFormat('  Saud?veis: %d%s', [LHealthyCount, sLineBreak]);
    LStats.AppendFormat('  Com problemas: %d%s', [LUnhealthyCount, sLineBreak]);
    LStats.AppendFormat('  Sa?de geral: %s%s', [IfThen(GetOverallHealth, 'OK', 'Problemas detectados'), sLineBreak]);

    Result := LStats.ToString;
  finally
    LStats.Free;
  end;
end;

procedure TCircuitBreakerManager.SetDefaultFailureThreshold(const AThreshold: Integer);
begin
  if AThreshold > 0 then
    FDefaultFailureThreshold := AThreshold;
end;

procedure TCircuitBreakerManager.SetDefaultSuccessThreshold(const AThreshold: Integer);
begin
  if AThreshold > 0 then
    FDefaultSuccessThreshold := AThreshold;
end;

procedure TCircuitBreakerManager.SetDefaultTimeout(const ATimeoutMs: Cardinal);
begin
  if ATimeoutMs > 0 then
    FDefaultTimeoutMs := ATimeoutMs;
end;

{ TCircuitBreakerFactory }

class function TCircuitBreakerFactory.CreateDefault(const AName: string): ICircuitBreaker;
begin
  Result := TCircuitBreaker.Create(AName, 5, 3, 60000); // 5 falhas, 3 sucessos, 60s timeout
end;

class function TCircuitBreakerFactory.CreateSensitive(const AName: string): ICircuitBreaker;
begin
  Result := TCircuitBreaker.Create(AName, 3, 2, 30000); // 3 falhas, 2 sucessos, 30s timeout
end;

class function TCircuitBreakerFactory.CreateTolerant(const AName: string): ICircuitBreaker;
begin
  Result := TCircuitBreaker.Create(AName, 10, 5, 120000); // 10 falhas, 5 sucessos, 120s timeout
end;

class function TCircuitBreakerFactory.CreateCustom(const AName: string;
  const AFailureThreshold: Integer; const ASuccessThreshold: Integer;
  const ATimeoutMs: Cardinal): ICircuitBreaker;
begin
  Result := TCircuitBreaker.Create(AName, AFailureThreshold, ASuccessThreshold, ATimeoutMs);
end;

end.

