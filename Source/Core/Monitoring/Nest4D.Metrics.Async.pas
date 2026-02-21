unit Nest4D.Metrics.Async;

interface

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  System.DateUtils,
  System.Generics.Collections,
  Nest4D.Metrics,
  Nest4D.Async;

type
  // Interface para métricas assíncronas
  INest4DAsyncMetrics = interface
    ['{A1B2C3D4-E5F6-7890-ABCD-EF1234567890}']
    // Métodos assíncronos
    procedure IncrementCounterAsync(const AName: String; const ATags: TDictionary<String, String> = nil; AValue: Double = 1.0);
    procedure SetGaugeAsync(const AName: String; AValue: Double; const ATags: TDictionary<String, String> = nil);
    procedure RecordHistogramAsync(const AName: String; AValue: Double; const ATags: TDictionary<String, String> = nil);
    procedure StartTimerAsync(const AName: String; const ATags: TDictionary<String, String> = nil);
    procedure StopTimerAsync(const AName: String; const ATags: TDictionary<String, String> = nil);
    procedure RecordErrorAsync(const AName: String; const AError: String; const ATags: TDictionary<String, String> = nil);
    procedure RecordPerformanceAsync(const AName: String; ADuration: Double; const ATags: TDictionary<String, String> = nil);
    procedure FlushAsync(const ACallback: TNest4DAsyncVoidCallback = nil);

    // Métodos síncronos para compatibilidade
    procedure IncrementCounter(const AName: String; const ATags: TDictionary<String, String> = nil; AValue: Double = 1.0);
    procedure SetGauge(const AName: String; AValue: Double; const ATags: TDictionary<String, String> = nil);
    procedure RecordHistogram(const AName: String; AValue: Double; const ATags: TDictionary<String, String> = nil);
    procedure StartTimer(const AName: String; const ATags: TDictionary<String, String> = nil);
    procedure StopTimer(const AName: String; const ATags: TDictionary<String, String> = nil);
    procedure RecordError(const AName: String; const AError: String; const ATags: TDictionary<String, String> = nil);
    procedure RecordPerformance(const AName: String; ADuration: Double; const ATags: TDictionary<String, String> = nil);
    procedure Flush;

    // Configuração
    procedure SetBatchSize(ASize: Integer);
    procedure SetFlushInterval(AInterval: Cardinal);
    function GetPendingMetricsCount: Integer;
  end;

  // Estrutura para métricas em lote
  TAsyncMetricEntry = record
    MetricType: TMetricType;
    Name: String;
    Value: Double;
    Tags: TDictionary<String, String>;
    Timestamp: TDateTime;
    ErrorMessage: String;

    constructor Create(AType: TMetricType; const AName: String; AValue: Double = 0; const ATags: TDictionary<String, String> = nil; const AError: String = '');
  end;

  // Coletor de métricas assíncrono
  TNest4DAsyncMetricsCollector = class(TInterfacedObject, INest4DAsyncMetrics)
  private
    FSyncCollector: IMetricsCollector;
    FAsyncExecutor: TNest4DAsyncExecutor;
    FMetricsBatch: TList<TAsyncMetricEntry>;
    FBatchLock: TCriticalSection;
    FBatchSize: Integer;
    FFlushInterval: Cardinal;
    FLastFlushTime: TDateTime;
    FFlushTimer: TThread;
    FTimers: TDictionary<String, TDateTime>;
    FTimersLock: TCriticalSection;
    FDroppedMetrics: Int64;
    FTotalMetrics: Int64;

    procedure ProcessBatch;
    procedure StartFlushTimer;
    procedure StopFlushTimer;
    procedure AddToBatch(const AEntry: TAsyncMetricEntry);
    procedure RecordInternalMetrics;
    function CreateTagsCopy(const ATags: TDictionary<String, String>): TDictionary<String, String>;
    function GenerateTimerKey(const AName: String; const ATags: TDictionary<String, String>): String;
  public
    constructor Create(const ASyncCollector: IMetricsCollector; ABatchSize: Integer = 500; AFlushInterval: Cardinal = 5000);
    destructor Destroy; override;

    // Métodos assíncronos
    procedure IncrementCounterAsync(const AName: String; const ATags: TDictionary<String, String> = nil; AValue: Double = 1.0);
    procedure SetGaugeAsync(const AName: String; AValue: Double; const ATags: TDictionary<String, String> = nil);
    procedure RecordHistogramAsync(const AName: String; AValue: Double; const ATags: TDictionary<String, String> = nil);
    procedure StartTimerAsync(const AName: String; const ATags: TDictionary<String, String> = nil);
    procedure StopTimerAsync(const AName: String; const ATags: TDictionary<String, String> = nil);
    procedure RecordErrorAsync(const AName: String; const AError: String; const ATags: TDictionary<String, String> = nil);
    procedure RecordPerformanceAsync(const AName: String; ADuration: Double; const ATags: TDictionary<String, String> = nil);
    procedure FlushAsync(const ACallback: TNest4DAsyncVoidCallback = nil);

    // Métodos síncronos para compatibilidade
    procedure IncrementCounter(const AName: String; const ATags: TDictionary<String, String> = nil; AValue: Double = 1.0);
    procedure SetGauge(const AName: String; AValue: Double; const ATags: TDictionary<String, String> = nil);
    procedure RecordHistogram(const AName: String; AValue: Double; const ATags: TDictionary<String, String> = nil);
    procedure StartTimer(const AName: String; const ATags: TDictionary<String, String> = nil);
    procedure StopTimer(const AName: String; const ATags: TDictionary<String, String> = nil);
    procedure RecordError(const AName: String; const AError: String; const ATags: TDictionary<String, String> = nil);
    procedure RecordPerformance(const AName: String; ADuration: Double; const ATags: TDictionary<String, String> = nil);
    procedure Flush;

    // Configuração
    procedure SetBatchSize(ASize: Integer);
    procedure SetFlushInterval(AInterval: Cardinal);
    function GetPendingMetricsCount: Integer;

    // Estatísticas
    property DroppedMetrics: Int64 read FDroppedMetrics;
    property TotalMetrics: Int64 read FTotalMetrics;
  end;

  // Thread para flush automático
  TNest4DMetricsFlushThread = class(TThread)
  private
    FCollector: TNest4DAsyncMetricsCollector;
    FInterval: Cardinal;
  protected
    procedure Execute; override;
  public
    constructor Create(ACollector: TNest4DAsyncMetricsCollector; AInterval: Cardinal);
  end;

  // Agregador de métricas para reduzir overhead
  TNest4DMetricsAggregator = class
  private
    FCounters: TDictionary<String, Double>;
    FGauges: TDictionary<String, Double>;
    FHistograms: TDictionary<String, TList<Double>>;
    FLock: TCriticalSection;

    function GetMetricKey(const AName: String; const ATags: TDictionary<String, String>): String;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AggregateCounter(const AName: String; AValue: Double; const ATags: TDictionary<String, String>);
    procedure AggregateGauge(const AName: String; AValue: Double; const ATags: TDictionary<String, String>);
    procedure AggregateHistogram(const AName: String; AValue: Double; const ATags: TDictionary<String, String>);

    procedure FlushToCollector(const ACollector: IMetricsCollector);
    procedure Clear;
  end;

// Funções utilitárias
function GetAsyncMetrics: INest4DAsyncMetrics;
function CreateAsyncMetrics(const ASyncCollector: IMetricsCollector = nil; ABatchSize: Integer = 500; AFlushInterval: Cardinal = 5000): INest4DAsyncMetrics;

implementation

uses
  System.Hash,
  Winapi.Windows;

var
  GAsyncMetrics: INest4DAsyncMetrics;
  GAsyncMetricsLock: TCriticalSection;

function GetAsyncMetrics: INest4DAsyncMetrics;
begin
  if not Assigned(GAsyncMetrics) then
  begin
    if not Assigned(GAsyncMetricsLock) then
      GAsyncMetricsLock := TCriticalSection.Create;

    GAsyncMetricsLock.Enter;
    try
      if not Assigned(GAsyncMetrics) then
        GAsyncMetrics := CreateAsyncMetrics;
    finally
      GAsyncMetricsLock.Leave;
    end;
  end;
  Result := GAsyncMetrics;
end;

function CreateAsyncMetrics(const ASyncCollector: IMetricsCollector = nil; ABatchSize: Integer = 500; AFlushInterval: Cardinal = 5000): INest4DAsyncMetrics;
var
  LCollector: IMetricsCollector;
begin
  if Assigned(ASyncCollector) then
    LCollector := ASyncCollector
  else
    LCollector := GetMetricsCollector;

  Result := TNest4DAsyncMetricsCollector.Create(LCollector, ABatchSize, AFlushInterval);
end;

{ TAsyncMetricEntry }

constructor TAsyncMetricEntry.Create(AType: TMetricType; const AName: String; AValue: Double = 0; const ATags: TDictionary<String, String> = nil; const AError: String = '');
var
  LPair: TPair<String, String>;
begin
  MetricType := AType;
  Name := AName;
  Value := AValue;
  Timestamp := Now;
  ErrorMessage := AError;

  if Assigned(ATags) then
  begin
    Tags := TDictionary<String, String>.Create;
    for LPair in ATags do
      Tags.Add(LPair.Key, LPair.Value);
  end
  else
    Tags := nil;
end;

{ TNest4DAsyncMetricsCollector }

constructor TNest4DAsyncMetricsCollector.Create(const ASyncCollector: IMetricsCollector; ABatchSize: Integer = 500; AFlushInterval: Cardinal = 5000);
begin
  inherited Create;
  FSyncCollector := ASyncCollector;
  FAsyncExecutor := TNest4DAsyncExecutor.Create;
  FMetricsBatch := TList<TAsyncMetricEntry>.Create;
  FBatchLock := TCriticalSection.Create;
  FTimers := TDictionary<String, TDateTime>.Create;
  FTimersLock := TCriticalSection.Create;
  FBatchSize := ABatchSize;
  FFlushInterval := AFlushInterval;
  FLastFlushTime := Now;
  FDroppedMetrics := 0;
  FTotalMetrics := 0;
  StartFlushTimer;
end;

destructor TNest4DAsyncMetricsCollector.Destroy;
begin
  StopFlushTimer;
  Flush;
  FreeAndNil(FAsyncExecutor);
  FreeAndNil(FMetricsBatch);
  FreeAndNil(FBatchLock);
  FreeAndNil(FTimers);
  FreeAndNil(FTimersLock);
  inherited;
end;

procedure TNest4DAsyncMetricsCollector.AddToBatch(const AEntry: TAsyncMetricEntry);
begin
  FBatchLock.Enter;
  try
    if FMetricsBatch.Count >= FBatchSize then
    begin
      Inc(FDroppedMetrics);
      Exit;
    end;

    FMetricsBatch.Add(AEntry);
    Inc(FTotalMetrics);

    if FMetricsBatch.Count >= FBatchSize then
      ProcessBatch;
  finally
    FBatchLock.Leave;
  end;
end;

procedure TNest4DAsyncMetricsCollector.ProcessBatch;
var
  LBatch: TList<TAsyncMetricEntry>;
  LEntry: TAsyncMetricEntry;
begin
  if FMetricsBatch.Count = 0 then
    Exit;

  LBatch := TList<TAsyncMetricEntry>.Create;
  try
    LBatch.AddRange(FMetricsBatch);
    FMetricsBatch.Clear;
    FLastFlushTime := Now;

    FAsyncExecutor.ExecuteAsync(
      procedure
      var
        LMetricEntry: TAsyncMetricEntry;
      begin
        for LMetricEntry in LBatch do
        begin
          try
            case LMetricEntry.MetricType of
              mtCounter:
                FSyncCollector.IncrementCounter(LMetricEntry.Name, LMetricEntry.Tags, LMetricEntry.Value);
              mtGauge:
                FSyncCollector.SetGauge(LMetricEntry.Name, LMetricEntry.Value, LMetricEntry.Tags);
              mtHistogram:
                FSyncCollector.RecordHistogram(LMetricEntry.Name, LMetricEntry.Value, LMetricEntry.Tags);
              mtTimer:
                FSyncCollector.RecordPerformance(LMetricEntry.Name, LMetricEntry.Value, LMetricEntry.Tags);
              mtError:
                FSyncCollector.RecordError(LMetricEntry.Name, LMetricEntry.ErrorMessage, LMetricEntry.Tags);
            end;
          except
            // Log error silently
          end;
        end;
      end
    );
  finally
    LBatch.Free;
  end;
end;

procedure TNest4DAsyncMetricsCollector.StartFlushTimer;
begin
  if Assigned(FFlushTimer) then
    Exit;

  FFlushTimer := TNest4DMetricsFlushThread.Create(Self, FFlushInterval);
end;

procedure TNest4DAsyncMetricsCollector.StopFlushTimer;
begin
  if Assigned(FFlushTimer) then
  begin
    FFlushTimer.Terminate;
    FFlushTimer.WaitFor;
    FreeAndNil(FFlushTimer);
  end;
end;

function TNest4DAsyncMetricsCollector.CreateTagsCopy(const ATags: TDictionary<String, String>): TDictionary<String, String>;
var
  LPair: TPair<String, String>;
begin
  if not Assigned(ATags) then
    Exit(nil);

  Result := TDictionary<String, String>.Create;
  for LPair in ATags do
    Result.Add(LPair.Key, LPair.Value);
end;

function TNest4DAsyncMetricsCollector.GenerateTimerKey(const AName: String; const ATags: TDictionary<String, String>): String;
var
  LPair: TPair<String, String>;
  LTagsStr: String;
begin
  Result := AName;
  if Assigned(ATags) then
  begin
    LTagsStr := '';
    for LPair in ATags do
      LTagsStr := LTagsStr + LPair.Key + '=' + LPair.Value + ';';
    Result := Result + '|' + LTagsStr;
  end;
end;

procedure TNest4DAsyncMetricsCollector.RecordInternalMetrics;
begin
  IncrementCounterAsync('nest4d.metrics.total', nil, FTotalMetrics);
  IncrementCounterAsync('nest4d.metrics.dropped', nil, FDroppedMetrics);
  SetGaugeAsync('nest4d.metrics.pending', GetPendingMetricsCount);
end;

// Métodos assíncronos

procedure TNest4DAsyncMetricsCollector.IncrementCounterAsync(const AName: String; const ATags: TDictionary<String, String> = nil; AValue: Double = 1.0);
var
  LEntry: TAsyncMetricEntry;
begin
  LEntry := TAsyncMetricEntry.Create(mtCounter, AName, AValue, ATags);
  AddToBatch(LEntry);
end;

procedure TNest4DAsyncMetricsCollector.SetGaugeAsync(const AName: String; AValue: Double; const ATags: TDictionary<String, String> = nil);
var
  LEntry: TAsyncMetricEntry;
begin
  LEntry := TAsyncMetricEntry.Create(mtGauge, AName, AValue, ATags);
  AddToBatch(LEntry);
end;

procedure TNest4DAsyncMetricsCollector.RecordHistogramAsync(const AName: String; AValue: Double; const ATags: TDictionary<String, String> = nil);
var
  LEntry: TAsyncMetricEntry;
begin
  LEntry := TAsyncMetricEntry.Create(mtHistogram, AName, AValue, ATags);
  AddToBatch(LEntry);
end;

procedure TNest4DAsyncMetricsCollector.StartTimerAsync(const AName: String; const ATags: TDictionary<String, String> = nil);
var
  LKey: String;
begin
  LKey := GenerateTimerKey(AName, ATags);
  FTimersLock.Enter;
  try
    FTimers.AddOrSetValue(LKey, Now);
  finally
    FTimersLock.Leave;
  end;
end;

procedure TNest4DAsyncMetricsCollector.StopTimerAsync(const AName: String; const ATags: TDictionary<String, String> = nil);
var
  LKey: String;
  LStartTime: TDateTime;
  LDuration: Double;
  LEntry: TAsyncMetricEntry;
begin
  LKey := GenerateTimerKey(AName, ATags);
  FTimersLock.Enter;
  try
    if FTimers.TryGetValue(LKey, LStartTime) then
    begin
      LDuration := MilliSecondsBetween(Now, LStartTime);
      FTimers.Remove(LKey);
      LEntry := TAsyncMetricEntry.Create(mtTimer, AName, LDuration, ATags);
      AddToBatch(LEntry);
    end;
  finally
    FTimersLock.Leave;
  end;
end;

procedure TNest4DAsyncMetricsCollector.RecordErrorAsync(const AName: String; const AError: String; const ATags: TDictionary<String, String> = nil);
var
  LEntry: TAsyncMetricEntry;
begin
  LEntry := TAsyncMetricEntry.Create(mtError, AName, 0, ATags, AError);
  AddToBatch(LEntry);
end;

procedure TNest4DAsyncMetricsCollector.RecordPerformanceAsync(const AName: String; ADuration: Double; const ATags: TDictionary<String, String> = nil);
var
  LEntry: TAsyncMetricEntry;
begin
  LEntry := TAsyncMetricEntry.Create(mtTimer, AName, ADuration, ATags);
  AddToBatch(LEntry);
end;

procedure TNest4DAsyncMetricsCollector.FlushAsync(const ACallback: TNest4DAsyncVoidCallback = nil);
begin
  FAsyncExecutor.ExecuteAsync(
    procedure
    begin
      FBatchLock.Enter;
      try
        ProcessBatch;
      finally
        FBatchLock.Leave;
      end;
      
      if Assigned(ACallback) then
        ACallback();
    end
  );
end;

// Métodos síncronos para compatibilidade

procedure TNest4DAsyncMetricsCollector.IncrementCounter(const AName: String; const ATags: TDictionary<String, String> = nil; AValue: Double = 1.0);
begin
  FSyncCollector.IncrementCounter(AName, ATags, AValue);
end;

procedure TNest4DAsyncMetricsCollector.SetGauge(const AName: String; AValue: Double; const ATags: TDictionary<String, String> = nil);
begin
  FSyncCollector.SetGauge(AName, AValue, ATags);
end;

procedure TNest4DAsyncMetricsCollector.RecordHistogram(const AName: String; AValue: Double; const ATags: TDictionary<String, String> = nil);
begin
  FSyncCollector.RecordHistogram(AName, AValue, ATags);
end;

procedure TNest4DAsyncMetricsCollector.StartTimer(const AName: String; const ATags: TDictionary<String, String> = nil);
begin
  FSyncCollector.StartTimer(AName, ATags);
end;

procedure TNest4DAsyncMetricsCollector.StopTimer(const AName: String; const ATags: TDictionary<String, String> = nil);
begin
  FSyncCollector.StopTimer(AName, ATags);
end;

procedure TNest4DAsyncMetricsCollector.RecordError(const AName: String; const AError: String; const ATags: TDictionary<String, String> = nil);
begin
  FSyncCollector.RecordError(AName, AError, ATags);
end;

procedure TNest4DAsyncMetricsCollector.RecordPerformance(const AName: String; ADuration: Double; const ATags: TDictionary<String, String> = nil);
begin
  FSyncCollector.RecordPerformance(AName, ADuration, ATags);
end;

procedure TNest4DAsyncMetricsCollector.Flush;
begin
  FBatchLock.Enter;
  try
    ProcessBatch;
  finally
    FBatchLock.Leave;
  end;
end;

// Configuração

procedure TNest4DAsyncMetricsCollector.SetBatchSize(ASize: Integer);
begin
  FBatchSize := ASize;
end;

procedure TNest4DAsyncMetricsCollector.SetFlushInterval(AInterval: Cardinal);
begin
  FFlushInterval := AInterval;
  StopFlushTimer;
  StartFlushTimer;
end;

function TNest4DAsyncMetricsCollector.GetPendingMetricsCount: Integer;
begin
  FBatchLock.Enter;
  try
    Result := FMetricsBatch.Count;
  finally
    FBatchLock.Leave;
  end;
end;

{ TNest4DMetricsFlushThread }

constructor TNest4DMetricsFlushThread.Create(ACollector: TNest4DAsyncMetricsCollector; AInterval: Cardinal);
begin
  inherited Create(False);
  FCollector := ACollector;
  FInterval := AInterval;
  FreeOnTerminate := False;
end;

procedure TNest4DMetricsFlushThread.Execute;
begin
  while not Terminated do
  begin
    Sleep(FInterval);
    if not Terminated then
    begin
      FCollector.FBatchLock.Enter;
      try
        if MilliSecondsBetween(Now, FCollector.FLastFlushTime) >= FInterval then
          FCollector.ProcessBatch;
      finally
        FCollector.FBatchLock.Leave;
      end;
    end;
  end;
end;

{ TNest4DMetricsAggregator }

constructor TNest4DMetricsAggregator.Create;
begin
  inherited;
  FCounters := TDictionary<String, Double>.Create;
  FGauges := TDictionary<String, Double>.Create;
  FHistograms := TDictionary<String, TList<Double>>.Create;
  FLock := TCriticalSection.Create;
end;

destructor TNest4DMetricsAggregator.Destroy;
var
  LPair: TPair<String, TList<Double>>;
begin
  for LPair in FHistograms do
    LPair.Value.Free;
  
  FreeAndNil(FCounters);
  FreeAndNil(FGauges);
  FreeAndNil(FHistograms);
  FreeAndNil(FLock);
  inherited;
end;

function TNest4DMetricsAggregator.GetMetricKey(const AName: String; const ATags: TDictionary<String, String>): String;
var
  LPair: TPair<String, String>;
  LTagsStr: String;
begin
  Result := AName;
  if Assigned(ATags) then
  begin
    LTagsStr := '';
    for LPair in ATags do
      LTagsStr := LTagsStr + LPair.Key + '=' + LPair.Value + ';';
    Result := Result + '|' + LTagsStr;
  end;
end;

procedure TNest4DMetricsAggregator.AggregateCounter(const AName: String; AValue: Double; const ATags: TDictionary<String, String>);
var
  LKey: String;
  LCurrentValue: Double;
begin
  LKey := GetMetricKey(AName, ATags);
  FLock.Enter;
  try
    if FCounters.TryGetValue(LKey, LCurrentValue) then
      FCounters.AddOrSetValue(LKey, LCurrentValue + AValue)
    else
      FCounters.Add(LKey, AValue);
  finally
    FLock.Leave;
  end;
end;

procedure TNest4DMetricsAggregator.AggregateGauge(const AName: String; AValue: Double; const ATags: TDictionary<String, String>);
var
  LKey: String;
begin
  LKey := GetMetricKey(AName, ATags);
  FLock.Enter;
  try
    FGauges.AddOrSetValue(LKey, AValue);
  finally
    FLock.Leave;
  end;
end;

procedure TNest4DMetricsAggregator.AggregateHistogram(const AName: String; AValue: Double; const ATags: TDictionary<String, String>);
var
  LKey: String;
  LValues: TList<Double>;
begin
  LKey := GetMetricKey(AName, ATags);
  FLock.Enter;
  try
    if not FHistograms.TryGetValue(LKey, LValues) then
    begin
      LValues := TList<Double>.Create;
      FHistograms.Add(LKey, LValues);
    end;
    LValues.Add(AValue);
  finally
    FLock.Leave;
  end;
end;

procedure TNest4DMetricsAggregator.FlushToCollector(const ACollector: IMetricsCollector);
var
  LCounterPair: TPair<String, Double>;
  LGaugePair: TPair<String, Double>;
  LHistogramPair: TPair<String, TList<Double>>;
  LValue: Double;
begin
  FLock.Enter;
  try
    // Flush counters
    for LCounterPair in FCounters do
      ACollector.IncrementCounter(LCounterPair.Key, nil, LCounterPair.Value);

    // Flush gauges
    for LGaugePair in FGauges do
      ACollector.SetGauge(LGaugePair.Key, LGaugePair.Value);

    // Flush histograms
    for LHistogramPair in FHistograms do
    begin
      for LValue in LHistogramPair.Value do
        ACollector.RecordHistogram(LHistogramPair.Key, LValue);
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TNest4DMetricsAggregator.Clear;
var
  LPair: TPair<String, TList<Double>>;
begin
  FLock.Enter;
  try
    FCounters.Clear;
    FGauges.Clear;
    
    for LPair in FHistograms do
      LPair.Value.Free;
    FHistograms.Clear;
  finally
    FLock.Leave;
  end;
end;

initialization
  GAsyncMetrics := nil;
  GAsyncMetricsLock := nil;

finalization
  if Assigned(GAsyncMetricsLock) then
  begin
    GAsyncMetricsLock.Free;
    GAsyncMetricsLock := nil;
  end;
  GAsyncMetrics := nil;

end.

