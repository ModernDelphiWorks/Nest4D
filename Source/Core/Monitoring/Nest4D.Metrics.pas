unit Nest4D.Metrics;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  System.SyncObjs,
  System.DateUtils,
  System.IOUtils,
  Winapi.Windows;

type
  // Tipos de métricas
  TMetricType = (mtCounter, mtGauge, mtHistogram, mtSummary, mtTimer, mtError);

  // Tags para métricas
  TMetricTags = TDictionary<String, String>;

  // Entrada de métrica
  TMetricEntry = class
  private
    FMetricType: TMetricType;
    FName: String;
    FValue: Double;
    FTags: TMetricTags;
    FTimestamp: TDateTime;
  public
    constructor Create(AType: TMetricType; const AName: String; AValue: Double; ATags: TMetricTags = nil);
    destructor Destroy; override;
    property MetricType: TMetricType read FMetricType write FMetricType;
    property Name: String read FName write FName;
    property Value: Double read FValue write FValue;
    property Tags: TMetricTags read FTags write FTags;
    property Timestamp: TDateTime read FTimestamp write FTimestamp;
  end;

  // Interface principal para coleta de métricas
  IMetricsCollector = interface
    ['{B1C2D3E4-F5A6-7890-BCDE-F12345678901}']
    // Métodos para diferentes tipos de métricas
    procedure IncrementCounter(const AName: String; AValue: Double = 1.0; ATags: TMetricTags = nil);
    procedure SetGauge(const AName: String; AValue: Double; ATags: TMetricTags = nil);
    procedure RecordHistogram(const AName: String; AValue: Double; ATags: TMetricTags = nil);
    procedure RecordSummary(const AName: String; AValue: Double; ATags: TMetricTags = nil);
    procedure StartTimer(const AName: String; ATags: TMetricTags = nil);
    procedure StopTimer(const AName: String; ATags: TMetricTags = nil);
    procedure RecordError(const AName: String; const AErrorType: String = ''; ATags: TMetricTags = nil);
    procedure RecordPerformance(const AName: String; AValue: Double; ATags: TMetricTags = nil);
    // Configuração e controle
    procedure Flush;
    procedure Reset;
    function IsEnabled: Boolean;
    procedure SetEnabled(AEnabled: Boolean);
    // Estatísticas
    function GetMetricCount: Integer;
    function GetMetricNames: TArray<String>;
    function GetMetricValue(const AName: String; ATags: TMetricTags = nil): Double;
  end;

  // Interface para exportadores de métricas
  IMetricsExporter = interface
    ['{C2D3E4F5-A6B7-8901-CDEF-234567890123}']
    procedure Export(const AMetrics: TArray<TMetricEntry>);
    function GetName: String;
    function IsEnabled: Boolean;
    procedure SetEnabled(AEnabled: Boolean);
  end;

  // Implementação básica do coletor de métricas
  TMetricsCollector = class(TInterfacedObject, IMetricsCollector)
  private
    FMetrics: TList<TMetricEntry>;
    FTimers: TDictionary<String, TDateTime>;
    FExporters: TList<IMetricsExporter>;
    FLock: TCriticalSection;
    FEnabled: Boolean;
    function CreateTimerKey(const AName: String; ATags: TMetricTags): String;
    function CloneTags(ATags: TMetricTags): TMetricTags;
    procedure AddMetric(const AEntry: TMetricEntry);
  public
    constructor Create;
    destructor Destroy; override;
    // Métodos para diferentes tipos de métricas
    procedure IncrementCounter(const AName: String; AValue: Double = 1.0; ATags: TMetricTags = nil);
    procedure SetGauge(const AName: String; AValue: Double; ATags: TMetricTags = nil);
    procedure RecordHistogram(const AName: String; AValue: Double; ATags: TMetricTags = nil);
    procedure RecordSummary(const AName: String; AValue: Double; ATags: TMetricTags = nil);
    procedure StartTimer(const AName: String; ATags: TMetricTags = nil);
    procedure StopTimer(const AName: String; ATags: TMetricTags = nil);
    procedure RecordError(const AName: String; const AErrorType: String = ''; ATags: TMetricTags = nil);
    procedure RecordPerformance(const AName: String; AValue: Double; ATags: TMetricTags = nil);
    // Configuração e controle
    procedure Flush;
    procedure Reset;
    function IsEnabled: Boolean;
    procedure SetEnabled(AEnabled: Boolean);
    // Estatísticas
    function GetMetricCount: Integer;
    function GetMetricNames: TArray<String>;
    function GetMetricValue(const AName: String; ATags: TMetricTags = nil): Double;
    // Exportadores
    procedure AddExporter(const AExporter: IMetricsExporter);
    procedure RemoveExporter(const AName: String);
  end;

  // Exportador para console
  TConsoleMetricsExporter = class(TInterfacedObject, IMetricsExporter)
  private
    FName: String;
    FEnabled: Boolean;
    FLock: TCriticalSection;
    function FormatMetric(const AEntry: TMetricEntry): String;
    function MetricTypeToString(AType: TMetricType): String;
    function FormatTags(ATags: TMetricTags): String;
  public
    constructor Create(const AName: String = 'Console');
    destructor Destroy; override;
    procedure Export(const AMetrics: TArray<TMetricEntry>);
    function GetName: String;
    function IsEnabled: Boolean;
    procedure SetEnabled(AEnabled: Boolean);
  end;

  // Exportador para arquivo
  TFileMetricsExporter = class(TInterfacedObject, IMetricsExporter)
  private
    FName: String;
    FFileName: String;
    FEnabled: Boolean;
    FLock: TCriticalSection;
    FFileStream: TFileStream;
    function FormatMetric(const AEntry: TMetricEntry): String;
    function MetricTypeToString(AType: TMetricType): String;
    function FormatTags(ATags: TMetricTags): String;
    procedure EnsureFileOpen;
  public
    constructor Create(const AFileName: String; const AName: String = 'File');
    destructor Destroy; override;
    procedure Export(const AMetrics: TArray<TMetricEntry>);
    function GetName: String;
    function IsEnabled: Boolean;
    procedure SetEnabled(AEnabled: Boolean);
    property FileName: String read FFileName;
  end;

// Funções utilitárias globais
function GetMetricsCollector: IMetricsCollector;
function CreateMetricsCollector: IMetricsCollector;
function CreateMetricTags: TMetricTags;
function MetricTypeFromString(const AType: String): TMetricType;
function MetricTypeToString(AType: TMetricType): String;

implementation

var
  GMetricsCollector: IMetricsCollector;
  GMetricsLock: TCriticalSection;

{ TMetricEntry }

constructor TMetricEntry.Create(AType: TMetricType; const AName: String; AValue: Double; ATags: TMetricTags = nil);
begin
  inherited Create;
  FMetricType := AType;
  FName := AName;
  FValue := AValue;
  FTimestamp := Now;

  if Assigned(ATags) then
  begin
    FTags := TMetricTags.Create;
    for var LPair in ATags do
      FTags.Add(LPair.Key, LPair.Value);
  end
  else
    FTags := nil;
end;

destructor TMetricEntry.Destroy;
begin
  if Assigned(FTags) then
    FTags.Free;
  inherited Destroy;
end;

{ TMetricsCollector }

constructor TMetricsCollector.Create;
begin
  inherited Create;
  FMetrics := TList<TMetricEntry>.Create;
  FTimers := TDictionary<String, TDateTime>.Create;
  FExporters := TList<IMetricsExporter>.Create;
  FLock := TCriticalSection.Create;
  FEnabled := True;
end;

destructor TMetricsCollector.Destroy;
var
  LFor: Integer;
begin
  // Limpar métricas - liberar cada instância individualmente
  if Assigned(FMetrics) then
  begin
    for LFor := 0 to FMetrics.Count - 1 do
      FMetrics[LFor].Free;
    FMetrics.Free;
  end;
  
  FTimers.Free;
  FExporters.Free;
  FLock.Free;
  inherited Destroy;
end;

function TMetricsCollector.CreateTimerKey(const AName: String; ATags: TMetricTags): String;
var
  LPair: TPair<String, String>;
begin
  Result := AName;
  if Assigned(ATags) then
  begin
    for LPair in ATags do
      Result := Result + '|' + LPair.Key + '=' + LPair.Value;
  end;
end;

function TMetricsCollector.CloneTags(ATags: TMetricTags): TMetricTags;
var
  LPair: TPair<String, String>;
begin
  if not Assigned(ATags) then
    Exit(nil);
    
  Result := TMetricTags.Create;
  for LPair in ATags do
    Result.Add(LPair.Key, LPair.Value);
end;

procedure TMetricsCollector.AddMetric(const AEntry: TMetricEntry);
begin
  if not FEnabled then Exit;
  
  FLock.Enter;
  try
    FMetrics.Add(AEntry);
  finally
    FLock.Leave;
  end;
end;

procedure TMetricsCollector.IncrementCounter(const AName: String; AValue: Double = 1.0; ATags: TMetricTags = nil);
var
  LEntry: TMetricEntry;
begin
  LEntry := TMetricEntry.Create(mtCounter, AName, AValue, ATags);
  AddMetric(LEntry);
end;

procedure TMetricsCollector.SetGauge(const AName: String; AValue: Double; ATags: TMetricTags = nil);
var
  LEntry: TMetricEntry;
begin
  LEntry := TMetricEntry.Create(mtGauge, AName, AValue, ATags);
  AddMetric(LEntry);
end;

procedure TMetricsCollector.RecordHistogram(const AName: String; AValue: Double; ATags: TMetricTags = nil);
var
  LEntry: TMetricEntry;
begin
  LEntry := TMetricEntry.Create(mtHistogram, AName, AValue, ATags);
  AddMetric(LEntry);
end;

procedure TMetricsCollector.RecordSummary(const AName: String; AValue: Double; ATags: TMetricTags = nil);
var
  LEntry: TMetricEntry;
begin
  LEntry := TMetricEntry.Create(mtSummary, AName, AValue, ATags);
  AddMetric(LEntry);
end;

procedure TMetricsCollector.StartTimer(const AName: String; ATags: TMetricTags = nil);
var
  LKey: String;
begin
  if not FEnabled then Exit;
  
  LKey := CreateTimerKey(AName, ATags);
  FLock.Enter;
  try
    FTimers.AddOrSetValue(LKey, Now);
  finally
    FLock.Leave;
  end;
end;

procedure TMetricsCollector.StopTimer(const AName: String; ATags: TMetricTags = nil);
var
  LKey: String;
  LStartTime: TDateTime;
  LDuration: Double;
  LEntry: TMetricEntry;
begin
  if not FEnabled then Exit;
  
  LKey := CreateTimerKey(AName, ATags);
  FLock.Enter;
  try
    if FTimers.TryGetValue(LKey, LStartTime) then
    begin
      LDuration := MilliSecondsBetween(Now, LStartTime);
      FTimers.Remove(LKey);
      
      LEntry := TMetricEntry.Create(mtTimer, AName, LDuration, ATags);
      FMetrics.Add(LEntry);
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TMetricsCollector.RecordError(const AName: String; const AErrorType: String = ''; ATags: TMetricTags = nil);
var
  LEntry: TMetricEntry;
  LTags: TMetricTags;
begin
  LTags := CloneTags(ATags);
  if not Assigned(LTags) then
    LTags := TMetricTags.Create;
    
  if AErrorType <> '' then
    LTags.AddOrSetValue('error_type', AErrorType);
    
  LEntry := TMetricEntry.Create(mtError, AName, 1.0, LTags);
  AddMetric(LEntry);
end;

procedure TMetricsCollector.RecordPerformance(const AName: String; AValue: Double; ATags: TMetricTags = nil);
var
  LEntry: TMetricEntry;
begin
  LEntry := TMetricEntry.Create(mtTimer, AName, AValue, ATags);
  AddMetric(LEntry);
end;

procedure TMetricsCollector.Flush;
var
  LMetrics: TArray<TMetricEntry>;
  LExporter: IMetricsExporter;
  LFor: Integer;
begin
  FLock.Enter;
  try
    // Copia as métricas
    SetLength(LMetrics, FMetrics.Count);
    for LFor := 0 to FMetrics.Count - 1 do
      LMetrics[LFor] := FMetrics[LFor];

    // Limpa a lista sem liberar as instâncias ainda
    FMetrics.Clear;
  finally
    FLock.Leave;
  end;

  // Exporta para todos os exportadores
  for LExporter in FExporters do
  begin
    if LExporter.IsEnabled then
      LExporter.Export(LMetrics);
  end;

  // Agora libera as instâncias após a exportação
  for LFor := 0 to High(LMetrics) do
    LMetrics[LFor].Free;
end;

procedure TMetricsCollector.Reset;
var
  LFor: Integer;
begin
  FLock.Enter;
  try
    // Liberar cada instância de TMetricEntry antes de limpar
    for LFor := 0 to FMetrics.Count - 1 do
      FMetrics[LFor].Free;
    FMetrics.Clear;
    FTimers.Clear;
  finally
    FLock.Leave;
  end;
end;

function TMetricsCollector.IsEnabled: Boolean;
begin
  Result := FEnabled;
end;

procedure TMetricsCollector.SetEnabled(AEnabled: Boolean);
begin
  FEnabled := AEnabled;
end;

function TMetricsCollector.GetMetricCount: Integer;
begin
  FLock.Enter;
  try
    Result := FMetrics.Count;
  finally
    FLock.Leave;
  end;
end;

function TMetricsCollector.GetMetricNames: TArray<String>;
var
  LNames: TList<String>;
  LFor: Integer;
begin
  LNames := TList<String>.Create;
  try
    FLock.Enter;
    try
      for LFor := 0 to FMetrics.Count - 1 do
      begin
        if LNames.IndexOf(FMetrics[LFor].Name) = -1 then
          LNames.Add(FMetrics[LFor].Name);
      end;
    finally
      FLock.Leave;
    end;
    
    Result := LNames.ToArray;
  finally
    LNames.Free;
  end;
end;

function TMetricsCollector.GetMetricValue(const AName: String; ATags: TMetricTags = nil): Double;
var
  LFor: Integer;
  LFound: Boolean;
begin
  Result := 0.0;
  LFound := False;

  FLock.Enter;
  try
    for LFor := FMetrics.Count - 1 downto 0 do
    begin
      if FMetrics[LFor].Name = AName then
      begin
        // TODO: Implementar comparação de tags se necessário
        Result := FMetrics[LFor].Value;
        LFound := True;
        Break;
      end;
    end;
  finally
    FLock.Leave;
  end;
  
  if not LFound then
    Result := 0.0;
end;

procedure TMetricsCollector.AddExporter(const AExporter: IMetricsExporter);
begin
  FLock.Enter;
  try
    FExporters.Add(AExporter);
  finally
    FLock.Leave;
  end;
end;

procedure TMetricsCollector.RemoveExporter(const AName: String);
var
  LFor: Integer;
begin
  FLock.Enter;
  try
    for LFor := FExporters.Count - 1 downto 0 do
    begin
      if FExporters[LFor].GetName = AName then
        FExporters.Delete(LFor);
    end;
  finally
    FLock.Leave;
  end;
end;

{ TConsoleMetricsExporter }

constructor TConsoleMetricsExporter.Create(const AName: String = 'Console');
begin
  inherited Create;
  FName := AName;
  FEnabled := True;
  FLock := TCriticalSection.Create;
end;

destructor TConsoleMetricsExporter.Destroy;
begin
  FLock.Free;
  inherited Destroy;
end;

function TConsoleMetricsExporter.FormatMetric(const AEntry: TMetricEntry): String;
var
  LTags: String;
begin
  LTags := FormatTags(AEntry.Tags);
  Result := Format('[%s] %s %s = %.2f%s', [
    FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', AEntry.Timestamp),
    MetricTypeToString(AEntry.MetricType),
    AEntry.Name,
    AEntry.Value,
    LTags
  ]);
end;

function TConsoleMetricsExporter.MetricTypeToString(AType: TMetricType): String;
begin
  case AType of
    mtCounter:   Result := 'COUNTER  ';
    mtGauge:     Result := 'GAUGE    ';
    mtHistogram: Result := 'HISTOGRAM';
    mtSummary:   Result := 'SUMMARY  ';
    mtTimer:     Result := 'TIMER    ';
    mtError:     Result := 'ERROR    ';
  else
    Result := 'UNKNOWN  ';
  end;
end;

function TConsoleMetricsExporter.FormatTags(ATags: TMetricTags): String;
var
  LPair: TPair<String, String>;
begin
  Result := '';
  if Assigned(ATags) and (ATags.Count > 0) then
  begin
    Result := ' [';
    for LPair in ATags do
      Result := Result + LPair.Key + '=' + LPair.Value + '; ';
    Result := Result.TrimRight([' ', ';']) + ']';
  end;
end;

procedure TConsoleMetricsExporter.Export(const AMetrics: TArray<TMetricEntry>);
var
  LFor: Integer;
  LFormattedMetric: String;
begin
  if not FEnabled then Exit;

  FLock.Enter;
  try
    for LFor := 0 to High(AMetrics) do
    begin
      LFormattedMetric := FormatMetric(AMetrics[LFor]);
      Writeln(LFormattedMetric);
    end;
  finally
    FLock.Leave;
  end;
end;

function TConsoleMetricsExporter.GetName: String;
begin
  Result := FName;
end;

function TConsoleMetricsExporter.IsEnabled: Boolean;
begin
  Result := FEnabled;
end;

procedure TConsoleMetricsExporter.SetEnabled(AEnabled: Boolean);
begin
  FEnabled := AEnabled;
end;

{ TFileMetricsExporter }

constructor TFileMetricsExporter.Create(const AFileName: String; const AName: String = 'File');
begin
  inherited Create;
  FName := AName;
  FFileName := AFileName;
  FEnabled := True;
  FLock := TCriticalSection.Create;
  FFileStream := nil;
end;

destructor TFileMetricsExporter.Destroy;
begin
  FFileStream.Free;
  FLock.Free;
  inherited Destroy;
end;

function TFileMetricsExporter.FormatMetric(const AEntry: TMetricEntry): String;
var
  LTags: String;
begin
  LTags := FormatTags(AEntry.Tags);
  Result := Format('[%s] %s %s = %.2f%s', [
    FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', AEntry.Timestamp),
    MetricTypeToString(AEntry.MetricType),
    AEntry.Name,
    AEntry.Value,
    LTags
  ]) + sLineBreak;
end;

function TFileMetricsExporter.MetricTypeToString(AType: TMetricType): String;
begin
  case AType of
    mtCounter:   Result := 'COUNTER  ';
    mtGauge:     Result := 'GAUGE    ';
    mtHistogram: Result := 'HISTOGRAM';
    mtSummary:   Result := 'SUMMARY  ';
    mtTimer:     Result := 'TIMER    ';
    mtError:     Result := 'ERROR    ';
  else
    Result := 'UNKNOWN  ';
  end;
end;

function TFileMetricsExporter.FormatTags(ATags: TMetricTags): String;
var
  LPair: TPair<String, String>;
begin
  Result := '';
  if Assigned(ATags) and (ATags.Count > 0) then
  begin
    Result := ' [';
    for LPair in ATags do
      Result := Result + LPair.Key + '=' + LPair.Value + '; ';
    Result := Result.TrimRight([' ', ';']) + ']';
  end;
end;

procedure TFileMetricsExporter.EnsureFileOpen;
begin
  if not Assigned(FFileStream) then
  begin
    if not TDirectory.Exists(ExtractFilePath(FFileName)) then
      TDirectory.CreateDirectory(ExtractFilePath(FFileName));
    FFileStream := TFileStream.Create(FFileName, fmCreate or fmShareDenyWrite);
  end;
end;

procedure TFileMetricsExporter.Export(const AMetrics: TArray<TMetricEntry>);
var
  LFor: Integer;
  LFormattedMetric: String;
  LBytes: TBytes;
begin
  if not FEnabled then Exit;

  FLock.Enter;
  try
    EnsureFileOpen;
    for LFor := 0 to High(AMetrics) do
    begin
      LFormattedMetric := FormatMetric(AMetrics[LFor]);
      LBytes := TEncoding.UTF8.GetBytes(LFormattedMetric);
      FFileStream.WriteBuffer(LBytes[0], Length(LBytes));
    end;
    // Flush não existe em TFileStream no Delphi
    // O buffer é automaticamente liberado
  finally
    FLock.Leave;
  end;
end;

function TFileMetricsExporter.GetName: String;
begin
  Result := FName;
end;

function TFileMetricsExporter.IsEnabled: Boolean;
begin
  Result := FEnabled;
end;

procedure TFileMetricsExporter.SetEnabled(AEnabled: Boolean);
begin
  FEnabled := AEnabled;
end;

// Funções utilitárias globais
function GetMetricsCollector: IMetricsCollector;
begin
  if not Assigned(GMetricsCollector) then
  begin
    if not Assigned(GMetricsLock) then
      GMetricsLock := TCriticalSection.Create;
      
    GMetricsLock.Enter;
    try
      if not Assigned(GMetricsCollector) then
      begin
        GMetricsCollector := CreateMetricsCollector;
        // Adiciona exportador padrão para console
        TMetricsCollector(GMetricsCollector).AddExporter(TConsoleMetricsExporter.Create);
      end;
    finally
      GMetricsLock.Leave;
    end;
  end;
  Result := GMetricsCollector;
end;

function CreateMetricsCollector: IMetricsCollector;
begin
  Result := TMetricsCollector.Create;
end;

function CreateMetricTags: TMetricTags;
begin
  Result := TMetricTags.Create;
end;

function MetricTypeFromString(const AType: String): TMetricType;
var
  LType: String;
begin
  LType := UpperCase(Trim(AType));
  if LType = 'COUNTER' then Result := mtCounter
  else if LType = 'GAUGE' then Result := mtGauge
  else if LType = 'HISTOGRAM' then Result := mtHistogram
  else if LType = 'SUMMARY' then Result := mtSummary
  else if LType = 'TIMER' then Result := mtTimer
  else if LType = 'ERROR' then Result := mtError
  else Result := mtCounter; // Padrão
end;

function MetricTypeToString(AType: TMetricType): String;
begin
  case AType of
    mtCounter:   Result := 'COUNTER';
    mtGauge:     Result := 'GAUGE';
    mtHistogram: Result := 'HISTOGRAM';
    mtSummary:   Result := 'SUMMARY';
    mtTimer:     Result := 'TIMER';
    mtError:     Result := 'ERROR';
  else
    Result := 'UNKNOWN';
  end;
end;

initialization
  GMetricsLock := TCriticalSection.Create;

finalization
  GMetricsCollector := nil;
  FreeAndNil(GMetricsLock);

end.