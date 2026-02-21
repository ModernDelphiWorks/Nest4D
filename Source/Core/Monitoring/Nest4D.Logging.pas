unit Nest4D.Logging;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  System.SyncObjs,
  System.IOUtils,
  System.DateUtils;

type
  // Níveis de log
  TLogLevel = (llTrace, llDebug, llInfo, llWarn, llError, llFatal);

  // Forward declarations
  TLogContext = class;
  ILogAppender = interface;
  IStructuredLogger = interface;

  // Contexto de log para dados estruturados
  TLogContext = class
  private
    FData: TDictionary<String, String>;
  public
    constructor Create;
    destructor Destroy; override;
    function Add(const AKey, AValue: String): TLogContext;
    function GetValue(const AKey: String): String;
    function HasKey(const AKey: String): Boolean;
    function GetKeys: TArray<String>;
    function Clone: TLogContext;
    procedure Clear;
    property Data: TDictionary<String, String> read FData;
  end;

  // Entrada de log
  TLogEntry = record
    Level: TLogLevel;
    Message: String;
    Timestamp: TDateTime;
    Context: TLogContext;
    ThreadId: Cardinal;
  end;

  // Interface para appenders de log
  ILogAppender = interface
    ['{B8F5E5A1-2C3D-4E5F-8A9B-1C2D3E4F5A6B}']
    procedure WriteLog(const AEntry: TLogEntry);
    procedure Flush;
    function GetName: String;
    function IsEnabled: Boolean;
    procedure SetEnabled(AEnabled: Boolean);
  end;

  // Interface principal do logger estruturado
  IStructuredLogger = interface
    ['{A1B2C3D4-E5F6-7890-ABCD-EF1234567890}']
    procedure Trace(const AMessage: String; AContext: TLogContext = nil);
    procedure Debug(const AMessage: String; AContext: TLogContext = nil);
    procedure Info(const AMessage: String; AContext: TLogContext = nil);
    procedure Warn(const AMessage: String; AContext: TLogContext = nil);
    procedure Error(const AMessage: String; AContext: TLogContext = nil);
    procedure Fatal(const AMessage: String; AContext: TLogContext = nil);
    procedure AddAppender(AAppender: ILogAppender);
    procedure RemoveAppender(const AName: String);
    procedure SetLevel(ALevel: TLogLevel);
    function GetLevel: TLogLevel;
  end;

  // Implementação do logger estruturado
  TStructuredLogger = class(TInterfacedObject, IStructuredLogger)
  private
    FLevel: TLogLevel;
    FAppenders: TList<ILogAppender>;
    FLock: TCriticalSection;
    procedure WriteToAppenders(const AEntry: TLogEntry);
  public
    constructor Create(ALevel: TLogLevel = llInfo);
    destructor Destroy; override;
    procedure Trace(const AMessage: String; AContext: TLogContext = nil);
    procedure Debug(const AMessage: String; AContext: TLogContext = nil);
    procedure Info(const AMessage: String; AContext: TLogContext = nil);
    procedure Warn(const AMessage: String; AContext: TLogContext = nil);
    procedure Error(const AMessage: String; AContext: TLogContext = nil);
    procedure Fatal(const AMessage: String; AContext: TLogContext = nil);
    procedure AddAppender(AAppender: ILogAppender);
    procedure RemoveAppender(const AName: String);
    procedure SetLevel(ALevel: TLogLevel);
    function GetLevel: TLogLevel;
  end;

  // Appender para console
  TConsoleLogAppender = class(TInterfacedObject, ILogAppender)
  private
    FName: String;
    FEnabled: Boolean;
    FLock: TCriticalSection;
    function FormatLogEntry(const AEntry: TLogEntry): String;
  public
    constructor Create(const AName: String = 'Console');
    destructor Destroy; override;
    procedure WriteLog(const AEntry: TLogEntry);
    procedure Flush;
    function GetName: String;
    function IsEnabled: Boolean;
    procedure SetEnabled(AEnabled: Boolean);
  end;

  // Appender para arquivo
  TFileLogAppender = class(TInterfacedObject, ILogAppender)
  private
    FName: String;
    FFileName: String;
    FEnabled: Boolean;
    FLock: TCriticalSection;
    FFileStream: TFileStream;
    function FormatLogEntry(const AEntry: TLogEntry): String;
    procedure EnsureFileOpen;
  public
    constructor Create(const AFileName: String; const AName: String = 'File');
    destructor Destroy; override;
    procedure WriteLog(const AEntry: TLogEntry);
    procedure Flush;
    function GetName: String;
    function IsEnabled: Boolean;
    procedure SetEnabled(AEnabled: Boolean);
    property FileName: String read FFileName;
  end;

// Funções utilitárias globais
function GetLogger: IStructuredLogger;
function CreateLogger(ALevel: TLogLevel = llInfo): IStructuredLogger;
function CreateLogContext: TLogContext;
function LogLevelFromString(const ALevel: String): TLogLevel;
function LogLevelToString(ALevel: TLogLevel): String;

implementation

uses
  Winapi.Windows;

var
  GLogger: IStructuredLogger;
  GLoggerLock: TCriticalSection;

{ TLogContext }

constructor TLogContext.Create;
begin
  inherited Create;
  FData := TDictionary<String, String>.Create;
end;

destructor TLogContext.Destroy;
begin
  FData.Free;
  inherited Destroy;
end;

function TLogContext.Add(const AKey, AValue: String): TLogContext;
begin
  FData.AddOrSetValue(AKey, AValue);
  Result := Self;
end;

function TLogContext.GetValue(const AKey: String): String;
begin
  if not FData.TryGetValue(AKey, Result) then
    Result := '';
end;

function TLogContext.HasKey(const AKey: String): Boolean;
begin
  Result := FData.ContainsKey(AKey);
end;

function TLogContext.GetKeys: TArray<String>;
begin
  Result := FData.Keys.ToArray;
end;

function TLogContext.Clone: TLogContext;
var
  LPair: TPair<String, String>;
begin
  Result := TLogContext.Create;
  for LPair in FData do
    Result.Add(LPair.Key, LPair.Value);
end;

procedure TLogContext.Clear;
begin
  FData.Clear;
end;

{ TLogEntry }

function CreateLogEntry(ALevel: TLogLevel; const AMessage: String; AContext: TLogContext = nil): TLogEntry;
begin
  Result.Level := ALevel;
  Result.Message := AMessage;
  Result.Timestamp := Now;
  Result.Context := AContext;
  Result.ThreadId := GetCurrentThreadId;
end;

{ TStructuredLogger }

constructor TStructuredLogger.Create(ALevel: TLogLevel = llInfo);
begin
  inherited Create;
  FLevel := ALevel;
  FAppenders := TList<ILogAppender>.Create;
  FLock := TCriticalSection.Create;
end;

destructor TStructuredLogger.Destroy;
begin
  FAppenders.Free;
  FLock.Free;
  inherited Destroy;
end;

procedure TStructuredLogger.WriteToAppenders(const AEntry: TLogEntry);
var
  LAppender: ILogAppender;
begin
  FLock.Enter;
  try
    for LAppender in FAppenders do
    begin
      if LAppender.IsEnabled then
        LAppender.WriteLog(AEntry);
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TStructuredLogger.Trace(const AMessage: String; AContext: TLogContext = nil);
var
  LEntry: TLogEntry;
begin
  if FLevel <= llTrace then
  begin
    LEntry := CreateLogEntry(llTrace, AMessage, AContext);
    WriteToAppenders(LEntry);
  end;
end;

procedure TStructuredLogger.Debug(const AMessage: String; AContext: TLogContext = nil);
var
  LEntry: TLogEntry;
begin
  if FLevel <= llDebug then
  begin
    LEntry := CreateLogEntry(llDebug, AMessage, AContext);
    WriteToAppenders(LEntry);
  end;
end;

procedure TStructuredLogger.Info(const AMessage: String; AContext: TLogContext = nil);
var
  LEntry: TLogEntry;
begin
  if FLevel <= llInfo then
  begin
    LEntry := CreateLogEntry(llInfo, AMessage, AContext);
    WriteToAppenders(LEntry);
  end;
end;

procedure TStructuredLogger.Warn(const AMessage: String; AContext: TLogContext = nil);
var
  LEntry: TLogEntry;
begin
  if FLevel <= llWarn then
  begin
    LEntry := CreateLogEntry(llWarn, AMessage, AContext);
    WriteToAppenders(LEntry);
  end;
end;

procedure TStructuredLogger.Error(const AMessage: String; AContext: TLogContext = nil);
var
  LEntry: TLogEntry;
begin
  if FLevel <= llError then
  begin
    LEntry := CreateLogEntry(llError, AMessage, AContext);
    WriteToAppenders(LEntry);
  end;
end;

procedure TStructuredLogger.Fatal(const AMessage: String; AContext: TLogContext = nil);
var
  LEntry: TLogEntry;
begin
  if FLevel <= llFatal then
  begin
    LEntry := CreateLogEntry(llFatal, AMessage, AContext);
    WriteToAppenders(LEntry);
  end;
end;

procedure TStructuredLogger.AddAppender(AAppender: ILogAppender);
begin
  FLock.Enter;
  try
    FAppenders.Add(AAppender);
  finally
    FLock.Leave;
  end;
end;

procedure TStructuredLogger.RemoveAppender(const AName: String);
var
  I: Integer;
begin
  FLock.Enter;
  try
    for I := FAppenders.Count - 1 downto 0 do
    begin
      if FAppenders[I].GetName = AName then
      begin
        FAppenders.Delete(I);
        Break;
      end;
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TStructuredLogger.SetLevel(ALevel: TLogLevel);
begin
  FLevel := ALevel;
end;

function TStructuredLogger.GetLevel: TLogLevel;
begin
  Result := FLevel;
end;

{ TConsoleLogAppender }

constructor TConsoleLogAppender.Create(const AName: String = 'Console');
begin
  inherited Create;
  FName := AName;
  FEnabled := True;
  FLock := TCriticalSection.Create;
end;

destructor TConsoleLogAppender.Destroy;
begin
  FLock.Free;
  inherited Destroy;
end;

function TConsoleLogAppender.FormatLogEntry(const AEntry: TLogEntry): String;
var
  LContext: String;
  LPair: TPair<String, String>;
begin
  LContext := '';
  if Assigned(AEntry.Context) and (AEntry.Context.Data.Count > 0) then
  begin
    LContext := ' [';
    for LPair in AEntry.Context.Data do
      LContext := LContext + LPair.Key + '=' + LPair.Value + '; ';
    LContext := LContext.TrimRight([' ', ';']) + ']';
  end;
  
  Result := Format('[%s] %s [Thread:%d] %s%s', [
    FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', AEntry.Timestamp),
    LogLevelToString(AEntry.Level),
    AEntry.ThreadId,
    AEntry.Message,
    LContext
  ]);
end;

procedure TConsoleLogAppender.WriteLog(const AEntry: TLogEntry);
var
  LFormattedMessage: String;
begin
  if not FEnabled then Exit;
  
  FLock.Enter;
  try
    LFormattedMessage := FormatLogEntry(AEntry);
    Writeln(LFormattedMessage);
  finally
    FLock.Leave;
  end;
end;

procedure TConsoleLogAppender.Flush;
begin
  // Console não precisa de flush explícito
end;

function TConsoleLogAppender.GetName: String;
begin
  Result := FName;
end;

function TConsoleLogAppender.IsEnabled: Boolean;
begin
  Result := FEnabled;
end;

procedure TConsoleLogAppender.SetEnabled(AEnabled: Boolean);
begin
  FEnabled := AEnabled;
end;

{ TFileLogAppender }

constructor TFileLogAppender.Create(const AFileName: String; const AName: String = 'File');
begin
  inherited Create;
  FName := AName;
  FFileName := AFileName;
  FEnabled := True;
  FLock := TCriticalSection.Create;
  FFileStream := nil;
end;

destructor TFileLogAppender.Destroy;
begin
  FFileStream.Free;
  FLock.Free;
  inherited Destroy;
end;

function TFileLogAppender.FormatLogEntry(const AEntry: TLogEntry): String;
var
  LContext: String;
  LPair: TPair<String, String>;
begin
  LContext := '';
  if Assigned(AEntry.Context) and (AEntry.Context.Data.Count > 0) then
  begin
    LContext := ' [';
    for LPair in AEntry.Context.Data do
      LContext := LContext + LPair.Key + '=' + LPair.Value + '; ';
    LContext := LContext.TrimRight([' ', ';']) + ']';
  end;
  
  Result := Format('[%s] %s [Thread:%d] %s%s', [
    FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', AEntry.Timestamp),
    LogLevelToString(AEntry.Level),
    AEntry.ThreadId,
    AEntry.Message,
    LContext
  ]) + sLineBreak;
end;

procedure TFileLogAppender.EnsureFileOpen;
begin
  if not Assigned(FFileStream) then
  begin
    if not TDirectory.Exists(ExtractFilePath(FFileName)) then
      TDirectory.CreateDirectory(ExtractFilePath(FFileName));
    FFileStream := TFileStream.Create(FFileName, fmCreate or fmShareDenyWrite);
  end;
end;

procedure TFileLogAppender.WriteLog(const AEntry: TLogEntry);
var
  LFormattedMessage: String;
  LBytes: TBytes;
begin
  if not FEnabled then Exit;
  
  FLock.Enter;
  try
    EnsureFileOpen;
    LFormattedMessage := FormatLogEntry(AEntry);
    LBytes := TEncoding.UTF8.GetBytes(LFormattedMessage);
    FFileStream.WriteBuffer(LBytes[0], Length(LBytes));
  finally
    FLock.Leave;
  end;
end;

procedure TFileLogAppender.Flush;
begin
  FLock.Enter;
  try
    if Assigned(FFileStream) then
    begin
      // TFileStream não tem método Flush, mas podemos forçar a escrita
      FFileStream.Position := FFileStream.Position;
    end;
  finally
    FLock.Leave;
  end;
end;

function TFileLogAppender.GetName: String;
begin
  Result := FName;
end;

function TFileLogAppender.IsEnabled: Boolean;
begin
  Result := FEnabled;
end;

procedure TFileLogAppender.SetEnabled(AEnabled: Boolean);
begin
  FEnabled := AEnabled;
end;

// Funções utilitárias globais

function GetLogger: IStructuredLogger;
begin
  if not Assigned(GLogger) then
  begin
    if not Assigned(GLoggerLock) then
      GLoggerLock := TCriticalSection.Create;
      
    GLoggerLock.Enter;
    try
      if not Assigned(GLogger) then
      begin
        GLogger := CreateLogger;
        // Adiciona appender padrão para console
        GLogger.AddAppender(TConsoleLogAppender.Create);
      end;
    finally
      GLoggerLock.Leave;
    end;
  end;
  Result := GLogger;
end;

function CreateLogger(ALevel: TLogLevel = llInfo): IStructuredLogger;
begin
  Result := TStructuredLogger.Create(ALevel);
end;

function CreateLogContext: TLogContext;
begin
  Result := TLogContext.Create;
end;

function LogLevelFromString(const ALevel: String): TLogLevel;
var
  LLevel: String;
begin
  LLevel := UpperCase(Trim(ALevel));
  if LLevel = 'TRACE' then Result := llTrace
  else if LLevel = 'DEBUG' then Result := llDebug
  else if LLevel = 'INFO' then Result := llInfo
  else if LLevel = 'WARN' then Result := llWarn
  else if LLevel = 'ERROR' then Result := llError
  else if LLevel = 'FATAL' then Result := llFatal
  else Result := llInfo; // Padrão
end;

function LogLevelToString(ALevel: TLogLevel): String;
begin
  case ALevel of
    llTrace: Result := 'TRACE';
    llDebug: Result := 'DEBUG';
    llInfo:  Result := 'INFO';
    llWarn:  Result := 'WARN';
    llError: Result := 'ERROR';
    llFatal: Result := 'FATAL';
  else
    Result := 'UNKNOWN';
  end;
end;

initialization
  GLoggerLock := TCriticalSection.Create;

finalization
  GLogger := nil;
  FreeAndNil(GLoggerLock);

end.