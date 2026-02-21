unit Nest4D.Logging.Async;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Generics.Collections,
  System.SyncObjs,
  Winapi.Windows,
  Nest4D.Logging;

type
  // Interface para logger assíncrono
  INest4DAsyncLogger = interface
    ['{A1B2C3D4-E5F6-7890-ABCD-EF1234567890}']
    procedure TraceAsync(const AMessage: String; const AContext: TLogContext = nil);
    procedure DebugAsync(const AMessage: String; const AContext: TLogContext = nil);
    procedure InfoAsync(const AMessage: String; const AContext: TLogContext = nil);
    procedure WarnAsync(const AMessage: String; const AContext: TLogContext = nil);
    procedure ErrorAsync(const AMessage: String; const AContext: TLogContext = nil; AException: Exception = nil);
    procedure FatalAsync(const AMessage: String; const AContext: TLogContext = nil; AException: Exception = nil);
    procedure FlushAsync;
  end;

  // Appender assíncrono que implementa ILogAppender
  TNest4DAsyncLogAppender = class(TInterfacedObject, ILogAppender)
  private
    FInnerAppender: ILogAppender;
    FLogQueue: TThreadedQueue<TLogEntry>;
    FWorkerThread: TThread;
    FShutdown: Boolean;
    FLock: TCriticalSection;
    FQueueSize: Integer;
    FDroppedLogs: Int64;
    FName: String;
    FEnabled: Boolean;
    procedure ProcessLogQueue;
  public
    constructor Create(const AInnerAppender: ILogAppender; AQueueSize: Integer = 1000);
    destructor Destroy; override;
    
    // Implementação da interface ILogAppender
    procedure WriteLog(const AEntry: TLogEntry);
    procedure Flush;
    function GetName: String;
    function IsEnabled: Boolean;
    procedure SetEnabled(AEnabled: Boolean);
    
    property DroppedLogs: Int64 read FDroppedLogs;
  end;

  // Logger assíncrono simples
  TNest4DAsyncLogger = class(TInterfacedObject, INest4DAsyncLogger)
  private
    FInnerLogger: IStructuredLogger;
  public
    constructor Create(const AInnerLogger: IStructuredLogger);
    destructor Destroy; override;
    
    procedure TraceAsync(const AMessage: String; const AContext: TLogContext = nil);
    procedure DebugAsync(const AMessage: String; const AContext: TLogContext = nil);
    procedure InfoAsync(const AMessage: String; const AContext: TLogContext = nil);
    procedure WarnAsync(const AMessage: String; const AContext: TLogContext = nil);
    procedure ErrorAsync(const AMessage: String; const AContext: TLogContext = nil; AException: Exception = nil);
    procedure FatalAsync(const AMessage: String; const AContext: TLogContext = nil; AException: Exception = nil);
    procedure FlushAsync;
  end;

// Funções utilitárias
function CreateAsyncLogger(const AInnerLogger: IStructuredLogger): INest4DAsyncLogger;
function WrapWithAsyncAppender(const AAppender: ILogAppender; AQueueSize: Integer = 1000): ILogAppender;

implementation

uses
  System.DateUtils;

type
  // Thread worker para processar logs assíncronos
  TAsyncLogWorker = class(TThread)
  private
    FAppender: TNest4DAsyncLogAppender;
  public
    constructor Create(AAppender: TNest4DAsyncLogAppender);
    procedure Execute; override;
  end;

function CreateLogEntry(ALevel: TLogLevel; const AMessage: String; AContext: TLogContext): TLogEntry;
begin
  Result.Level := ALevel;
  Result.Message := AMessage;
  Result.Timestamp := Now;
  Result.Context := AContext;
  Result.ThreadId := GetCurrentThreadId;
end;

{ TNest4DAsyncLogAppender }

constructor TNest4DAsyncLogAppender.Create(const AInnerAppender: ILogAppender; AQueueSize: Integer);
begin
  inherited Create;
  FInnerAppender := AInnerAppender;
  FQueueSize := AQueueSize;
  FLogQueue := TThreadedQueue<TLogEntry>.Create(FQueueSize, INFINITE, 100);
  FLock := TCriticalSection.Create;
  FShutdown := False;
  FDroppedLogs := 0;
  FEnabled := True;
  FName := 'AsyncLogAppender';
  
  // Inicia thread worker
  FWorkerThread := TAsyncLogWorker.Create(Self);
end;

destructor TNest4DAsyncLogAppender.Destroy;
begin
  FShutdown := True;
  
  if Assigned(FWorkerThread) then
  begin
    FWorkerThread.Terminate;
    FWorkerThread.WaitFor;
    FWorkerThread.Free;
  end;
  
  FreeAndNil(FLogQueue);
  FreeAndNil(FLock);
  inherited;
end;

procedure TNest4DAsyncLogAppender.ProcessLogQueue;
var
  LEntry: TLogEntry;
begin
  while not FShutdown do
  begin
    if FLogQueue.PopItem(LEntry) = wrSignaled then
    begin
      try
        if Assigned(FInnerAppender) then
          FInnerAppender.WriteLog(LEntry);
      except
        // Silenciosamente ignora erros para evitar loops
      end;
    end;
  end;
end;

procedure TNest4DAsyncLogAppender.WriteLog(const AEntry: TLogEntry);
begin
  if not FEnabled then
    Exit;
    
  if FLogQueue.PushItem(AEntry) <> wrSignaled then
  begin
    FLock.Enter;
    try
      Inc(FDroppedLogs);
    finally
      FLock.Leave;
    end;
  end;
end;

procedure TNest4DAsyncLogAppender.Flush;
begin
  // Aguarda a fila esvaziar
  while FLogQueue.QueueSize > 0 do
    Sleep(10);
    
  if Assigned(FInnerAppender) then
    FInnerAppender.Flush;
end;

function TNest4DAsyncLogAppender.GetName: String;
begin
  Result := FName;
end;

function TNest4DAsyncLogAppender.IsEnabled: Boolean;
begin
  Result := FEnabled;
end;

procedure TNest4DAsyncLogAppender.SetEnabled(AEnabled: Boolean);
begin
  FEnabled := AEnabled;
end;

{ TNest4DAsyncLogger }

constructor TNest4DAsyncLogger.Create(const AInnerLogger: IStructuredLogger);
begin
  inherited Create;
  FInnerLogger := AInnerLogger;
end;

destructor TNest4DAsyncLogger.Destroy;
begin
  inherited;
end;

procedure TNest4DAsyncLogger.TraceAsync(const AMessage: String; const AContext: TLogContext);
begin
  TThread.CreateAnonymousThread(
    procedure
    begin
      if Assigned(FInnerLogger) then
        FInnerLogger.Trace(AMessage, AContext);
    end).Start;
end;

procedure TNest4DAsyncLogger.DebugAsync(const AMessage: String; const AContext: TLogContext);
begin
  TThread.CreateAnonymousThread(
    procedure
    begin
      if Assigned(FInnerLogger) then
        FInnerLogger.Debug(AMessage, AContext);
    end).Start;
end;

procedure TNest4DAsyncLogger.InfoAsync(const AMessage: String; const AContext: TLogContext);
begin
  TThread.CreateAnonymousThread(
    procedure
    begin
      if Assigned(FInnerLogger) then
        FInnerLogger.Info(AMessage, AContext);
    end).Start;
end;

procedure TNest4DAsyncLogger.WarnAsync(const AMessage: String; const AContext: TLogContext);
begin
  TThread.CreateAnonymousThread(
    procedure
    begin
      if Assigned(FInnerLogger) then
        FInnerLogger.Warn(AMessage, AContext);
    end).Start;
end;

procedure TNest4DAsyncLogger.ErrorAsync(const AMessage: String; const AContext: TLogContext; AException: Exception);
begin
  TThread.CreateAnonymousThread(
    procedure
    begin
      if Assigned(FInnerLogger) then
        FInnerLogger.Error(AMessage, AContext);
    end).Start;
end;

procedure TNest4DAsyncLogger.FatalAsync(const AMessage: String; const AContext: TLogContext; AException: Exception);
begin
  TThread.CreateAnonymousThread(
    procedure
    begin
      if Assigned(FInnerLogger) then
        FInnerLogger.Fatal(AMessage, AContext);
    end).Start;
end;

procedure TNest4DAsyncLogger.FlushAsync;
begin
  TThread.CreateAnonymousThread(
    procedure
    begin
      if Assigned(FInnerLogger) then
      begin
        // Como IStructuredLogger não tem Flush, vamos apenas aguardar um pouco
        // para permitir que logs pendentes sejam processados
        Sleep(100);
      end;
    end).Start;
end;

{ TAsyncLogWorker }

constructor TAsyncLogWorker.Create(AAppender: TNest4DAsyncLogAppender);
begin
  inherited Create(False);
  FAppender := AAppender;
end;

procedure TAsyncLogWorker.Execute;
begin
  FAppender.ProcessLogQueue;
end;

// Funções utilitárias

function CreateAsyncLogger(const AInnerLogger: IStructuredLogger): INest4DAsyncLogger;
begin
  Result := TNest4DAsyncLogger.Create(AInnerLogger);
end;

function WrapWithAsyncAppender(const AAppender: ILogAppender; AQueueSize: Integer): ILogAppender;
begin
  Result := TNest4DAsyncLogAppender.Create(AAppender, AQueueSize);
end;

end.

