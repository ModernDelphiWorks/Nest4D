unit Nest4D.Config.Hotreload;

interface

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.DateUtils,
  System.SyncObjs,
  System.Generics.Collections,
  Nest4D.Config,
  Nest4D.Config.Manager;

type
  // Tipos de eventos de arquivo
  TFileChangeType = (fctCreated, fctModified, fctDeleted, fctRenamed);

  // Informa??es sobre mudan?a de arquivo
  TFileChangeInfo = record
    FileName: string;
    ChangeType: TFileChangeType;
    Timestamp: TDateTime;
    OldFileName: string; // Para rename
  end;

  // Evento para notifica??o de mudan?a
  TFileChangeEvent = procedure(const AChangeInfo: TFileChangeInfo) of object;

  // Evento para notifica??o de erro de configura??o
  TConfigErrorEvent = procedure(Sender: TObject; const ErrorMessage: string) of object;

  // Configura??o do watcher
  TFileWatcherConfig = record
    WatchPath: string;
    FileFilter: string; // *.json, *.yaml, etc.
    PollingInterval: Integer; // ms
    EnableSubdirectories: Boolean;
    DebounceTime: Integer; // ms - evita m?ltiplos eventos

    class function Default: TFileWatcherConfig; static;
  end;

  // Thread para monitoramento de arquivos
  TNest4DFileWatcherThread = class(TThread)
  private
    FConfig: TFileWatcherConfig;
    FOnFileChange: TFileChangeEvent;
    FFileStates: TDictionary<string, TDateTime>;
    FLock: TCriticalSection;
    FLastEventTime: TDictionary<string, TDateTime>;

    procedure CheckFiles;
    procedure NotifyFileChange(const AChangeInfo: TFileChangeInfo);
    function ShouldProcessEvent(const AFileName: string): Boolean;
    function GetFileList: TArray<string>;

  protected
    procedure Execute; override;

  public
    constructor Create(const AConfig: TFileWatcherConfig; AOnFileChange: TFileChangeEvent);
    destructor Destroy; override;

    procedure AddWatchFile(const AFileName: string);
    procedure RemoveWatchFile(const AFileName: string);
    procedure ClearWatchFiles;

    property Config: TFileWatcherConfig read FConfig write FConfig;
  end;

  // Gerenciador de hot reload para configura??es
  TNest4DConfigHotReload = class
  private
    FEnabled: Boolean;
    FWatcherThread: TNest4DFileWatcherThread;
    FConfigManager: TNest4DConfigManager;
    FWatchedFiles: TStringList;
    FLock: TCriticalSection;
    FOnConfigChanged: TNotifyEvent;
    FOnConfigError: TConfigErrorEvent;
    FLastReloadTime: TDateTime;
    FReloadCount: Integer;

    procedure HandleFileChange(const AChangeInfo: TFileChangeInfo);
    procedure ReloadConfiguration(const AFileName: string);
    function IsConfigFile(const AFileName: string): Boolean;

  public
    constructor Create(AConfigManager: TNest4DConfigManager);
    destructor Destroy; override;

    // Controle do hot reload
    procedure Start;
    procedure Stop;
    procedure Restart;

    // Gerenciamento de arquivos monitorados
    procedure AddWatchFile(const AFileName: string);
    procedure RemoveWatchFile(const AFileName: string);
    procedure ClearWatchFiles;

    // Configura??o
    procedure SetWatchPath(const APath: string);
    procedure SetPollingInterval(const AInterval: Integer);
    procedure SetFileFilter(const AFilter: string);

    // Propriedades
    property Enabled: Boolean read FEnabled write FEnabled;
    property WatchedFiles: TStringList read FWatchedFiles;
    property LastReloadTime: TDateTime read FLastReloadTime;
    property ReloadCount: Integer read FReloadCount;

    // Eventos
    property OnConfigChanged: TNotifyEvent read FOnConfigChanged write FOnConfigChanged;
    property OnConfigError: TConfigErrorEvent read FOnConfigError write FOnConfigError;
  end;

  // Utilit?rios para hot reload
  TNest4DHotReloadUtils = class
  public
    // Verificar se arquivo ? v?lido para monitoramento
    class function IsValidConfigFile(const AFileName: string): Boolean;

    // Obter lista de arquivos de configura??o em um diret?rio
    class function GetConfigFiles(const APath: string; const ARecursive: Boolean = False): TArray<string>;

    // Verificar se arquivo foi modificado recentemente
    class function IsFileRecentlyModified(const AFileName: string; const AThresholdSeconds: Integer = 5): Boolean;

    // Criar backup de arquivo de configura??o
    class function CreateConfigBackup(const AFileName: string): string;

    // Restaurar backup de configura??o
    class function RestoreConfigBackup(const ABackupFileName: string): Boolean;

    // Validar integridade de arquivo de configura??o
    class function ValidateConfigFile(const AFileName: string): Boolean;
  end;

implementation

uses
  System.JSON,
  System.RegularExpressions;

{ TFileWatcherConfig }

class function TFileWatcherConfig.Default: TFileWatcherConfig;
begin
  Result.WatchPath := TPath.GetDirectoryName(ParamStr(0));
  Result.FileFilter := '*.json;*.yaml;*.yml;*.ini;*.conf';
  Result.PollingInterval := 1000; // 1 segundo
  Result.EnableSubdirectories := True;
  Result.DebounceTime := 500; // 500ms
end;

{ TNest4DFileWatcherThread }

constructor TNest4DFileWatcherThread.Create(const AConfig: TFileWatcherConfig; AOnFileChange: TFileChangeEvent);
begin
  inherited Create(False);
  FConfig := AConfig;
  FOnFileChange := AOnFileChange;
  FFileStates := TDictionary<string, TDateTime>.Create;
  FLastEventTime := TDictionary<string, TDateTime>.Create;
  FLock := TCriticalSection.Create;
  FreeOnTerminate := False;
end;

destructor TNest4DFileWatcherThread.Destroy;
begin
  Terminate;
  WaitFor;
  FFileStates.Free;
  FLastEventTime.Free;
  FLock.Free;
  inherited;
end;

procedure TNest4DFileWatcherThread.Execute;
begin
  while not Terminated do
  begin
    try
      CheckFiles;
      Sleep(FConfig.PollingInterval);
    except
      on E: Exception do
      begin
        // Log error silently and continue
        Sleep(FConfig.PollingInterval * 2); // Wait longer on error
      end;
    end;
  end;
end;

procedure TNest4DFileWatcherThread.CheckFiles;
var
  LFiles: TArray<string>;
  LFile: string;
  LCurrentTime: TDateTime;
  LLastModified: TDateTime;
  LChangeInfo: TFileChangeInfo;
begin
  if not TDirectory.Exists(FConfig.WatchPath) then
    Exit;

  LFiles := GetFileList;
  LCurrentTime := Now;

  FLock.Enter;
  try
    // Verificar arquivos existentes
    for LFile in LFiles do
    begin
      if not TFile.Exists(LFile) then
        Continue;

      LLastModified := TFile.GetLastWriteTime(LFile);

      if FFileStates.ContainsKey(LFile) then
      begin
        // Arquivo j? conhecido - verificar modifica??o
        if FFileStates[LFile] <> LLastModified then
        begin
          if ShouldProcessEvent(LFile) then
          begin
            LChangeInfo.FileName := LFile;
            LChangeInfo.ChangeType := fctModified;
            LChangeInfo.Timestamp := LCurrentTime;
            LChangeInfo.OldFileName := '';

            NotifyFileChange(LChangeInfo);
            FLastEventTime.AddOrSetValue(LFile, LCurrentTime);
          end;

          FFileStates.AddOrSetValue(LFile, LLastModified);
        end;
      end
      else
      begin
        // Arquivo novo
        FFileStates.Add(LFile, LLastModified);

        if ShouldProcessEvent(LFile) then
        begin
          LChangeInfo.FileName := LFile;
          LChangeInfo.ChangeType := fctCreated;
          LChangeInfo.Timestamp := LCurrentTime;
          LChangeInfo.OldFileName := '';

          NotifyFileChange(LChangeInfo);
          FLastEventTime.AddOrSetValue(LFile, LCurrentTime);
        end;
      end;
    end;

    // Verificar arquivos removidos
    var LKeysToRemove: TArray<string>;
    SetLength(LKeysToRemove, 0);

    for LFile in FFileStates.Keys do
    begin
      if not TFile.Exists(LFile) then
      begin
        SetLength(LKeysToRemove, Length(LKeysToRemove) + 1);
        LKeysToRemove[High(LKeysToRemove)] := LFile;

        if ShouldProcessEvent(LFile) then
        begin
          LChangeInfo.FileName := LFile;
          LChangeInfo.ChangeType := fctDeleted;
          LChangeInfo.Timestamp := LCurrentTime;
          LChangeInfo.OldFileName := '';

          NotifyFileChange(LChangeInfo);
        end;
      end;
    end;

    // Remover arquivos deletados do tracking
    for LFile in LKeysToRemove do
    begin
      FFileStates.Remove(LFile);
      FLastEventTime.Remove(LFile);
    end;

  finally
    FLock.Leave;
  end;
end;

function TNest4DFileWatcherThread.GetFileList: TArray<string>;
var
  LSearchOption: TSearchOption;
  LFilters: TArray<string>;
  LFilter: string;
  LFiles: TArray<string>;
  LAllFiles: TStringList;
  LFile: string;
begin
  LAllFiles := TStringList.Create;
  try
    if FConfig.EnableSubdirectories then
      LSearchOption := TSearchOption.soAllDirectories
    else
      LSearchOption := TSearchOption.soTopDirectoryOnly;

    // Dividir filtros por ;
    LFilters := FConfig.FileFilter.Split([';']);

    for LFilter in LFilters do
    begin
      if Trim(LFilter) = '' then
        Continue;

      try
        LFiles := TDirectory.GetFiles(FConfig.WatchPath, Trim(LFilter), LSearchOption);
        for LFile in LFiles do
          if LAllFiles.IndexOf(LFile) = -1 then
            LAllFiles.Add(LFile);
      except
        // Ignorar erros de filtro inv?lido
      end;
    end;

    SetLength(Result, LAllFiles.Count);
    for var I := 0 to LAllFiles.Count - 1 do
      Result[I] := LAllFiles[I];

  finally
    LAllFiles.Free;
  end;
end;

function TNest4DFileWatcherThread.ShouldProcessEvent(const AFileName: string): Boolean;
var
  LLastEvent: TDateTime;
begin
  Result := True;

  if FLastEventTime.TryGetValue(AFileName, LLastEvent) then
  begin
    // Aplicar debounce - evitar m?ltiplos eventos muito pr?ximos
    Result := MilliSecondsBetween(Now, LLastEvent) >= FConfig.DebounceTime;
  end;
end;

procedure TNest4DFileWatcherThread.NotifyFileChange(const AChangeInfo: TFileChangeInfo);
begin
  if Assigned(FOnFileChange) then
  begin
    // Executar no thread principal se necess?rio
    TThread.Synchronize(Self,
      procedure
      begin
        FOnFileChange(AChangeInfo);
      end);
  end;
end;

procedure TNest4DFileWatcherThread.AddWatchFile(const AFileName: string);
begin
  FLock.Enter;
  try
    if TFile.Exists(AFileName) then
      FFileStates.AddOrSetValue(AFileName, TFile.GetLastWriteTime(AFileName));
  finally
    FLock.Leave;
  end;
end;

procedure TNest4DFileWatcherThread.RemoveWatchFile(const AFileName: string);
begin
  FLock.Enter;
  try
    FFileStates.Remove(AFileName);
    FLastEventTime.Remove(AFileName);
  finally
    FLock.Leave;
  end;
end;

procedure TNest4DFileWatcherThread.ClearWatchFiles;
begin
  FLock.Enter;
  try
    FFileStates.Clear;
    FLastEventTime.Clear;
  finally
    FLock.Leave;
  end;
end;

{ TNest4DConfigHotReload }

constructor TNest4DConfigHotReload.Create(AConfigManager: TNest4DConfigManager);
begin
  inherited Create;
  FConfigManager := AConfigManager;
  FWatchedFiles := TStringList.Create;
  FLock := TCriticalSection.Create;
  FEnabled := False;
  FLastReloadTime := 0;
  FReloadCount := 0;
end;

destructor TNest4DConfigHotReload.Destroy;
begin
  Stop;
  FWatchedFiles.Free;
  FLock.Free;
  inherited;
end;

procedure TNest4DConfigHotReload.Start;
var
  LConfig: TFileWatcherConfig;
begin
  if FEnabled then
    Exit;

  LConfig := TFileWatcherConfig.Default;

  // Configurar path baseado no arquivo de configura??o atual
  if FConfigManager.ConfigFile <> '' then
    LConfig.WatchPath := TPath.GetDirectoryName(FConfigManager.ConfigFile)
  else
    LConfig.WatchPath := TPath.GetDirectoryName(ParamStr(0));

  FWatcherThread := TNest4DFileWatcherThread.Create(LConfig, HandleFileChange);

  // Adicionar arquivos j? monitorados
  FLock.Enter;
  try
    for var I := 0 to FWatchedFiles.Count - 1 do
      FWatcherThread.AddWatchFile(FWatchedFiles[I]);
  finally
    FLock.Leave;
  end;

  FEnabled := True;
end;

procedure TNest4DConfigHotReload.Stop;
begin
  if not FEnabled then
    Exit;

  FEnabled := False;

  if Assigned(FWatcherThread) then
  begin
    FWatcherThread.Terminate;
    FWatcherThread.WaitFor;
    FWatcherThread.Free;
    FWatcherThread := nil;
  end;
end;

procedure TNest4DConfigHotReload.Restart;
begin
  Stop;
  Start;
end;

procedure TNest4DConfigHotReload.HandleFileChange(const AChangeInfo: TFileChangeInfo);
begin
  if not FEnabled then
    Exit;

  // Verificar se ? um arquivo de configura??o v?lido
  if not IsConfigFile(AChangeInfo.FileName) then
    Exit;

  // Verificar se est? na lista de arquivos monitorados
  FLock.Enter;
  try
    if FWatchedFiles.IndexOf(AChangeInfo.FileName) = -1 then
      Exit;
  finally
    FLock.Leave;
  end;

  case AChangeInfo.ChangeType of
    fctModified, fctCreated:
      ReloadConfiguration(AChangeInfo.FileName);
    fctDeleted:
        begin
          // Arquivo deletado - notificar erro
          if Assigned(FOnConfigError) then
            FOnConfigError(Self, Format('Arquivo de configura??o deletado: %s', [AChangeInfo.FileName]));
        end;
  end;
end;

procedure TNest4DConfigHotReload.ReloadConfiguration(const AFileName: string);
begin
  try
    // Aguardar um pouco para garantir que o arquivo foi completamente escrito
    Sleep(100);

    // Tentar recarregar a configura??o
    if SameText(AFileName, FConfigManager.ConfigFile) then
    begin
      FConfigManager.LoadFromFile(AFileName, FConfigManager.LoadOptions);
      FLastReloadTime := Now;
      Inc(FReloadCount);

      if Assigned(FOnConfigChanged) then
        FOnConfigChanged(Self);
    end;

  except
    on E: Exception do
    begin
      if Assigned(FOnConfigError) then
        FOnConfigError(Self, Format('Erro ao recarregar configura??o de %s: %s', [AFileName, E.Message]));
    end;
  end;
end;

function TNest4DConfigHotReload.IsConfigFile(const AFileName: string): Boolean;
var
  LExt: string;
begin
  LExt := LowerCase(TPath.GetExtension(AFileName));
  Result := (LExt = '.json') or (LExt = '.yaml') or (LExt = '.yml') or
            (LExt = '.ini') or (LExt = '.conf');
end;

procedure TNest4DConfigHotReload.AddWatchFile(const AFileName: string);
begin
  FLock.Enter;
  try
    if FWatchedFiles.IndexOf(AFileName) = -1 then
    begin
      FWatchedFiles.Add(AFileName);

      if Assigned(FWatcherThread) then
        FWatcherThread.AddWatchFile(AFileName);
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TNest4DConfigHotReload.RemoveWatchFile(const AFileName: string);
var
  LIndex: Integer;
begin
  FLock.Enter;
  try
    LIndex := FWatchedFiles.IndexOf(AFileName);
    if LIndex >= 0 then
    begin
      FWatchedFiles.Delete(LIndex);

      if Assigned(FWatcherThread) then
        FWatcherThread.RemoveWatchFile(AFileName);
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TNest4DConfigHotReload.ClearWatchFiles;
begin
  FLock.Enter;
  try
    FWatchedFiles.Clear;

    if Assigned(FWatcherThread) then
      FWatcherThread.ClearWatchFiles;
  finally
    FLock.Leave;
  end;
end;

procedure TNest4DConfigHotReload.SetWatchPath(const APath: string);
begin
  if Assigned(FWatcherThread) then
    FWatcherThread.Config := TFileWatcherConfig.Default;
end;

procedure TNest4DConfigHotReload.SetPollingInterval(const AInterval: Integer);
begin
  if Assigned(FWatcherThread) then
  begin
    var LConfig := FWatcherThread.Config;
    LConfig.PollingInterval := AInterval;
    FWatcherThread.Config := LConfig;
  end;
end;

procedure TNest4DConfigHotReload.SetFileFilter(const AFilter: string);
begin
  if Assigned(FWatcherThread) then
  begin
    var LConfig := FWatcherThread.Config;
    LConfig.FileFilter := AFilter;
    FWatcherThread.Config := LConfig;
  end;
end;

{ TNest4DHotReloadUtils }

class function TNest4DHotReloadUtils.IsValidConfigFile(const AFileName: string): Boolean;
var
  LExt: string;
begin
  Result := False;

  if not TFile.Exists(AFileName) then
    Exit;

  LExt := LowerCase(TPath.GetExtension(AFileName));
  Result := (LExt = '.json') or (LExt = '.yaml') or (LExt = '.yml') or
            (LExt = '.ini') or (LExt = '.conf');

  if Result then
    Result := ValidateConfigFile(AFileName);
end;

class function TNest4DHotReloadUtils.GetConfigFiles(const APath: string; const ARecursive: Boolean): TArray<string>;
var
  LSearchOption: TSearchOption;
  LFiles: TStringList;
  LJsonFiles, LYamlFiles, LIniFiles: TArray<string>;
  LFile: string;
begin
  LFiles := TStringList.Create;
  try
    if ARecursive then
      LSearchOption := TSearchOption.soAllDirectories
    else
      LSearchOption := TSearchOption.soTopDirectoryOnly;

    // Buscar diferentes tipos de arquivo de configura??o
    LJsonFiles := TDirectory.GetFiles(APath, '*.json', LSearchOption);
    LYamlFiles := TDirectory.GetFiles(APath, '*.yaml', LSearchOption);
    LIniFiles := TDirectory.GetFiles(APath, '*.ini', LSearchOption);

    for LFile in LJsonFiles do
      LFiles.Add(LFile);
    for LFile in LYamlFiles do
      LFiles.Add(LFile);
    for LFile in LIniFiles do
      LFiles.Add(LFile);

    SetLength(Result, LFiles.Count);
    for var I := 0 to LFiles.Count - 1 do
      Result[I] := LFiles[I];

  finally
    LFiles.Free;
  end;
end;

class function TNest4DHotReloadUtils.IsFileRecentlyModified(const AFileName: string; const AThresholdSeconds: Integer): Boolean;
var
  LLastModified: TDateTime;
begin
  Result := False;

  if not TFile.Exists(AFileName) then
    Exit;

  LLastModified := TFile.GetLastWriteTime(AFileName);
  Result := SecondsBetween(Now, LLastModified) <= AThresholdSeconds;
end;

class function TNest4DHotReloadUtils.CreateConfigBackup(const AFileName: string): string;
var
  LBackupName: string;
  LTimestamp: string;
begin
  Result := '';

  if not TFile.Exists(AFileName) then
    Exit;

  LTimestamp := FormatDateTime('yyyymmdd_hhnnss', Now);
  LBackupName := TPath.ChangeExtension(AFileName, '.backup_' + LTimestamp + TPath.GetExtension(AFileName));

  try
    TFile.Copy(AFileName, LBackupName);
    Result := LBackupName;
  except
    // Falha ao criar backup
    Result := '';
  end;
end;

class function TNest4DHotReloadUtils.RestoreConfigBackup(const ABackupFileName: string): Boolean;
var
  LOriginalName: string;
  LRegex: TRegEx;
begin
  Result := False;

  if not TFile.Exists(ABackupFileName) then
    Exit;

  // Extrair nome original do backup
  LRegex := TRegEx.Create('\.backup_\d{8}_\d{6}');
  LOriginalName := LRegex.Replace(ABackupFileName, '');

  try
    TFile.Copy(ABackupFileName, LOriginalName, True);
    Result := True;
  except
    // Falha ao restaurar
    Result := False;
  end;
end;

class function TNest4DHotReloadUtils.ValidateConfigFile(const AFileName: string): Boolean;
var
  LContent: string;
  LJsonValue: TJSONValue;
begin
  Result := False;

  if not TFile.Exists(AFileName) then
    Exit;

  try
    LContent := TFile.ReadAllText(AFileName, TEncoding.UTF8);

    // Validar JSON
    if LowerCase(TPath.GetExtension(AFileName)) = '.json' then
    begin
      LJsonValue := TJSONObject.ParseJSONValue(LContent);
      try
        Result := Assigned(LJsonValue);
      finally
        LJsonValue.Free;
      end;
    end
    else
    begin
      // Para outros formatos, apenas verificar se n?o est? vazio
      Result := Trim(LContent) <> '';
    end;

  except
    Result := False;
  end;
end;

end.

