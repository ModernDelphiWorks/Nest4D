unit Nest4D.Config.Manager;

interface

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  System.IOUtils,
  System.JSON,
  Nest4D.Config,
  Nest4D.Config.Validator,
  Nest4D.Exception;

type
  // Eventos do gerenciador de configura??o
  TNest4DConfigChangeEvent = procedure(const AOldConfig, ANewConfig: TNest4DConfig) of object;
  TNest4DConfigErrorEvent = procedure(const AError: string; const AException: Exception) of object;
  TNest4DConfigValidationEvent = procedure(const AResults: TNest4DValidationResults) of object;

  // Tipos de ambiente
  TNest4DEnvironment = (envDevelopment, envProduction, envTesting, envStaging, envCustom);

  // Op??es de carregamento
  TNest4DLoadOptions = record
    ValidateOnLoad: Boolean;
    ThrowOnValidationError: Boolean;
    EnableHotReload: Boolean;
    AutoSave: Boolean;
    BackupOnChange: Boolean;

    class function Default: TNest4DLoadOptions; static;
    class function Production: TNest4DLoadOptions; static;
    class function Development: TNest4DLoadOptions; static;
  end;

  // Gerenciador principal de configura??o
  TNest4DConfigManager = class
  private
    class var FInstance: TNest4DConfigManager;
    class var FLock: TCriticalSection;
  private
    FConfig: TNest4DConfig;
    FConfigLock: TMultiReadExclusiveWriteSynchronizer;
    FValidator: TNest4DConfigValidator;
    FEnvironment: TNest4DEnvironment;
    FConfigFile: string;
    FLoadOptions: TNest4DLoadOptions;
    FLastModified: TDateTime;
    FHotReloadEnabled: Boolean;
    FFileWatcher: TThread;
    // Eventos
    FOnConfigChange: TNest4DConfigChangeEvent;
    FOnConfigError: TNest4DConfigErrorEvent;
    FOnValidation: TNest4DConfigValidationEvent;
    constructor Create;
    procedure InternalLoadConfig(const AFileName: string; const AOptions: TNest4DLoadOptions);
    procedure InternalValidateConfig(const AConfig: TNest4DConfig);
    procedure InternalBackupConfig;
    procedure DoConfigChange(const AOldConfig, ANewConfig: TNest4DConfig);
    procedure DoConfigError(const AError: string; const AException: Exception = nil);
    procedure DoValidation(const AResults: TNest4DValidationResults);
    function GetEnvironmentConfigFile(const AEnvironment: TNest4DEnvironment): string;
    function GetEnvironmentFromString(const AEnvStr: string): TNest4DEnvironment;
  public
    destructor Destroy; override;
    // Singleton
    class function Instance: TNest4DConfigManager;
    class procedure ReleaseInstance;
    // Configura??o atual (thread-safe)
    function GetConfig: TNest4DConfig;
    procedure SetConfig(const AConfig: TNest4DConfig; const AValidate: Boolean = True);
    // Carregamento e salvamento
    procedure LoadFromFile(const AFileName: string; const AOptions: TNest4DLoadOptions);
    procedure LoadFromEnvironment(const AEnvironment: TNest4DEnvironment; const AOptions: TNest4DLoadOptions);
    procedure LoadFromJSON(const AJSON: string; const AOptions: TNest4DLoadOptions);
    procedure LoadDefaults(const AEnvironment: TNest4DEnvironment; const AOptions: TNest4DLoadOptions);
    procedure SaveToFile(const AFileName: string = '');
    procedure SaveToJSON(out AJSON: string);
    // Valida??o
    function ValidateCurrentConfig: TNest4DValidationResults;
    function IsConfigValid: Boolean;
    // Hot reload
    procedure EnableHotReload(const AEnabled: Boolean = True);
    procedure CheckForConfigChanges;
    // Backup e restore
    procedure CreateBackup(const ABackupName: string = '');
    procedure RestoreFromBackup(const ABackupName: string);
    function ListBackups: TArray<string>;
    // Ambiente
    procedure SetEnvironment(const AEnvironment: TNest4DEnvironment);
    function GetEnvironment: TNest4DEnvironment;
    function GetEnvironmentName: string;
    // Configura??es espec?ficas (thread-safe)
    function GetLoggingConfig: TLoggingConfig;
    function GetMetricsConfig: TMetricsConfig;
    function GetHealthConfig: THealthConfig;
    function GetCacheConfig: TCacheConfig;
    function GetPoolConfig: THandlerPoolConfig;
    function GetResilienceConfig: TResilienceConfig;
    function GetFrameworkConfig: TFrameworkConfig;
    procedure UpdateLoggingConfig(const AConfig: TLoggingConfig);
    procedure UpdateMetricsConfig(const AConfig: TMetricsConfig);
    procedure UpdateHealthConfig(const AConfig: THealthConfig);
    procedure UpdateCacheConfig(const AConfig: TCacheConfig);
    procedure UpdatePoolConfig(const AConfig: THandlerPoolConfig);
    procedure UpdateResilienceConfig(const AConfig: TResilienceConfig);
    procedure UpdateFrameworkConfig(const AConfig: TFrameworkConfig);
    // Propriedades
    property Config: TNest4DConfig read GetConfig;
    property Environment: TNest4DEnvironment read GetEnvironment write SetEnvironment;
    property EnvironmentName: string read GetEnvironmentName;
    property ConfigFile: string read FConfigFile;
    property LoadOptions: TNest4DLoadOptions read FLoadOptions write FLoadOptions;
    property HotReloadEnabled: Boolean read FHotReloadEnabled;
    // Eventos
    property OnConfigChange: TNest4DConfigChangeEvent read FOnConfigChange write FOnConfigChange;
    property OnConfigError: TNest4DConfigErrorEvent read FOnConfigError write FOnConfigError;
    property OnValidation: TNest4DConfigValidationEvent read FOnValidation write FOnValidation;
  end;

  // Thread para monitoramento de arquivos (Hot Reload)
  TNest4DFileWatcherThread = class(TThread)
  private
    FConfigManager: TNest4DConfigManager;
    FFileName: string;
    FCheckInterval: Integer;

  protected
    procedure Execute; override;

  public
    constructor Create(const AConfigManager: TNest4DConfigManager; const AFileName: string;
      const ACheckInterval: Integer = 1000);
  end;

// Fun??es globais de conveni?ncia
function Nest4DConfig: TNest4DConfigManager;
function GetNest4DConfig: TNest4DConfig;
function GetLoggingConfig: TLoggingConfig;
function GetMetricsConfig: TMetricsConfig;
function GetHealthConfig: THealthConfig;
function GetCacheConfig: TCacheConfig;
function GetPoolConfig: THandlerPoolConfig;
function GetResilienceConfig: TResilienceConfig;
function GetFrameworkConfig: TFrameworkConfig;

// Fun??es de inicializa??o r?pida
procedure InitializeNest4DConfig(const AEnvironment: TNest4DEnvironment = envDevelopment);
procedure InitializeNest4DConfigFromFile(const AFileName: string);
procedure InitializeNest4DConfigFromJSON(const AJSON: string);

implementation

uses
  System.DateUtils;

{ TNest4DLoadOptions }

class function TNest4DLoadOptions.Default: TNest4DLoadOptions;
begin
  Result.ValidateOnLoad := True;
  Result.ThrowOnValidationError := False;
  Result.EnableHotReload := False;
  Result.AutoSave := False;
  Result.BackupOnChange := False;
end;

class function TNest4DLoadOptions.Production: TNest4DLoadOptions;
begin
  Result.ValidateOnLoad := True;
  Result.ThrowOnValidationError := True;
  Result.EnableHotReload := True;
  Result.AutoSave := False;
  Result.BackupOnChange := True;
end;

class function TNest4DLoadOptions.Development: TNest4DLoadOptions;
begin
  Result.ValidateOnLoad := True;
  Result.ThrowOnValidationError := False;
  Result.EnableHotReload := True;
  Result.AutoSave := True;
  Result.BackupOnChange := False;
end;

{ TNest4DConfigManager }

constructor TNest4DConfigManager.Create;
begin
  inherited;
  FConfigLock := TMultiReadExclusiveWriteSynchronizer.Create;
  FValidator := TNest4DConfigValidator.Instance;
  FEnvironment := envDevelopment;
  FLoadOptions := TNest4DLoadOptions.Default;
  FHotReloadEnabled := False;
  FFileWatcher := nil;

  // Criar configuração padrão
  FConfig := TNest4DConfig.Create;
  FConfig.LoadDefaults;
end;

destructor TNest4DConfigManager.Destroy;
begin
  EnableHotReload(False);

  if Assigned(FConfig) then
    FConfig.Free;

  FConfigLock.Free;
  inherited;
end;

class function TNest4DConfigManager.Instance: TNest4DConfigManager;
begin
  if not Assigned(FLock) then
    FLock := TCriticalSection.Create;

  FLock.Enter;
  try
    if not Assigned(FInstance) then
      FInstance := TNest4DConfigManager.Create;
    Result := FInstance;
  finally
    FLock.Leave;
  end;
end;

class procedure TNest4DConfigManager.ReleaseInstance;
begin
  if Assigned(FLock) then
  begin
    FLock.Enter;
    try
      if Assigned(FInstance) then
      begin
        FInstance.Free;
        FInstance := nil;
      end;
    finally
      FLock.Leave;
    end;

    FLock.Free;
    FLock := nil;
  end;
end;

function TNest4DConfigManager.GetConfig: TNest4DConfig;
begin
  FConfigLock.BeginRead;
  try
    Result := FConfig.Clone;
  finally
    FConfigLock.EndRead;
  end;
end;

procedure TNest4DConfigManager.SetConfig(const AConfig: TNest4DConfig; const AValidate: Boolean);
var
  LOldConfig: TNest4DConfig;
  LResults: TNest4DValidationResults;
begin
  if not Assigned(AConfig) then
    raise ENest4DConfigException.Create('Configura??o n?o pode ser nula');

  // Validar se solicitado
  if AValidate then
  begin
    LResults := FValidator.ValidateConfig(AConfig);
    try
      DoValidation(LResults);

      if FLoadOptions.ThrowOnValidationError and FValidator.HasErrors(LResults) then
        raise ENest4DConfigException.Create('Configura??o cont?m erros de valida??o');
    finally
      LResults.Free;
    end;
  end;

  FConfigLock.BeginWrite;
  try
    LOldConfig := FConfig.Clone;
    try
      FConfig.Free;
      FConfig := AConfig.Clone;

      // Backup se solicitado
      if FLoadOptions.BackupOnChange then
        InternalBackupConfig;

      // Auto save se solicitado
      if FLoadOptions.AutoSave and (FConfigFile <> '') then
        SaveToFile;

      // Disparar evento
      DoConfigChange(LOldConfig, FConfig);
    finally
      LOldConfig.Free;
    end;
  finally
    FConfigLock.EndWrite;
  end;
end;

procedure TNest4DConfigManager.LoadFromFile(const AFileName: string; const AOptions: TNest4DLoadOptions);
begin
  if not TFile.Exists(AFileName) then
    raise ENest4DConfigException.CreateFmt('Arquivo de configura??o n?o encontrado: %s', [AFileName]);

  FConfigFile := AFileName;
  FLoadOptions := AOptions;
  FLastModified := TFile.GetLastWriteTime(AFileName);

  InternalLoadConfig(AFileName, AOptions);

  // Habilitar hot reload se solicitado
  if AOptions.EnableHotReload then
    EnableHotReload(True);
end;

procedure TNest4DConfigManager.LoadFromEnvironment(const AEnvironment: TNest4DEnvironment; const AOptions: TNest4DLoadOptions);
var
  LFileName: string;
begin
  FEnvironment := AEnvironment;
  LFileName := GetEnvironmentConfigFile(AEnvironment);

  if TFile.Exists(LFileName) then
    LoadFromFile(LFileName, AOptions)
  else
    LoadDefaults(AEnvironment, AOptions);
end;

procedure TNest4DConfigManager.LoadFromJSON(const AJSON: string; const AOptions: TNest4DLoadOptions);
var
  LConfig: TNest4DConfig;
begin
  LConfig := TNest4DConfig.Create;
  try
    LConfig.LoadFromJSON(AJSON);

    FLoadOptions := AOptions;
    SetConfig(LConfig, AOptions.ValidateOnLoad);
  finally
    LConfig.Free;
  end;
end;

procedure TNest4DConfigManager.LoadDefaults(const AEnvironment: TNest4DEnvironment; const AOptions: TNest4DLoadOptions);
var
  LConfig: TNest4DConfig;
begin
  LConfig := TNest4DConfig.Create;
  try
    LConfig.LoadDefaults;

    FEnvironment := AEnvironment;
    FLoadOptions := AOptions;
    SetConfig(LConfig, AOptions.ValidateOnLoad);
  finally
    LConfig.Free;
  end;
end;

procedure TNest4DConfigManager.SaveToFile(const AFileName: string);
var
  LFileName: string;
  LJSON: string;
begin
  LFileName := AFileName;
  if LFileName = '' then
    LFileName := FConfigFile;

  if LFileName = '' then
    raise ENest4DConfigException.Create('Nome do arquivo n?o especificado');

  FConfigLock.BeginRead;
  try
    FConfig.SaveToJSON(LJSON);
  finally
    FConfigLock.EndRead;
  end;

  TFile.WriteAllText(LFileName, LJSON, TEncoding.UTF8);
  FLastModified := TFile.GetLastWriteTime(LFileName);
end;

procedure TNest4DConfigManager.SaveToJSON(out AJSON: string);
begin
  FConfigLock.BeginRead;
  try
    FConfig.SaveToJSON(AJSON);
  finally
    FConfigLock.EndRead;
  end;
end;

function TNest4DConfigManager.ValidateCurrentConfig: TNest4DValidationResults;
begin
  FConfigLock.BeginRead;
  try
    Result := FValidator.ValidateConfig(FConfig);
  finally
    FConfigLock.EndRead;
  end;
end;

function TNest4DConfigManager.IsConfigValid: Boolean;
var
  LResults: TNest4DValidationResults;
begin
  LResults := ValidateCurrentConfig;
  try
    Result := not FValidator.HasErrors(LResults);
  finally
    LResults.Free;
  end;
end;

procedure TNest4DConfigManager.EnableHotReload(const AEnabled: Boolean);
begin
  if AEnabled = FHotReloadEnabled then
    Exit;

  if AEnabled then
  begin
    if (FConfigFile <> '') and TFile.Exists(FConfigFile) then
    begin
      FFileWatcher := TNest4DFileWatcherThread.Create(Self, FConfigFile);
      FHotReloadEnabled := True;
    end;
  end
  else
  begin
    if Assigned(FFileWatcher) then
    begin
      FFileWatcher.Terminate;
      FFileWatcher.WaitFor;
      FFileWatcher.Free;
      FFileWatcher := nil;
    end;
    FHotReloadEnabled := False;
  end;
end;

procedure TNest4DConfigManager.CheckForConfigChanges;
var
  LCurrentModified: TDateTime;
begin
  if (FConfigFile = '') or not TFile.Exists(FConfigFile) then
    Exit;

  LCurrentModified := TFile.GetLastWriteTime(FConfigFile);
  if LCurrentModified > FLastModified then
  begin
    try
      InternalLoadConfig(FConfigFile, FLoadOptions);
      FLastModified := LCurrentModified;
    except
      on E: Exception do
        DoConfigError('Erro ao recarregar configura??o', E);
    end;
  end;
end;

procedure TNest4DConfigManager.CreateBackup(const ABackupName: string);
var
  LBackupName: string;
  LBackupDir: string;
  LBackupFile: string;
begin
  LBackupName := ABackupName;
  if LBackupName = '' then
    LBackupName := FormatDateTime('yyyymmdd_hhnnss', Now);

  LBackupDir := TPath.Combine(TPath.GetDirectoryName(FConfigFile), 'backups');
  if not TDirectory.Exists(LBackupDir) then
    TDirectory.CreateDirectory(LBackupDir);

  LBackupFile := TPath.Combine(LBackupDir, Format('config_%s.json', [LBackupName]));
  SaveToFile(LBackupFile);
end;

procedure TNest4DConfigManager.RestoreFromBackup(const ABackupName: string);
var
  LBackupDir: string;
  LBackupFile: string;
begin
  LBackupDir := TPath.Combine(TPath.GetDirectoryName(FConfigFile), 'backups');
  LBackupFile := TPath.Combine(LBackupDir, Format('config_%s.json', [ABackupName]));

  if not TFile.Exists(LBackupFile) then
    raise ENest4DConfigException.CreateFmt('Backup n?o encontrado: %s', [ABackupName]);

  LoadFromFile(LBackupFile, FLoadOptions);
end;

function TNest4DConfigManager.ListBackups: TArray<string>;
var
  LBackupDir: string;
  LFiles: TArray<string>;
  LFile: string;
  LBackupName: string;
  LBackups: TArray<string>;
  I: Integer;
begin
  LBackupDir := TPath.Combine(TPath.GetDirectoryName(FConfigFile), 'backups');
  if not TDirectory.Exists(LBackupDir) then
  begin
    SetLength(Result, 0);
    Exit;
  end;

  LFiles := TDirectory.GetFiles(LBackupDir, 'config_*.json');
  SetLength(LBackups, Length(LFiles));

  for I := 0 to High(LFiles) do
  begin
    LFile := TPath.GetFileNameWithoutExtension(LFiles[I]);
    LBackupName := Copy(LFile, 8, Length(LFile) - 7); // Remove 'config_'
    LBackups[I] := LBackupName;
  end;

  Result := LBackups;
end;

procedure TNest4DConfigManager.SetEnvironment(const AEnvironment: TNest4DEnvironment);
begin
  if AEnvironment <> FEnvironment then
  begin
    FEnvironment := AEnvironment;
    LoadFromEnvironment(AEnvironment, FLoadOptions);
  end;
end;

function TNest4DConfigManager.GetEnvironment: TNest4DEnvironment;
begin
  Result := FEnvironment;
end;

function TNest4DConfigManager.GetEnvironmentName: string;
begin
  case FEnvironment of
    envDevelopment: Result := 'Development';
    envProduction: Result := 'Production';
    envTesting: Result := 'Testing';
    envStaging: Result := 'Staging';
    envCustom: Result := 'Custom';
  else
    Result := 'Unknown';
  end;
end;

// M?todos para configura??es espec?ficas
function TNest4DConfigManager.GetLoggingConfig: TLoggingConfig;
begin
  FConfigLock.BeginRead;
  try
    Result := FConfig.Logging;
  finally
    FConfigLock.EndRead;
  end;
end;

function TNest4DConfigManager.GetMetricsConfig: TMetricsConfig;
begin
  FConfigLock.BeginRead;
  try
    Result := FConfig.Metrics;
  finally
    FConfigLock.EndRead;
  end;
end;

function TNest4DConfigManager.GetHealthConfig: THealthConfig;
begin
  FConfigLock.BeginRead;
  try
    Result := FConfig.Health;
  finally
    FConfigLock.EndRead;
  end;
end;

function TNest4DConfigManager.GetCacheConfig: TCacheConfig;
begin
  FConfigLock.BeginRead;
  try
    Result := FConfig.Cache;
  finally
    FConfigLock.EndRead;
  end;
end;

function TNest4DConfigManager.GetPoolConfig: THandlerPoolConfig;
begin
  FConfigLock.BeginRead;
  try
    Result := FConfig.HandlerPool;
  finally
    FConfigLock.EndRead;
  end;
end;

function TNest4DConfigManager.GetResilienceConfig: TResilienceConfig;
begin
  FConfigLock.BeginRead;
  try
    Result := FConfig.Resilience;
  finally
    FConfigLock.EndRead;
  end;
end;

function TNest4DConfigManager.GetFrameworkConfig: TFrameworkConfig;
begin
  FConfigLock.BeginRead;
  try
    Result := FConfig.Framework;
  finally
    FConfigLock.EndRead;
  end;
end;

procedure TNest4DConfigManager.UpdateLoggingConfig(const AConfig: TLoggingConfig);
var
  LNewConfig: TNest4DConfig;
begin
  LNewConfig := GetConfig;
  try
    LNewConfig.Logging := AConfig;
    SetConfig(LNewConfig, True);
  finally
    LNewConfig.Free;
  end;
end;

procedure TNest4DConfigManager.UpdateMetricsConfig(const AConfig: TMetricsConfig);
var
  LNewConfig: TNest4DConfig;
begin
  LNewConfig := GetConfig;
  try
    LNewConfig.Metrics := AConfig;
    SetConfig(LNewConfig, True);
  finally
    LNewConfig.Free;
  end;
end;

procedure TNest4DConfigManager.UpdateHealthConfig(const AConfig: THealthConfig);
var
  LNewConfig: TNest4DConfig;
begin
  LNewConfig := GetConfig;
  try
    LNewConfig.Health := AConfig;
    SetConfig(LNewConfig, True);
  finally
    LNewConfig.Free;
  end;
end;

procedure TNest4DConfigManager.UpdateCacheConfig(const AConfig: TCacheConfig);
var
  LNewConfig: TNest4DConfig;
begin
  LNewConfig := GetConfig;
  try
    LNewConfig.Cache := AConfig;
    SetConfig(LNewConfig, True);
  finally
    LNewConfig.Free;
  end;
end;

procedure TNest4DConfigManager.UpdatePoolConfig(const AConfig: THandlerPoolConfig);
var
  LNewConfig: TNest4DConfig;
begin
  LNewConfig := GetConfig;
  try
    LNewConfig.HandlerPool := AConfig;
    SetConfig(LNewConfig, True);
  finally
    LNewConfig.Free;
  end;
end;

procedure TNest4DConfigManager.UpdateResilienceConfig(const AConfig: TResilienceConfig);
var
  LNewConfig: TNest4DConfig;
begin
  LNewConfig := GetConfig;
  try
    LNewConfig.Resilience := AConfig;
    SetConfig(LNewConfig, True);
  finally
    LNewConfig.Free;
  end;
end;

procedure TNest4DConfigManager.UpdateFrameworkConfig(const AConfig: TFrameworkConfig);
var
  LNewConfig: TNest4DConfig;
begin
  LNewConfig := GetConfig;
  try
    LNewConfig.Framework := AConfig;
    SetConfig(LNewConfig, True);
  finally
    LNewConfig.Free;
  end;
end;

// M?todos internos
procedure TNest4DConfigManager.InternalLoadConfig(const AFileName: string; const AOptions: TNest4DLoadOptions);
var
  LConfig: TNest4DConfig;
begin
  LConfig := TNest4DConfig.Create;
  try
    LConfig.LoadFromFile(AFileName);
    SetConfig(LConfig, AOptions.ValidateOnLoad);
  finally
    LConfig.Free;
  end;
end;

procedure TNest4DConfigManager.InternalValidateConfig(const AConfig: TNest4DConfig);
var
  LResults: TNest4DValidationResults;
begin
  LResults := FValidator.ValidateConfig(AConfig);
  try
    DoValidation(LResults);
  finally
    LResults.Free;
  end;
end;

procedure TNest4DConfigManager.InternalBackupConfig;
begin
  try
    CreateBackup;
  except
    on E: Exception do
      DoConfigError('Erro ao criar backup', E);
  end;
end;

procedure TNest4DConfigManager.DoConfigChange(const AOldConfig, ANewConfig: TNest4DConfig);
begin
  if Assigned(FOnConfigChange) then
  begin
    try
      FOnConfigChange(AOldConfig, ANewConfig);
    except
      on E: Exception do
        DoConfigError('Erro no evento OnConfigChange', E);
    end;
  end;
end;

procedure TNest4DConfigManager.DoConfigError(const AError: string; const AException: Exception);
begin
  if Assigned(FOnConfigError) then
  begin
    try
      FOnConfigError(AError, AException);
    except
      // Evitar loop infinito de erros
    end;
  end;
end;

procedure TNest4DConfigManager.DoValidation(const AResults: TNest4DValidationResults);
begin
  if Assigned(FOnValidation) then
  begin
    try
      FOnValidation(AResults);
    except
      on E: Exception do
        DoConfigError('Erro no evento OnValidation', E);
    end;
  end;
end;

function TNest4DConfigManager.GetEnvironmentConfigFile(const AEnvironment: TNest4DEnvironment): string;
var
  LEnvName: string;
begin
  case AEnvironment of
    envDevelopment: LEnvName := 'development';
    envProduction: LEnvName := 'production';
    envTesting: LEnvName := 'testing';
    envStaging: LEnvName := 'staging';
  else
    LEnvName := 'custom';
  end;

  Result := Format('config/Nest4D.%s.json', [LEnvName]);
end;

function TNest4DConfigManager.GetEnvironmentFromString(const AEnvStr: string): TNest4DEnvironment;
var
  LEnvStr: string;
begin
  LEnvStr := LowerCase(Trim(AEnvStr));

  if LEnvStr = 'development' then
    Result := envDevelopment
  else if LEnvStr = 'production' then
    Result := envProduction
  else if LEnvStr = 'testing' then
    Result := envTesting
  else if LEnvStr = 'staging' then
    Result := envStaging
  else
    Result := envCustom;
end;

{ TNest4DFileWatcherThread }

constructor TNest4DFileWatcherThread.Create(const AConfigManager: TNest4DConfigManager;
  const AFileName: string; const ACheckInterval: Integer);
begin
  inherited Create(False);
  FConfigManager := AConfigManager;
  FFileName := AFileName;
  FCheckInterval := ACheckInterval;
  FreeOnTerminate := False;
end;

procedure TNest4DFileWatcherThread.Execute;
begin
  while not Terminated do
  begin
    try
      if Assigned(FConfigManager) then
        FConfigManager.CheckForConfigChanges;
    except
      // Ignorar erros para n?o parar o monitoramento
    end;

    Sleep(FCheckInterval);
  end;
end;

{ Fun??es globais }

function Nest4DConfig: TNest4DConfigManager;
begin
  Result := TNest4DConfigManager.Instance;
end;

function GetNest4DConfig: TNest4DConfig;
begin
  Result := TNest4DConfigManager.Instance.GetConfig;
end;

function GetLoggingConfig: TLoggingConfig;
begin
  Result := TNest4DConfigManager.Instance.GetLoggingConfig;
end;

function GetMetricsConfig: TMetricsConfig;
begin
  Result := TNest4DConfigManager.Instance.GetMetricsConfig;
end;

function GetHealthConfig: THealthConfig;
begin
  Result := TNest4DConfigManager.Instance.GetHealthConfig;
end;

function GetCacheConfig: TCacheConfig;
begin
  Result := TNest4DConfigManager.Instance.GetCacheConfig;
end;

function GetPoolConfig: THandlerPoolConfig;
begin
  Result := TNest4DConfigManager.Instance.GetPoolConfig;
end;

function GetResilienceConfig: TResilienceConfig;
begin
  Result := TNest4DConfigManager.Instance.GetResilienceConfig;
end;

function GetFrameworkConfig: TFrameworkConfig;
begin
  Result := TNest4DConfigManager.Instance.GetFrameworkConfig;
end;

{ Fun??es de inicializa??o }

procedure InitializeNest4DConfig(const AEnvironment: TNest4DEnvironment);
begin
  TNest4DConfigManager.Instance.LoadDefaults(AEnvironment, TNest4DLoadOptions.Default);
end;

procedure InitializeNest4DConfigFromFile(const AFileName: string);
begin
  TNest4DConfigManager.Instance.LoadFromFile(AFileName, TNest4DLoadOptions.Default);
end;

procedure InitializeNest4DConfigFromJSON(const AJSON: string);
begin
  TNest4DConfigManager.Instance.LoadFromJSON(AJSON, TNest4DLoadOptions.Default);
end;

initialization
  // Inicializar com configura??es padr?o de desenvolvimento
  InitializeNest4DConfig(envDevelopment);

finalization
  TNest4DConfigManager.ReleaseInstance;

end.

