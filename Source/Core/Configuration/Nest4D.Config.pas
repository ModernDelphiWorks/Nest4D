unit Nest4D.Config;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  System.JSON,
  System.IOUtils,
  System.StrUtils,
  System.Variants,
  Nest4D.Logging;

type
  // Tipos de ambiente
  TNest4DEnvironment = (neUnknown, neDevelopment, neStaging, neProduction, neTest);
  
  // Opções de carregamento de configuração
  TNest4DLoadOptions = set of (loIgnoreErrors, loValidateSchema, loMergeDefaults, loOverrideExisting);
  
  // Tipos de configuração
  TConfigValueType = (cvtString, cvtInteger, cvtFloat, cvtBoolean, cvtObject, cvtArray);
  
  // Prioridade de configuração
  TConfigPriority = (cpLow, cpNormal, cpHigh, cpCritical);
  
  // Status de configuração
  TConfigStatus = (csUnloaded, csLoading, csLoaded, csError, csValidated);

  // Interface para valor de configuração
  IConfigValue = interface
    ['{A1B2C3D4-E5F6-7890-ABCD-EF1234567890}']
    function GetName: String;
    function GetValue: Variant;
    function GetValueType: TConfigValueType;
    function GetDefaultValue: Variant;
    function GetDescription: String;
    function GetIsRequired: Boolean;
    function GetIsSecret: Boolean;
    
    procedure SetValue(const AValue: Variant);
    procedure SetDefaultValue(const AValue: Variant);
    procedure SetDescription(const ADescription: String);
    procedure SetIsRequired(ARequired: Boolean);
    procedure SetIsSecret(ASecret: Boolean);
    
    function AsString: String;
    function AsInteger: Integer;
    function AsFloat: Double;
    function AsBoolean: Boolean;
    function AsObject: TJSONObject;
    function AsArray: TJSONArray;
    
    function IsEmpty: Boolean;
    function HasValue: Boolean;
    function Validate: Boolean;
    
    property Name: String read GetName;
    property Value: Variant read GetValue write SetValue;
    property ValueType: TConfigValueType read GetValueType;
    property DefaultValue: Variant read GetDefaultValue write SetDefaultValue;
    property Description: String read GetDescription write SetDescription;
    property IsRequired: Boolean read GetIsRequired write SetIsRequired;
    property IsSecret: Boolean read GetIsSecret write SetIsSecret;
  end;

  // Interface para seção de configuração
  IConfigSection = interface
    ['{B2C3D4E5-F6A7-8901-BCDE-F23456789012}']
    function GetName: String;
    function GetParent: IConfigSection;
    function GetValues: TArray<IConfigValue>;
    function GetSections: TArray<IConfigSection>;
    function GetFullPath: String;
    
    function GetValue(const AName: String): IConfigValue;
    function GetSection(const AName: String): IConfigSection;
    function HasValue(const AName: String): Boolean;
    function HasSection(const AName: String): Boolean;
    
    procedure AddValue(const AValue: IConfigValue);
    procedure AddSection(const ASection: IConfigSection);
    procedure RemoveValue(const AName: String);
    procedure RemoveSection(const AName: String);
    
    function GetValueAsString(const AName: String; const ADefault: String = ''): String;
    function GetValueAsInteger(const AName: String; const ADefault: Integer = 0): Integer;
    function GetValueAsFloat(const AName: String; const ADefault: Double = 0.0): Double;
    function GetValueAsBoolean(const AName: String; const ADefault: Boolean = False): Boolean;
    
    procedure SetValue(const AName: String; const AValue: Variant);
    
    property Name: String read GetName;
    property Parent: IConfigSection read GetParent;
    property Values: TArray<IConfigValue> read GetValues;
    property Sections: TArray<IConfigSection> read GetSections;
    property FullPath: String read GetFullPath;
  end;

  // Interface para configuração
  IConfiguration = interface
    ['{C3D4E5F6-A7B8-9012-CDEF-345678901234}']
    function GetEnvironment: TNest4DEnvironment;
    function GetStatus: TConfigStatus;
    function GetRootSection: IConfigSection;
    function GetLoadOptions: TNest4DLoadOptions;
    function GetConfigFile: String;
    function GetLastModified: TDateTime;
    function GetVersion: String;
    
    procedure SetEnvironment(AEnvironment: TNest4DEnvironment);
    procedure SetLoadOptions(AOptions: TNest4DLoadOptions);
    
    function LoadFromFile(const AFileName: String): Boolean;
    function LoadFromString(const AConfigData: String): Boolean;
    function LoadFromJSON(const AJSONObject: TJSONObject): Boolean;
    function SaveToFile(const AFileName: String): Boolean;
    function SaveToString: String;
    function SaveToJSON: TJSONObject;
    
    function GetSection(const APath: String): IConfigSection;
    function GetValue(const APath: String): IConfigValue;
    function HasSection(const APath: String): Boolean;
    function HasValue(const APath: String): Boolean;
    
    function GetString(const APath: String; const ADefault: String = ''): String;
    function GetInteger(const APath: String; const ADefault: Integer = 0): Integer;
    function GetFloat(const APath: String; const ADefault: Double = 0.0): Double;
    function GetBoolean(const APath: String; const ADefault: Boolean = False): Boolean;
    
    procedure SetString(const APath: String; const AValue: String);
    procedure SetInteger(const APath: String; const AValue: Integer);
    procedure SetFloat(const APath: String; const AValue: Double);
    procedure SetBoolean(const APath: String; const AValue: Boolean);
    
    function Validate: Boolean;
    function Reload: Boolean;
    procedure Clear;
    function Clone: IConfiguration;
    function Merge(const AOther: IConfiguration): Boolean;
    
    property Environment: TNest4DEnvironment read GetEnvironment write SetEnvironment;
    property Status: TConfigStatus read GetStatus;
    property RootSection: IConfigSection read GetRootSection;
    property LoadOptions: TNest4DLoadOptions read GetLoadOptions write SetLoadOptions;
    property ConfigFile: String read GetConfigFile;
    property LastModified: TDateTime read GetLastModified;
    property Version: String read GetVersion;
  end;

  // Interface para provedor de configuração
  IConfigProvider = interface
    ['{D4E5F6A7-B8C9-0123-DEF0-456789012345}']
    function GetName: String;
    function GetPriority: TConfigPriority;
    function GetIsEnabled: Boolean;
    
    procedure SetPriority(APriority: TConfigPriority);
    procedure SetEnabled(AEnabled: Boolean);
    
    function CanLoad(const ASource: String): Boolean;
    function Load(const ASource: String): IConfiguration;
    function Save(const AConfiguration: IConfiguration; const ADestination: String): Boolean;
    
    property Name: String read GetName;
    property Priority: TConfigPriority read GetPriority write SetPriority;
    property IsEnabled: Boolean read GetIsEnabled write SetEnabled;
  end;

  // Interface para gerenciador de configuração
  IConfigManager = interface
    ['{E5F6A7B8-C9D0-1234-EF01-567890123456}']
    function GetCurrentConfiguration: IConfiguration;
    function GetProviders: TArray<IConfigProvider>;
    function GetEnvironment: TNest4DEnvironment;
    
    procedure SetEnvironment(AEnvironment: TNest4DEnvironment);
    
    procedure RegisterProvider(const AProvider: IConfigProvider);
    procedure UnregisterProvider(const AProviderName: String);
    function GetProvider(const AProviderName: String): IConfigProvider;
    
    function LoadConfiguration(const ASource: String; AOptions: TNest4DLoadOptions = []): IConfiguration;
    function SaveConfiguration(const AConfiguration: IConfiguration; const ADestination: String): Boolean;
    
    procedure SetCurrentConfiguration(const AConfiguration: IConfiguration);
    function ReloadCurrentConfiguration: Boolean;
    
    function GetString(const APath: String; const ADefault: String = ''): String;
    function GetInteger(const APath: String; const ADefault: Integer = 0): Integer;
    function GetFloat(const APath: String; const ADefault: Double = 0.0): Double;
    function GetBoolean(const APath: String; const ADefault: Boolean = False): Boolean;
    
    procedure SetString(const APath: String; const AValue: String);
    procedure SetInteger(const APath: String; const AValue: Integer);
    procedure SetFloat(const APath: String; const AValue: Double);
    procedure SetBoolean(const APath: String; const AValue: Boolean);
    
    property CurrentConfiguration: IConfiguration read GetCurrentConfiguration;
    property Providers: TArray<IConfigProvider> read GetProviders;
    property Environment: TNest4DEnvironment read GetEnvironment write SetEnvironment;
  end;

  // Configurações específicas para Logging
  ILoggingConfig = interface
    ['{F6A7B8C9-D0E1-2345-F012-678901234567}']
    function GetLevel: String;
    function GetOutputPath: String;
    function GetMaxFileSize: Integer;
    function GetMaxFiles: Integer;
    function GetFormat: String;
    function GetIsEnabled: Boolean;
    function GetIsAsyncEnabled: Boolean;
    function GetBatchSize: Integer;
    function GetFlushInterval: Integer;
    
    procedure SetLevel(const ALevel: String);
    procedure SetOutputPath(const APath: String);
    procedure SetMaxFileSize(ASize: Integer);
    procedure SetMaxFiles(ACount: Integer);
    procedure SetFormat(const AFormat: String);
    procedure SetEnabled(AEnabled: Boolean);
    procedure SetAsyncEnabled(AEnabled: Boolean);
    procedure SetBatchSize(ASize: Integer);
    procedure SetFlushInterval(AInterval: Integer);
    
    property Level: String read GetLevel write SetLevel;
    property OutputPath: String read GetOutputPath write SetOutputPath;
    property MaxFileSize: Integer read GetMaxFileSize write SetMaxFileSize;
    property MaxFiles: Integer read GetMaxFiles write SetMaxFiles;
    property Format: String read GetFormat write SetFormat;
    property IsEnabled: Boolean read GetIsEnabled write SetEnabled;
    property IsAsyncEnabled: Boolean read GetIsAsyncEnabled write SetAsyncEnabled;
    property BatchSize: Integer read GetBatchSize write SetBatchSize;
    property FlushInterval: Integer read GetFlushInterval write SetFlushInterval;
  end;

  // Configurações específicas para Metrics
  IMetricsConfig = interface
    ['{A7B8C9D0-E1F2-3456-0123-789012345678}']
    function GetIsEnabled: Boolean;
    function GetCollectionInterval: Integer;
    function GetExportInterval: Integer;
    function GetMaxMetrics: Integer;
    function GetExportFormat: String;
    function GetExportPath: String;
    function GetIsAsyncEnabled: Boolean;
    function GetBatchSize: Integer;
    
    procedure SetEnabled(AEnabled: Boolean);
    procedure SetCollectionInterval(AInterval: Integer);
    procedure SetExportInterval(AInterval: Integer);
    procedure SetMaxMetrics(AMax: Integer);
    procedure SetExportFormat(const AFormat: String);
    procedure SetExportPath(const APath: String);
    procedure SetAsyncEnabled(AEnabled: Boolean);
    procedure SetBatchSize(ASize: Integer);
    
    property IsEnabled: Boolean read GetIsEnabled write SetEnabled;
    property CollectionInterval: Integer read GetCollectionInterval write SetCollectionInterval;
    property ExportInterval: Integer read GetExportInterval write SetExportInterval;
    property MaxMetrics: Integer read GetMaxMetrics write SetMaxMetrics;
    property ExportFormat: String read GetExportFormat write SetExportFormat;
    property ExportPath: String read GetExportPath write SetExportPath;
    property IsAsyncEnabled: Boolean read GetIsAsyncEnabled write SetAsyncEnabled;
    property BatchSize: Integer read GetBatchSize write SetBatchSize;
  end;

  // Classes de configuração específicas
  TLoggingConfig = class
  public
    Enabled: Boolean;
    Level: TLogLevel;
    EnableConsole: Boolean;
    EnableFile: Boolean;
    EnableDatabase: Boolean;
    FilePath: string;
    LogToFile: Boolean;
    LogToConsole: Boolean;
    LogFileName: string;
    MaxFileSize: Integer;
    MaxBackupFiles: Integer;
    Format: string;
    
    constructor Create;
  end;

  TMetricsConfig = class
  public
    Enabled: Boolean;
    Port: Integer;
    Endpoint: string;
    MetricsInterval: Integer;
    MaxMetricsHistory: Integer;
    EnablePrometheus: Boolean;
    PrometheusEndpoint: string;
    
    constructor Create;
  end;

  THealthConfig = class
  public
    Enabled: Boolean;
    Port: Integer;
    Endpoint: string;
    CheckInterval: Integer;
    Timeout: Integer;
    EnableDatabaseCheck: Boolean;
    DatabaseConnectionString: string;
    
    constructor Create;
  end;

  TCacheConfig = class
  public
    EnableTTL: Boolean;
    DefaultTTL: Integer;
    MaxCacheSize: Integer;
    EnableBackgroundCleanup: Boolean;
    CleanupInterval: Integer;
    EnableCompression: Boolean;
    CompressionThreshold: Integer;
    EnableDistributedCache: Boolean;
    RedisConnectionString: string;
    
    constructor Create;
  end;

  THandlerPoolConfig = class
  public
    MinSize: Integer;
    MaxSize: Integer;
    GrowthFactor: Double;
    ShrinkFactor: Double;
    IdleTimeout: Integer;
    MinTimeoutThreshold: Integer;
    MaxTimeoutThreshold: Integer;
    
    constructor Create;
  end;

  TResilienceConfig = class
  public
    Enabled: Boolean;
    Endpoint: string;
    EnableRetry: Boolean;
    MaxRetryAttempts: Integer;
    BaseDelay: Integer;
    MaxDelay: Integer;
    EnableCircuitBreaker: Boolean;
    FailureThreshold: Integer;
    SuccessThreshold: Integer;
    Timeout: Integer;
    EnableBulkhead: Boolean;
    MaxConcurrentCalls: Integer;
    MaxWaitTime: Integer;
    
    constructor Create;
  end;

  TFrameworkConfig = class
  public
    Port: Integer;
    Host: string;
    MaxConnections: Integer;
    Timeout: Integer;
    EnableCompression: Boolean;
    CompressionLevel: Integer;
    EnableSSL: Boolean;
    SSLCertFile: string;
    SSLKeyFile: string;
    SlowRequestThresholdMs: Integer;
    
    constructor Create;
  end;

  // Classe principal de configuração do Nest4D
  TNest4DConfig = class
  private
    FLogging: TLoggingConfig;
    FMetrics: TMetricsConfig;
    FHealth: THealthConfig;
    FCache: TCacheConfig;
    FHandlerPool: THandlerPoolConfig;
    FResilience: TResilienceConfig;
    FFramework: TFrameworkConfig;
  public
    constructor Create;
    destructor Destroy; override;
    
    // Métodos de carregamento e salvamento
    procedure LoadDefaults;
    procedure LoadFromFile(const AFileName: string);
    procedure LoadFromJSON(const AJSON: string);
    procedure SaveToJSON(out AJSON: string);
    function Clone: TNest4DConfig;
    
    property Logging: TLoggingConfig read FLogging write FLogging;
    property Metrics: TMetricsConfig read FMetrics write FMetrics;
    property Health: THealthConfig read FHealth write FHealth;
    property Cache: TCacheConfig read FCache write FCache;
    property HandlerPool: THandlerPoolConfig read FHandlerPool write FHandlerPool;
    property Resilience: TResilienceConfig read FResilience write FResilience;
    property Framework: TFrameworkConfig read FFramework write FFramework;
  end;

  // Implementação básica do valor de configuração
  TConfigValue = class(TInterfacedObject, IConfigValue)
  private
    FName: String;
    FValue: Variant;
    FValueType: TConfigValueType;
    FDefaultValue: Variant;
    FDescription: String;
    FIsRequired: Boolean;
    FIsSecret: Boolean;
  public
    constructor Create(const AName: String; const AValue: Variant; AValueType: TConfigValueType);
    
    function GetName: String;
    function GetValue: Variant;
    function GetValueType: TConfigValueType;
    function GetDefaultValue: Variant;
    function GetDescription: String;
    function GetIsRequired: Boolean;
    function GetIsSecret: Boolean;
    
    procedure SetValue(const AValue: Variant);
    procedure SetDefaultValue(const AValue: Variant);
    procedure SetDescription(const ADescription: String);
    procedure SetIsRequired(ARequired: Boolean);
    procedure SetIsSecret(ASecret: Boolean);
    
    function AsString: String;
    function AsInteger: Integer;
    function AsFloat: Double;
    function AsBoolean: Boolean;
    function AsObject: TJSONObject;
    function AsArray: TJSONArray;
    
    function IsEmpty: Boolean;
    function HasValue: Boolean;
    function Validate: Boolean;
  end;

  // Implementação básica da seção de configuração
  TConfigSection = class(TInterfacedObject, IConfigSection)
  private
    FName: String;
    FParent: IConfigSection;
    FValues: TList<IConfigValue>;
    FSections: TList<IConfigSection>;
  public
    constructor Create(const AName: String; AParent: IConfigSection = nil);
    destructor Destroy; override;
    
    function GetName: String;
    function GetParent: IConfigSection;
    function GetValues: TArray<IConfigValue>;
    function GetSections: TArray<IConfigSection>;
    function GetFullPath: String;
    
    function GetValue(const AName: String): IConfigValue;
    function GetSection(const AName: String): IConfigSection;
    function HasValue(const AName: String): Boolean;
    function HasSection(const AName: String): Boolean;
    
    procedure AddValue(const AValue: IConfigValue);
    procedure AddSection(const ASection: IConfigSection);
    procedure RemoveValue(const AName: String);
    procedure RemoveSection(const AName: String);
    
    function GetValueAsString(const AName: String; const ADefault: String = ''): String;
    function GetValueAsInteger(const AName: String; const ADefault: Integer = 0): Integer;
    function GetValueAsFloat(const AName: String; const ADefault: Double = 0.0): Double;
    function GetValueAsBoolean(const AName: String; const ADefault: Boolean = False): Boolean;
    
    procedure SetValue(const AName: String; const AValue: Variant);
  end;

  // Implementação básica da configuração
  TConfiguration = class(TInterfacedObject, IConfiguration)
  private
    FEnvironment: TNest4DEnvironment;
    FStatus: TConfigStatus;
    FRootSection: IConfigSection;
    FLoadOptions: TNest4DLoadOptions;
    FConfigFile: String;
    FLastModified: TDateTime;
    FVersion: String;
  public
    constructor Create;
    
    function GetEnvironment: TNest4DEnvironment;
    function GetStatus: TConfigStatus;
    function GetRootSection: IConfigSection;
    function GetLoadOptions: TNest4DLoadOptions;
    function GetConfigFile: String;
    function GetLastModified: TDateTime;
    function GetVersion: String;
    
    procedure SetEnvironment(AEnvironment: TNest4DEnvironment);
    procedure SetLoadOptions(AOptions: TNest4DLoadOptions);
    
    function LoadFromFile(const AFileName: String): Boolean;
    function LoadFromString(const AConfigData: String): Boolean;
    function LoadFromJSON(const AJSONObject: TJSONObject): Boolean;
    function SaveToFile(const AFileName: String): Boolean;
    function SaveToString: String;
    function SaveToJSON: TJSONObject;
    
    function GetSection(const APath: String): IConfigSection;
    function GetValue(const APath: String): IConfigValue;
    function HasSection(const APath: String): Boolean;
    function HasValue(const APath: String): Boolean;
    
    function GetString(const APath: String; const ADefault: String = ''): String;
    function GetInteger(const APath: String; const ADefault: Integer = 0): Integer;
    function GetFloat(const APath: String; const ADefault: Double = 0.0): Double;
    function GetBoolean(const APath: String; const ADefault: Boolean = False): Boolean;
    
    procedure SetString(const APath: String; const AValue: String);
    procedure SetInteger(const APath: String; const AValue: Integer);
    procedure SetFloat(const APath: String; const AValue: Double);
    procedure SetBoolean(const APath: String; const AValue: Boolean);
    
    function Validate: Boolean;
    function Reload: Boolean;
    procedure Clear;
    function Clone: IConfiguration;
    function Merge(const AOther: IConfiguration): Boolean;
  end;

  // Implementação básica do provedor de configuração JSON
  TJSONConfigProvider = class(TInterfacedObject, IConfigProvider)
  private
    FName: String;
    FPriority: TConfigPriority;
    FEnabled: Boolean;
  public
    constructor Create;
    
    function GetName: String;
    function GetPriority: TConfigPriority;
    function GetIsEnabled: Boolean;
    
    procedure SetPriority(APriority: TConfigPriority);
    procedure SetEnabled(AEnabled: Boolean);
    
    function CanLoad(const ASource: String): Boolean;
    function Load(const ASource: String): IConfiguration;
    function Save(const AConfiguration: IConfiguration; const ADestination: String): Boolean;
  end;

  // Implementação básica do gerenciador de configuração
  TConfigManager = class(TInterfacedObject, IConfigManager)
  private
    FCurrentConfiguration: IConfiguration;
    FProviders: TList<IConfigProvider>;
    FEnvironment: TNest4DEnvironment;
  public
    constructor Create;
    destructor Destroy; override;
    
    function GetCurrentConfiguration: IConfiguration;
    function GetProviders: TArray<IConfigProvider>;
    function GetEnvironment: TNest4DEnvironment;
    
    procedure SetEnvironment(AEnvironment: TNest4DEnvironment);
    
    procedure RegisterProvider(const AProvider: IConfigProvider);
    procedure UnregisterProvider(const AProviderName: String);
    function GetProvider(const AProviderName: String): IConfigProvider;
    
    function LoadConfiguration(const ASource: String; AOptions: TNest4DLoadOptions = []): IConfiguration;
    function SaveConfiguration(const AConfiguration: IConfiguration; const ADestination: String): Boolean;
    
    procedure SetCurrentConfiguration(const AConfiguration: IConfiguration);
    function ReloadCurrentConfiguration: Boolean;
    
    function GetString(const APath: String; const ADefault: String = ''): String;
    function GetInteger(const APath: String; const ADefault: Integer = 0): Integer;
    function GetFloat(const APath: String; const ADefault: Double = 0.0): Double;
    function GetBoolean(const APath: String; const ADefault: Boolean = False): Boolean;
    
    procedure SetString(const APath: String; const AValue: String);
    procedure SetInteger(const APath: String; const AValue: Integer);
    procedure SetFloat(const APath: String; const AValue: Double);
    procedure SetBoolean(const APath: String; const AValue: Boolean);
  end;

// Funções utilitárias globais
function GetConfigManager: IConfigManager;
function CreateConfiguration: IConfiguration;
function CreateConfigSection(const AName: String): IConfigSection;
function CreateConfigValue(const AName: String; const AValue: Variant): IConfigValue;
function EnvironmentToString(AEnvironment: TNest4DEnvironment): String;
function StringToEnvironment(const AEnvironment: String): TNest4DEnvironment;
function GetLoggingConfig: ILoggingConfig;
function GetMetricsConfig: IMetricsConfig;
function InitializeNest4DConfig(const AConfigFile: String = ''): Boolean;

implementation

var
  GConfigManager: IConfigManager;

{ TConfigValue }

constructor TConfigValue.Create(const AName: String; const AValue: Variant; AValueType: TConfigValueType);
begin
  inherited Create;
  FName := AName;
  FValue := AValue;
  FValueType := AValueType;
  FDefaultValue := Null;
  FDescription := '';
  FIsRequired := False;
  FIsSecret := False;
end;

function TConfigValue.GetName: String;
begin
  Result := FName;
end;

function TConfigValue.GetValue: Variant;
begin
  Result := FValue;
end;

function TConfigValue.GetValueType: TConfigValueType;
begin
  Result := FValueType;
end;

function TConfigValue.GetDefaultValue: Variant;
begin
  Result := FDefaultValue;
end;

function TConfigValue.GetDescription: String;
begin
  Result := FDescription;
end;

function TConfigValue.GetIsRequired: Boolean;
begin
  Result := FIsRequired;
end;

function TConfigValue.GetIsSecret: Boolean;
begin
  Result := FIsSecret;
end;

procedure TConfigValue.SetValue(const AValue: Variant);
begin
  FValue := AValue;
end;

procedure TConfigValue.SetDefaultValue(const AValue: Variant);
begin
  FDefaultValue := AValue;
end;

procedure TConfigValue.SetDescription(const ADescription: String);
begin
  FDescription := ADescription;
end;

procedure TConfigValue.SetIsRequired(ARequired: Boolean);
begin
  FIsRequired := ARequired;
end;

procedure TConfigValue.SetIsSecret(ASecret: Boolean);
begin
  FIsSecret := ASecret;
end;

function TConfigValue.AsString: String;
begin
  if VarIsNull(FValue) then
    Result := VarToStr(FDefaultValue)
  else
    Result := VarToStr(FValue);
end;

function TConfigValue.AsInteger: Integer;
begin
  if VarIsNull(FValue) then
    Result := VarAsType(FDefaultValue, varInteger)
  else
    Result := VarAsType(FValue, varInteger);
end;

function TConfigValue.AsFloat: Double;
begin
  if VarIsNull(FValue) then
    Result := VarAsType(FDefaultValue, varDouble)
  else
    Result := VarAsType(FValue, varDouble);
end;

function TConfigValue.AsBoolean: Boolean;
begin
  if VarIsNull(FValue) then
    Result := VarAsType(FDefaultValue, varBoolean)
  else
    Result := VarAsType(FValue, varBoolean);
end;

function TConfigValue.AsObject: TJSONObject;
begin
  Result := nil;
  // Implementação básica - pode ser expandida
end;

function TConfigValue.AsArray: TJSONArray;
begin
  Result := nil;
  // Implementação básica - pode ser expandida
end;

function TConfigValue.IsEmpty: Boolean;
begin
  Result := VarIsNull(FValue) or VarIsEmpty(FValue);
end;

function TConfigValue.HasValue: Boolean;
begin
  Result := not IsEmpty;
end;

function TConfigValue.Validate: Boolean;
begin
  Result := not (FIsRequired and IsEmpty);
end;

{ TConfigSection }

constructor TConfigSection.Create(const AName: String; AParent: IConfigSection = nil);
begin
  inherited Create;
  FName := AName;
  FParent := AParent;
  FValues := TList<IConfigValue>.Create;
  FSections := TList<IConfigSection>.Create;
end;

destructor TConfigSection.Destroy;
begin
  FValues.Free;
  FSections.Free;
  inherited Destroy;
end;

function TConfigSection.GetName: String;
begin
  Result := FName;
end;

function TConfigSection.GetParent: IConfigSection;
begin
  Result := FParent;
end;

function TConfigSection.GetValues: TArray<IConfigValue>;
begin
  Result := FValues.ToArray;
end;

function TConfigSection.GetSections: TArray<IConfigSection>;
begin
  Result := FSections.ToArray;
end;

function TConfigSection.GetFullPath: String;
var
  LPath: String;
  LParent: IConfigSection;
begin
  LPath := FName;
  LParent := FParent;
  while Assigned(LParent) do
  begin
    LPath := LParent.Name + '.' + LPath;
    LParent := LParent.Parent;
  end;
  Result := LPath;
end;

function TConfigSection.GetValue(const AName: String): IConfigValue;
var
  LFor: Integer;
begin
  Result := nil;
  for LFor := 0 to FValues.Count - 1 do
  begin
    if SameText(FValues[LFor].Name, AName) then
    begin
      Result := FValues[LFor];
      Break;
    end;
  end;
end;

function TConfigSection.GetSection(const AName: String): IConfigSection;
var
  LFor: Integer;
begin
  Result := nil;
  for LFor := 0 to FSections.Count - 1 do
  begin
    if SameText(FSections[LFor].Name, AName) then
    begin
      Result := FSections[LFor];
      Break;
    end;
  end;
end;

function TConfigSection.HasValue(const AName: String): Boolean;
begin
  Result := Assigned(GetValue(AName));
end;

function TConfigSection.HasSection(const AName: String): Boolean;
begin
  Result := Assigned(GetSection(AName));
end;

procedure TConfigSection.AddValue(const AValue: IConfigValue);
begin
  if Assigned(AValue) then
    FValues.Add(AValue);
end;

procedure TConfigSection.AddSection(const ASection: IConfigSection);
begin
  if Assigned(ASection) then
    FSections.Add(ASection);
end;

procedure TConfigSection.RemoveValue(const AName: String);
var
  LFor: Integer;
begin
  for LFor := FValues.Count - 1 downto 0 do
  begin
    if SameText(FValues[LFor].Name, AName) then
    begin
      FValues.Delete(LFor);
      Break;
    end;
  end;
end;

procedure TConfigSection.RemoveSection(const AName: String);
var
  LFor: Integer;
begin
  for LFor := FSections.Count - 1 downto 0 do
  begin
    if SameText(FSections[LFor].Name, AName) then
    begin
      FSections.Delete(LFor);
      Break;
    end;
  end;
end;

function TConfigSection.GetValueAsString(const AName: String; const ADefault: String = ''): String;
var
  LValue: IConfigValue;
begin
  LValue := GetValue(AName);
  if Assigned(LValue) then
    Result := LValue.AsString
  else
    Result := ADefault;
end;

function TConfigSection.GetValueAsInteger(const AName: String; const ADefault: Integer = 0): Integer;
var
  LValue: IConfigValue;
begin
  LValue := GetValue(AName);
  if Assigned(LValue) then
    Result := LValue.AsInteger
  else
    Result := ADefault;
end;

function TConfigSection.GetValueAsFloat(const AName: String; const ADefault: Double = 0.0): Double;
var
  LValue: IConfigValue;
begin
  LValue := GetValue(AName);
  if Assigned(LValue) then
    Result := LValue.AsFloat
  else
    Result := ADefault;
end;

function TConfigSection.GetValueAsBoolean(const AName: String; const ADefault: Boolean = False): Boolean;
var
  LValue: IConfigValue;
begin
  LValue := GetValue(AName);
  if Assigned(LValue) then
    Result := LValue.AsBoolean
  else
    Result := ADefault;
end;

procedure TConfigSection.SetValue(const AName: String; const AValue: Variant);
var
  LValue: IConfigValue;
  LValueType: TConfigValueType;
begin
  LValue := GetValue(AName);
  if not Assigned(LValue) then
  begin
    // Determinar tipo baseado no valor
    case VarType(AValue) of
      varString, varUString: LValueType := cvtString;
      varInteger, varSmallint, varByte: LValueType := cvtInteger;
      varSingle, varDouble, varCurrency: LValueType := cvtFloat;
      varBoolean: LValueType := cvtBoolean;
    else
      LValueType := cvtString;
    end;
    
    LValue := TConfigValue.Create(AName, AValue, LValueType);
    AddValue(LValue);
  end
  else
    LValue.SetValue(AValue);
end;

{ TConfiguration }

constructor TConfiguration.Create;
begin
  inherited Create;
  FEnvironment := neDevelopment;
  FStatus := csUnloaded;
  FRootSection := TConfigSection.Create('root');
  FLoadOptions := [];
  FConfigFile := '';
  FLastModified := 0;
  FVersion := '1.0.0';
end;

function TConfiguration.GetEnvironment: TNest4DEnvironment;
begin
  Result := FEnvironment;
end;

function TConfiguration.GetStatus: TConfigStatus;
begin
  Result := FStatus;
end;

function TConfiguration.GetRootSection: IConfigSection;
begin
  Result := FRootSection;
end;

function TConfiguration.GetLoadOptions: TNest4DLoadOptions;
begin
  Result := FLoadOptions;
end;

function TConfiguration.GetConfigFile: String;
begin
  Result := FConfigFile;
end;

function TConfiguration.GetLastModified: TDateTime;
begin
  Result := FLastModified;
end;

function TConfiguration.GetVersion: String;
begin
  Result := FVersion;
end;

procedure TConfiguration.SetEnvironment(AEnvironment: TNest4DEnvironment);
begin
  FEnvironment := AEnvironment;
end;

procedure TConfiguration.SetLoadOptions(AOptions: TNest4DLoadOptions);
begin
  FLoadOptions := AOptions;
end;

function TConfiguration.LoadFromFile(const AFileName: String): Boolean;
begin
  Result := False;
  try
    FConfigFile := AFileName;
    if TFile.Exists(AFileName) then
    begin
      FLastModified := TFile.GetLastWriteTime(AFileName);
      FStatus := csLoaded;
      Result := True;
    end;
  except
    FStatus := csError;
  end;
end;

function TConfiguration.LoadFromString(const AConfigData: String): Boolean;
begin
  Result := False;
  try
    // Implementação básica
    FStatus := csLoaded;
    Result := True;
  except
    FStatus := csError;
  end;
end;

function TConfiguration.LoadFromJSON(const AJSONObject: TJSONObject): Boolean;
begin
  Result := False;
  try
    // Implementação básica
    FStatus := csLoaded;
    Result := True;
  except
    FStatus := csError;
  end;
end;

function TConfiguration.SaveToFile(const AFileName: String): Boolean;
begin
  Result := False;
  try
    // Implementação básica
    Result := True;
  except
    // Error handling
  end;
end;

function TConfiguration.SaveToString: String;
begin
  Result := '';
  // Implementação básica
end;

function TConfiguration.SaveToJSON: TJSONObject;
begin
  Result := TJSONObject.Create;
  // Implementação básica
end;

function TConfiguration.GetSection(const APath: String): IConfigSection;
var
  LParts: TArray<String>;
  LSection: IConfigSection;
  LFor: Integer;
begin
  Result := nil;
  LParts := APath.Split(['.']);
  LSection := FRootSection;

  for LFor := 0 to High(LParts) do
  begin
    if Assigned(LSection) then
      LSection := LSection.GetSection(LParts[LFor])
    else
      Break;
  end;
  
  Result := LSection;
end;

function TConfiguration.GetValue(const APath: String): IConfigValue;
var
  LParts: TArray<String>;
  LSection: IConfigSection;
  LFor: Integer;
begin
  Result := nil;
  LParts := APath.Split(['.']);
  LSection := FRootSection;

  // Navegar até a seção pai
  for LFor := 0 to High(LParts) - 1 do
  begin
    if Assigned(LSection) then
      LSection := LSection.GetSection(LParts[LFor])
    else
      Break;
  end;
  
  // Obter o valor
  if Assigned(LSection) and (Length(LParts) > 0) then
    Result := LSection.GetValue(LParts[High(LParts)]);
end;

function TConfiguration.HasSection(const APath: String): Boolean;
begin
  Result := Assigned(GetSection(APath));
end;

function TConfiguration.HasValue(const APath: String): Boolean;
begin
  Result := Assigned(GetValue(APath));
end;

function TConfiguration.GetString(const APath: String; const ADefault: String = ''): String;
var
  LValue: IConfigValue;
begin
  LValue := GetValue(APath);
  if Assigned(LValue) then
    Result := LValue.AsString
  else
    Result := ADefault;
end;

function TConfiguration.GetInteger(const APath: String; const ADefault: Integer = 0): Integer;
var
  LValue: IConfigValue;
begin
  LValue := GetValue(APath);
  if Assigned(LValue) then
    Result := LValue.AsInteger
  else
    Result := ADefault;
end;

function TConfiguration.GetFloat(const APath: String; const ADefault: Double = 0.0): Double;
var
  LValue: IConfigValue;
begin
  LValue := GetValue(APath);
  if Assigned(LValue) then
    Result := LValue.AsFloat
  else
    Result := ADefault;
end;

function TConfiguration.GetBoolean(const APath: String; const ADefault: Boolean = False): Boolean;
var
  LValue: IConfigValue;
begin
  LValue := GetValue(APath);
  if Assigned(LValue) then
    Result := LValue.AsBoolean
  else
    Result := ADefault;
end;

procedure TConfiguration.SetString(const APath: String; const AValue: String);
var
  LParts: TArray<String>;
  LSection: IConfigSection;
  LFor: Integer;
begin
  LParts := APath.Split(['.']);
  LSection := FRootSection;

  // Navegar/criar seções até a seção pai
  for LFor := 0 to High(LParts) - 1 do
  begin
    if not LSection.HasSection(LParts[LFor]) then
      LSection.AddSection(TConfigSection.Create(LParts[LFor], LSection));
    LSection := LSection.GetSection(LParts[LFor]);
  end;
  
  // Definir o valor
  if Assigned(LSection) and (Length(LParts) > 0) then
    LSection.SetValue(LParts[High(LParts)], AValue);
end;

procedure TConfiguration.SetInteger(const APath: String; const AValue: Integer);
begin
  SetString(APath, IntToStr(AValue));
end;

procedure TConfiguration.SetFloat(const APath: String; const AValue: Double);
begin
  SetString(APath, FloatToStr(AValue));
end;

procedure TConfiguration.SetBoolean(const APath: String; const AValue: Boolean);
begin
  SetString(APath, BoolToStr(AValue, True));
end;

function TConfiguration.Validate: Boolean;
begin
  Result := True;
  FStatus := csValidated;
end;

function TConfiguration.Reload: Boolean;
begin
  Result := False;
  if FConfigFile <> '' then
    Result := LoadFromFile(FConfigFile);
end;

procedure TConfiguration.Clear;
begin
  FRootSection := TConfigSection.Create('root');
  FStatus := csUnloaded;
end;

function TConfiguration.Clone: IConfiguration;
var
  LClone: TConfiguration;
begin
  LClone := TConfiguration.Create;
  LClone.FEnvironment := FEnvironment;
  LClone.FLoadOptions := FLoadOptions;
  LClone.FVersion := FVersion;
  // Implementação básica - pode ser expandida para clonar seções
  Result := LClone;
end;

function TConfiguration.Merge(const AOther: IConfiguration): Boolean;
begin
  Result := False;
  // Implementação básica - pode ser expandida
end;

{ TJSONConfigProvider }

constructor TJSONConfigProvider.Create;
begin
  inherited Create;
  FName := 'JSON';
  FPriority := cpNormal;
  FEnabled := True;
end;

function TJSONConfigProvider.GetName: String;
begin
  Result := FName;
end;

function TJSONConfigProvider.GetPriority: TConfigPriority;
begin
  Result := FPriority;
end;

function TJSONConfigProvider.GetIsEnabled: Boolean;
begin
  Result := FEnabled;
end;

procedure TJSONConfigProvider.SetPriority(APriority: TConfigPriority);
begin
  FPriority := APriority;
end;

procedure TJSONConfigProvider.SetEnabled(AEnabled: Boolean);
begin
  FEnabled := AEnabled;
end;

function TJSONConfigProvider.CanLoad(const ASource: String): Boolean;
begin
  Result := FEnabled and (ExtractFileExt(ASource).ToLower = '.json');
end;

function TJSONConfigProvider.Load(const ASource: String): IConfiguration;
var
  LConfig: TConfiguration;
begin
  LConfig := TConfiguration.Create;
  if LConfig.LoadFromFile(ASource) then
    Result := LConfig
  else
    Result := nil;
end;

function TJSONConfigProvider.Save(const AConfiguration: IConfiguration; const ADestination: String): Boolean;
begin
  Result := False;
  if Assigned(AConfiguration) then
    Result := AConfiguration.SaveToFile(ADestination);
end;

{ TConfigManager }

constructor TConfigManager.Create;
begin
  inherited Create;
  FCurrentConfiguration := TConfiguration.Create;
  FProviders := TList<IConfigProvider>.Create;
  FEnvironment := neDevelopment;
  
  // Registrar provedor padrão
  RegisterProvider(TJSONConfigProvider.Create);
end;

destructor TConfigManager.Destroy;
begin
  FProviders.Free;
  inherited Destroy;
end;

function TConfigManager.GetCurrentConfiguration: IConfiguration;
begin
  Result := FCurrentConfiguration;
end;

function TConfigManager.GetProviders: TArray<IConfigProvider>;
begin
  Result := FProviders.ToArray;
end;

function TConfigManager.GetEnvironment: TNest4DEnvironment;
begin
  Result := FEnvironment;
end;

procedure TConfigManager.SetEnvironment(AEnvironment: TNest4DEnvironment);
begin
  FEnvironment := AEnvironment;
  if Assigned(FCurrentConfiguration) then
    FCurrentConfiguration.SetEnvironment(AEnvironment);
end;

procedure TConfigManager.RegisterProvider(const AProvider: IConfigProvider);
begin
  if Assigned(AProvider) then
    FProviders.Add(AProvider);
end;

procedure TConfigManager.UnregisterProvider(const AProviderName: String);
var
  LFor: Integer;
begin
  for LFor := FProviders.Count - 1 downto 0 do
  begin
    if SameText(FProviders[LFor].Name, AProviderName) then
    begin
      FProviders.Delete(LFor);
      Break;
    end;
  end;
end;

function TConfigManager.GetProvider(const AProviderName: String): IConfigProvider;
var
  LFor: Integer;
begin
  Result := nil;
  for LFor := 0 to FProviders.Count - 1 do
  begin
    if SameText(FProviders[LFor].Name, AProviderName) then
    begin
      Result := FProviders[LFor];
      Break;
    end;
  end;
end;

function TConfigManager.LoadConfiguration(const ASource: String; AOptions: TNest4DLoadOptions = []): IConfiguration;
var
  LFor: Integer;
  LProvider: IConfigProvider;
begin
  Result := nil;

  for LFor := 0 to FProviders.Count - 1 do
  begin
    LProvider := FProviders[LFor];
    if LProvider.CanLoad(ASource) then
    begin
      Result := LProvider.Load(ASource);
      if Assigned(Result) then
      begin
        Result.SetLoadOptions(AOptions);
        Result.SetEnvironment(FEnvironment);
        Break;
      end;
    end;
  end;
end;

function TConfigManager.SaveConfiguration(const AConfiguration: IConfiguration; const ADestination: String): Boolean;
var
  LFor: Integer;
  LProvider: IConfigProvider;
begin
  Result := False;

  for LFor := 0 to FProviders.Count - 1 do
  begin
    LProvider := FProviders[LFor];
    if LProvider.CanLoad(ADestination) then
    begin
      Result := LProvider.Save(AConfiguration, ADestination);
      Break;
    end;
  end;
end;

procedure TConfigManager.SetCurrentConfiguration(const AConfiguration: IConfiguration);
begin
  FCurrentConfiguration := AConfiguration;
end;

function TConfigManager.ReloadCurrentConfiguration: Boolean;
begin
  Result := False;
  if Assigned(FCurrentConfiguration) then
    Result := FCurrentConfiguration.Reload;
end;

function TConfigManager.GetString(const APath: String; const ADefault: String = ''): String;
begin
  if Assigned(FCurrentConfiguration) then
    Result := FCurrentConfiguration.GetString(APath, ADefault)
  else
    Result := ADefault;
end;

function TConfigManager.GetInteger(const APath: String; const ADefault: Integer = 0): Integer;
begin
  if Assigned(FCurrentConfiguration) then
    Result := FCurrentConfiguration.GetInteger(APath, ADefault)
  else
    Result := ADefault;
end;

function TConfigManager.GetFloat(const APath: String; const ADefault: Double = 0.0): Double;
begin
  if Assigned(FCurrentConfiguration) then
    Result := FCurrentConfiguration.GetFloat(APath, ADefault)
  else
    Result := ADefault;
end;

function TConfigManager.GetBoolean(const APath: String; const ADefault: Boolean = False): Boolean;
begin
  if Assigned(FCurrentConfiguration) then
    Result := FCurrentConfiguration.GetBoolean(APath, ADefault)
  else
    Result := ADefault;
end;

procedure TConfigManager.SetString(const APath: String; const AValue: String);
begin
  if Assigned(FCurrentConfiguration) then
    FCurrentConfiguration.SetString(APath, AValue);
end;

procedure TConfigManager.SetInteger(const APath: String; const AValue: Integer);
begin
  if Assigned(FCurrentConfiguration) then
    FCurrentConfiguration.SetInteger(APath, AValue);
end;

procedure TConfigManager.SetFloat(const APath: String; const AValue: Double);
begin
  if Assigned(FCurrentConfiguration) then
    FCurrentConfiguration.SetFloat(APath, AValue);
end;

procedure TConfigManager.SetBoolean(const APath: String; const AValue: Boolean);
begin
  if Assigned(FCurrentConfiguration) then
    FCurrentConfiguration.SetBoolean(APath, AValue);
end;

// Implementações das classes de configuração

constructor TLoggingConfig.Create;
begin
  inherited Create;
  LogToFile := True;
  LogToConsole := True;
  LogFileName := 'nest4d.log';
  MaxFileSize := 10485760; // 10MB
  MaxBackupFiles := 5;
  Level := llInfo;
  Format := '[%timestamp%] [%level%] %message%';
end;

constructor TMetricsConfig.Create;
begin
  inherited Create;
  Enabled := True;
  Port := 9090;
  Endpoint := '/metrics';
  MetricsInterval := 30000; // 30 seconds
  MaxMetricsHistory := 1000;
  EnablePrometheus := False;
  PrometheusEndpoint := '/prometheus';
end;

constructor THealthConfig.Create;
begin
  inherited Create;
  Enabled := True;
  Port := 8080;
  Endpoint := '/health';
  CheckInterval := 30000; // 30 seconds
  Timeout := 5000; // 5 seconds
  EnableDatabaseCheck := False;
  DatabaseConnectionString := '';
end;

constructor TCacheConfig.Create;
begin
  inherited Create;
  EnableTTL := True;
  DefaultTTL := 3600; // 1 hour
  MaxCacheSize := 1000;
  EnableBackgroundCleanup := True;
  CleanupInterval := 300000; // 5 minutes
  EnableCompression := False;
  CompressionThreshold := 1024; // 1KB
  EnableDistributedCache := False;
  RedisConnectionString := '';
end;

constructor THandlerPoolConfig.Create;
begin
  inherited Create;
  MinSize := 5;
  MaxSize := 50;
  GrowthFactor := 1.5;
  ShrinkFactor := 0.75;
  IdleTimeout := 300000; // 5 minutes
  MinTimeoutThreshold := 1000; // 1 second
  MaxTimeoutThreshold := 30000; // 30 seconds
end;

constructor TResilienceConfig.Create;
begin
  inherited Create;
  Enabled := True;
  Endpoint := '/resilience';
  EnableRetry := True;
  MaxRetryAttempts := 3;
  BaseDelay := 1000; // 1 second
  MaxDelay := 30000; // 30 seconds
  EnableCircuitBreaker := True;
  FailureThreshold := 5;
  SuccessThreshold := 3;
  Timeout := 30000; // 30 seconds
  EnableBulkhead := False;
  MaxConcurrentCalls := 10;
  MaxWaitTime := 5000; // 5 seconds
end;

constructor TFrameworkConfig.Create;
begin
  inherited Create;
  Port := 8080;
  Host := 'localhost';
  MaxConnections := 1000;
  Timeout := 30000; // 30 seconds
  EnableCompression := True;
  CompressionLevel := 6;
  EnableSSL := False;
  SSLCertFile := '';
  SSLKeyFile := '';
  SlowRequestThresholdMs := 1000; // 1 second
end;

constructor TNest4DConfig.Create;
begin
  inherited Create;
  FLogging := TLoggingConfig.Create;
  FMetrics := TMetricsConfig.Create;
  FHealth := THealthConfig.Create;
  FCache := TCacheConfig.Create;
  FHandlerPool := THandlerPoolConfig.Create;
  FResilience := TResilienceConfig.Create;
  FFramework := TFrameworkConfig.Create;
end;

destructor TNest4DConfig.Destroy;
begin
  FreeAndNil(FLogging);
  FreeAndNil(FMetrics);
  FreeAndNil(FHealth);
  FreeAndNil(FCache);
  FreeAndNil(FHandlerPool);
  FreeAndNil(FResilience);
  FreeAndNil(FFramework);
  inherited Destroy;
end;

procedure TNest4DConfig.LoadDefaults;
begin
  // Recriar todas as configurações com valores padrão
  FreeAndNil(FLogging);
  FreeAndNil(FMetrics);
  FreeAndNil(FHealth);
  FreeAndNil(FCache);
  FreeAndNil(FHandlerPool);
  FreeAndNil(FResilience);
  FreeAndNil(FFramework);
  
  FLogging := TLoggingConfig.Create;
  FMetrics := TMetricsConfig.Create;
  FHealth := THealthConfig.Create;
  FCache := TCacheConfig.Create;
  FHandlerPool := THandlerPoolConfig.Create;
  FResilience := TResilienceConfig.Create;
  FFramework := TFrameworkConfig.Create;
end;

procedure TNest4DConfig.LoadFromFile(const AFileName: string);
var
  LFileContent: string;
begin
  if not TFile.Exists(AFileName) then
    raise Exception.CreateFmt('Arquivo de configuração não encontrado: %s', [AFileName]);
    
  LFileContent := TFile.ReadAllText(AFileName, TEncoding.UTF8);
  LoadFromJSON(LFileContent);
end;

procedure TNest4DConfig.LoadFromJSON(const AJSON: string);
var
  LJSONObj: TJSONObject;
begin
  LJSONObj := TJSONObject.ParseJSONValue(AJSON) as TJSONObject;
  if not Assigned(LJSONObj) then
    raise Exception.Create('JSON inválido fornecido');
    
  try
    // Implementação básica - pode ser expandida para carregar configurações do JSON
    // Por enquanto, apenas carrega os valores padrão
    LoadDefaults;
  finally
    LJSONObj.Free;
  end;
end;

procedure TNest4DConfig.SaveToJSON(out AJSON: string);
var
  LJSONObj: TJSONObject;
begin
  LJSONObj := TJSONObject.Create;
  try
    // Implementação básica - pode ser expandida para salvar todas as configurações
    LJSONObj.AddPair('version', '1.0');
    LJSONObj.AddPair('environment', 'development');
    AJSON := LJSONObj.ToString;
  finally
    LJSONObj.Free;
  end;
end;

function TNest4DConfig.Clone: TNest4DConfig;
begin
  Result := TNest4DConfig.Create;
  // Implementação básica - copia as configurações atuais
  // Por enquanto, apenas cria uma nova instância com valores padrão
end;

// Funções utilitárias globais

function GetConfigManager: IConfigManager;
begin
  if not Assigned(GConfigManager) then
    GConfigManager := TConfigManager.Create;
  Result := GConfigManager;
end;

function CreateConfiguration: IConfiguration;
begin
  Result := TConfiguration.Create;
end;

function CreateConfigSection(const AName: String): IConfigSection;
begin
  Result := TConfigSection.Create(AName);
end;

function CreateConfigValue(const AName: String; const AValue: Variant): IConfigValue;
var
  LValueType: TConfigValueType;
begin
  case VarType(AValue) of
    varString, varUString: LValueType := cvtString;
    varInteger, varSmallint, varByte: LValueType := cvtInteger;
    varSingle, varDouble, varCurrency: LValueType := cvtFloat;
    varBoolean: LValueType := cvtBoolean;
  else
    LValueType := cvtString;
  end;
  
  Result := TConfigValue.Create(AName, AValue, LValueType);
end;

function EnvironmentToString(AEnvironment: TNest4DEnvironment): String;
begin
  case AEnvironment of
    neUnknown:     Result := 'unknown';
    neDevelopment: Result := 'development';
    neStaging:     Result := 'staging';
    neProduction:  Result := 'production';
    neTest:        Result := 'test';
  else
    Result := 'unknown';
  end;
end;

function StringToEnvironment(const AEnvironment: String): TNest4DEnvironment;
var
  LEnv: String;
begin
  LEnv := AEnvironment.ToLower;
  if LEnv = 'development' then
    Result := neDevelopment
  else if LEnv = 'staging' then
    Result := neStaging
  else if LEnv = 'production' then
    Result := neProduction
  else if LEnv = 'test' then
    Result := neTest
  else
    Result := neUnknown;
end;

function GetLoggingConfig: ILoggingConfig;
begin
  Result := nil;
  // Implementação básica - pode ser expandida
end;

function GetMetricsConfig: IMetricsConfig;
begin
  Result := nil;
  // Implementação básica - pode ser expandida
end;

function InitializeNest4DConfig(const AConfigFile: String = ''): Boolean;
var
  LConfigFile: String;
  LConfig: IConfiguration;
begin
  Result := False;
  try
    if AConfigFile <> '' then
      LConfigFile := AConfigFile
    else
      LConfigFile := 'nest4d.config.json';
      
    LConfig := GetConfigManager.LoadConfiguration(LConfigFile);
    if Assigned(LConfig) then
    begin
      GetConfigManager.SetCurrentConfiguration(LConfig);
      Result := True;
    end;
  except
    // Error handling
  end;
end;

initialization
  // Inicialização automática

finalization
  GConfigManager := nil;

end.