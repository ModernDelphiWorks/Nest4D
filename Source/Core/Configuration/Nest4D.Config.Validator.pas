unit Nest4D.Config.Validator;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  Nest4D.Config;

type
  // Tipos de valida??o
  TNest4DValidationType = (vtError, vtWarning, vtInfo);

  // Resultado de valida??o
  TNest4DValidationResult = record
    ValidationType: TNest4DValidationType;
    Section: string;
    PropertyName: string;
    Message: string;
    Value: string;

    class function Error(const ASection, AProperty, AMessage, AValue: string): TNest4DValidationResult; static;
    class function Warning(const ASection, AProperty, AMessage, AValue: string): TNest4DValidationResult; static;
    class function Info(const ASection, AProperty, AMessage, AValue: string): TNest4DValidationResult; static;
  end;

  // Lista de resultados de valida??o
  TNest4DValidationResults = TList<TNest4DValidationResult>;

  // Interface para validadores espec?ficos
  INest4DConfigValidator = interface
    ['{B8F5E2A1-4C3D-4E5F-8A9B-1C2D3E4F5A6B}']
    function Validate(const AConfig: TNest4DConfig): TNest4DValidationResults;
    function GetValidatorName: string;
  end;

  // Validador principal
  TNest4DConfigValidator = class
  private
    FValidators: TList<INest4DConfigValidator>;
    class var FInstance: TNest4DConfigValidator;
    constructor Create;
  public
    destructor Destroy; override;

    class function Instance: TNest4DConfigValidator;
    class procedure ReleaseInstance;

    procedure RegisterValidator(const AValidator: INest4DConfigValidator);
    procedure UnregisterValidator(const AValidator: INest4DConfigValidator);

    function ValidateConfig(const AConfig: TNest4DConfig): TNest4DValidationResults;
    function HasErrors(const AResults: TNest4DValidationResults): Boolean;
    function HasWarnings(const AResults: TNest4DValidationResults): Boolean;

    procedure LogValidationResults(const AResults: TNest4DValidationResults);
  end;

  // Validadores espec?ficos
  TLoggingConfigValidator = class(TInterfacedObject, INest4DConfigValidator)
  public
    function Validate(const AConfig: TNest4DConfig): TNest4DValidationResults;
    function GetValidatorName: string;
  end;

  TMetricsConfigValidator = class(TInterfacedObject, INest4DConfigValidator)
  public
    function Validate(const AConfig: TNest4DConfig): TNest4DValidationResults;
    function GetValidatorName: string;
  end;

  THealthConfigValidator = class(TInterfacedObject, INest4DConfigValidator)
  public
    function Validate(const AConfig: TNest4DConfig): TNest4DValidationResults;
    function GetValidatorName: string;
  end;

  TCacheConfigValidator = class(TInterfacedObject, INest4DConfigValidator)
  public
    function Validate(const AConfig: TNest4DConfig): TNest4DValidationResults;
    function GetValidatorName: string;
  end;

  TPoolConfigValidator = class(TInterfacedObject, INest4DConfigValidator)
  public
    function Validate(const AConfig: TNest4DConfig): TNest4DValidationResults;
    function GetValidatorName: string;
  end;

  TResilienceConfigValidator = class(TInterfacedObject, INest4DConfigValidator)
  public
    function Validate(const AConfig: TNest4DConfig): TNest4DValidationResults;
    function GetValidatorName: string;
  end;

  TFrameworkConfigValidator = class(TInterfacedObject, INest4DConfigValidator)
  public
    function Validate(const AConfig: TNest4DConfig): TNest4DValidationResults;
    function GetValidatorName: string;
  end;

// Fun??es utilit?rias para valida??o
function IsValidPort(const APort: Integer): Boolean;
function IsValidHost(const AHost: string): Boolean;
function IsValidFilePath(const APath: string): Boolean;
function IsValidURL(const AURL: string): Boolean;
function IsValidConnectionString(const AConnectionString: string): Boolean;
function IsValidTimeInterval(const AInterval: Integer): Boolean;
function IsValidPercentage(const AValue: Double): Boolean;
function IsValidThreshold(const AValue: Integer): Boolean;

implementation

uses
  System.IOUtils,
  System.RegularExpressions;

{ TNest4DValidationResult }

class function TNest4DValidationResult.Error(const ASection, AProperty, AMessage, AValue: string): TNest4DValidationResult;
begin
  Result.ValidationType := vtError;
  Result.Section := ASection;
  Result.PropertyName := AProperty;
  Result.Message := AMessage;
  Result.Value := AValue;
end;

class function TNest4DValidationResult.Warning(const ASection, AProperty, AMessage, AValue: string): TNest4DValidationResult;
begin
  Result.ValidationType := vtWarning;
  Result.Section := ASection;
  Result.PropertyName := AProperty;
  Result.Message := AMessage;
  Result.Value := AValue;
end;

class function TNest4DValidationResult.Info(const ASection, AProperty, AMessage, AValue: string): TNest4DValidationResult;
begin
  Result.ValidationType := vtInfo;
  Result.Section := ASection;
  Result.PropertyName := AProperty;
  Result.Message := AMessage;
  Result.Value := AValue;
end;

{ TNest4DConfigValidator }

constructor TNest4DConfigValidator.Create;
begin
  inherited;
  FValidators := TList<INest4DConfigValidator>.Create;

  // Registrar validadores padr?o
  RegisterValidator(TLoggingConfigValidator.Create);
  RegisterValidator(TMetricsConfigValidator.Create);
  RegisterValidator(THealthConfigValidator.Create);
  RegisterValidator(TCacheConfigValidator.Create);
  RegisterValidator(TPoolConfigValidator.Create);
  RegisterValidator(TResilienceConfigValidator.Create);
  RegisterValidator(TFrameworkConfigValidator.Create);
end;

destructor TNest4DConfigValidator.Destroy;
begin
  FValidators.Free;
  inherited;
end;

class function TNest4DConfigValidator.Instance: TNest4DConfigValidator;
begin
  if not Assigned(FInstance) then
    FInstance := TNest4DConfigValidator.Create;
  Result := FInstance;
end;

class procedure TNest4DConfigValidator.ReleaseInstance;
begin
  if Assigned(FInstance) then
  begin
    FInstance.Free;
    FInstance := nil;
  end;
end;

procedure TNest4DConfigValidator.RegisterValidator(const AValidator: INest4DConfigValidator);
begin
  if Assigned(AValidator) and (FValidators.IndexOf(AValidator) = -1) then
    FValidators.Add(AValidator);
end;

procedure TNest4DConfigValidator.UnregisterValidator(const AValidator: INest4DConfigValidator);
begin
  if Assigned(AValidator) then
    FValidators.Remove(AValidator);
end;

function TNest4DConfigValidator.ValidateConfig(const AConfig: TNest4DConfig): TNest4DValidationResults;
var
  LValidator: INest4DConfigValidator;
  LResults: TNest4DValidationResults;
  LResult: TNest4DValidationResult;
begin
  Result := TNest4DValidationResults.Create;

  if not Assigned(AConfig) then
  begin
    Result.Add(TNest4DValidationResult.Error('General', 'Config', 'Configura??o n?o pode ser nula', ''));
    Exit;
  end;

  // Executar todos os validadores registrados
  for LValidator in FValidators do
  begin
    try
      LResults := LValidator.Validate(AConfig);
      if Assigned(LResults) then
      begin
        for LResult in LResults do
          Result.Add(LResult);
        LResults.Free;
      end;
    except
      on E: Exception do
        Result.Add(TNest4DValidationResult.Error('Validator', LValidator.GetValidatorName,
          'Erro durante valida??o: ' + E.Message, ''));
    end;
  end;
end;

function TNest4DConfigValidator.HasErrors(const AResults: TNest4DValidationResults): Boolean;
var
  LResult: TNest4DValidationResult;
begin
  Result := False;
  if not Assigned(AResults) then
    Exit;

  for LResult in AResults do
  begin
    if LResult.ValidationType = vtError then
    begin
      Result := True;
      Break;
    end;
  end;
end;

function TNest4DConfigValidator.HasWarnings(const AResults: TNest4DValidationResults): Boolean;
var
  LResult: TNest4DValidationResult;
begin
  Result := False;
  if not Assigned(AResults) then
    Exit;

  for LResult in AResults do
  begin
    if LResult.ValidationType = vtWarning then
    begin
      Result := True;
      Break;
    end;
  end;
end;

procedure TNest4DConfigValidator.LogValidationResults(const AResults: TNest4DValidationResults);
var
  LResult: TNest4DValidationResult;
  LTypeStr: string;
begin
  if not Assigned(AResults) then
    Exit;

  for LResult in AResults do
  begin
    case LResult.ValidationType of
      vtError: LTypeStr := 'ERROR';
      vtWarning: LTypeStr := 'WARNING';
      vtInfo: LTypeStr := 'INFO';
    end;

    // Log usando o sistema de logging do Nest4D (se dispon?vel)
    // Por enquanto, usar WriteLn para debug
    WriteLn(Format('[%s] %s.%s: %s (Value: %s)',
      [LTypeStr, LResult.Section, LResult.PropertyName, LResult.Message, LResult.Value]));
  end;
end;

{ TLoggingConfigValidator }

function TLoggingConfigValidator.Validate(const AConfig: TNest4DConfig): TNest4DValidationResults;
begin
  Result := TNest4DValidationResults.Create;

  // Validar arquivo de log
  if AConfig.Logging.LogToFile and (Trim(AConfig.Logging.LogFileName) = '') then
    Result.Add(TNest4DValidationResult.Error('Logging', 'LogFileName',
      'Nome do arquivo de log ? obrigat?rio quando LogToFile est? habilitado', AConfig.Logging.LogFileName));

  if AConfig.Logging.LogToFile and not IsValidFilePath(AConfig.Logging.LogFileName) then
    Result.Add(TNest4DValidationResult.Warning('Logging', 'LogFileName',
      'Caminho do arquivo de log pode ser inv?lido', AConfig.Logging.LogFileName));

  // Validar tamanho m?ximo do arquivo
  if AConfig.Logging.MaxFileSize <= 0 then
    Result.Add(TNest4DValidationResult.Error('Logging', 'MaxFileSize',
      'Tamanho m?ximo do arquivo deve ser maior que zero', IntToStr(AConfig.Logging.MaxFileSize)));

  if AConfig.Logging.MaxFileSize > 1073741824 then // 1GB
    Result.Add(TNest4DValidationResult.Warning('Logging', 'MaxFileSize',
      'Tamanho m?ximo do arquivo muito grande (>1GB)', IntToStr(AConfig.Logging.MaxFileSize)));

  // Validar n?mero de backups
  if AConfig.Logging.MaxBackupFiles < 0 then
    Result.Add(TNest4DValidationResult.Error('Logging', 'MaxBackupFiles',
      'N?mero de arquivos de backup n?o pode ser negativo', IntToStr(AConfig.Logging.MaxBackupFiles)));

  if AConfig.Logging.MaxBackupFiles > 100 then
    Result.Add(TNest4DValidationResult.Warning('Logging', 'MaxBackupFiles',
      'N?mero de arquivos de backup muito alto (>100)', IntToStr(AConfig.Logging.MaxBackupFiles)));

  // Validar se pelo menos uma sa?da est? habilitada
  if not AConfig.Logging.LogToFile and not AConfig.Logging.LogToConsole then
    Result.Add(TNest4DValidationResult.Warning('Logging', 'Output',
      'Nenhuma sa?da de log est? habilitada', ''));
end;

function TLoggingConfigValidator.GetValidatorName: string;
begin
  Result := 'LoggingConfigValidator';
end;

{ TMetricsConfigValidator }

function TMetricsConfigValidator.Validate(const AConfig: TNest4DConfig): TNest4DValidationResults;
begin
  Result := TNest4DValidationResults.Create;

  // Validar porta
  if AConfig.Metrics.Enabled and not IsValidPort(AConfig.Metrics.Port) then
    Result.Add(TNest4DValidationResult.Error('Metrics', 'Port',
      'Porta inv?lida para m?tricas', IntToStr(AConfig.Metrics.Port)));

  // Validar endpoint
  if AConfig.Metrics.Enabled and (Trim(AConfig.Metrics.Endpoint) = '') then
    Result.Add(TNest4DValidationResult.Error('Metrics', 'Endpoint',
      'Endpoint de m?tricas ? obrigat?rio quando habilitado', AConfig.Metrics.Endpoint));

  // Validar intervalo de m?tricas
  if AConfig.Metrics.MetricsInterval <= 0 then
    Result.Add(TNest4DValidationResult.Error('Metrics', 'MetricsInterval',
      'Intervalo de m?tricas deve ser maior que zero', IntToStr(AConfig.Metrics.MetricsInterval)));

  if AConfig.Metrics.MetricsInterval < 1000 then // Menos de 1 segundo
    Result.Add(TNest4DValidationResult.Warning('Metrics', 'MetricsInterval',
      'Intervalo de m?tricas muito baixo (<1s)', IntToStr(AConfig.Metrics.MetricsInterval)));

  // Validar hist?rico de m?tricas
  if AConfig.Metrics.MaxMetricsHistory <= 0 then
    Result.Add(TNest4DValidationResult.Error('Metrics', 'MaxMetricsHistory',
      'Hist?rico m?ximo de m?tricas deve ser maior que zero', IntToStr(AConfig.Metrics.MaxMetricsHistory)));

  // Validar endpoint do Prometheus
  if AConfig.Metrics.EnablePrometheus and (Trim(AConfig.Metrics.PrometheusEndpoint) = '') then
    Result.Add(TNest4DValidationResult.Error('Metrics', 'PrometheusEndpoint',
      'Endpoint do Prometheus ? obrigat?rio quando habilitado', AConfig.Metrics.PrometheusEndpoint));
end;

function TMetricsConfigValidator.GetValidatorName: string;
begin
  Result := 'MetricsConfigValidator';
end;

{ THealthConfigValidator }

function THealthConfigValidator.Validate(const AConfig: TNest4DConfig): TNest4DValidationResults;
begin
  Result := TNest4DValidationResults.Create;

  // Validar porta
  if AConfig.Health.Enabled and not IsValidPort(AConfig.Health.Port) then
    Result.Add(TNest4DValidationResult.Error('Health', 'Port',
      'Porta inv?lida para health check', IntToStr(AConfig.Health.Port)));

  // Validar endpoint
  if AConfig.Health.Enabled and (Trim(AConfig.Health.Endpoint) = '') then
    Result.Add(TNest4DValidationResult.Error('Health', 'Endpoint',
      'Endpoint de health check ? obrigat?rio quando habilitado', AConfig.Health.Endpoint));

  // Validar intervalo de verifica??o
  if not IsValidTimeInterval(AConfig.Health.CheckInterval) then
    Result.Add(TNest4DValidationResult.Error('Health', 'CheckInterval',
      'Intervalo de verifica??o inv?lido', IntToStr(AConfig.Health.CheckInterval)));

  // Validar timeout
  if not IsValidTimeInterval(AConfig.Health.Timeout) then
    Result.Add(TNest4DValidationResult.Error('Health', 'Timeout',
      'Timeout inv?lido', IntToStr(AConfig.Health.Timeout)));

  // Validar se timeout ? menor que intervalo
  if AConfig.Health.Timeout >= AConfig.Health.CheckInterval then
    Result.Add(TNest4DValidationResult.Warning('Health', 'Timeout',
      'Timeout deve ser menor que o intervalo de verifica??o',
      Format('Timeout: %d, Interval: %d', [AConfig.Health.Timeout, AConfig.Health.CheckInterval])));

  // Validar connection string do banco
  if AConfig.Health.EnableDatabaseCheck and (Trim(AConfig.Health.DatabaseConnectionString) = '') then
    Result.Add(TNest4DValidationResult.Error('Health', 'DatabaseConnectionString',
      'Connection string do banco ? obrigat?ria quando verifica??o de banco est? habilitada',
      AConfig.Health.DatabaseConnectionString));
end;

function THealthConfigValidator.GetValidatorName: string;
begin
  Result := 'HealthConfigValidator';
end;

{ TCacheConfigValidator }

function TCacheConfigValidator.Validate(const AConfig: TNest4DConfig): TNest4DValidationResults;
begin
  Result := TNest4DValidationResults.Create;

  // Validar TTL padr?o
  if AConfig.Cache.EnableTTL and (AConfig.Cache.DefaultTTL <= 0) then
    Result.Add(TNest4DValidationResult.Error('Cache', 'DefaultTTL',
      'TTL padr?o deve ser maior que zero quando TTL est? habilitado', IntToStr(AConfig.Cache.DefaultTTL)));

  // Validar tamanho m?ximo do cache
  if AConfig.Cache.MaxCacheSize <= 0 then
    Result.Add(TNest4DValidationResult.Error('Cache', 'MaxCacheSize',
      'Tamanho m?ximo do cache deve ser maior que zero', IntToStr(AConfig.Cache.MaxCacheSize)));

  // Validar intervalo de limpeza
  if AConfig.Cache.EnableBackgroundCleanup and not IsValidTimeInterval(AConfig.Cache.CleanupInterval) then
    Result.Add(TNest4DValidationResult.Error('Cache', 'CleanupInterval',
      'Intervalo de limpeza inv?lido', IntToStr(AConfig.Cache.CleanupInterval)));

  // Validar threshold de compress?o
  if AConfig.Cache.EnableCompression and (AConfig.Cache.CompressionThreshold <= 0) then
    Result.Add(TNest4DValidationResult.Error('Cache', 'CompressionThreshold',
      'Threshold de compress?o deve ser maior que zero', IntToStr(AConfig.Cache.CompressionThreshold)));

  // Validar Redis connection string
  if AConfig.Cache.EnableDistributedCache and (Trim(AConfig.Cache.RedisConnectionString) = '') then
    Result.Add(TNest4DValidationResult.Error('Cache', 'RedisConnectionString',
      'Connection string do Redis ? obrigat?ria quando cache distribu?do est? habilitado',
      AConfig.Cache.RedisConnectionString));
end;

function TCacheConfigValidator.GetValidatorName: string;
begin
  Result := 'CacheConfigValidator';
end;

{ TPoolConfigValidator }

function TPoolConfigValidator.Validate(const AConfig: TNest4DConfig): TNest4DValidationResults;
begin
  Result := TNest4DValidationResults.Create;

  // Validar tamanhos m?nimo e m?ximo
  if AConfig.HandlerPool.MinSize <= 0 then
      Result.Add(TNest4DValidationResult.Error('Pool', 'MinSize',
        'Tamanho m?nimo do pool deve ser maior que zero', IntToStr(AConfig.HandlerPool.MinSize)));

  if AConfig.HandlerPool.MaxSize <= 0 then
      Result.Add(TNest4DValidationResult.Error('Pool', 'MaxSize',
        'Tamanho m?ximo do pool deve ser maior que zero', IntToStr(AConfig.HandlerPool.MaxSize)));

  if AConfig.HandlerPool.MinSize > AConfig.HandlerPool.MaxSize then
      Result.Add(TNest4DValidationResult.Error('Pool', 'Size',
        'Tamanho m?nimo n?o pode ser maior que o m?ximo',
        Format('Min: %d, Max: %d', [AConfig.HandlerPool.MinSize, AConfig.HandlerPool.MaxSize])));

  // Validar fatores de crescimento e redu??o
  if not IsValidPercentage(AConfig.HandlerPool.GrowthFactor) then
      Result.Add(TNest4DValidationResult.Error('Pool', 'GrowthFactor',
        'Fator de crescimento inv?lido', FloatToStr(AConfig.HandlerPool.GrowthFactor)));

  if not IsValidPercentage(AConfig.HandlerPool.ShrinkFactor) then
      Result.Add(TNest4DValidationResult.Error('Pool', 'ShrinkFactor',
        'Fator de redu??o inv?lido', FloatToStr(AConfig.HandlerPool.ShrinkFactor)));

  // Validar timeouts
  if not IsValidTimeInterval(AConfig.HandlerPool.IdleTimeout) then
      Result.Add(TNest4DValidationResult.Error('Pool', 'IdleTimeout',
        'Timeout de idle inv?lido', IntToStr(AConfig.HandlerPool.IdleTimeout)));

  // Validar thresholds de timeout
  if AConfig.HandlerPool.MinTimeoutThreshold > AConfig.HandlerPool.MaxTimeoutThreshold then
      Result.Add(TNest4DValidationResult.Error('Pool', 'TimeoutThreshold',
        'Threshold m?nimo n?o pode ser maior que o m?ximo',
        Format('Min: %d, Max: %d', [AConfig.HandlerPool.MinTimeoutThreshold, AConfig.HandlerPool.MaxTimeoutThreshold])));
end;

function TPoolConfigValidator.GetValidatorName: string;
begin
  Result := 'PoolConfigValidator';
end;

{ TResilienceConfigValidator }

function TResilienceConfigValidator.Validate(const AConfig: TNest4DConfig): TNest4DValidationResults;
begin
  Result := TNest4DValidationResults.Create;

  // Validar endpoint
  if AConfig.Resilience.Enabled and (Trim(AConfig.Resilience.Endpoint) = '') then
    Result.Add(TNest4DValidationResult.Error('Resilience', 'Endpoint',
      'Endpoint de resili?ncia ? obrigat?rio quando habilitado', AConfig.Resilience.Endpoint));

  // Validar configura??es de retry
  if AConfig.Resilience.EnableRetry then
  begin
    if AConfig.Resilience.MaxRetryAttempts <= 0 then
      Result.Add(TNest4DValidationResult.Error('Resilience', 'MaxRetryAttempts',
        'N?mero m?ximo de tentativas deve ser maior que zero', IntToStr(AConfig.Resilience.MaxRetryAttempts)));

    if AConfig.Resilience.BaseDelay <= 0 then
      Result.Add(TNest4DValidationResult.Error('Resilience', 'BaseDelay',
        'Delay base deve ser maior que zero', IntToStr(AConfig.Resilience.BaseDelay)));

    if AConfig.Resilience.MaxDelay <= AConfig.Resilience.BaseDelay then
      Result.Add(TNest4DValidationResult.Error('Resilience', 'MaxDelay',
        'Delay m?ximo deve ser maior que o delay base',
        Format('Base: %d, Max: %d', [AConfig.Resilience.BaseDelay, AConfig.Resilience.MaxDelay])));
  end;

  // Validar configura??es de circuit breaker
  if AConfig.Resilience.EnableCircuitBreaker then
  begin
    if not IsValidThreshold(AConfig.Resilience.FailureThreshold) then
      Result.Add(TNest4DValidationResult.Error('Resilience', 'FailureThreshold',
        'Threshold de falha inv?lido', IntToStr(AConfig.Resilience.FailureThreshold)));

    if not IsValidThreshold(AConfig.Resilience.SuccessThreshold) then
      Result.Add(TNest4DValidationResult.Error('Resilience', 'SuccessThreshold',
        'Threshold de sucesso inv?lido', IntToStr(AConfig.Resilience.SuccessThreshold)));

    if not IsValidTimeInterval(AConfig.Resilience.Timeout) then
      Result.Add(TNest4DValidationResult.Error('Resilience', 'Timeout',
        'Timeout inv?lido', IntToStr(AConfig.Resilience.Timeout)));
  end;

  // Validar configura??es de bulkhead
  if AConfig.Resilience.EnableBulkhead then
  begin
    if AConfig.Resilience.MaxConcurrentCalls <= 0 then
      Result.Add(TNest4DValidationResult.Error('Resilience', 'MaxConcurrentCalls',
        'N?mero m?ximo de chamadas concorrentes deve ser maior que zero',
        IntToStr(AConfig.Resilience.MaxConcurrentCalls)));

    if not IsValidTimeInterval(AConfig.Resilience.MaxWaitTime) then
      Result.Add(TNest4DValidationResult.Error('Resilience', 'MaxWaitTime',
        'Tempo m?ximo de espera inv?lido', IntToStr(AConfig.Resilience.MaxWaitTime)));
  end;
end;

function TResilienceConfigValidator.GetValidatorName: string;
begin
  Result := 'ResilienceConfigValidator';
end;

{ TFrameworkConfigValidator }

function TFrameworkConfigValidator.Validate(const AConfig: TNest4DConfig): TNest4DValidationResults;
begin
  Result := TNest4DValidationResults.Create;

  // Validar porta
  if not IsValidPort(AConfig.Framework.Port) then
    Result.Add(TNest4DValidationResult.Error('Framework', 'Port',
      'Porta inv?lida', IntToStr(AConfig.Framework.Port)));

  // Validar host
  if not IsValidHost(AConfig.Framework.Host) then
    Result.Add(TNest4DValidationResult.Error('Framework', 'Host',
      'Host inv?lido', AConfig.Framework.Host));

  // Validar n?mero m?ximo de conex?es
  if AConfig.Framework.MaxConnections <= 0 then
    Result.Add(TNest4DValidationResult.Error('Framework', 'MaxConnections',
      'N?mero m?ximo de conex?es deve ser maior que zero', IntToStr(AConfig.Framework.MaxConnections)));

  // Validar timeout
  if not IsValidTimeInterval(AConfig.Framework.Timeout) then
    Result.Add(TNest4DValidationResult.Error('Framework', 'Timeout',
      'Timeout inv?lido', IntToStr(AConfig.Framework.Timeout)));

  // Validar n?vel de compress?o
  if AConfig.Framework.EnableCompression and
     ((AConfig.Framework.CompressionLevel < 1) or (AConfig.Framework.CompressionLevel > 9)) then
    Result.Add(TNest4DValidationResult.Error('Framework', 'CompressionLevel',
      'N?vel de compress?o deve estar entre 1 e 9', IntToStr(AConfig.Framework.CompressionLevel)));

  // Validar arquivos SSL
  if AConfig.Framework.EnableSSL then
  begin
    if not IsValidFilePath(AConfig.Framework.SSLCertFile) then
      Result.Add(TNest4DValidationResult.Error('Framework', 'SSLCertFile',
        'Arquivo de certificado SSL inv?lido', AConfig.Framework.SSLCertFile));

    if not IsValidFilePath(AConfig.Framework.SSLKeyFile) then
      Result.Add(TNest4DValidationResult.Error('Framework', 'SSLKeyFile',
        'Arquivo de chave SSL inv?lido', AConfig.Framework.SSLKeyFile));
  end;

  // Validar threshold de requisi??es lentas
  if AConfig.Framework.SlowRequestThresholdMs <= 0 then
    Result.Add(TNest4DValidationResult.Warning('Framework', 'SlowRequestThresholdMs',
      'Threshold de requisi??es lentas deve ser maior que zero', IntToStr(AConfig.Framework.SlowRequestThresholdMs)));
end;

function TFrameworkConfigValidator.GetValidatorName: string;
begin
  Result := 'FrameworkConfigValidator';
end;

{ Fun??es utilit?rias }

function IsValidPort(const APort: Integer): Boolean;
begin
  Result := (APort > 0) and (APort <= 65535);
end;

function IsValidHost(const AHost: string): Boolean;
var
  LHost: string;
begin
  LHost := Trim(AHost);
  Result := (LHost <> '') and
            (TRegEx.IsMatch(LHost, '^[a-zA-Z0-9.-]+$') or
             TRegEx.IsMatch(LHost, '^\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3}$'));
end;

function IsValidFilePath(const APath: string): Boolean;
var
  LPath: string;
begin
  LPath := Trim(APath);
  Result := (LPath <> '') and not TRegEx.IsMatch(LPath, '[<>:"|?*]');
end;

function IsValidURL(const AURL: string): Boolean;
var
  LURL: string;
begin
  LURL := Trim(AURL);
  Result := (LURL <> '') and
            (TRegEx.IsMatch(LURL, '^https?://[a-zA-Z0-9.-]+(:[0-9]+)?(/.*)?$'));
end;

function IsValidConnectionString(const AConnectionString: string): Boolean;
var
  LConnectionString: string;
begin
  LConnectionString := Trim(AConnectionString);
  Result := (LConnectionString <> '') and (Length(LConnectionString) > 10);
end;

function IsValidTimeInterval(const AInterval: Integer): Boolean;
begin
  Result := (AInterval > 0) and (AInterval <= 86400000); // M?ximo 24 horas em ms
end;

function IsValidPercentage(const AValue: Double): Boolean;
begin
  Result := (AValue > 0.0) and (AValue <= 10.0); // Fator entre 0 e 10
end;

function IsValidThreshold(const AValue: Integer): Boolean;
begin
  Result := (AValue > 0) and (AValue <= 1000);
end;

initialization

finalization
  TNest4DConfigValidator.ReleaseInstance;

end.

