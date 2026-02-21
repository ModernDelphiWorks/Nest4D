unit Nest4D.Config.Environment;

interface

uses
  System.SysUtils,
  System.Classes,
  System.StrUtils,
  System.Generics.Collections,
  Nest4D.Config,
  Nest4D.Logging;

type
  // Classe para gerenciamento de vari?veis de ambiente
  TNest4DEnvironmentLoader = class
  private
    class function GetEnvironmentVariable(const AName: string; const ADefault: string = ''): string;
    class function GetEnvironmentVariableAsInteger(const AName: string; const ADefault: Integer): Integer;
    class function GetEnvironmentVariableAsBoolean(const AName: string; const ADefault: Boolean): Boolean;
    class function GetEnvironmentVariableAsFloat(const AName: string; const ADefault: Double): Double;
    class function GetEnvironmentVariableAsStringArray(const AName: string; const ASeparator: string = ','): TArray<string>;

    // M?todos para carregar configura??es espec?ficas
    class procedure LoadLoggingFromEnvironment(var AConfig: TLoggingConfig);
    class procedure LoadMetricsFromEnvironment(var AConfig: TMetricsConfig);
    class procedure LoadHealthFromEnvironment(var AConfig: THealthConfig);
    class procedure LoadCacheFromEnvironment(var AConfig: TCacheConfig);
    class procedure LoadPoolFromEnvironment(var AConfig: THandlerPoolConfig); static;
    class procedure LoadResilienceFromEnvironment(var AConfig: TResilienceConfig);
    class procedure LoadFrameworkFromEnvironment(var AConfig: TFrameworkConfig);

  public
    // M?todo principal para carregar configura??o completa
    class procedure LoadConfigFromEnvironment(var AConfig: TNest4DConfig);

    // M?todos para verificar exist?ncia de vari?veis
    class function HasEnvironmentVariable(const AName: string): Boolean;
    class function ListNest4DEnvironmentVariables: TArray<string>;

    // M?todos para definir vari?veis (?til para testes)
    class procedure SetEnvironmentVariable(const AName, AValue: string);
    class procedure ClearEnvironmentVariable(const AName: string);

    // M?todos de conveni?ncia para valida??o
    class function ValidateEnvironmentVariables: TArray<string>; // Retorna lista de erros
    class procedure PrintEnvironmentVariables; // Para debug
  end;

  // Constantes para nomes das vari?veis de ambiente
  TNest4DEnvVars = class
  public
    // Logging
    const LOGGING_ENABLED = 'Nest4D_LOGGING_ENABLED';
    const LOGGING_LEVEL = 'Nest4D_LOGGING_LEVEL';
    const LOGGING_ENABLE_CONSOLE = 'Nest4D_LOGGING_ENABLE_CONSOLE';
    const LOGGING_ENABLE_FILE = 'Nest4D_LOGGING_ENABLE_FILE';
    const LOGGING_ENABLE_DATABASE = 'Nest4D_LOGGING_ENABLE_DATABASE';
    const LOGGING_FILE_PATH = 'Nest4D_LOGGING_FILE_PATH';
    const LOGGING_MAX_FILE_SIZE = 'Nest4D_LOGGING_MAX_FILE_SIZE';
    const LOGGING_MAX_FILES = 'Nest4D_LOGGING_MAX_FILES';
    const LOGGING_ENABLE_ROTATION = 'Nest4D_LOGGING_ENABLE_ROTATION';
    const LOGGING_ENABLE_ASYNC = 'Nest4D_LOGGING_ENABLE_ASYNC';
    const LOGGING_BUFFER_SIZE = 'Nest4D_LOGGING_BUFFER_SIZE';
    const LOGGING_FLUSH_INTERVAL = 'Nest4D_LOGGING_FLUSH_INTERVAL';
    const LOGGING_DATETIME_FORMAT = 'Nest4D_LOGGING_DATETIME_FORMAT';
    const LOGGING_LOG_FORMAT = 'Nest4D_LOGGING_LOG_FORMAT';
    const LOGGING_CATEGORIES = 'Nest4D_LOGGING_CATEGORIES';

    // Metrics
    const METRICS_ENABLED = 'Nest4D_METRICS_ENABLED';
    const METRICS_PORT = 'Nest4D_METRICS_PORT';
    const METRICS_ENDPOINT = 'Nest4D_METRICS_ENDPOINT';
    const METRICS_INTERVAL = 'Nest4D_METRICS_INTERVAL';
    const METRICS_MAX_HISTORY = 'Nest4D_METRICS_MAX_HISTORY';
    const METRICS_ENABLE_PROMETHEUS = 'Nest4D_METRICS_ENABLE_PROMETHEUS';
    const METRICS_ENABLE_CUSTOM = 'Nest4D_METRICS_ENABLE_CUSTOM';
    const METRICS_PROMETHEUS_PORT = 'Nest4D_METRICS_PROMETHEUS_PORT';
    const METRICS_PROMETHEUS_PATH = 'Nest4D_METRICS_PROMETHEUS_PATH';
    const METRICS_PROMETHEUS_ENDPOINT = 'Nest4D_METRICS_PROMETHEUS_ENDPOINT';
    const METRICS_COLLECTION_INTERVAL = 'Nest4D_METRICS_COLLECTION_INTERVAL';
    const METRICS_RETENTION_PERIOD = 'Nest4D_METRICS_RETENTION_PERIOD';
    const METRICS_ENABLE_HISTOGRAMS = 'Nest4D_METRICS_ENABLE_HISTOGRAMS';
    const METRICS_ENABLE_COUNTERS = 'Nest4D_METRICS_ENABLE_COUNTERS';
    const METRICS_ENABLE_GAUGES = 'Nest4D_METRICS_ENABLE_GAUGES';
    const METRICS_CUSTOM_METRICS = 'Nest4D_METRICS_CUSTOM_METRICS';

    // Health
    const HEALTH_ENABLED = 'Nest4D_HEALTH_ENABLED';
    const HEALTH_ENDPOINT = 'Nest4D_HEALTH_ENDPOINT';
    const HEALTH_PORT = 'Nest4D_HEALTH_PORT';
    const HEALTH_CHECK_INTERVAL = 'Nest4D_HEALTH_CHECK_INTERVAL';
    const HEALTH_TIMEOUT = 'Nest4D_HEALTH_TIMEOUT';
    const HEALTH_ENABLE_DETAILED = 'Nest4D_HEALTH_ENABLE_DETAILED';
    const HEALTH_ENABLE_DEPENDENCIES = 'Nest4D_HEALTH_ENABLE_DEPENDENCIES';
    const HEALTH_CHECKS = 'Nest4D_HEALTH_CHECKS';

    // Cache
    const CACHE_ENABLED = 'Nest4D_CACHE_ENABLED';
    const CACHE_PROVIDER = 'Nest4D_CACHE_PROVIDER';
    const CACHE_DEFAULT_TTL = 'Nest4D_CACHE_DEFAULT_TTL';
    const CACHE_MAX_SIZE = 'Nest4D_CACHE_MAX_SIZE';
    const CACHE_ENABLE_TTL = 'Nest4D_CACHE_ENABLE_TTL';
    const CACHE_ENABLE_BACKGROUND_CLEANUP = 'Nest4D_CACHE_ENABLE_BACKGROUND_CLEANUP';
    const CACHE_CLEANUP_INTERVAL = 'Nest4D_CACHE_CLEANUP_INTERVAL';
    const CACHE_ENABLE_COMPRESSION = 'Nest4D_CACHE_ENABLE_COMPRESSION';
    const CACHE_ENABLE_ENCRYPTION = 'Nest4D_CACHE_ENABLE_ENCRYPTION';
    const CACHE_CONNECTION_STRING = 'Nest4D_CACHE_CONNECTION_STRING';
    const CACHE_REGIONS = 'Nest4D_CACHE_REGIONS';

    // Handler Pool
    const HANDLER_POOL_MIN_SIZE = 'Nest4D_HANDLER_POOL_MIN_SIZE';
    const HANDLER_POOL_MAX_SIZE = 'Nest4D_HANDLER_POOL_MAX_SIZE';
    const HANDLER_POOL_IDLE_TIMEOUT = 'Nest4D_HANDLER_POOL_IDLE_TIMEOUT';
    const HANDLER_POOL_GROWTH_FACTOR = 'Nest4D_HANDLER_POOL_GROWTH_FACTOR';
    const HANDLER_POOL_SHRINK_FACTOR = 'Nest4D_HANDLER_POOL_SHRINK_FACTOR';
    const HANDLER_POOL_CLEANUP_INTERVAL = 'Nest4D_HANDLER_POOL_CLEANUP_INTERVAL';
    const HANDLER_POOL_ENABLE_METRICS = 'Nest4D_HANDLER_POOL_ENABLE_METRICS';
    const HANDLER_POOL_ENABLE_AUTO_GROWTH = 'Nest4D_HANDLER_POOL_ENABLE_AUTO_GROWTH';
    const HANDLER_POOL_ENABLE_AUTO_SHRINK = 'Nest4D_HANDLER_POOL_ENABLE_AUTO_SHRINK';
    const HANDLER_POOL_MAX_IDLE_TIME = 'Nest4D_HANDLER_POOL_MAX_IDLE_TIME';
    const HANDLER_POOL_MIN_TIMEOUT_THRESHOLD = 'Nest4D_HANDLER_POOL_MIN_TIMEOUT_THRESHOLD';
    const HANDLER_POOL_MAX_TIMEOUT_THRESHOLD = 'Nest4D_HANDLER_POOL_MAX_TIMEOUT_THRESHOLD';

    // Resilience
    const RESILIENCE_ENABLED = 'Nest4D_RESILIENCE_ENABLED';
    const RESILIENCE_ENABLE_RETRY = 'Nest4D_RESILIENCE_ENABLE_RETRY';
    const RESILIENCE_ENABLE_CIRCUIT_BREAKER = 'Nest4D_RESILIENCE_ENABLE_CIRCUIT_BREAKER';
    const RESILIENCE_ENABLE_FALLBACK = 'Nest4D_RESILIENCE_ENABLE_FALLBACK';
    const RESILIENCE_ENABLE_BULKHEAD = 'Nest4D_RESILIENCE_ENABLE_BULKHEAD';
    const RESILIENCE_RETRY_ATTEMPTS = 'Nest4D_RESILIENCE_RETRY_ATTEMPTS';
    const RESILIENCE_RETRY_DELAY = 'Nest4D_RESILIENCE_RETRY_DELAY';
    const RESILIENCE_CIRCUIT_BREAKER_THRESHOLD = 'Nest4D_RESILIENCE_CIRCUIT_BREAKER_THRESHOLD';
    const RESILIENCE_CIRCUIT_BREAKER_TIMEOUT = 'Nest4D_RESILIENCE_CIRCUIT_BREAKER_TIMEOUT';
    const RESILIENCE_CIRCUIT_BREAKER_RESET_TIMEOUT = 'Nest4D_RESILIENCE_CIRCUIT_BREAKER_RESET_TIMEOUT';
    const RESILIENCE_BULKHEAD_MAX_CONCURRENT = 'Nest4D_RESILIENCE_BULKHEAD_MAX_CONCURRENT';
    const RESILIENCE_BULKHEAD_QUEUE_SIZE = 'Nest4D_RESILIENCE_BULKHEAD_QUEUE_SIZE';
    const RESILIENCE_ENABLE_METRICS = 'Nest4D_RESILIENCE_ENABLE_METRICS';

    // Framework
    const FRAMEWORK_PORT = 'Nest4D_FRAMEWORK_PORT';
    const FRAMEWORK_HOST = 'Nest4D_FRAMEWORK_HOST';
    const FRAMEWORK_ENABLE_CORS = 'Nest4D_FRAMEWORK_ENABLE_CORS';
    const FRAMEWORK_ENABLE_COMPRESSION = 'Nest4D_FRAMEWORK_ENABLE_COMPRESSION';
    const FRAMEWORK_ENABLE_SECURITY = 'Nest4D_FRAMEWORK_ENABLE_SECURITY';
    const FRAMEWORK_MAX_REQUEST_SIZE = 'Nest4D_FRAMEWORK_MAX_REQUEST_SIZE';
    const FRAMEWORK_REQUEST_TIMEOUT = 'Nest4D_FRAMEWORK_REQUEST_TIMEOUT';
    const FRAMEWORK_ENABLE_SWAGGER = 'Nest4D_FRAMEWORK_ENABLE_SWAGGER';
    const FRAMEWORK_SWAGGER_PATH = 'Nest4D_FRAMEWORK_SWAGGER_PATH';
    const FRAMEWORK_ENABLE_RATE_LIMIT = 'Nest4D_FRAMEWORK_ENABLE_RATE_LIMIT';
    const FRAMEWORK_RATE_LIMIT_REQUESTS = 'Nest4D_FRAMEWORK_RATE_LIMIT_REQUESTS';
    const FRAMEWORK_RATE_LIMIT_WINDOW = 'Nest4D_FRAMEWORK_RATE_LIMIT_WINDOW';
    const FRAMEWORK_CORS_ORIGINS = 'Nest4D_FRAMEWORK_CORS_ORIGINS';
    const FRAMEWORK_MAX_CONNECTIONS = 'Nest4D_FRAMEWORK_MAX_CONNECTIONS';
    const FRAMEWORK_TIMEOUT = 'Nest4D_FRAMEWORK_TIMEOUT';
    const FRAMEWORK_COMPRESSION_LEVEL = 'Nest4D_FRAMEWORK_COMPRESSION_LEVEL';
    const FRAMEWORK_ENABLE_SSL = 'Nest4D_FRAMEWORK_ENABLE_SSL';
    const FRAMEWORK_SSL_CERT_FILE = 'Nest4D_FRAMEWORK_SSL_CERT_FILE';
    const FRAMEWORK_SSL_KEY_FILE = 'Nest4D_FRAMEWORK_SSL_KEY_FILE';
    const FRAMEWORK_SLOW_REQUEST_THRESHOLD = 'Nest4D_FRAMEWORK_SLOW_REQUEST_THRESHOLD';

    // Geral
    const ENVIRONMENT = 'Nest4D_ENVIRONMENT';
    const CONFIG_FILE = 'Nest4D_CONFIG_FILE';
    const ENABLE_HOT_RELOAD = 'Nest4D_ENABLE_HOT_RELOAD';
    const ENABLE_VALIDATION = 'Nest4D_ENABLE_VALIDATION';
  end;

implementation

uses
  System.IOUtils,
  {$IFDEF MSWINDOWS}
  Winapi.Windows
  {$ENDIF};

{ TNest4DEnvironmentLoader }

class function TNest4DEnvironmentLoader.GetEnvironmentVariable(const AName: string; const ADefault: string): string;
begin
  Result := System.SysUtils.GetEnvironmentVariable(AName);
  if Result = '' then
    Result := ADefault;
end;

class function TNest4DEnvironmentLoader.GetEnvironmentVariableAsInteger(const AName: string; const ADefault: Integer): Integer;
var
  LValue: string;
begin
  LValue := GetEnvironmentVariable(AName);
  if not TryStrToInt(LValue, Result) then
    Result := ADefault;
end;

class function TNest4DEnvironmentLoader.GetEnvironmentVariableAsBoolean(const AName: string; const ADefault: Boolean): Boolean;
var
  LValue: string;
begin
  LValue := LowerCase(Trim(GetEnvironmentVariable(AName)));

  if (LValue = 'true') or (LValue = '1') or (LValue = 'yes') or (LValue = 'on') then
    Result := True
  else if (LValue = 'false') or (LValue = '0') or (LValue = 'no') or (LValue = 'off') then
    Result := False
  else
    Result := ADefault;
end;

class function TNest4DEnvironmentLoader.GetEnvironmentVariableAsFloat(const AName: string; const ADefault: Double): Double;
var
  LValue: string;
begin
  LValue := GetEnvironmentVariable(AName);
  if not TryStrToFloat(LValue, Result) then
    Result := ADefault;
end;

class function TNest4DEnvironmentLoader.GetEnvironmentVariableAsStringArray(const AName: string; const ASeparator: string): TArray<string>;
var
  LValue: string;
begin
  LValue := GetEnvironmentVariable(AName);
  if LValue = '' then
    Result := nil
  else
    Result := SplitString(LValue, ASeparator);
end;

class procedure TNest4DEnvironmentLoader.LoadConfigFromEnvironment(var AConfig: TNest4DConfig);
var
  LLogging: TLoggingConfig;
  LMetrics: TMetricsConfig;
  LHealth: THealthConfig;
  LCache: TCacheConfig;
  LHandlerPool: THandlerPoolConfig;
  LResilience: TResilienceConfig;
  LFramework: TFrameworkConfig;
begin
  LLogging := AConfig.Logging;
  LoadLoggingFromEnvironment(LLogging);
  AConfig.Logging := LLogging;
  
  LMetrics := AConfig.Metrics;
  LoadMetricsFromEnvironment(LMetrics);
  AConfig.Metrics := LMetrics;
  
  LHealth := AConfig.Health;
  LoadHealthFromEnvironment(LHealth);
  AConfig.Health := LHealth;
  
  LCache := AConfig.Cache;
  LoadCacheFromEnvironment(LCache);
  AConfig.Cache := LCache;
  
  LHandlerPool := AConfig.HandlerPool;
  LoadPoolFromEnvironment(LHandlerPool);
  AConfig.HandlerPool := LHandlerPool;
  
  LResilience := AConfig.Resilience;
  LoadResilienceFromEnvironment(LResilience);
  AConfig.Resilience := LResilience;
  
  LFramework := AConfig.Framework;
  LoadFrameworkFromEnvironment(LFramework);
  AConfig.Framework := LFramework;
end;

class procedure TNest4DEnvironmentLoader.LoadLoggingFromEnvironment(var AConfig: TLoggingConfig);
var
  StrValue: string;
begin
  if HasEnvironmentVariable(TNest4DEnvVars.LOGGING_ENABLED) then
    AConfig.Enabled := GetEnvironmentVariableAsBoolean(TNest4DEnvVars.LOGGING_ENABLED, AConfig.Enabled);

  if HasEnvironmentVariable(TNest4DEnvVars.LOGGING_LEVEL) then
  begin
    StrValue := GetEnvironmentVariable(TNest4DEnvVars.LOGGING_LEVEL);
    if SameText(StrValue, 'Trace') then AConfig.Level := llTrace
    else if SameText(StrValue, 'Debug') then AConfig.Level := llDebug
    else if SameText(StrValue, 'Info') then AConfig.Level := llInfo
    else if SameText(StrValue, 'Warn') then AConfig.Level := llWarn
    else if SameText(StrValue, 'Error') then AConfig.Level := llError
    else if SameText(StrValue, 'Fatal') then AConfig.Level := llFatal;
  end;

  if HasEnvironmentVariable(TNest4DEnvVars.LOGGING_ENABLE_CONSOLE) then
    AConfig.EnableConsole := GetEnvironmentVariableAsBoolean(TNest4DEnvVars.LOGGING_ENABLE_CONSOLE, AConfig.EnableConsole);

  if HasEnvironmentVariable(TNest4DEnvVars.LOGGING_ENABLE_FILE) then
    AConfig.EnableFile := GetEnvironmentVariableAsBoolean(TNest4DEnvVars.LOGGING_ENABLE_FILE, AConfig.EnableFile);

  if HasEnvironmentVariable(TNest4DEnvVars.LOGGING_ENABLE_DATABASE) then
    AConfig.EnableDatabase := GetEnvironmentVariableAsBoolean(TNest4DEnvVars.LOGGING_ENABLE_DATABASE, AConfig.EnableDatabase);

  if HasEnvironmentVariable(TNest4DEnvVars.LOGGING_FILE_PATH) then
    AConfig.FilePath := GetEnvironmentVariable(TNest4DEnvVars.LOGGING_FILE_PATH, AConfig.FilePath);

  if HasEnvironmentVariable(TNest4DEnvVars.LOGGING_MAX_FILE_SIZE) then
    AConfig.MaxFileSize := GetEnvironmentVariableAsInteger(TNest4DEnvVars.LOGGING_MAX_FILE_SIZE, AConfig.MaxFileSize);

  if HasEnvironmentVariable(TNest4DEnvVars.LOGGING_MAX_FILES) then
    AConfig.MaxBackupFiles := GetEnvironmentVariableAsInteger(TNest4DEnvVars.LOGGING_MAX_FILES, AConfig.MaxBackupFiles);

  if HasEnvironmentVariable(TNest4DEnvVars.LOGGING_LOG_FORMAT) then
    AConfig.Format := GetEnvironmentVariable(TNest4DEnvVars.LOGGING_LOG_FORMAT, AConfig.Format);
end;

class procedure TNest4DEnvironmentLoader.LoadMetricsFromEnvironment(var AConfig: TMetricsConfig);
begin
  if HasEnvironmentVariable(TNest4DEnvVars.METRICS_ENABLED) then
    AConfig.Enabled := GetEnvironmentVariableAsBoolean(TNest4DEnvVars.METRICS_ENABLED, AConfig.Enabled);

  if HasEnvironmentVariable(TNest4DEnvVars.METRICS_PORT) then
      AConfig.Port := GetEnvironmentVariableAsInteger(TNest4DEnvVars.METRICS_PORT, AConfig.Port);

    if HasEnvironmentVariable(TNest4DEnvVars.METRICS_ENDPOINT) then
      AConfig.Endpoint := GetEnvironmentVariable(TNest4DEnvVars.METRICS_ENDPOINT, AConfig.Endpoint);

    if HasEnvironmentVariable(TNest4DEnvVars.METRICS_INTERVAL) then
      AConfig.MetricsInterval := GetEnvironmentVariableAsInteger(TNest4DEnvVars.METRICS_INTERVAL, AConfig.MetricsInterval);

    if HasEnvironmentVariable(TNest4DEnvVars.METRICS_MAX_HISTORY) then
      AConfig.MaxMetricsHistory := GetEnvironmentVariableAsInteger(TNest4DEnvVars.METRICS_MAX_HISTORY, AConfig.MaxMetricsHistory);

    if HasEnvironmentVariable(TNest4DEnvVars.METRICS_ENABLE_PROMETHEUS) then
      AConfig.EnablePrometheus := GetEnvironmentVariableAsBoolean(TNest4DEnvVars.METRICS_ENABLE_PROMETHEUS, AConfig.EnablePrometheus);

    if HasEnvironmentVariable(TNest4DEnvVars.METRICS_PROMETHEUS_ENDPOINT) then
      AConfig.PrometheusEndpoint := GetEnvironmentVariable(TNest4DEnvVars.METRICS_PROMETHEUS_ENDPOINT, AConfig.PrometheusEndpoint);

  if HasEnvironmentVariable(TNest4DEnvVars.METRICS_PROMETHEUS_PATH) then
    AConfig.Endpoint := GetEnvironmentVariable(TNest4DEnvVars.METRICS_PROMETHEUS_PATH, AConfig.Endpoint);

  if HasEnvironmentVariable(TNest4DEnvVars.METRICS_COLLECTION_INTERVAL) then
    AConfig.MetricsInterval := GetEnvironmentVariableAsInteger(TNest4DEnvVars.METRICS_COLLECTION_INTERVAL, AConfig.MetricsInterval);

  if HasEnvironmentVariable(TNest4DEnvVars.METRICS_RETENTION_PERIOD) then
    AConfig.MaxMetricsHistory := GetEnvironmentVariableAsInteger(TNest4DEnvVars.METRICS_RETENTION_PERIOD, AConfig.MaxMetricsHistory);
end;

class procedure TNest4DEnvironmentLoader.LoadHealthFromEnvironment(var AConfig: THealthConfig);
begin
  if HasEnvironmentVariable(TNest4DEnvVars.HEALTH_ENABLED) then
    AConfig.Enabled := GetEnvironmentVariableAsBoolean(TNest4DEnvVars.HEALTH_ENABLED, AConfig.Enabled);

  if HasEnvironmentVariable(TNest4DEnvVars.HEALTH_ENDPOINT) then
    AConfig.Endpoint := GetEnvironmentVariable(TNest4DEnvVars.HEALTH_ENDPOINT, AConfig.Endpoint);

  if HasEnvironmentVariable(TNest4DEnvVars.HEALTH_PORT) then
    AConfig.Port := GetEnvironmentVariableAsInteger(TNest4DEnvVars.HEALTH_PORT, AConfig.Port);

  if HasEnvironmentVariable(TNest4DEnvVars.HEALTH_CHECK_INTERVAL) then
    AConfig.CheckInterval := GetEnvironmentVariableAsInteger(TNest4DEnvVars.HEALTH_CHECK_INTERVAL, AConfig.CheckInterval);

  if HasEnvironmentVariable(TNest4DEnvVars.HEALTH_TIMEOUT) then
    AConfig.Timeout := GetEnvironmentVariableAsInteger(TNest4DEnvVars.HEALTH_TIMEOUT, AConfig.Timeout);

  if HasEnvironmentVariable(TNest4DEnvVars.HEALTH_PORT) then
    AConfig.Port := GetEnvironmentVariableAsInteger(TNest4DEnvVars.HEALTH_PORT, AConfig.Port);

  if HasEnvironmentVariable(TNest4DEnvVars.HEALTH_ENDPOINT) then
    AConfig.Endpoint := GetEnvironmentVariable(TNest4DEnvVars.HEALTH_ENDPOINT, AConfig.Endpoint);

  if HasEnvironmentVariable(TNest4DEnvVars.HEALTH_CHECK_INTERVAL) then
    AConfig.CheckInterval := GetEnvironmentVariableAsInteger(TNest4DEnvVars.HEALTH_CHECK_INTERVAL, AConfig.CheckInterval);
end;

class procedure TNest4DEnvironmentLoader.LoadCacheFromEnvironment(var AConfig: TCacheConfig);
begin
  if HasEnvironmentVariable(TNest4DEnvVars.CACHE_ENABLE_TTL) then
    AConfig.EnableTTL := GetEnvironmentVariableAsBoolean(TNest4DEnvVars.CACHE_ENABLE_TTL, AConfig.EnableTTL);

  if HasEnvironmentVariable(TNest4DEnvVars.CACHE_DEFAULT_TTL) then
    AConfig.DefaultTTL := GetEnvironmentVariableAsInteger(TNest4DEnvVars.CACHE_DEFAULT_TTL, AConfig.DefaultTTL);

  if HasEnvironmentVariable(TNest4DEnvVars.CACHE_MAX_SIZE) then
    AConfig.MaxCacheSize := GetEnvironmentVariableAsInteger(TNest4DEnvVars.CACHE_MAX_SIZE, AConfig.MaxCacheSize);

  if HasEnvironmentVariable(TNest4DEnvVars.CACHE_ENABLE_TTL) then
    AConfig.EnableTTL := GetEnvironmentVariableAsBoolean(TNest4DEnvVars.CACHE_ENABLE_TTL, AConfig.EnableTTL);

  if HasEnvironmentVariable(TNest4DEnvVars.CACHE_ENABLE_BACKGROUND_CLEANUP) then
    AConfig.EnableBackgroundCleanup := GetEnvironmentVariableAsBoolean(TNest4DEnvVars.CACHE_ENABLE_BACKGROUND_CLEANUP, AConfig.EnableBackgroundCleanup);

  if HasEnvironmentVariable(TNest4DEnvVars.CACHE_CLEANUP_INTERVAL) then
    AConfig.CleanupInterval := GetEnvironmentVariableAsInteger(TNest4DEnvVars.CACHE_CLEANUP_INTERVAL, AConfig.CleanupInterval);

  if HasEnvironmentVariable(TNest4DEnvVars.CACHE_ENABLE_COMPRESSION) then
    AConfig.EnableCompression := GetEnvironmentVariableAsBoolean(TNest4DEnvVars.CACHE_ENABLE_COMPRESSION, AConfig.EnableCompression);

  if HasEnvironmentVariable(TNest4DEnvVars.CACHE_ENABLE_COMPRESSION) then
    AConfig.EnableCompression := GetEnvironmentVariableAsBoolean(TNest4DEnvVars.CACHE_ENABLE_COMPRESSION, AConfig.EnableCompression);

  if HasEnvironmentVariable(TNest4DEnvVars.CACHE_CONNECTION_STRING) then
    AConfig.RedisConnectionString := GetEnvironmentVariable(TNest4DEnvVars.CACHE_CONNECTION_STRING, AConfig.RedisConnectionString);
end;

class procedure TNest4DEnvironmentLoader.LoadPoolFromEnvironment(var AConfig: THandlerPoolConfig);
begin
  if HasEnvironmentVariable(TNest4DEnvVars.HANDLER_POOL_MIN_SIZE) then
    AConfig.MinSize := GetEnvironmentVariableAsInteger(TNest4DEnvVars.HANDLER_POOL_MIN_SIZE, AConfig.MinSize);

  if HasEnvironmentVariable(TNest4DEnvVars.HANDLER_POOL_MAX_SIZE) then
    AConfig.MaxSize := GetEnvironmentVariableAsInteger(TNest4DEnvVars.HANDLER_POOL_MAX_SIZE, AConfig.MaxSize);

  if HasEnvironmentVariable(TNest4DEnvVars.HANDLER_POOL_IDLE_TIMEOUT) then
    AConfig.IdleTimeout := GetEnvironmentVariableAsInteger(TNest4DEnvVars.HANDLER_POOL_IDLE_TIMEOUT, AConfig.IdleTimeout);

  if HasEnvironmentVariable(TNest4DEnvVars.HANDLER_POOL_GROWTH_FACTOR) then
    AConfig.GrowthFactor := GetEnvironmentVariableAsFloat(TNest4DEnvVars.HANDLER_POOL_GROWTH_FACTOR, AConfig.GrowthFactor);

  if HasEnvironmentVariable(TNest4DEnvVars.HANDLER_POOL_SHRINK_FACTOR) then
    AConfig.ShrinkFactor := GetEnvironmentVariableAsFloat(TNest4DEnvVars.HANDLER_POOL_SHRINK_FACTOR, AConfig.ShrinkFactor);

  if HasEnvironmentVariable(TNest4DEnvVars.HANDLER_POOL_MIN_TIMEOUT_THRESHOLD) then
    AConfig.MinTimeoutThreshold := GetEnvironmentVariableAsInteger(TNest4DEnvVars.HANDLER_POOL_MIN_TIMEOUT_THRESHOLD, AConfig.MinTimeoutThreshold);

  if HasEnvironmentVariable(TNest4DEnvVars.HANDLER_POOL_MAX_TIMEOUT_THRESHOLD) then
    AConfig.MaxTimeoutThreshold := GetEnvironmentVariableAsInteger(TNest4DEnvVars.HANDLER_POOL_MAX_TIMEOUT_THRESHOLD, AConfig.MaxTimeoutThreshold);

  if HasEnvironmentVariable(TNest4DEnvVars.HANDLER_POOL_MIN_TIMEOUT_THRESHOLD) then
    AConfig.MinTimeoutThreshold := GetEnvironmentVariableAsInteger(TNest4DEnvVars.HANDLER_POOL_MIN_TIMEOUT_THRESHOLD, AConfig.MinTimeoutThreshold);

  if HasEnvironmentVariable(TNest4DEnvVars.HANDLER_POOL_MAX_TIMEOUT_THRESHOLD) then
    AConfig.MaxTimeoutThreshold := GetEnvironmentVariableAsInteger(TNest4DEnvVars.HANDLER_POOL_MAX_TIMEOUT_THRESHOLD, AConfig.MaxTimeoutThreshold);
end;

class procedure TNest4DEnvironmentLoader.LoadResilienceFromEnvironment(var AConfig: TResilienceConfig);
begin
  if HasEnvironmentVariable(TNest4DEnvVars.RESILIENCE_ENABLED) then
    AConfig.Enabled := GetEnvironmentVariableAsBoolean(TNest4DEnvVars.RESILIENCE_ENABLED, AConfig.Enabled);

  if HasEnvironmentVariable(TNest4DEnvVars.RESILIENCE_ENABLE_RETRY) then
    AConfig.EnableRetry := GetEnvironmentVariableAsBoolean(TNest4DEnvVars.RESILIENCE_ENABLE_RETRY, AConfig.EnableRetry);

  if HasEnvironmentVariable(TNest4DEnvVars.RESILIENCE_ENABLE_CIRCUIT_BREAKER) then
    AConfig.EnableCircuitBreaker := GetEnvironmentVariableAsBoolean(TNest4DEnvVars.RESILIENCE_ENABLE_CIRCUIT_BREAKER, AConfig.EnableCircuitBreaker);

  if HasEnvironmentVariable(TNest4DEnvVars.RESILIENCE_ENABLE_BULKHEAD) then
    AConfig.EnableBulkhead := GetEnvironmentVariableAsBoolean(TNest4DEnvVars.RESILIENCE_ENABLE_BULKHEAD, AConfig.EnableBulkhead);

  if HasEnvironmentVariable(TNest4DEnvVars.RESILIENCE_RETRY_ATTEMPTS) then
    AConfig.MaxRetryAttempts := GetEnvironmentVariableAsInteger(TNest4DEnvVars.RESILIENCE_RETRY_ATTEMPTS, AConfig.MaxRetryAttempts);

  if HasEnvironmentVariable(TNest4DEnvVars.RESILIENCE_RETRY_DELAY) then
    AConfig.BaseDelay := GetEnvironmentVariableAsInteger(TNest4DEnvVars.RESILIENCE_RETRY_DELAY, AConfig.BaseDelay);

  if HasEnvironmentVariable(TNest4DEnvVars.RESILIENCE_CIRCUIT_BREAKER_THRESHOLD) then
    AConfig.FailureThreshold := GetEnvironmentVariableAsInteger(TNest4DEnvVars.RESILIENCE_CIRCUIT_BREAKER_THRESHOLD, AConfig.FailureThreshold);

  if HasEnvironmentVariable(TNest4DEnvVars.RESILIENCE_CIRCUIT_BREAKER_TIMEOUT) then
    AConfig.Timeout := GetEnvironmentVariableAsInteger(TNest4DEnvVars.RESILIENCE_CIRCUIT_BREAKER_TIMEOUT, AConfig.Timeout);

  if HasEnvironmentVariable(TNest4DEnvVars.RESILIENCE_BULKHEAD_MAX_CONCURRENT) then
    AConfig.MaxConcurrentCalls := GetEnvironmentVariableAsInteger(TNest4DEnvVars.RESILIENCE_BULKHEAD_MAX_CONCURRENT, AConfig.MaxConcurrentCalls);

  if HasEnvironmentVariable(TNest4DEnvVars.RESILIENCE_BULKHEAD_QUEUE_SIZE) then
    AConfig.MaxWaitTime := GetEnvironmentVariableAsInteger(TNest4DEnvVars.RESILIENCE_BULKHEAD_QUEUE_SIZE, AConfig.MaxWaitTime);
end;

class procedure TNest4DEnvironmentLoader.LoadFrameworkFromEnvironment(var AConfig: TFrameworkConfig);
begin
  if HasEnvironmentVariable(TNest4DEnvVars.FRAMEWORK_PORT) then
    AConfig.Port := GetEnvironmentVariableAsInteger(TNest4DEnvVars.FRAMEWORK_PORT, AConfig.Port);

  if HasEnvironmentVariable(TNest4DEnvVars.FRAMEWORK_HOST) then
    AConfig.Host := GetEnvironmentVariable(TNest4DEnvVars.FRAMEWORK_HOST, AConfig.Host);

  if HasEnvironmentVariable(TNest4DEnvVars.FRAMEWORK_ENABLE_COMPRESSION) then
    AConfig.EnableCompression := GetEnvironmentVariableAsBoolean(TNest4DEnvVars.FRAMEWORK_ENABLE_COMPRESSION, AConfig.EnableCompression);

  if HasEnvironmentVariable(TNest4DEnvVars.FRAMEWORK_MAX_CONNECTIONS) then
    AConfig.MaxConnections := GetEnvironmentVariableAsInteger(TNest4DEnvVars.FRAMEWORK_MAX_CONNECTIONS, AConfig.MaxConnections);

  if HasEnvironmentVariable(TNest4DEnvVars.FRAMEWORK_TIMEOUT) then
    AConfig.Timeout := GetEnvironmentVariableAsInteger(TNest4DEnvVars.FRAMEWORK_TIMEOUT, AConfig.Timeout);

  if HasEnvironmentVariable(TNest4DEnvVars.FRAMEWORK_COMPRESSION_LEVEL) then
    AConfig.CompressionLevel := GetEnvironmentVariableAsInteger(TNest4DEnvVars.FRAMEWORK_COMPRESSION_LEVEL, AConfig.CompressionLevel);

  if HasEnvironmentVariable(TNest4DEnvVars.FRAMEWORK_ENABLE_SSL) then
    AConfig.EnableSSL := GetEnvironmentVariableAsBoolean(TNest4DEnvVars.FRAMEWORK_ENABLE_SSL, AConfig.EnableSSL);

  if HasEnvironmentVariable(TNest4DEnvVars.FRAMEWORK_SSL_CERT_FILE) then
    AConfig.SSLCertFile := GetEnvironmentVariable(TNest4DEnvVars.FRAMEWORK_SSL_CERT_FILE, AConfig.SSLCertFile);

  if HasEnvironmentVariable(TNest4DEnvVars.FRAMEWORK_SSL_KEY_FILE) then
    AConfig.SSLKeyFile := GetEnvironmentVariable(TNest4DEnvVars.FRAMEWORK_SSL_KEY_FILE, AConfig.SSLKeyFile);

  if HasEnvironmentVariable(TNest4DEnvVars.FRAMEWORK_SLOW_REQUEST_THRESHOLD) then
    AConfig.SlowRequestThresholdMs := GetEnvironmentVariableAsInteger(TNest4DEnvVars.FRAMEWORK_SLOW_REQUEST_THRESHOLD, AConfig.SlowRequestThresholdMs);
end;

class function TNest4DEnvironmentLoader.HasEnvironmentVariable(const AName: string): Boolean;
begin
  Result := System.SysUtils.GetEnvironmentVariable(AName) <> '';
end;

class function TNest4DEnvironmentLoader.ListNest4DEnvironmentVariables: TArray<string>;
var
  LList: TStringList;
  LEnvVars: TStringList;
  I: Integer;
  LVarName: string;
begin
  LList := TStringList.Create;
  LEnvVars := TStringList.Create;
  try
    // Obter todas as vari?veis de ambiente (m?todo espec?fico do Windows)
    {$IFDEF MSWINDOWS}
    // Implementa??o simplificada - usar apenas vari?veis conhecidas
    // GetEnvironmentStrings n?o ? diretamente acess?vel
    {$ENDIF}

    // Filtrar apenas as vari?veis Nest4D_*
    for I := 0 to LEnvVars.Count - 1 do
    begin
      LVarName := LEnvVars.Names[I];
      if StartsText('Nest4D_', LVarName) then
        LList.Add(LVarName);
    end;

    // Converter para array
    SetLength(Result, LList.Count);
    for I := 0 to LList.Count - 1 do
      Result[I] := LList[I];
  finally
    LList.Free;
    LEnvVars.Free;
  end;
end;

class procedure TNest4DEnvironmentLoader.SetEnvironmentVariable(const AName, AValue: string);
begin
  {$IFDEF MSWINDOWS}
  Winapi.Windows.SetEnvironmentVariable(PChar(AName), PChar(AValue));
  {$ELSE}
  setenv(PAnsiChar(AnsiString(AName)), PAnsiChar(AnsiString(AValue)), 1);
  {$ENDIF}
end;

class procedure TNest4DEnvironmentLoader.ClearEnvironmentVariable(const AName: string);
begin
  {$IFDEF MSWINDOWS}
  Winapi.Windows.SetEnvironmentVariable(PChar(AName), nil);
  {$ELSE}
  unsetenv(PAnsiChar(AnsiString(AName)));
  {$ENDIF}
end;

class function TNest4DEnvironmentLoader.ValidateEnvironmentVariables: TArray<string>;
var
  LErrors: TStringList;
  LPort: Integer;
  LValue: string;
begin
  LErrors := TStringList.Create;
  try
    // Validar portas
    if HasEnvironmentVariable(TNest4DEnvVars.FRAMEWORK_PORT) then
    begin
      LPort := GetEnvironmentVariableAsInteger(TNest4DEnvVars.FRAMEWORK_PORT, 0);
      if (LPort <= 0) or (LPort > 65535) then
        LErrors.Add(Format('Porta inv?lida em %s: %d', [TNest4DEnvVars.FRAMEWORK_PORT, LPort]));
    end;

    if HasEnvironmentVariable(TNest4DEnvVars.METRICS_PROMETHEUS_PORT) then
    begin
      LPort := GetEnvironmentVariableAsInteger(TNest4DEnvVars.METRICS_PROMETHEUS_PORT, 0);
      if (LPort <= 0) or (LPort > 65535) then
        LErrors.Add(Format('Porta inv?lida em %s: %d', [TNest4DEnvVars.METRICS_PROMETHEUS_PORT, LPort]));
    end;

    if HasEnvironmentVariable(TNest4DEnvVars.HEALTH_PORT) then
    begin
      LPort := GetEnvironmentVariableAsInteger(TNest4DEnvVars.HEALTH_PORT, 0);
      if (LPort <= 0) or (LPort > 65535) then
        LErrors.Add(Format('Porta inv?lida em %s: %d', [TNest4DEnvVars.HEALTH_PORT, LPort]));
    end;

    // Validar caminhos de arquivo
    if HasEnvironmentVariable(TNest4DEnvVars.LOGGING_FILE_PATH) then
    begin
      LValue := GetEnvironmentVariable(TNest4DEnvVars.LOGGING_FILE_PATH);
      if (LValue = '') or (Length(LValue) > 260) then
        LErrors.Add(Format('Caminho inv?lido em %s: %s', [TNest4DEnvVars.LOGGING_FILE_PATH, LValue]));
    end;

    // Validar valores num?ricos positivos
    if HasEnvironmentVariable(TNest4DEnvVars.HANDLER_POOL_MIN_SIZE) then
    begin
      LPort := GetEnvironmentVariableAsInteger(TNest4DEnvVars.HANDLER_POOL_MIN_SIZE, 0);
      if LPort <= 0 then
        LErrors.Add(Format('Valor deve ser positivo em %s: %d', [TNest4DEnvVars.HANDLER_POOL_MIN_SIZE, LPort]));
    end;

    if HasEnvironmentVariable(TNest4DEnvVars.HANDLER_POOL_MAX_SIZE) then
    begin
      LPort := GetEnvironmentVariableAsInteger(TNest4DEnvVars.HANDLER_POOL_MAX_SIZE, 0);
      if LPort <= 0 then
        LErrors.Add(Format('Valor deve ser positivo em %s: %d', [TNest4DEnvVars.HANDLER_POOL_MAX_SIZE, LPort]));
    end;

    // Converter para array
    SetLength(Result, LErrors.Count);
    for LPort := 0 to LErrors.Count - 1 do
      Result[LPort] := LErrors[LPort];
  finally
    LErrors.Free;
  end;
end;

class procedure TNest4DEnvironmentLoader.PrintEnvironmentVariables;
var
  LVars: TArray<string>;
  LVar: string;
  LValue: string;
begin
  LVars := ListNest4DEnvironmentVariables;

  WriteLn('=== Vari?veis de Ambiente Nest4D ===');
  if Length(LVars) = 0 then
  begin
    WriteLn('Nenhuma vari?vel de ambiente Nest4D encontrada.');
    Exit;
  end;

  for LVar in LVars do
  begin
    LValue := GetEnvironmentVariable(LVar);
    WriteLn(Format('%s = %s', [LVar, LValue]));
  end;
  WriteLn('=====================================');
end;

end.

