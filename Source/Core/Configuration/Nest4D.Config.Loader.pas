unit Nest4D.Config.Loader;

interface

uses
  System.SysUtils,
  System.Classes,
  System.JSON,
  System.IOUtils,
  System.StrUtils,
  System.TypInfo,
  Nest4D.Config,
  Nest4D.Logging;

type
  // Classe utilit?ria para carregamento de configura??es
  TNest4DConfigLoader = class
  private
    class function ParseLogLevel(const AValue: string): TLogLevel;
    class function ParseEnvironment(const AValue: string): TNest4DEnvironment;
    class function ParseRetryStrategy(const AValue: string): string;
    class function ParseStringArray(const AValue: string): TArray<string>;
    class function LoadLoggingFromJSON(const AJSON: TJSONObject): TLoggingConfig;
    class function LoadMetricsFromJSON(const AJSON: TJSONObject): TMetricsConfig;
    class function LoadHealthFromJSON(const AJSON: TJSONObject): THealthConfig;
    class function LoadCacheFromJSON(const AJSON: TJSONObject): TCacheConfig;
    class function LoadPoolFromJSON(const AJSON: TJSONObject): THandlerPoolConfig;
    class function LoadResilienceFromJSON(const AJSON: TJSONObject): TResilienceConfig;
    class function LoadFrameworkFromJSON(const AJSON: TJSONObject): TFrameworkConfig;
    class function LoadLoggingFromEnvironment: TLoggingConfig;
    class function LoadMetricsFromEnvironment: TMetricsConfig;
    class function LoadHealthFromEnvironment: THealthConfig;
    class function LoadCacheFromEnvironment: TCacheConfig;
    class function LoadPoolFromEnvironment: THandlerPoolConfig;
    class function LoadResilienceFromEnvironment: TResilienceConfig;
    class function LoadFrameworkFromEnvironment: TFrameworkConfig;
  public
    class function LoadFromFile(const AFileName: string): TNest4DConfig;
    class function LoadFromJSON(const AJSON: string): TNest4DConfig;
    class function LoadFromEnvironment: TNest4DConfig;
    class function SaveToFile(const AConfig: TNest4DConfig; const AFileName: string): Boolean;
    class function SaveToJSON(const AConfig: TNest4DConfig): string;
  end;

implementation

uses
  System.Variants;

{ TNest4DConfigLoader }

class function TNest4DConfigLoader.ParseLogLevel(const AValue: string): TLogLevel;
var
  LValue: string;
begin
  LValue := LowerCase(Trim(AValue));
  if LValue = 'trace' then
    Result := llTrace
  else if LValue = 'debug' then
    Result := llDebug
  else if LValue = 'info' then
    Result := llInfo
  else if LValue = 'warn' then
    Result := llWarn
  else if LValue = 'error' then
    Result := llError
  else if LValue = 'fatal' then
    Result := llFatal
  else
    Result := llInfo; // Default
end;

class function TNest4DConfigLoader.ParseEnvironment(const AValue: string): TNest4DEnvironment;
var
  LValue: string;
begin
  LValue := LowerCase(Trim(AValue));
  if LValue = 'development' then
    Result := neDevelopment
  else if LValue = 'production' then
    Result := neProduction
  else if LValue = 'test' then
    Result := neTest
  else if LValue = 'staging' then
    Result := neStaging
  else
    Result := neDevelopment; // Default
end;

class function TNest4DConfigLoader.ParseRetryStrategy(const AValue: string): string;
var
  LValue: string;
begin
  LValue := LowerCase(Trim(AValue));
  if LValue = 'fixed' then
    Result := 'fixed'
  else if LValue = 'exponential' then
    Result := 'exponential'
  else if LValue = 'linear' then
    Result := 'linear'
  else
    Result := 'exponential'; // Default
end;

class function TNest4DConfigLoader.ParseStringArray(const AValue: string): TArray<string>;
var
  LItems: TArray<string>;
  I: Integer;
begin
  if Trim(AValue) = '' then
  begin
    SetLength(Result, 0);
    Exit;
  end;

  LItems := SplitString(AValue, ',');
  SetLength(Result, Length(LItems));

  for I := 0 to High(LItems) do
    Result[I] := Trim(LItems[I]);
end;

class function TNest4DConfigLoader.LoadLoggingFromJSON(const AJSON: TJSONObject): TLoggingConfig;
var
  LLogLevel: string;
  LValue: TJSONValue;
begin
  Result := TLoggingConfig.Create;

  if AJSON.TryGetValue<Boolean>('enabled', Result.Enabled) then;
  if AJSON.TryGetValue<string>('logLevel', LLogLevel) then
    Result.Level := ParseLogLevel(LLogLevel);
  if AJSON.TryGetValue<Boolean>('logToFile', Result.LogToFile) then;
  if AJSON.TryGetValue<Boolean>('logToConsole', Result.LogToConsole) then;
  if AJSON.TryGetValue<string>('logFileName', Result.LogFileName) then;
  if AJSON.TryGetValue('maxFileSize', LValue) and TryStrToInt(LValue.Value, Result.MaxFileSize) then;
  if AJSON.TryGetValue('maxBackupFiles', LValue) and TryStrToInt(LValue.Value, Result.MaxBackupFiles) then;
  if AJSON.TryGetValue<string>('format', Result.Format) then;
  // Propriedade EnableStructuredLogging não existe em TLoggingConfig
  // if AJSON.TryGetValue<Boolean>('enableStructuredLogging', Result.EnableStructuredLogging) then;
end;

class function TNest4DConfigLoader.LoadMetricsFromJSON(const AJSON: TJSONObject): TMetricsConfig;
var
  LValue: TJSONValue;
begin
  Result := TMetricsConfig.Create;

  if AJSON.TryGetValue<Boolean>('enabled', Result.Enabled) then;
  if AJSON.TryGetValue<string>('endpoint', Result.Endpoint) then;
  if AJSON.TryGetValue('port', LValue) and TryStrToInt(LValue.Value, Result.Port) then;
  // Propriedades CollectSystemMetrics e CollectCustomMetrics não existem em TMetricsConfig
  // if AJSON.TryGetValue<Boolean>('collectSystemMetrics', Result.CollectSystemMetrics) then;
  // if AJSON.TryGetValue<Boolean>('collectCustomMetrics', Result.CollectCustomMetrics) then;
  if AJSON.TryGetValue('metricsInterval', LValue) and TryStrToInt(LValue.Value, Result.MetricsInterval) then;
  if AJSON.TryGetValue<Boolean>('enablePrometheus', Result.EnablePrometheus) then;
  if AJSON.TryGetValue<string>('prometheusEndpoint', Result.PrometheusEndpoint) then;
  if AJSON.TryGetValue('maxMetricsHistory', LValue) and TryStrToInt(LValue.Value, Result.MaxMetricsHistory) then;
end;

class function TNest4DConfigLoader.LoadHealthFromJSON(const AJSON: TJSONObject): THealthConfig;
var
  LValue: TJSONValue;
begin
  Result := THealthConfig.Create;

  if AJSON.TryGetValue<Boolean>('enabled', Result.Enabled) then;
  if AJSON.TryGetValue<string>('endpoint', Result.Endpoint) then;
  if AJSON.TryGetValue('port', LValue) and TryStrToInt(LValue.Value, Result.Port) then;
  if AJSON.TryGetValue('checkInterval', LValue) and TryStrToInt(LValue.Value, Result.CheckInterval) then;
  if AJSON.TryGetValue('timeout', LValue) and TryStrToInt(LValue.Value, Result.Timeout) then;
  if AJSON.TryGetValue<Boolean>('enableDatabaseCheck', Result.EnableDatabaseCheck) then;
  // Propriedades EnableExternalServiceCheck e ExternalServices não existem em THealthConfig
  // if AJSON.TryGetValue<Boolean>('enableExternalServiceCheck', Result.EnableExternalServiceCheck) then;
  if AJSON.TryGetValue<string>('databaseConnectionString', Result.DatabaseConnectionString) then;

  // Propriedade ExternalServices não existe em THealthConfig
  // if AJSON.TryGetValue<TJSONArray>('externalServices', LServicesArray) then
  // begin
  //   SetLength(Result.ExternalServices, LServicesArray.Count);
  //   for I := 0 to LServicesArray.Count - 1 do
  //     Result.ExternalServices[I] := LServicesArray.Items[I].Value;
  // end;
end;

class function TNest4DConfigLoader.LoadCacheFromJSON(const AJSON: TJSONObject): TCacheConfig;
var
  LValue: TJSONValue;
begin
  Result := TCacheConfig.Create;

  // Propriedade Enabled não existe em TCacheConfig
  // if AJSON.TryGetValue<Boolean>('enabled', Result.Enabled) then;
  if AJSON.TryGetValue<Boolean>('enableTTL', Result.EnableTTL) then;
  if AJSON.TryGetValue('defaultTTL', LValue) and TryStrToInt(LValue.Value, Result.DefaultTTL) then;
  if AJSON.TryGetValue('maxCacheSize', LValue) and TryStrToInt(LValue.Value, Result.MaxCacheSize) then;
  if AJSON.TryGetValue<Boolean>('enableBackgroundCleanup', Result.EnableBackgroundCleanup) then;
  if AJSON.TryGetValue('cleanupInterval', LValue) and TryStrToInt(LValue.Value, Result.CleanupInterval) then;
  if AJSON.TryGetValue<Boolean>('enableCompression', Result.EnableCompression) then;
  if AJSON.TryGetValue('compressionThreshold', LValue) and TryStrToInt(LValue.Value, Result.CompressionThreshold) then;
  if AJSON.TryGetValue<Boolean>('enableDistributedCache', Result.EnableDistributedCache) then;
  if AJSON.TryGetValue<string>('redisConnectionString', Result.RedisConnectionString) then;
end;

class function TNest4DConfigLoader.LoadPoolFromJSON(const AJSON: TJSONObject): THandlerPoolConfig;
var
  LValue: TJSONValue;
begin
  Result := THandlerPoolConfig.Create;

  if AJSON.TryGetValue('minSize', LValue) and TryStrToInt(LValue.Value, Result.MinSize) then;
  if AJSON.TryGetValue('maxSize', LValue) and TryStrToInt(LValue.Value, Result.MaxSize) then;
  if AJSON.TryGetValue('idleTimeout', LValue) and TryStrToInt(LValue.Value, Result.IdleTimeout) then;
  if AJSON.TryGetValue<Double>('growthFactor', Result.GrowthFactor) then;
  if AJSON.TryGetValue<Double>('shrinkFactor', Result.ShrinkFactor) then;
  // Propriedade CleanupInterval não existe em THandlerPoolConfig
  // if AJSON.TryGetValue<Integer>('cleanupInterval', Result.CleanupInterval) then;
  if AJSON.TryGetValue('minTimeoutThreshold', LValue) and TryStrToInt(LValue.Value, Result.MinTimeoutThreshold) then;
  if AJSON.TryGetValue('maxTimeoutThreshold', LValue) and TryStrToInt(LValue.Value, Result.MaxTimeoutThreshold) then;
  // Propriedade EnableAutoScaling não existe em THandlerPoolConfig
  // if AJSON.TryGetValue<Boolean>('enableAutoScaling', Result.EnableAutoScaling) then;
end;

class function TNest4DConfigLoader.LoadResilienceFromJSON(const AJSON: TJSONObject): TResilienceConfig;
var
  LValue: TJSONValue;
begin
  Result := TResilienceConfig.Create;

  if AJSON.TryGetValue<Boolean>('enabled', Result.Enabled) then;
  if AJSON.TryGetValue<string>('endpoint', Result.Endpoint) then;
  if AJSON.TryGetValue<Boolean>('enableRetry', Result.EnableRetry) then;
  if AJSON.TryGetValue('maxRetryAttempts', LValue) and TryStrToInt(LValue.Value, Result.MaxRetryAttempts) then;
  if AJSON.TryGetValue('baseDelay', LValue) and TryStrToInt(LValue.Value, Result.BaseDelay) then;
  if AJSON.TryGetValue('maxDelay', LValue) and TryStrToInt(LValue.Value, Result.MaxDelay) then;
  if AJSON.TryGetValue<Boolean>('enableCircuitBreaker', Result.EnableCircuitBreaker) then;
  if AJSON.TryGetValue('failureThreshold', LValue) and TryStrToInt(LValue.Value, Result.FailureThreshold) then;
  if AJSON.TryGetValue('successThreshold', LValue) and TryStrToInt(LValue.Value, Result.SuccessThreshold) then;
  if AJSON.TryGetValue('timeout', LValue) and TryStrToInt(LValue.Value, Result.Timeout) then;
  if AJSON.TryGetValue<Boolean>('enableBulkhead', Result.EnableBulkhead) then;
  if AJSON.TryGetValue('maxConcurrentCalls', LValue) and TryStrToInt(LValue.Value, Result.MaxConcurrentCalls) then;
  if AJSON.TryGetValue('maxWaitTime', LValue) and TryStrToInt(LValue.Value, Result.MaxWaitTime) then;
end;

class function TNest4DConfigLoader.LoadFrameworkFromJSON(const AJSON: TJSONObject): TFrameworkConfig;
var
  LValue: TJSONValue;
begin
  Result := TFrameworkConfig.Create;

  if AJSON.TryGetValue('port', LValue) and TryStrToInt(LValue.Value, Result.Port) then;
  if AJSON.TryGetValue<string>('host', Result.Host) then;
  if AJSON.TryGetValue('maxConnections', LValue) and TryStrToInt(LValue.Value, Result.MaxConnections) then;
  if AJSON.TryGetValue('timeout', LValue) and TryStrToInt(LValue.Value, Result.Timeout) then;
  if AJSON.TryGetValue<Boolean>('enableCompression', Result.EnableCompression) then;
  if AJSON.TryGetValue('compressionLevel', LValue) and TryStrToInt(LValue.Value, Result.CompressionLevel) then;
  if AJSON.TryGetValue<Boolean>('enableSSL', Result.EnableSSL) then;
  if AJSON.TryGetValue<string>('sslCertFile', Result.SSLCertFile) then;
  if AJSON.TryGetValue<string>('sslKeyFile', Result.SSLKeyFile) then;
  if AJSON.TryGetValue('slowRequestThresholdMs', LValue) and TryStrToInt(LValue.Value, Result.SlowRequestThresholdMs) then;
end;

class function TNest4DConfigLoader.LoadLoggingFromEnvironment: TLoggingConfig;
begin
  Result := TLoggingConfig.Create;

  Result.Enabled := StrToBoolDef(GetEnvironmentVariable('NEST4D_LOGGING_ENABLED'), Result.Enabled);
  Result.Level := ParseLogLevel(GetEnvironmentVariable('NEST4D_LOGGING_LEVEL'));
  Result.LogToFile := StrToBoolDef(GetEnvironmentVariable('NEST4D_LOGGING_TO_FILE'), Result.LogToFile);
  Result.LogToConsole := StrToBoolDef(GetEnvironmentVariable('NEST4D_LOGGING_TO_CONSOLE'), Result.LogToConsole);
  
  if GetEnvironmentVariable('NEST4D_LOGGING_FILENAME') <> '' then
    Result.LogFileName := GetEnvironmentVariable('NEST4D_LOGGING_FILENAME');
    
  if GetEnvironmentVariable('NEST4D_LOGGING_MAX_FILE_SIZE') <> '' then
    Result.MaxFileSize := StrToInt64Def(GetEnvironmentVariable('NEST4D_LOGGING_MAX_FILE_SIZE'), Result.MaxFileSize);
    
  if GetEnvironmentVariable('NEST4D_LOGGING_MAX_BACKUP_FILES') <> '' then
    Result.MaxBackupFiles := StrToIntDef(GetEnvironmentVariable('NEST4D_LOGGING_MAX_BACKUP_FILES'), Result.MaxBackupFiles);
    
  if GetEnvironmentVariable('NEST4D_LOGGING_FORMAT') <> '' then
    Result.Format := GetEnvironmentVariable('NEST4D_LOGGING_FORMAT');
    
  // Propriedade EnableStructuredLogging não existe em TLoggingConfig
  // Result.EnableStructuredLogging := StrToBoolDef(GetEnvironmentVariable('NEST4D_LOGGING_STRUCTURED'), Result.EnableStructuredLogging);
end;

class function TNest4DConfigLoader.LoadMetricsFromEnvironment: TMetricsConfig;
begin
  Result := TMetricsConfig.Create;

  Result.Enabled := StrToBoolDef(GetEnvironmentVariable('NEST4D_METRICS_ENABLED'), Result.Enabled);
  
  if GetEnvironmentVariable('NEST4D_METRICS_ENDPOINT') <> '' then
    Result.Endpoint := GetEnvironmentVariable('NEST4D_METRICS_ENDPOINT');
    
  if GetEnvironmentVariable('NEST4D_METRICS_PORT') <> '' then
    Result.Port := StrToIntDef(GetEnvironmentVariable('NEST4D_METRICS_PORT'), Result.Port);
    
  // Propriedades CollectSystemMetrics e CollectCustomMetrics não existem em TMetricsConfig
  
  if GetEnvironmentVariable('NEST4D_METRICS_INTERVAL') <> '' then
    Result.MetricsInterval := StrToIntDef(GetEnvironmentVariable('NEST4D_METRICS_INTERVAL'), Result.MetricsInterval);
    
  Result.EnablePrometheus := StrToBoolDef(GetEnvironmentVariable('NEST4D_METRICS_PROMETHEUS'), Result.EnablePrometheus);
  
  if GetEnvironmentVariable('NEST4D_METRICS_PROMETHEUS_ENDPOINT') <> '' then
    Result.PrometheusEndpoint := GetEnvironmentVariable('NEST4D_METRICS_PROMETHEUS_ENDPOINT');
    
  if GetEnvironmentVariable('NEST4D_METRICS_MAX_HISTORY') <> '' then
    Result.MaxMetricsHistory := StrToIntDef(GetEnvironmentVariable('NEST4D_METRICS_MAX_HISTORY'), Result.MaxMetricsHistory);
end;

class function TNest4DConfigLoader.LoadHealthFromEnvironment: THealthConfig;
begin
  Result := THealthConfig.Create;

  Result.Enabled := StrToBoolDef(GetEnvironmentVariable('NEST4D_HEALTH_ENABLED'), Result.Enabled);
  
  if GetEnvironmentVariable('NEST4D_HEALTH_ENDPOINT') <> '' then
    Result.Endpoint := GetEnvironmentVariable('NEST4D_HEALTH_ENDPOINT');
    
  if GetEnvironmentVariable('NEST4D_HEALTH_PORT') <> '' then
    Result.Port := StrToIntDef(GetEnvironmentVariable('NEST4D_HEALTH_PORT'), Result.Port);
    
  if GetEnvironmentVariable('NEST4D_HEALTH_CHECK_INTERVAL') <> '' then
    Result.CheckInterval := StrToIntDef(GetEnvironmentVariable('NEST4D_HEALTH_CHECK_INTERVAL'), Result.CheckInterval);
    
  if GetEnvironmentVariable('NEST4D_HEALTH_TIMEOUT') <> '' then
    Result.Timeout := StrToIntDef(GetEnvironmentVariable('NEST4D_HEALTH_TIMEOUT'), Result.Timeout);
    
  Result.EnableDatabaseCheck := StrToBoolDef(GetEnvironmentVariable('NEST4D_HEALTH_DATABASE_CHECK'), Result.EnableDatabaseCheck);
  // Propriedade EnableExternalServiceCheck não existe em THealthConfig
  // Result.EnableExternalServiceCheck := StrToBoolDef(GetEnvironmentVariable('NEST4D_HEALTH_EXTERNAL_CHECK'), Result.EnableExternalServiceCheck);
  
  if GetEnvironmentVariable('NEST4D_HEALTH_DATABASE_CONNECTION') <> '' then
    Result.DatabaseConnectionString := GetEnvironmentVariable('NEST4D_HEALTH_DATABASE_CONNECTION');
    
  // Propriedades ExternalServices e EnableExternalServiceCheck não existem em THealthConfig
  // if GetEnvironmentVariable('NEST4D_HEALTH_EXTERNAL_SERVICES') <> '' then
  //   Result.ExternalServices := ParseStringArray(GetEnvironmentVariable('NEST4D_HEALTH_EXTERNAL_SERVICES'));
end;

class function TNest4DConfigLoader.LoadCacheFromEnvironment: TCacheConfig;
begin
  Result := TCacheConfig.Create;

  // Propriedade Enabled não existe em TCacheConfig
  // Result.Enabled := StrToBoolDef(GetEnvironmentVariable('NEST4D_CACHE_ENABLED'), Result.Enabled);
  Result.EnableTTL := StrToBoolDef(GetEnvironmentVariable('NEST4D_CACHE_TTL_ENABLED'), Result.EnableTTL);
  
  if GetEnvironmentVariable('NEST4D_CACHE_DEFAULT_TTL') <> '' then
    Result.DefaultTTL := StrToIntDef(GetEnvironmentVariable('NEST4D_CACHE_DEFAULT_TTL'), Result.DefaultTTL);
    
  if GetEnvironmentVariable('NEST4D_CACHE_MAX_SIZE') <> '' then
    Result.MaxCacheSize := StrToInt64Def(GetEnvironmentVariable('NEST4D_CACHE_MAX_SIZE'), Result.MaxCacheSize);
    
  Result.EnableBackgroundCleanup := StrToBoolDef(GetEnvironmentVariable('NEST4D_CACHE_BACKGROUND_CLEANUP'), Result.EnableBackgroundCleanup);
  
  if GetEnvironmentVariable('NEST4D_CACHE_CLEANUP_INTERVAL') <> '' then
    Result.CleanupInterval := StrToIntDef(GetEnvironmentVariable('NEST4D_CACHE_CLEANUP_INTERVAL'), Result.CleanupInterval);
    
  Result.EnableCompression := StrToBoolDef(GetEnvironmentVariable('NEST4D_CACHE_COMPRESSION'), Result.EnableCompression);
  
  if GetEnvironmentVariable('NEST4D_CACHE_COMPRESSION_THRESHOLD') <> '' then
    Result.CompressionThreshold := StrToIntDef(GetEnvironmentVariable('NEST4D_CACHE_COMPRESSION_THRESHOLD'), Result.CompressionThreshold);
    
  Result.EnableDistributedCache := StrToBoolDef(GetEnvironmentVariable('NEST4D_CACHE_DISTRIBUTED'), Result.EnableDistributedCache);
  
  if GetEnvironmentVariable('NEST4D_CACHE_REDIS_CONNECTION') <> '' then
    Result.RedisConnectionString := GetEnvironmentVariable('NEST4D_CACHE_REDIS_CONNECTION');
end;

class function TNest4DConfigLoader.LoadPoolFromEnvironment: THandlerPoolConfig;
begin
  Result := THandlerPoolConfig.Create;

  if GetEnvironmentVariable('NEST4D_HANDLER_POOL_MIN_SIZE') <> '' then
    Result.MinSize := StrToIntDef(GetEnvironmentVariable('NEST4D_HANDLER_POOL_MIN_SIZE'), Result.MinSize);
    
  if GetEnvironmentVariable('NEST4D_HANDLER_POOL_MAX_SIZE') <> '' then
    Result.MaxSize := StrToIntDef(GetEnvironmentVariable('NEST4D_HANDLER_POOL_MAX_SIZE'), Result.MaxSize);
    
  if GetEnvironmentVariable('NEST4D_HANDLER_POOL_IDLE_TIMEOUT') <> '' then
    Result.IdleTimeout := StrToIntDef(GetEnvironmentVariable('NEST4D_HANDLER_POOL_IDLE_TIMEOUT'), Result.IdleTimeout);
    
  if GetEnvironmentVariable('NEST4D_HANDLER_POOL_GROWTH_FACTOR') <> '' then
    Result.GrowthFactor := StrToFloatDef(GetEnvironmentVariable('NEST4D_HANDLER_POOL_GROWTH_FACTOR'), Result.GrowthFactor);
    
  if GetEnvironmentVariable('NEST4D_HANDLER_POOL_SHRINK_FACTOR') <> '' then
    Result.ShrinkFactor := StrToFloatDef(GetEnvironmentVariable('NEST4D_HANDLER_POOL_SHRINK_FACTOR'), Result.ShrinkFactor);
    
  // Propriedades CleanupInterval e EnableAutoScaling não existem em THandlerPoolConfig
    
  if GetEnvironmentVariable('NEST4D_HANDLER_POOL_MIN_TIMEOUT_THRESHOLD') <> '' then
    Result.MinTimeoutThreshold := StrToIntDef(GetEnvironmentVariable('NEST4D_HANDLER_POOL_MIN_TIMEOUT_THRESHOLD'), Result.MinTimeoutThreshold);
    
  if GetEnvironmentVariable('NEST4D_HANDLER_POOL_MAX_TIMEOUT_THRESHOLD') <> '' then
    Result.MaxTimeoutThreshold := StrToIntDef(GetEnvironmentVariable('NEST4D_HANDLER_POOL_MAX_TIMEOUT_THRESHOLD'), Result.MaxTimeoutThreshold);
end;

class function TNest4DConfigLoader.LoadResilienceFromEnvironment: TResilienceConfig;
begin
  Result := TResilienceConfig.Create;

  Result.Enabled := StrToBoolDef(GetEnvironmentVariable('NEST4D_RESILIENCE_ENABLED'), Result.Enabled);
  
  if GetEnvironmentVariable('NEST4D_RESILIENCE_ENDPOINT') <> '' then
    Result.Endpoint := GetEnvironmentVariable('NEST4D_RESILIENCE_ENDPOINT');
    
  Result.EnableRetry := StrToBoolDef(GetEnvironmentVariable('NEST4D_RESILIENCE_RETRY'), Result.EnableRetry);
  
  Result.MaxRetryAttempts := StrToIntDef(GetEnvironmentVariable('NEST4D_RESILIENCE_MAX_RETRIES'), Result.MaxRetryAttempts);
  
  if GetEnvironmentVariable('NEST4D_RESILIENCE_BASE_DELAY') <> '' then
    Result.BaseDelay := StrToIntDef(GetEnvironmentVariable('NEST4D_RESILIENCE_BASE_DELAY'), Result.BaseDelay);
    
  if GetEnvironmentVariable('NEST4D_RESILIENCE_MAX_DELAY') <> '' then
    Result.MaxDelay := StrToIntDef(GetEnvironmentVariable('NEST4D_RESILIENCE_MAX_DELAY'), Result.MaxDelay);
  
  Result.EnableCircuitBreaker := StrToBoolDef(GetEnvironmentVariable('NEST4D_RESILIENCE_CIRCUIT_BREAKER'), Result.EnableCircuitBreaker);
  Result.FailureThreshold := StrToIntDef(GetEnvironmentVariable('NEST4D_RESILIENCE_FAILURE_THRESHOLD'), Result.FailureThreshold);
  Result.SuccessThreshold := StrToIntDef(GetEnvironmentVariable('NEST4D_RESILIENCE_SUCCESS_THRESHOLD'), Result.SuccessThreshold);
  
  if GetEnvironmentVariable('NEST4D_RESILIENCE_TIMEOUT') <> '' then
    Result.Timeout := StrToIntDef(GetEnvironmentVariable('NEST4D_RESILIENCE_TIMEOUT'), Result.Timeout);
    
  Result.EnableBulkhead := StrToBoolDef(GetEnvironmentVariable('NEST4D_RESILIENCE_BULKHEAD'), Result.EnableBulkhead);
  Result.MaxConcurrentCalls := StrToIntDef(GetEnvironmentVariable('NEST4D_RESILIENCE_MAX_CONCURRENT'), Result.MaxConcurrentCalls);
  Result.MaxWaitTime := StrToIntDef(GetEnvironmentVariable('NEST4D_RESILIENCE_MAX_WAIT'), Result.MaxWaitTime);
end;

class function TNest4DConfigLoader.LoadFrameworkFromEnvironment: TFrameworkConfig;
begin
  Result := TFrameworkConfig.Create;

  if GetEnvironmentVariable('NEST4D_FRAMEWORK_HOST') <> '' then
    Result.Host := GetEnvironmentVariable('NEST4D_FRAMEWORK_HOST');
    
  if GetEnvironmentVariable('NEST4D_FRAMEWORK_PORT') <> '' then
    Result.Port := StrToIntDef(GetEnvironmentVariable('NEST4D_FRAMEWORK_PORT'), Result.Port);
    
  if GetEnvironmentVariable('NEST4D_FRAMEWORK_MAX_CONNECTIONS') <> '' then
    Result.MaxConnections := StrToIntDef(GetEnvironmentVariable('NEST4D_FRAMEWORK_MAX_CONNECTIONS'), Result.MaxConnections);
    
  if GetEnvironmentVariable('NEST4D_FRAMEWORK_TIMEOUT') <> '' then
    Result.Timeout := StrToIntDef(GetEnvironmentVariable('NEST4D_FRAMEWORK_TIMEOUT'), Result.Timeout);
    
  if GetEnvironmentVariable('NEST4D_FRAMEWORK_COMPRESSION') <> '' then
    Result.EnableCompression := StrToBoolDef(GetEnvironmentVariable('NEST4D_FRAMEWORK_COMPRESSION'), Result.EnableCompression);
    
  if GetEnvironmentVariable('NEST4D_FRAMEWORK_COMPRESSION_LEVEL') <> '' then
    Result.CompressionLevel := StrToIntDef(GetEnvironmentVariable('NEST4D_FRAMEWORK_COMPRESSION_LEVEL'), Result.CompressionLevel);
    
  if GetEnvironmentVariable('NEST4D_FRAMEWORK_SSL') <> '' then
    Result.EnableSSL := StrToBoolDef(GetEnvironmentVariable('NEST4D_FRAMEWORK_SSL'), Result.EnableSSL);
  
  if GetEnvironmentVariable('NEST4D_FRAMEWORK_SSL_CERT') <> '' then
    Result.SSLCertFile := GetEnvironmentVariable('NEST4D_FRAMEWORK_SSL_CERT');
    
  if GetEnvironmentVariable('NEST4D_FRAMEWORK_SSL_KEY') <> '' then
    Result.SSLKeyFile := GetEnvironmentVariable('NEST4D_FRAMEWORK_SSL_KEY');
    
  if GetEnvironmentVariable('NEST4D_FRAMEWORK_SLOW_THRESHOLD') <> '' then
    Result.SlowRequestThresholdMs := StrToIntDef(GetEnvironmentVariable('NEST4D_FRAMEWORK_SLOW_THRESHOLD'), Result.SlowRequestThresholdMs);
end;

class function TNest4DConfigLoader.LoadFromFile(const AFileName: string): TNest4DConfig;
var
  LJSONString: string;
begin
  if not TFile.Exists(AFileName) then
    raise Exception.CreateFmt('Configuration file not found: %s', [AFileName]);
    
  LJSONString := TFile.ReadAllText(AFileName, TEncoding.UTF8);
  Result := LoadFromJSON(LJSONString);
end;

class function TNest4DConfigLoader.LoadFromJSON(const AJSON: string): TNest4DConfig;
var
  LJSONObject: TJSONObject;
  LLoggingJSON, LMetricsJSON, LHealthJSON, LCacheJSON, LPoolJSON, LResilienceJSON, LFrameworkJSON: TJSONObject;
begin
  Result := TNest4DConfig.Create;
  
  LJSONObject := TJSONObject.ParseJSONValue(AJSON) as TJSONObject;
  if not Assigned(LJSONObject) then
    raise Exception.Create('Invalid JSON configuration');
    
  try
    if LJSONObject.TryGetValue<TJSONObject>('logging', LLoggingJSON) then
      Result.Logging := LoadLoggingFromJSON(LLoggingJSON);
      
    if LJSONObject.TryGetValue<TJSONObject>('metrics', LMetricsJSON) then
      Result.Metrics := LoadMetricsFromJSON(LMetricsJSON);
      
    if LJSONObject.TryGetValue<TJSONObject>('health', LHealthJSON) then
      Result.Health := LoadHealthFromJSON(LHealthJSON);
      
    if LJSONObject.TryGetValue<TJSONObject>('cache', LCacheJSON) then
      Result.Cache := LoadCacheFromJSON(LCacheJSON);
      
    if LJSONObject.TryGetValue<TJSONObject>('pool', LPoolJSON) then
      Result.HandlerPool := LoadPoolFromJSON(LPoolJSON);
      
    if LJSONObject.TryGetValue<TJSONObject>('resilience', LResilienceJSON) then
      Result.Resilience := LoadResilienceFromJSON(LResilienceJSON);
      
    if LJSONObject.TryGetValue<TJSONObject>('framework', LFrameworkJSON) then
      Result.Framework := LoadFrameworkFromJSON(LFrameworkJSON);
  finally
    LJSONObject.Free;
  end;
end;

class function TNest4DConfigLoader.LoadFromEnvironment: TNest4DConfig;
begin
  Result := TNest4DConfig.Create;
  
  Result.Logging := LoadLoggingFromEnvironment;
  Result.Metrics := LoadMetricsFromEnvironment;
  Result.Health := LoadHealthFromEnvironment;
  Result.Cache := LoadCacheFromEnvironment;
  Result.HandlerPool := LoadPoolFromEnvironment;
  Result.Resilience := LoadResilienceFromEnvironment;
  Result.Framework := LoadFrameworkFromEnvironment;
end;

class function TNest4DConfigLoader.SaveToFile(const AConfig: TNest4DConfig; const AFileName: string): Boolean;
var
  LJSONString: string;
begin
  try
    LJSONString := SaveToJSON(AConfig);
    TFile.WriteAllText(AFileName, LJSONString, TEncoding.UTF8);
    Result := True;
  except
    Result := False;
  end;
end;

class function TNest4DConfigLoader.SaveToJSON(const AConfig: TNest4DConfig): string;
var
  LJSONObject: TJSONObject;
  LLoggingJSON, LMetricsJSON, LHealthJSON, LCacheJSON, LPoolJSON, LResilienceJSON, LFrameworkJSON: TJSONObject;
  I: Integer;
  LServicesArray: TJSONArray;
begin
  LJSONObject := TJSONObject.Create;
  try
    // Logging configuration
    LLoggingJSON := TJSONObject.Create;
    LLoggingJSON.AddPair('enabled', TJSONBool.Create(AConfig.Logging.Enabled));
    LLoggingJSON.AddPair('logLevel', TJSONString.Create(GetEnumName(TypeInfo(TLogLevel), Ord(AConfig.Logging.Level))));
    LLoggingJSON.AddPair('logToFile', TJSONBool.Create(AConfig.Logging.LogToFile));
    LLoggingJSON.AddPair('logToConsole', TJSONBool.Create(AConfig.Logging.LogToConsole));
    LLoggingJSON.AddPair('logFileName', AConfig.Logging.LogFileName);
    LLoggingJSON.AddPair('maxFileSize', TJSONNumber.Create(AConfig.Logging.MaxFileSize));
    LLoggingJSON.AddPair('maxBackupFiles', TJSONNumber.Create(AConfig.Logging.MaxBackupFiles));
    LLoggingJSON.AddPair('format', AConfig.Logging.Format);
    // Propriedade EnableStructuredLogging não existe em TLoggingConfig
    // LLoggingJSON.AddPair('enableStructuredLogging', TJSONBool.Create(AConfig.Logging.EnableStructuredLogging));
    LJSONObject.AddPair('logging', LLoggingJSON);
    
    // Metrics configuration
    LMetricsJSON := TJSONObject.Create;
    LMetricsJSON.AddPair('enabled', TJSONBool.Create(AConfig.Metrics.Enabled));
    LMetricsJSON.AddPair('endpoint', AConfig.Metrics.Endpoint);
    LMetricsJSON.AddPair('port', TJSONNumber.Create(AConfig.Metrics.Port));
    // Propriedades CollectSystemMetrics e CollectCustomMetrics não existem em TMetricsConfig
    // LMetricsJSON.AddPair('collectSystemMetrics', TJSONBool.Create(AConfig.Metrics.CollectSystemMetrics));
    // LMetricsJSON.AddPair('collectCustomMetrics', TJSONBool.Create(AConfig.Metrics.CollectCustomMetrics));
    LMetricsJSON.AddPair('metricsInterval', TJSONNumber.Create(AConfig.Metrics.MetricsInterval));
    LMetricsJSON.AddPair('enablePrometheus', TJSONBool.Create(AConfig.Metrics.EnablePrometheus));
    LMetricsJSON.AddPair('prometheusEndpoint', AConfig.Metrics.PrometheusEndpoint);
    LMetricsJSON.AddPair('maxMetricsHistory', TJSONNumber.Create(AConfig.Metrics.MaxMetricsHistory));
    LJSONObject.AddPair('metrics', LMetricsJSON);
    
    // Health configuration
    LHealthJSON := TJSONObject.Create;
    LHealthJSON.AddPair('enabled', TJSONBool.Create(AConfig.Health.Enabled));
    LHealthJSON.AddPair('endpoint', AConfig.Health.Endpoint);
    LHealthJSON.AddPair('port', TJSONNumber.Create(AConfig.Health.Port));
    LHealthJSON.AddPair('checkInterval', TJSONNumber.Create(AConfig.Health.CheckInterval));
    LHealthJSON.AddPair('timeout', TJSONNumber.Create(AConfig.Health.Timeout));
    LHealthJSON.AddPair('enableDatabaseCheck', TJSONBool.Create(AConfig.Health.EnableDatabaseCheck));
    LHealthJSON.AddPair('databaseConnectionString', AConfig.Health.DatabaseConnectionString);
    
    // Propriedades EnableExternalServiceCheck e ExternalServices não existem em THealthConfig
    // LHealthJSON.AddPair('enableExternalServiceCheck', TJSONBool.Create(AConfig.Health.EnableExternalServiceCheck));
    // LServicesArray := TJSONArray.Create;
    // for I := 0 to High(AConfig.Health.ExternalServices) do
    //   LServicesArray.AddElement(TJSONString.Create(AConfig.Health.ExternalServices[I]));
    // LHealthJSON.AddPair('externalServices', LServicesArray);
    LJSONObject.AddPair('health', LHealthJSON);
    
    // Cache configuration
    LCacheJSON := TJSONObject.Create;
    // Propriedade Enabled não existe em TCacheConfig
    // LCacheJSON.AddPair('enabled', TJSONBool.Create(AConfig.Cache.Enabled));
    LCacheJSON.AddPair('enableTTL', TJSONBool.Create(AConfig.Cache.EnableTTL));
    LCacheJSON.AddPair('defaultTTL', TJSONNumber.Create(AConfig.Cache.DefaultTTL));
    LCacheJSON.AddPair('maxCacheSize', TJSONNumber.Create(AConfig.Cache.MaxCacheSize));
    LCacheJSON.AddPair('enableBackgroundCleanup', TJSONBool.Create(AConfig.Cache.EnableBackgroundCleanup));
    LCacheJSON.AddPair('cleanupInterval', TJSONNumber.Create(AConfig.Cache.CleanupInterval));
    LCacheJSON.AddPair('enableCompression', TJSONBool.Create(AConfig.Cache.EnableCompression));
    LCacheJSON.AddPair('compressionThreshold', TJSONNumber.Create(AConfig.Cache.CompressionThreshold));
    LCacheJSON.AddPair('enableDistributedCache', TJSONBool.Create(AConfig.Cache.EnableDistributedCache));
    LCacheJSON.AddPair('redisConnectionString', AConfig.Cache.RedisConnectionString);
    LJSONObject.AddPair('cache', LCacheJSON);
    
    // Pool configuration
    LPoolJSON := TJSONObject.Create;
    LPoolJSON.AddPair('minSize', TJSONNumber.Create(AConfig.HandlerPool.MinSize));
    LPoolJSON.AddPair('maxSize', TJSONNumber.Create(AConfig.HandlerPool.MaxSize));
    LPoolJSON.AddPair('idleTimeout', TJSONNumber.Create(AConfig.HandlerPool.IdleTimeout));
    LPoolJSON.AddPair('growthFactor', TJSONNumber.Create(AConfig.HandlerPool.GrowthFactor));
    LPoolJSON.AddPair('shrinkFactor', TJSONNumber.Create(AConfig.HandlerPool.ShrinkFactor));
    // Propriedades CleanupInterval, MinTimeoutThreshold, MaxTimeoutThreshold e EnableAutoScaling não existem em THandlerPoolConfig
    // LPoolJSON.AddPair('cleanupInterval', TJSONNumber.Create(AConfig.HandlerPool.CleanupInterval));
    // LPoolJSON.AddPair('minTimeoutThreshold', TJSONNumber.Create(AConfig.HandlerPool.MinTimeoutThreshold));
    // LPoolJSON.AddPair('maxTimeoutThreshold', TJSONNumber.Create(AConfig.HandlerPool.MaxTimeoutThreshold));
    // LPoolJSON.AddPair('enableAutoScaling', TJSONBool.Create(AConfig.HandlerPool.EnableAutoScaling));
    LJSONObject.AddPair('pool', LPoolJSON);
    
    // Resilience configuration
    LResilienceJSON := TJSONObject.Create;
    LResilienceJSON.AddPair('enabled', TJSONBool.Create(AConfig.Resilience.Enabled));
    LResilienceJSON.AddPair('endpoint', AConfig.Resilience.Endpoint);
    LResilienceJSON.AddPair('enableRetry', TJSONBool.Create(AConfig.Resilience.EnableRetry));
    LResilienceJSON.AddPair('maxRetryAttempts', TJSONNumber.Create(AConfig.Resilience.MaxRetryAttempts));
    LResilienceJSON.AddPair('baseDelay', TJSONNumber.Create(AConfig.Resilience.BaseDelay));
    LResilienceJSON.AddPair('maxDelay', TJSONNumber.Create(AConfig.Resilience.MaxDelay));
    LResilienceJSON.AddPair('enableCircuitBreaker', TJSONBool.Create(AConfig.Resilience.EnableCircuitBreaker));
    LResilienceJSON.AddPair('failureThreshold', TJSONNumber.Create(AConfig.Resilience.FailureThreshold));
    LResilienceJSON.AddPair('successThreshold', TJSONNumber.Create(AConfig.Resilience.SuccessThreshold));
    LResilienceJSON.AddPair('timeout', TJSONNumber.Create(AConfig.Resilience.Timeout));
    LResilienceJSON.AddPair('enableBulkhead', TJSONBool.Create(AConfig.Resilience.EnableBulkhead));
    LResilienceJSON.AddPair('maxConcurrentCalls', TJSONNumber.Create(AConfig.Resilience.MaxConcurrentCalls));
    LResilienceJSON.AddPair('maxWaitTime', TJSONNumber.Create(AConfig.Resilience.MaxWaitTime));
    LJSONObject.AddPair('resilience', LResilienceJSON);
    
    // Framework configuration
    LFrameworkJSON := TJSONObject.Create;
    LFrameworkJSON.AddPair('host', TJSONString.Create(AConfig.Framework.Host));
    LFrameworkJSON.AddPair('port', TJSONNumber.Create(AConfig.Framework.Port));
    LFrameworkJSON.AddPair('maxConnections', TJSONNumber.Create(AConfig.Framework.MaxConnections));
    LFrameworkJSON.AddPair('timeout', TJSONNumber.Create(AConfig.Framework.Timeout));
    LFrameworkJSON.AddPair('enableCompression', TJSONBool.Create(AConfig.Framework.EnableCompression));
    LFrameworkJSON.AddPair('compressionLevel', TJSONNumber.Create(AConfig.Framework.CompressionLevel));
    LFrameworkJSON.AddPair('enableSSL', TJSONBool.Create(AConfig.Framework.EnableSSL));
    LFrameworkJSON.AddPair('sslCertFile', TJSONString.Create(AConfig.Framework.SSLCertFile));
    LFrameworkJSON.AddPair('sslKeyFile', TJSONString.Create(AConfig.Framework.SSLKeyFile));
    LFrameworkJSON.AddPair('slowRequestThresholdMs', TJSONNumber.Create(AConfig.Framework.SlowRequestThresholdMs));
    LJSONObject.AddPair('framework', LFrameworkJSON);
    
    Result := LJSONObject.ToJSON;
  finally
    LJSONObject.Free;
  end;
end;

end.

