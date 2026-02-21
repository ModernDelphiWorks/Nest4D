unit Nest4D.Config.Defaults;

interface

uses
  Nest4D.Config,
  Nest4D.Logging;

type
  // Classe para configura??es padr?o por ambiente
  TNest4DConfigDefaults = class
  public
    // Configura??es padr?o por ambiente
    class function Development: TNest4DConfig;
    class function Production: TNest4DConfig;
    class function Testing: TNest4DConfig;
    class function Staging: TNest4DConfig;

    // Configura??es espec?ficas por componente
    class function DevelopmentLogging: TLoggingConfig;
    class function ProductionLogging: TLoggingConfig;
    class function TestingLogging: TLoggingConfig;

    class function DevelopmentMetrics: TMetricsConfig;
    class function ProductionMetrics: TMetricsConfig;
    class function TestingMetrics: TMetricsConfig;

    class function DevelopmentHealth: THealthConfig;
    class function ProductionHealth: THealthConfig;
    class function TestingHealth: THealthConfig;

    class function DevelopmentCache: TCacheConfig;
    class function ProductionCache: TCacheConfig;
    class function TestingCache: TCacheConfig;

    class function DevelopmentHandlerPool: THandlerPoolConfig;
    class function ProductionHandlerPool: THandlerPoolConfig;
    class function TestingHandlerPool: THandlerPoolConfig;

    class function DevelopmentResilience: TResilienceConfig;
    class function ProductionResilience: TResilienceConfig;
    class function TestingResilience: TResilienceConfig;

    class function DevelopmentFramework: TFrameworkConfig;
    class function ProductionFramework: TFrameworkConfig;
    class function TestingFramework: TFrameworkConfig;
  end;

implementation

{ TNest4DConfigDefaults }

// Configura??es completas por ambiente
class function TNest4DConfigDefaults.Development: TNest4DConfig;
begin
  Result := TNest4DConfig.Create;
  Result.Logging := DevelopmentLogging;
  Result.Metrics := DevelopmentMetrics;
  Result.Health := DevelopmentHealth;
  Result.Cache := DevelopmentCache;
  Result.HandlerPool := DevelopmentHandlerPool;
  Result.Resilience := DevelopmentResilience;
  Result.Framework := DevelopmentFramework;
end;

class function TNest4DConfigDefaults.Production: TNest4DConfig;
begin
  Result := TNest4DConfig.Create;
  Result.Logging := ProductionLogging;
  Result.Metrics := ProductionMetrics;
  Result.Health := ProductionHealth;
  Result.Cache := ProductionCache;
  Result.HandlerPool := ProductionHandlerPool;
  Result.Resilience := ProductionResilience;
  Result.Framework := ProductionFramework;
end;

class function TNest4DConfigDefaults.Testing: TNest4DConfig;
begin
  Result := TNest4DConfig.Create;
  Result.Logging := TestingLogging;
  Result.Metrics := TestingMetrics;
  Result.Health := TestingHealth;
  Result.Cache := TestingCache;
  Result.HandlerPool := TestingHandlerPool;
  Result.Resilience := TestingResilience;
  Result.Framework := TestingFramework;
end;

class function TNest4DConfigDefaults.Staging: TNest4DConfig;
begin
  // Staging usa configura??es similares ? produ??o, mas com mais logging
  Result := TNest4DConfig.Create;
  Result.Logging := ProductionLogging;
  Result.Logging.Level := llInfo;
  Result.Logging.LogToConsole := True;
  Result.Metrics := ProductionMetrics;
  Result.Health := ProductionHealth;
  Result.Cache := ProductionCache;
  Result.HandlerPool := ProductionHandlerPool;
  Result.Resilience := ProductionResilience;
  Result.Framework := ProductionFramework;
end;

// Configura??es de Logging por ambiente
class function TNest4DConfigDefaults.DevelopmentLogging: TLoggingConfig;
begin
  Result := TLoggingConfig.Create;
  Result.Enabled := True;
  Result.Level := llDebug;
  Result.EnableConsole := True;
  Result.EnableFile := True;
  Result.EnableDatabase := False;
  Result.FilePath := 'logs/Nest4D-dev.log';
  Result.LogToConsole := True;
  Result.LogToFile := True;
  Result.LogFileName := 'logs/Nest4D-dev.log';
  Result.MaxFileSize := 10 * 1024 * 1024; // 10MB
  Result.MaxBackupFiles := 5;
  Result.Format := '[%datetime%] [%level%] [%thread%] %message%';
end;

class function TNest4DConfigDefaults.ProductionLogging: TLoggingConfig;
begin
  Result := TLoggingConfig.Create;
  Result.Enabled := True;
  Result.Level := llWarn; // Apenas warnings e erros
  Result.EnableConsole := False;
  Result.EnableFile := True;
  Result.EnableDatabase := False;
  Result.FilePath := 'logs/Nest4D-prod.log';
  Result.LogToConsole := False;
  Result.LogToFile := True;
  Result.LogFileName := 'logs/Nest4D-prod.log';
  Result.MaxFileSize := 50 * 1024 * 1024; // 50MB
  Result.MaxBackupFiles := 10;
  Result.Format := '[%datetime%] [%level%] %message%';
end;

class function TNest4DConfigDefaults.TestingLogging: TLoggingConfig;
begin
  Result := TLoggingConfig.Create;
  Result.Enabled := True;
  Result.Level := llInfo;
  Result.EnableConsole := True;
  Result.EnableFile := True;
  Result.EnableDatabase := False;
  Result.FilePath := 'logs/Nest4D-test.log';
  Result.LogToConsole := True;
  Result.LogToFile := True;
  Result.LogFileName := 'logs/Nest4D-test.log';
  Result.MaxFileSize := 5 * 1024 * 1024; // 5MB
  Result.MaxBackupFiles := 3;
  Result.Format := '[%datetime%] [%level%] [%test%] %message%';
end;

// Configura??es de Metrics por ambiente
class function TNest4DConfigDefaults.DevelopmentMetrics: TMetricsConfig;
begin
  Result := TMetricsConfig.Create;
  Result.Enabled := True;
  Result.Port := 9090;
  Result.Endpoint := '/metrics';
  Result.MetricsInterval := 5000; // 5 segundos
  Result.MaxMetricsHistory := 3600; // 1 hora
  Result.EnablePrometheus := True;
  Result.PrometheusEndpoint := '/metrics';
end;

class function TNest4DConfigDefaults.ProductionMetrics: TMetricsConfig;
begin
  Result := TMetricsConfig.Create;
  Result.Enabled := True;
  Result.Port := 9090;
  Result.Endpoint := '/metrics';
  Result.MetricsInterval := 15000; // 15 segundos
  Result.MaxMetricsHistory := 86400; // 24 horas
  Result.EnablePrometheus := True;
  Result.PrometheusEndpoint := '/metrics';
end;

class function TNest4DConfigDefaults.TestingMetrics: TMetricsConfig;
begin
  Result := TMetricsConfig.Create;
  Result.Enabled := False; // Desabilitado por padrão em testes
  Result.Port := 9091;
  Result.Endpoint := '/test-metrics';
  Result.MetricsInterval := 1000; // 1 segundo
  Result.MaxMetricsHistory := 300; // 5 minutos
  Result.EnablePrometheus := False;
  Result.PrometheusEndpoint := '/test-metrics';
end;

// Configura??es de Health por ambiente
class function TNest4DConfigDefaults.DevelopmentHealth: THealthConfig;
begin
  Result := THealthConfig.Create;
  Result.Enabled := True;
  Result.Port := 8080;
  Result.Endpoint := '/health';
  Result.CheckInterval := 30000; // 30 segundos
  Result.Timeout := 5000; // 5 segundos
  Result.EnableDatabaseCheck := True;
  Result.DatabaseConnectionString := 'localhost:5432';
end;

class function TNest4DConfigDefaults.ProductionHealth: THealthConfig;
begin
  Result := THealthConfig.Create;
  Result.Enabled := True;
  Result.Port := 8080;
  Result.Endpoint := '/health';
  Result.CheckInterval := 60000; // 60 segundos
  Result.Timeout := 10000; // 10 segundos
  Result.EnableDatabaseCheck := True;
  Result.DatabaseConnectionString := 'prod-db:5432';
end;

class function TNest4DConfigDefaults.TestingHealth: THealthConfig;
begin
  Result := THealthConfig.Create;
  Result.Enabled := True;
  Result.Port := 8081;
  Result.Endpoint := '/test-health';
  Result.CheckInterval := 10000; // 10 segundos
  Result.Timeout := 2000; // 2 segundos
  Result.EnableDatabaseCheck := False; // Desabilitado em testes
  Result.DatabaseConnectionString := 'test-db:5432';
end;

// Configura??es de Cache por ambiente
class function TNest4DConfigDefaults.DevelopmentCache: TCacheConfig;
begin
  Result := TCacheConfig.Create;
  Result.EnableTTL := True;
  Result.DefaultTTL := 300; // 5 minutos
  Result.MaxCacheSize := 1000;
  Result.EnableBackgroundCleanup := True;
  Result.CleanupInterval := 60; // 1 minuto
  Result.EnableCompression := False;
  Result.CompressionThreshold := 1024;
  Result.EnableDistributedCache := False;
  Result.RedisConnectionString := '';
end;

class function TNest4DConfigDefaults.ProductionCache: TCacheConfig;
begin
  Result := TCacheConfig.Create;
  Result.EnableTTL := True;
  Result.DefaultTTL := 3600; // 1 hora
  Result.MaxCacheSize := 10000;
  Result.EnableBackgroundCleanup := True;
  Result.CleanupInterval := 300; // 5 minutos
  Result.EnableCompression := True;
  Result.CompressionThreshold := 1024;
  Result.EnableDistributedCache := True;
  Result.RedisConnectionString := 'redis://localhost:6379';
end;

class function TNest4DConfigDefaults.TestingCache: TCacheConfig;
begin
  Result := TCacheConfig.Create;
  Result.EnableTTL := True;
  Result.DefaultTTL := 60; // 1 minuto
  Result.MaxCacheSize := 100;
  Result.EnableBackgroundCleanup := True;
  Result.CleanupInterval := 30; // 30 segundos
  Result.EnableCompression := False;
  Result.CompressionThreshold := 1024;
  Result.EnableDistributedCache := False;
  Result.RedisConnectionString := '';
end;

// Configura??es de Resilience por ambiente
class function TNest4DConfigDefaults.DevelopmentResilience: TResilienceConfig;
begin
  Result := TResilienceConfig.Create;
  Result.Enabled := True;
  Result.Endpoint := '/resilience';
  Result.EnableRetry := True;
  Result.MaxRetryAttempts := 3;
  Result.BaseDelay := 1000;
  Result.MaxDelay := 30000;
  Result.EnableCircuitBreaker := True;
  Result.FailureThreshold := 5;
  Result.SuccessThreshold := 3;
  Result.Timeout := 5000; // 5 segundos
  Result.EnableBulkhead := True;
  Result.MaxConcurrentCalls := 10;
  Result.MaxWaitTime := 30000;
end;

class function TNest4DConfigDefaults.ProductionResilience: TResilienceConfig;
begin
  Result := TResilienceConfig.Create;
  Result.Enabled := True;
  Result.Endpoint := '/resilience';
  Result.EnableRetry := True;
  Result.MaxRetryAttempts := 5;
  Result.BaseDelay := 2000;
  Result.MaxDelay := 60000;
  Result.EnableCircuitBreaker := True;
  Result.FailureThreshold := 10;
  Result.SuccessThreshold := 5;
  Result.Timeout := 10000; // 10 segundos
  Result.EnableBulkhead := True;
  Result.MaxConcurrentCalls := 50;
  Result.MaxWaitTime := 60000;
end;

class function TNest4DConfigDefaults.TestingResilience: TResilienceConfig;
begin
  Result := TResilienceConfig.Create;
  Result.Enabled := False; // Desabilitado em testes por padrão
  Result.Endpoint := '/test-resilience';
  Result.EnableRetry := True;
  Result.MaxRetryAttempts := 1;
  Result.BaseDelay := 500;
  Result.MaxDelay := 10000;
  Result.EnableCircuitBreaker := False;
  Result.FailureThreshold := 3;
  Result.SuccessThreshold := 2;
  Result.Timeout := 2000; // 2 segundos
  Result.EnableBulkhead := False;
  Result.MaxConcurrentCalls := 5;
  Result.MaxWaitTime := 10000;
end;

// Configura??es de Framework por ambiente
class function TNest4DConfigDefaults.DevelopmentFramework: TFrameworkConfig;
begin
  Result := TFrameworkConfig.Create;
  Result.Port := 8080;
  Result.Host := 'localhost';
  Result.MaxConnections := 100;
  Result.Timeout := 30000;
  Result.EnableCompression := False;
  Result.CompressionLevel := 1;
  Result.EnableSSL := False;
  Result.SSLCertFile := '';
  Result.SSLKeyFile := '';
  Result.SlowRequestThresholdMs := 1000;
end;

class function TNest4DConfigDefaults.ProductionFramework: TFrameworkConfig;
begin
  Result := TFrameworkConfig.Create;
  Result.Port := 80;
  Result.Host := '0.0.0.0';
  Result.MaxConnections := 1000;
  Result.Timeout := 60000;
  Result.EnableCompression := True;
  Result.CompressionLevel := 6;
  Result.EnableSSL := True;
  Result.SSLCertFile := '/etc/ssl/certs/server.crt';
  Result.SSLKeyFile := '/etc/ssl/private/server.key';
  Result.SlowRequestThresholdMs := 5000;
end;

class function TNest4DConfigDefaults.TestingFramework: TFrameworkConfig;
begin
  Result := TFrameworkConfig.Create;
  Result.Port := 8081;
  Result.Host := 'localhost';
  Result.MaxConnections := 50;
  Result.Timeout := 10000;
  Result.EnableCompression := False;
  Result.CompressionLevel := 1;
  Result.EnableSSL := False;
  Result.SSLCertFile := '';
  Result.SSLKeyFile := '';
  Result.SlowRequestThresholdMs := 500;
end;

// Handler Pool configurations
class function TNest4DConfigDefaults.DevelopmentHandlerPool: THandlerPoolConfig;
begin
  Result := THandlerPoolConfig.Create;
  Result.MinSize := 2;
  Result.MaxSize := 10;
  Result.GrowthFactor := 1.5;
  Result.ShrinkFactor := 0.5;
  Result.IdleTimeout := 30000; // 30 segundos
  Result.MinTimeoutThreshold := 5000;
  Result.MaxTimeoutThreshold := 60000;
end;

class function TNest4DConfigDefaults.ProductionHandlerPool: THandlerPoolConfig;
begin
  Result := THandlerPoolConfig.Create;
  Result.MinSize := 5;
  Result.MaxSize := 50;
  Result.GrowthFactor := 2.0;
  Result.ShrinkFactor := 0.5;
  Result.IdleTimeout := 60000; // 1 minuto
  Result.MinTimeoutThreshold := 10000;
  Result.MaxTimeoutThreshold := 120000;
end;

class function TNest4DConfigDefaults.TestingHandlerPool: THandlerPoolConfig;
begin
  Result := THandlerPoolConfig.Create;
  Result.MinSize := 1;
  Result.MaxSize := 5;
  Result.GrowthFactor := 1.2;
  Result.ShrinkFactor := 0.5;
  Result.IdleTimeout := 10000; // 10 segundos
  Result.MinTimeoutThreshold := 2000;
  Result.MaxTimeoutThreshold := 30000;
end;

end.

