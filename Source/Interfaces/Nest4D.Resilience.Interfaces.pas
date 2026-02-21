unit Nest4D.Resilience.Interfaces;

interface

uses
  System.SysUtils,
  System.Classes,
  Nest4D.Request.Data;

type
  // Enums para estrat?gias e estados
  TRetryStrategy = (rsFixed, rsLinear, rsExponential, rsCustom);
  TCircuitBreakerState = (cbsClosed, cbsOpen, cbsHalfOpen);
  TFallbackType = (ftAlternativeEndpoint, ftCachedResponse, ftDefaultResponse, ftCustomHandler);

  // Forward declarations
  IRestFrameworkAdapter = interface;
  IRetryPolicy = interface;
  IFallbackService = interface;
  ICircuitBreaker = interface;
  IResilienceConfig = interface;

  // Estruturas de dados
  TRetryAttempt = record
    AttemptNumber: Integer;
    LastException: Exception;
    ElapsedTime: Cardinal;
    NextRetryDelay: Cardinal;
  end;

  TCircuitBreakerMetrics = record
    FailureCount: Integer;
    SuccessCount: Integer;
    LastFailureTime: TDateTime;
    LastSuccessTime: TDateTime;
    State: TCircuitBreakerState;
    StateChangedAt: TDateTime;
  end;

  TFallbackResult = record
    Success: Boolean;
    Response: string;
    FallbackType: TFallbackType;
    ExecutionTime: Cardinal;
  end;

  TResilienceMetrics = record
    TotalRequests: Int64;
    SuccessfulRequests: Int64;
    FailedRequests: Int64;
    RetriedRequests: Int64;
    FallbackExecutions: Int64;
    CircuitBreakerTrips: Int64;
    AverageResponseTime: Double;
    LastUpdated: TDateTime;
  end;

  // Callback types
  TRetryCallback = function(const AAttempt: TRetryAttempt): Boolean of object;
  TFallbackCallback = function(const AException: Exception; const ARequestData: TRequestData): TFallbackResult of object;
  TCircuitBreakerCallback = procedure(const AState: TCircuitBreakerState; const AMetrics: TCircuitBreakerMetrics) of object;

  // Interface principal para abstra??o de frameworks REST
  IRestFrameworkAdapter = interface
    ['{B8E5F2A1-4C3D-4E5F-8A9B-1C2D3E4F5A6B}']
    // M?todos de execu??o
    function ExecuteRequest(const ARequestData: TRequestData): Boolean;
    function GetLastResponse: string;
    function GetLastStatusCode: Integer;
    function GetLastException: Exception;

    // M?todos de configura??o
    procedure SetTimeout(const ATimeoutMs: Cardinal);
    function GetTimeout: Cardinal;

    // M?todos de identifica??o
    function GetFrameworkName: string;
    function GetFrameworkVersion: string;

    // Propriedades
    property LastResponse: string read GetLastResponse;
    property LastStatusCode: Integer read GetLastStatusCode;
    property LastException: Exception read GetLastException;
    property Timeout: Cardinal read GetTimeout write SetTimeout;
  end;

  // Interface para pol?ticas de retry
  IRetryPolicy = interface
    ['{C9F6E3B2-5D4E-4F6A-9B8C-2D3E4F5A6B7C}']
    // Configura??o
    procedure SetMaxAttempts(const AMaxAttempts: Integer);
    procedure SetRetryStrategy(const AStrategy: TRetryStrategy);
    procedure SetBaseDelay(const ADelayMs: Cardinal);
    procedure SetMaxDelay(const AMaxDelayMs: Cardinal);
    procedure SetRetryCallback(const ACallback: TRetryCallback);

    // Execu??o
    function ShouldRetry(const AAttempt: TRetryAttempt): Boolean;
    function CalculateDelay(const AAttemptNumber: Integer): Cardinal;
    function Execute(const AAdapter: IRestFrameworkAdapter; const ARequestData: TRequestData): Boolean;

    // Propriedades
    function GetMaxAttempts: Integer;
    function GetRetryStrategy: TRetryStrategy;
    function GetBaseDelay: Cardinal;
    function GetMaxDelay: Cardinal;

    property MaxAttempts: Integer read GetMaxAttempts write SetMaxAttempts;
    property RetryStrategy: TRetryStrategy read GetRetryStrategy write SetRetryStrategy;
    property BaseDelay: Cardinal read GetBaseDelay write SetBaseDelay;
    property MaxDelay: Cardinal read GetMaxDelay write SetMaxDelay;
  end;

  // Interface para servi?os de fallback
  IFallbackService = interface
    ['{D0A7F4C3-6E5F-4A7B-8C9D-3E4F5A6B7C8D}']
    // Configura??o
    procedure AddFallbackEndpoint(const AOriginalEndpoint, AFallbackEndpoint: string);
    procedure SetCachedResponse(const AEndpoint, AResponse: string; const AExpirationTime: TDateTime);
    procedure SetDefaultResponse(const AEndpoint, AResponse: string);
    procedure SetFallbackCallback(const ACallback: TFallbackCallback);

    // Execu??o
    function ExecuteFallback(const AException: Exception; const ARequestData: TRequestData): TFallbackResult;
    function HasFallback(const AEndpoint: string): Boolean;

    // Limpeza
    procedure ClearExpiredCache;
    procedure ClearAllCache;
  end;

  // Interface para Circuit Breaker
  ICircuitBreaker = interface
    ['{E1B8A5D4-7F6A-4B8C-9D0E-4F5A6B7C8D9E}']
    // Configura??o
    procedure SetFailureThreshold(const AThreshold: Integer);
    procedure SetRecoveryTimeout(const ATimeoutMs: Cardinal);
    procedure SetSuccessThreshold(const AThreshold: Integer);
    procedure SetStateChangeCallback(const ACallback: TCircuitBreakerCallback);

    // Estado
    function GetState: TCircuitBreakerState;
    function GetMetrics: TCircuitBreakerMetrics;
    function CanExecute: Boolean;

    // Execu??o
    procedure RecordSuccess;
    procedure RecordFailure(const AException: Exception);
    procedure Reset;
    procedure ForceOpen;
    procedure ForceClose;

    // Propriedades
    property State: TCircuitBreakerState read GetState;
    property Metrics: TCircuitBreakerMetrics read GetMetrics;
  end;

  // Interface para configura??o de resili?ncia
  IResilienceConfig = interface
    ['{F2C9B6E5-8A7B-4C9D-0E1F-5A6B7C8D9E0F}']
    // Configura??o geral
    procedure SetEnabled(const AEnabled: Boolean);
    function IsEnabled: Boolean;

    // Configura??o por endpoint
    procedure ConfigureEndpoint(const AEndpoint: string; const ARetryPolicy: IRetryPolicy;
      const AFallbackService: IFallbackService; const ACircuitBreaker: ICircuitBreaker);
    procedure RemoveEndpointConfig(const AEndpoint: string);

    // Obten??o de configura??es
    function GetRetryPolicy(const AEndpoint: string): IRetryPolicy;
    function GetFallbackService(const AEndpoint: string): IFallbackService;
    function GetCircuitBreaker(const AEndpoint: string): ICircuitBreaker;

    // Configura??es padr?o
    procedure SetDefaultRetryPolicy(const ARetryPolicy: IRetryPolicy);
    procedure SetDefaultFallbackService(const AFallbackService: IFallbackService);
    procedure SetDefaultCircuitBreaker(const ACircuitBreaker: ICircuitBreaker);

    function GetDefaultRetryPolicy: IRetryPolicy;
    function GetDefaultFallbackService: IFallbackService;
    function GetDefaultCircuitBreaker: ICircuitBreaker;
  end;

  // Interface principal do interceptor de resili?ncia
  IResilienceInterceptor = interface
    ['{A3D0C7F6-9B8C-4D0E-1F2A-6B7C8D9E0F1A}']
    // Configura??o
    procedure SetFrameworkAdapter(const AAdapter: IRestFrameworkAdapter);
    procedure SetResilienceConfig(const AConfig: IResilienceConfig);

    // Execu??o
    function ExecuteWithResilience(const ARequestData: TRequestData): Boolean;

    // M?tricas
    function GetMetrics(const AEndpoint: string): TResilienceMetrics;
    function GetGlobalMetrics: TResilienceMetrics;
    procedure ResetMetrics(const AEndpoint: string = '');

    // Propriedades
    property FrameworkAdapter: IRestFrameworkAdapter write SetFrameworkAdapter;
    property ResilienceConfig: IResilienceConfig write SetResilienceConfig;
  end;

implementation

end.
