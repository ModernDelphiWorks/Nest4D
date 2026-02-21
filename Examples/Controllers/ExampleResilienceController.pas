unit ExampleResilienceController;

{$mode delphi}

interface

uses
  Nest4D.core.controller,
  Nest4D.core.decorators,
  Nest4D.resilience.decorators,
  Nest4D.core.interfaces,
  SysUtils;

type
  { TExampleResilienceController }
  
  [Controller('/api/resilience')]
  TExampleResilienceController = class(TInterfacedObject, IController)
  public
    // Exemplo básico com retry - tenta até 3 vezes com delay de 1 segundo
    [Get('/basic-retry')]
    [Retry(3, 1000)] // 3 tentativas, 1000ms de delay
    function BasicRetryExample(const ARequest: IRouteRequest): TReturnPair;
    
    // Exemplo com circuit breaker - abre após 5 falhas em 60 segundos
    [Get('/circuit-breaker')]
    [CircuitBreaker(5, 60000, 30000)] // 5 falhas, janela de 60s, timeout de 30s
    function CircuitBreakerExample(const ARequest: IRouteRequest): TReturnPair;
    
    // Exemplo com fallback - executa método alternativo em caso de falha
    [Get('/fallback')]
    [Fallback('FallbackMethod')]
    function FallbackExample(const ARequest: IRouteRequest): TReturnPair;
    
    // Exemplo combinado - retry + circuit breaker + fallback
    [Get('/combined')]
    [Retry(2, 500)]                    // 2 tentativas com 500ms
    [CircuitBreaker(3, 30000, 15000)]  // 3 falhas, janela de 30s, timeout de 15s
    [Fallback('CombinedFallback')]
    function CombinedExample(const ARequest: IRouteRequest): TReturnPair;
    
    // Exemplo com configuração avançada de retry (backoff exponencial)
    [Get('/advanced-retry')]
    [Retry(5, 1000, 'exponential', 2.0, 0.1)] // 5 tentativas, delay inicial 1s, backoff exponencial com multiplicador 2.0 e jitter 0.1
    function AdvancedRetryExample(const ARequest: IRouteRequest): TReturnPair;
    
    // Exemplo simulando serviço externo instável
    [Get('/unstable-service')]
    [Retry(3, 2000)]
    [CircuitBreaker(5, 60000, 30000)]
    [Fallback('UnstableServiceFallback')]
    function UnstableServiceExample(const ARequest: IRouteRequest): TReturnPair;
    
  private
    // Métodos de fallback
    function FallbackMethod(const ARequest: IRouteRequest): TReturnPair;
    function CombinedFallback(const ARequest: IRouteRequest): TReturnPair;
    function UnstableServiceFallback(const ARequest: IRouteRequest): TReturnPair;
    
    // Simuladores de falha para demonstração
    function SimulateRandomFailure: Boolean;
    function SimulateExternalServiceCall: String;
  end;

implementation

uses
  Nest4D.core.response;

{ TExampleResilienceController }

function TExampleResilienceController.BasicRetryExample(const ARequest: IRouteRequest): TReturnPair;
begin
  // Simula uma operação que pode falhar
  if SimulateRandomFailure then
    raise Exception.Create('Falha simulada no serviço');
    
  Result := TResponse.Ok('Operação executada com sucesso após retry!');
end;

function TExampleResilienceController.CircuitBreakerExample(const ARequest: IRouteRequest): TReturnPair;
begin
  // Simula chamada para serviço externo
  if Random(10) < 7 then // 70% de chance de falha
    raise Exception.Create('Serviço externo indisponível');
    
  Result := TResponse.Ok('Serviço externo respondeu com sucesso!');
end;

function TExampleResilienceController.FallbackExample(const ARequest: IRouteRequest): TReturnPair;
begin
  // Simula falha que acionará o fallback
  if Random(10) < 8 then // 80% de chance de falha
    raise Exception.Create('Falha que acionará fallback');
    
  Result := TResponse.Ok('Operação principal executada com sucesso!');
end;

function TExampleResilienceController.CombinedExample(const ARequest: IRouteRequest): TReturnPair;
var
  LData: String;
begin
  // Simula operação complexa que pode falhar
  LData := SimulateExternalServiceCall;
  Result := TResponse.Ok(Format('Dados obtidos: %s', [LData]));
end;

function TExampleResilienceController.AdvancedRetryExample(const ARequest: IRouteRequest): TReturnPair;
begin
  // Simula operação que falha nas primeiras tentativas
  if Random(10) < 6 then // 60% de chance de falha
    raise Exception.Create('Falha temporária - retry com backoff exponencial');
    
  Result := TResponse.Ok('Operação executada após retry avançado!');
end;

function TExampleResilienceController.UnstableServiceExample(const ARequest: IRouteRequest): TReturnPair;
begin
  // Simula serviço muito instável
  if Random(10) < 9 then // 90% de chance de falha
    raise Exception.Create('Serviço extremamente instável');
    
  Result := TResponse.Ok('Serviço instável respondeu (raro!)'); 
end;

// Métodos de Fallback

function TExampleResilienceController.FallbackMethod(const ARequest: IRouteRequest): TReturnPair;
begin
  Result := TResponse.Ok('Resposta do método de fallback - operação alternativa executada');
end;

function TExampleResilienceController.CombinedFallback(const ARequest: IRouteRequest): TReturnPair;
begin
  Result := TResponse.Ok('Fallback combinado - dados obtidos de cache local');
end;

function TExampleResilienceController.UnstableServiceFallback(const ARequest: IRouteRequest): TReturnPair;
begin
  Result := TResponse.Ok('Fallback para serviço instável - usando dados em cache ou serviço alternativo');
end;

// Métodos auxiliares para simulação

function TExampleResilienceController.SimulateRandomFailure: Boolean;
begin
  Result := Random(10) < 5; // 50% de chance de falha
end;

function TExampleResilienceController.SimulateExternalServiceCall: String;
begin
  if Random(10) < 6 then // 60% de chance de falha
    raise Exception.Create('Falha na chamada do serviço externo');
    
  Result := Format('Dados do serviço externo - timestamp: %s', [FormatDateTime('yyyy-mm-dd hh:nn:ss', Now)]);
end;

end.