# Resiliência Opcional por Endpoint no Nest4D

Este documento explica como usar a funcionalidade de resiliência opcional por endpoint no Nest4D Horse.

## Visão Geral

A partir desta versão, o sistema de resiliência do Nest4D pode ser controlado individualmente para cada endpoint, permitindo maior flexibilidade na aplicação de políticas de resiliência.

## Uso Básico

### Comportamento Padrão (Compatibilidade)

O comportamento padrão mantém a resiliência habilitada para todos os endpoints:

```pascal
procedure TConfigRouteHandler.RegisterRoutes;
begin
  inherited;
  // Resiliência habilitada por padrão
  RouteGet(Rota.Configurar, Find);
  RoutePost(Rota.Configurar, Insert);
  RoutePut(Rota.Configurar, Update);
  RouteDelete(Rota.Configurar, Delete);
end;
```

### Controle Granular de Resiliência

Agora você pode controlar a resiliência individualmente para cada endpoint:

```pascal
procedure TConfigRouteHandler.RegisterRoutes;
begin
  inherited;
  // Endpoint com resiliência habilitada
  RouteGet(Rota.Configurar, Find, True);
  
  // Endpoint com resiliência desabilitada
  RoutePost(Rota.Configurar, Insert, False);
  
  // Endpoint crítico com resiliência habilitada
  RoutePut(Rota.Configurar, Update, True);
  
  // Endpoint simples sem resiliência
  RouteDelete(Rota.Configurar, Delete, False);
end;
```

## Métodos Disponíveis

Todos os métodos de rota possuem duas versões:

### Versão Original (Resiliência Padrão)
```pascal
function RouteGet(const ARoute: String; const ACallback: THorseCallbackRequestResponse): TRouteHandlerHorse;
function RoutePost(const ARoute: String; const ACallback: THorseCallbackRequestResponse): TRouteHandlerHorse;
function RoutePut(const ARoute: String; const ACallback: THorseCallbackRequestResponse): TRouteHandlerHorse;
function RoutePatch(const ARoute: String; const ACallback: THorseCallbackRequestResponse): TRouteHandlerHorse;
function RouteDelete(const ARoute: String; const ACallback: THorseCallbackRequestResponse): TRouteHandlerHorse;
```

### Versão com Controle de Resiliência
```pascal
function RouteGet(const ARoute: String; const ACallback: THorseCallbackRequestResponse; const AEnableResilience: Boolean): TRouteHandlerHorse;
function RoutePost(const ARoute: String; const ACallback: THorseCallbackRequestResponse; const AEnableResilience: Boolean): TRouteHandlerHorse;
function RoutePut(const ARoute: String; const ACallback: THorseCallbackRequestResponse; const AEnableResilience: Boolean): TRouteHandlerHorse;
function RoutePatch(const ARoute: String; const ACallback: THorseCallbackRequestResponse; const AEnableResilience: Boolean): TRouteHandlerHorse;
function RouteDelete(const ARoute: String; const ACallback: THorseCallbackRequestResponse; const AEnableResilience: Boolean): TRouteHandlerHorse;
```

## Casos de Uso Recomendados

### Quando Habilitar Resiliência (True)
- Endpoints que fazem chamadas para serviços externos
- Operações críticas que precisam de retry automático
- Endpoints com alta latência ou instabilidade
- Operações de escrita importantes

### Quando Desabilitar Resiliência (False)
- Endpoints de health check simples
- Operações muito rápidas e confiáveis
- Endpoints que já possuem sua própria lógica de retry
- Operações onde o overhead da resiliência não é desejado

## Exemplo Prático

```pascal
unit MyAPI.Routes;

interface

uses
  Nest4D.Route.Handler.Horse;

type
  TMyAPIRoutes = class(TRouteHandlerHorse)
  public
    procedure RegisterRoutes; override;
  private
    procedure GetHealthCheck(Req: THorseRequest; Res: THorseResponse);
    procedure PostCriticalData(Req: THorseRequest; Res: THorseResponse);
    procedure GetSimpleData(Req: THorseRequest; Res: THorseResponse);
    procedure DeleteImportantRecord(Req: THorseRequest; Res: THorseResponse);
  end;

implementation

procedure TMyAPIRoutes.RegisterRoutes;
begin
  inherited;
  
  // Health check simples - sem resiliência
  RouteGet('/health', GetHealthCheck, False);
  
  // Operação crítica - com resiliência
  RoutePost('/critical-data', PostCriticalData, True);
  
  // Consulta simples - sem resiliência
  RouteGet('/simple-data', GetSimpleData, False);
  
  // Exclusão importante - com resiliência
  RouteDelete('/important/:id', DeleteImportantRecord, True);
end;

// Implementação dos métodos...

end.
```

## Configuração Global

A configuração global de resiliência ainda pode ser feita através do `TNest4DHorseConfig`:

```pascal
TNest4DHorseConfig.EnableResilience(True)
  .ResilienceConfig
    .SetPolicy(rpDefault)
    .SetDefaultTimeout(30000);
```

Esta configuração afeta apenas os endpoints que usam resiliência (parâmetro `True` ou versão sem parâmetro).

## Compatibilidade

Esta implementação mantém 100% de compatibilidade com código existente:
- Código antigo continua funcionando sem modificações
- A resiliência permanece habilitada por padrão
- Apenas adiciona a opção de controle granular quando necessário

## Benefícios

1. **Flexibilidade**: Controle individual por endpoint
2. **Performance**: Desabilitar resiliência onde não é necessária
3. **Compatibilidade**: Código existente continua funcionando
4. **Simplicidade**: Fácil de usar e entender
5. **Granularidade**: Aplicar políticas diferentes conforme a necessidade