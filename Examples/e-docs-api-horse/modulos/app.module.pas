unit app.module;

interface

uses
  app.route,
  Nest4D.Module,
  config.module,
  config.route.handler,
  xml.route.handler,
  pdf.route.handler,
  nfe.route.handler,
  nfe.module;

type
  TGardNFeMiddleware = class(TRouteMiddleware)
  public
    function Call(const AReq: IRouteRequest): boolean; override;
  end;

  TAppModule = class(TModule)
  public
    constructor Create; override;
    function Routes: TRoutes; override;
    function Binds: TBinds; override;
    function RouteHandlers: TRouteHandlers; override;
    function ExportedBinds: TExportedBinds; override;
  end;

implementation

uses
  nfebr.lib.include;

{ TAppModule }

function TAppModule.Binds: TBinds;
begin
  Result := [Bind<TNFeConfig>.Singleton];
end;

constructor TAppModule.Create;
begin
  inherited;

end;

function TAppModule.ExportedBinds: TExportedBinds;
begin
  Result := [];
end;

function TAppModule.RouteHandlers: TRouteHandlers;
begin
  Result := [TConfigRouteHandler,
             TNFeRouteHandler,
             TXMLRouteHandler,
             TPDFRouteHandler];
end;

function TAppModule.Routes: TRoutes;
begin
  Result := [RouteModule(Rota.Configurar, TConfigModule),
             RouteModule(Rota.NFeTransmitir, TNFeModule),
             RouteModule(Rota.NFeTransmitirLote, TNFeModule),
             RouteModule(Rota.NFeCancelar, TNFeModule),
             RouteModule(Rota.NFeCorrigir, TNFeModule),
             RouteModule(Rota.NFeInutilizar, TNFeModule),
             RouteModule(Rota.NFeServicoStatus, TNFeModule, [TGardNFeMiddleware]),
             RouteModule(Rota.NFeConsultar, TNFeModule)];
end;

{ TGardNFeMiddleware }

function TGardNFeMiddleware.Call(const AReq: IRouteRequest): boolean;
begin
  Result := true;
end;

end.

