unit nfe.module;

interface

uses
  core.module,
  pdf.module,
  xml.module,
  nfebr.lib.nfe,
  app.route,
  nest4d,
  Nest4D.module;

type
  TNFeModule = class(TModule)
  public
    constructor Create; override;
    function Routes: TRoutes; override;
    function Binds: TBinds; override;
    function Imports: TImports; override;
  end;

implementation

uses
  nfe.service,
  nfe.repository,
  nfe.infra,
  nfe.controller,
  nfebr.lib.include;

{ TNFeModule }

function TNFeModule.Binds: TBinds;
begin
  Result := [Bind<TNFeInfra>.Factory,
             Bind<TNFeRepository>.Factory,
             Bind<TNFeService>.Factory,
             Bind<TNFeController>.Singleton];
end;

constructor TNFeModule.Create;
begin
  inherited;

end;

function TNFeModule.Imports: TImports;
begin
  Result := [TCoreModule];
end;

function TNFeModule.Routes: TRoutes;
begin
  Result := [RouteModule(Rota.NFeTransmitidaPDF, TPDFModule),
             RouteModule(Rota.NFeCancelamentoPDF, TPDFModule),
             RouteModule(Rota.NFeCartaCorrecaoPDF, TPDFModule),
             RouteModule(Rota.NFeInutilizacaoPDF, TPDFModule),
             RouteModule(Rota.NFeTransmitidaXML, TXMLModule),
             RouteModule(Rota.NFeCancelamentoXML, TXMLModule),
             RouteModule(Rota.NFeCartaCorrecaoXML, TXMLModule),
             RouteModule(Rota.NFeInutilizacaoXML, TXMLModule)];
end;

end.


