unit nfe.controller;

interface

uses
  core.include,
  core.types,
  nfebr.model,
  nfe.service,
  nfe.interfaces;

type
  TNFeController = class(TInterfacedObject, INFe)
  private
    FService: TNFeService;
  public
    constructor Create(const AService: TNFeService);
    destructor Destroy; override;
    // Serviços
    function NFeTransmitir(const AJson: String): TNFeResponse;
    function NFeTransmitirLote(const AJson: String): TNFeResponse;
    function NFeConsultar(const AJson: String): TNFeResponse;
    function NFeCancelar(const AJson: String): TNFeResponse;
    function NFeInutilizar(const AJson: String): TNFeResponse;
    function NFeCorrigir(const AJson: String): TNFeResponse;
    function NFeServicoStatus: TNFeResponse;
  end;

implementation

{ TControllerServer }

constructor TNFeController.Create(const AService: TNFeService);
begin
  FService := AService;
end;

destructor TNFeController.Destroy;
begin
  FService.Free;
  inherited;
end;

function TNFeController.NFeTransmitir(const AJson: String): TNFeResponse;
begin
  Result := FService.NFeTransmitir(AJson);
end;

function TNFeController.NFeCancelar(const AJson: String): TNFeResponse;
begin
  Result := FService.NFeCancelar(AJson);
end;

function TNFeController.NFeCorrigir(const AJson: String): TNFeResponse;
begin
  Result := FService.NFeCorrigir(AJson);
end;

function TNFeController.NFeConsultar(const AJson: String): TNFeResponse;
begin
  Result := FService.NFeConsultar(AJson);
end;

function TNFeController.NFeInutilizar(const AJson: String): TNFeResponse;
begin
  Result := FService.NFeInutilizar(AJson);
end;

function TNFeController.NFeTransmitirLote(const AJson: String): TNFeResponse;
begin
  Result := FService.NFeTransmitirLote(AJson);
end;

function TNFeController.NFeServicoStatus: TNFeResponse;
begin
  Result := FService.NFeServicoStatus;
end;

end.
