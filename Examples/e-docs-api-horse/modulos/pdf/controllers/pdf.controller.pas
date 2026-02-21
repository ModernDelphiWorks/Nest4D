unit pdf.controller;

interface

uses
  core.include,
  core.types,
  pdf.service,
  pdf.interfaces;

type
  TPDFController = class(TInterfacedObject, IPDF)
  private
    FService: TPDFService;
  public
    constructor Create(const AReq: TPDFService);
    destructor Destroy; override;
    function NFePDF(const AChave: String): TPDFResponse;
    function NFeCancelamentoPDF(const AChave: String): TPDFResponse;
    function NFeInutilizacaoPDF(const AProtocolo: String): TPDFResponse;
    function NFeCartaCorrecaoPDF(const AChave: String): TPDFResponse;
  end;

implementation

{ TPDFController }

constructor TPDFController.Create(const AReq: TPDFService);
begin
  FService := AReq;
end;

destructor TPDFController.Destroy;
begin
  FService.Free;
  inherited;
end;

function TPDFController.NFeCancelamentoPDF(const AChave: String): TPDFResponse;
begin
  Result := FService.NFeCancelamentoPDF(AChave);
end;

function TPDFController.NFeCartaCorrecaoPDF(const AChave: String): TPDFResponse;
begin
  Result := FService.NFeCartaCorrecaoPDF(AChave);
end;

function TPDFController.NFeInutilizacaoPDF(const AProtocolo: String): TPDFResponse;
begin
  Result := FService.NFeInutilizacaoPDF(AProtocolo);
end;

function TPDFController.NFePDF(const AChave: String): TPDFResponse;
begin
  Result := FService.NFePDF(AChave);
end;

end.
