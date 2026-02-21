unit xml.controller;

interface

uses
  core.include,
  core.types,
  xml.service,
  xml.interfaces;

type
  TXMLController = class(TInterfacedObject, IXML)
  private
    FService: TXMLService;
  public
    constructor Create(const AReq: TXMLService);
    destructor Destroy; override;
    function NFeXML(const AChave: String): TXMLResponse;
    function NFeCancelamentoXML(const AChave: String): TXMLResponse;
    function NFeInutilizacaoXML(const AProtocolo: String): TXMLResponse;
    function NFeCartaCorrecaoXML(const AChave: String): TXMLResponse;
  end;

implementation

{ TXMLController }

constructor TXMLController.Create(const AReq: TXMLService);
begin
  FService := AReq;
end;

destructor TXMLController.Destroy;
begin
  FService.Free;
  inherited;
end;

function TXMLController.NFeCancelamentoXML(const AChave: String): TXMLResponse;
begin
  Result := FService.NFeCancelamentoXML(AChave);
end;

function TXMLController.NFeCartaCorrecaoXML(const AChave: String): TXMLResponse;
begin
  Result := FService.NFeCartaCorrecaoXML(AChave);
end;

function TXMLController.NFeInutilizacaoXML(const AProtocolo: String): TXMLResponse;
begin
  Result := FService.NFeInutilizacaoXML(AProtocolo);
end;

function TXMLController.NFeXML(const AChave: String): TXMLResponse;
begin
  Result := FService.NFeXML(AChave);
end;

end.
