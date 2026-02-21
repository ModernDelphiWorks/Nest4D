unit xml.service;

interface

uses
  SysUtils,
  core.types,
  core.exception,
  xml.repository,
  xml.interfaces;

type
  TXMLService = class(TInterfacedObject, IXML)
  private
    FRepository: TXMLRepository;
  public
    constructor Create(const ARepository: TXMLRepository);
    destructor Destroy; override;
    function NFeXML(const AChave: String): TXMLResponse;
    function NFeCancelamentoXML(const AChave: String): TXMLResponse;
    function NFeInutilizacaoXML(const AProtocolo: String): TXMLResponse;
    function NFeCartaCorrecaoXML(const AChave: String): TXMLResponse;
  end;

implementation

{ TXMLService }

constructor TXMLService.Create(const ARepository: TXMLRepository);
begin
  FRepository := ARepository;
end;

destructor TXMLService.Destroy;
begin
  FRepository.Free;
  inherited;
end;

function TXMLService.NFeCancelamentoXML(const AChave: String): TXMLResponse;
begin
  try
    Result := TXMLResponse.Success(FRepository.NFeCancelamentoXML(AChave));
  except
    on E: Exception do
      Result := TXMLResponse.Failure(ERequestException.Create(E.Message));
  end;
end;

function TXMLService.NFeCartaCorrecaoXML(const AChave: String): TXMLResponse;
begin
  try
    Result := TXMLResponse.Success(FRepository.NFeCartaCorrecaoXML(AChave));
  except
    on E: Exception do
      Result := TXMLResponse.Failure(ERequestException.Create(E.Message));
  end;
end;

function TXMLService.NFeInutilizacaoXML(const AProtocolo: String): TXMLResponse;
begin
  try
    Result := TXMLResponse.Success(FRepository.NFeInutilizacaoXML(AProtocolo));
  except
    on E: Exception do
      Result := TXMLResponse.Failure(ERequestException.Create(E.Message));
  end;
end;

function TXMLService.NFeXML(const AChave: String): TXMLResponse;
begin
  try
    Result := TXMLResponse.Success(FRepository.NFeXML(AChave));
  except
    on E: EXMLNotExistException do
      Result := TXMLResponse.Failure(ERequestException.Create(E.Message, E.Status));
  end;
end;

end.
