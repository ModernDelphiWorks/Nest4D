unit xml.repository;

interface

uses
  SysUtils,
  Classes,
  core.types,
  xml.infra,
  xml.interfaces,
  nfebr.config;

type
  TXMLRepository = class
  private
    FInfra: TXMLInfra;
    FConfig: TNFeConfig;
  public
    constructor Create(const AInfra: TXMLInfra);
    destructor Destroy; override;
    function NFeXML(const AChave: String): TFileStream;
    function NFeCancelamentoXML(const AChave: String): TFileStream;
    function NFeInutilizacaoXML(const AProtocolo: String): TFileStream;
    function NFeCartaCorrecaoXML(const AChave: String): TFileStream;
  end;

implementation

uses
  nest4d,
  core.exception;

{ TXMLRepository }

constructor TXMLRepository.Create(const AInfra: TXMLInfra);
begin
  FConfig := GetNest4D.Get<TNFeConfig>;
  FInfra := AInfra;
  FInfra.NFeConfig := FConfig;
end;

destructor TXMLRepository.Destroy;
begin
  FConfig := nil;
  FInfra.Free;
  inherited;
end;

function TXMLRepository.NFeCancelamentoXML(const AChave: String): TFileStream;
var
  LFile: String;
begin
  LFile := FInfra.NFeCancelamentoXML(AChave);
  if LFile = '' then
    raise EXMLNotExistException.Create(Format('O arquivo XML com a chave: [%s-nfe.xml] não foi encontrado.', [AChave]));

  Result := TFileStream.Create(LFile, fmOpenRead or fmShareDenyWrite);
end;

function TXMLRepository.NFeCartaCorrecaoXML(const AChave: String): TFileStream;
var
  LFile: String;
begin
  LFile := FInfra.NFeCartaCorrecaoXML(AChave);
  if LFile = '' then
    raise EXMLNotExistException.Create(Format('O arquivo XML com a chave: [%s-nfe.xml] não foi encontrado.', [AChave]));

  Result := TFileStream.Create(LFile, fmOpenRead or fmShareDenyWrite);
end;

function TXMLRepository.NFeInutilizacaoXML(const AProtocolo: String): TFileStream;
var
  LFile: String;
begin
  LFile := FInfra.NFeInutilizacaoXML(AProtocolo);
  if LFile = '' then
    raise EXMLNotExistException.Create(Format('O arquivo XML com o protocolo: [%s] não foi encontrado.', [AProtocolo]));

  Result := TFileStream.Create(LFile, fmOpenRead or fmShareDenyWrite);
end;

function TXMLRepository.NFeXML(const AChave: String): TFileStream;
var
  LFile: String;
begin
  LFile := FInfra.NFeXML(AChave);
  if LFile = '' then
    raise EXMLNotExistException.Create(Format('O arquivo XML com a chave: [%s-nfe.xml] não foi encontrado.', [AChave]));

  Result := TFileStream.Create(LFile, fmOpenRead or fmShareDenyWrite);
end;

end.

