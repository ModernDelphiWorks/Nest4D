unit pdf.repository;

interface

uses
  SysUtils,
  Classes,
  core.types,
  pdf.infra,
  pdf.interfaces,
  nfebr.config;

type
  TPDFRepository = class
  private
    FInfra: TPDFInfra;
    FConfig: TNFeConfig;
  public
    constructor Create(const AInfra: TPDFInfra);
    destructor Destroy; override;
    function NFePDF(const AChave: String): TFileStream;
    function NFeCancelamentoPDF(const AChave: String): TFileStream;
    function NFeInutilizacaoPDF(const AProtocolo: String): TFileStream;
    function NFeCartaCorrecaoPDF(const AChave: String): TFileStream;
  end;

implementation

uses
  nest4d,
  core.exception;

{ TPDFRepository }

constructor TPDFRepository.Create(const AInfra: TPDFInfra);
begin
  FConfig := GetNest4D.Get<TNFeConfig>;
  FInfra := AInfra;
  FInfra.NFeConfig := FConfig;
end;

destructor TPDFRepository.Destroy;
begin
  FConfig := nil;
  FInfra.Free;
  inherited;
end;

function TPDFRepository.NFeCancelamentoPDF(const AChave: String): TFileStream;
var
  LFile: String;
begin
  LFile := FInfra.NFeCancelamentoPDF(AChave);
  if LFile = '' then
    raise EPDFNotExistException.Create(Format('O arquivo PDF com a chave: [%s-nfe.pdf] não foi encontrado.', [AChave]));

  Result := TFileStream.Create(LFile, fmOpenRead or fmShareDenyWrite);
end;

function TPDFRepository.NFeCartaCorrecaoPDF(const AChave: String): TFileStream;
var
  LFile: String;
begin
  LFile := FInfra.NFeCartaCorrecaoPDF(AChave);
  if LFile = '' then
    raise EPDFNotExistException.Create(Format('O arquivo PDF com a chave: [%s-nfe.pdf] não foi encontrado.', [AChave]));

  Result := TFileStream.Create(LFile, fmOpenRead or fmShareDenyWrite);
end;

function TPDFRepository.NFeInutilizacaoPDF(const AProtocolo: String): TFileStream;
var
  LFile: String;
begin
  LFile := FInfra.NFeInutilizacaoPDF(AProtocolo);
  if LFile = '' then
    raise EPDFNotExistException.Create(Format('O arquivo PDF com o protocolo: [%s] não foi encontrado.', [AProtocolo]));

  Result := TFileStream.Create(LFile, fmOpenRead or fmShareDenyWrite);
end;

function TPDFRepository.NFePDF(const AChave: String): TFileStream;
var
  LFile: String;
begin
  LFile := FInfra.NFePDF(AChave);
  if LFile = '' then
    raise EPDFNotExistException.Create(Format('O arquivo PDF com a chave: [%s-nfe.pdf] não foi encontrado.', [AChave]));

  Result := TFileStream.Create(LFile, fmOpenRead or fmShareDenyWrite);
end;

end.

