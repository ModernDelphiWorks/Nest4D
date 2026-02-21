unit nfe.acbr.command.cancela.consulta;

interface

uses
  SysUtils,
  System.Classes,
  ACBrNFe,
  ACBrNFe.Classes,
  pcnConversaoNFe,
  pcnConversao,
//  pcnEnvEventoNFe,
//  pcnEventoNFe,
  nfebr.lib.enum,
  nfebr.lib.utils,
  nfebr.lib.include,
  nfe.acbr.interfaces;

type
  TNFeCancelaConsultaCommand = class(TInterfacedObject, INFeCancelaConsultaCommand)
  private
    FACBrNFe: TACBrNFe;
    function _FactoryResponse: TNFeResponseCancelaConsulta;
  public
    class function New(ANFe: TACBrNFe): INFeCancelaConsultaCommand;
    constructor Create(ANFe: TACBrNFe);
    destructor Destroy; override;
    function Execute(const AReq: TNFeRequestConsulta): TNFeResponseCancelaConsulta;
  end;

implementation

uses
  nfe.acbr.helper;

{ TNFeConsultaCommand }

constructor TNFeCancelaConsultaCommand.Create(ANFe: TACBrNFe);
begin
  FACBrNFe := ANFe;
end;

destructor TNFeCancelaConsultaCommand.Destroy;
begin
  FACBrNFe := nil;
  inherited;
end;

function TNFeCancelaConsultaCommand.Execute(const AReq: TNFeRequestConsulta): TNFeResponseCancelaConsulta;
begin
  FACBrNFe.WebServices.Consulta.Clear;
  try
    FACBrNFe.Consultar(AReq.Chave);
  finally
    Result := _FactoryResponse;
  end;
end;

class function TNFeCancelaConsultaCommand.New(ANFe: TACBrNFe): INFeCancelaConsultaCommand;
begin
  Result := Self.Create(ANFe);
end;

function TNFeCancelaConsultaCommand._FactoryResponse: TNFeResponseCancelaConsulta;
var
  LResponse: TNFeResponseCancelaConsulta;
begin
  LResponse := TNFeResponseCancelaConsulta.Create;
  LResponse.Versao := FACBrNFe.ResponseCancelaConsulta.versao;
  LResponse.Ambiente := TpAmbToStr(FACBrNFe.ResponseCancelaConsulta.TpAmb);
  LResponse.VerAplic := FACBrNFe.ResponseCancelaConsulta.verAplic;
  LResponse.CodStatus := FACBrNFe.ResponseCancelaConsulta.cStat;
  LResponse.Motivo := FACBrNFe.ResponseCancelaConsulta.XMotivo;
  LResponse.Mensagem := FACBrNFe.ResponseConsulta.Msg;
  LResponse.UF := FACBrNFe.ResponseCancelaConsulta.cUF;
  LResponse.DataRecbto := FACBrNFe.ResponseCancelaConsulta.DhRecbto;
  LResponse.ChaveAcesso := FACBrNFe.ResponseCancelaConsulta.chNFE;
  LResponse.Protocolo := FACBrNFe.ResponseCancelaConsulta.nProt;
  LResponse.CodMsg := FACBrNFe.ResponseConsulta.protNFe.cMsg;
  LResponse.Msg := FACBrNFe.ResponseConsulta.protNFe.xMsg;
  LResponse.DigValor := FACBrNFe.ResponseConsulta.protNFe.digVal;
  if FACBrNFe.ResponseConsulta.retCancNFe.nProt <> '' then
  begin
    LResponse.InfoCancela.Ambiente := TpAmbToStr(FACBrNFe.ResponseConsulta.retCancNFe.tpAmb);
    LResponse.InfoCancela.VerAplic := FACBrNFe.ResponseConsulta.retCancNFe.verAplic;
    LResponse.InfoCancela.CodStatus := FACBrNFe.ResponseConsulta.retCancNFe.cStat;
    LResponse.InfoCancela.Motivo := FACBrNFe.ResponseConsulta.retCancNFe.xMotivo;
    LResponse.InfoCancela.UF := FACBrNFe.ResponseConsulta.retCancNFe.cUF;
    LResponse.InfoCancela.ChaveAcesso := FACBrNFe.ResponseConsulta.retCancNFe.chNFE;
    LResponse.InfoCancela.DataRecbto := FACBrNFe.ResponseConsulta.retCancNFe.dhRecbto;
    LResponse.InfoCancela.Protocolo := FACBrNFe.ResponseConsulta.retCancNFe.nProt;
  end;
  Result := LResponse;
end;

end.
