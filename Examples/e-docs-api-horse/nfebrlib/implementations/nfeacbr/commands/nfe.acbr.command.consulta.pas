unit nfe.acbr.command.consulta;

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
  TNFeConsultaCommand = class(TInterfacedObject, INFeConsultaCommand)
  private
    FACBrNFe: TACBrNFe;
    function _FactoryResponse: TNFeResponseConsulta;
  public
    class function New(ANFe: TACBrNFe): INFeConsultaCommand;
    constructor Create(ANFe: TACBrNFe);
    destructor Destroy; override;
    function Execute(const AReq: TNFeRequestConsulta): TNFeResponseConsulta;
  end;

implementation

uses
  nfe.acbr.helper;

{ TNFeConsultaCommand }

constructor TNFeConsultaCommand.Create(ANFe: TACBrNFe);
begin
  FACBrNFe := ANFe;
end;

destructor TNFeConsultaCommand.Destroy;
begin
  FACBrNFe := nil;
  inherited;
end;

function TNFeConsultaCommand.Execute(const AReq: TNFeRequestConsulta): TNFeResponseConsulta;
begin
  FACBrNFe.WebServices.Consulta.Clear;
  try
    FACBrNFe.Consultar(AReq.Chave);
  finally
    Result := _FactoryResponse;
  end;
end;

class function TNFeConsultaCommand.New(ANFe: TACBrNFe): INFeConsultaCommand;
begin
  Result := Self.Create(ANFe);
end;

function TNFeConsultaCommand._FactoryResponse: TNFeResponseConsulta;
var
  LResponse: TNFeResponseConsulta;
begin
  LResponse := TNFeResponseConsulta.Create;
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
  Result := LResponse;
end;

end.
