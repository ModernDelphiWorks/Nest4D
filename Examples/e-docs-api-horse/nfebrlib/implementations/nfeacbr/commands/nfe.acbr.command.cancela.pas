unit nfe.acbr.command.cancela;

interface

uses
  SysUtils,
  ACBrNFe,
  ACBrUtil.Strings,
  ACBrNFe.Classes,
  ACBrNFe.EnvEvento,
  pcnConversaoNFe,
  pcnConversao,
//  pcnEventoNFe,
//  pcnEnvEventoNFe,
  nfebr.lib.enum,
  nfebr.lib.utils,
  nfebr.lib.include,
  nfe.acbr.interfaces;

type
  TNFeCancelaCommand = class(TInterfacedObject, INFeCancelaCommand)
  private
    FACBrNFe: TACBrNFe;
    function _FactoryResponse: TNFeResponseCancela;
  public
    class function New(ANFe: TACBrNFe): INFeCancelaCommand;
    constructor Create(ANFe: TACBrNFe);
    destructor Destroy; override;
    function Execute(const AReq: TNFeRequestCancela): TNFeResponseCancela;
  end;

implementation

uses
  nfe.acbr.helper;

{ TNFeCancelaCommand }

constructor TNFeCancelaCommand.Create(ANFe: TACBrNFe);
begin
  FACBrNFe := ANFe;
end;

destructor TNFeCancelaCommand.Destroy;
begin
  FACBrNFe := nil;
  inherited;
end;

function TNFeCancelaCommand.Execute(const AReq: TNFeRequestCancela): TNFeResponseCancela;
var
  LEvent: TEventoNFe;
begin
  LEvent := FACBrNFe.EventoNFe;
  LEvent.Evento.Clear;
  LEvent.idLote := TUtils.GerarNumeroSequencial;
  with LEvent.Evento.New do
  begin
    infEvento.chNFe := OnlyNumber(AReq.Chave);
    infEvento.cOrgao := AReq.Orgao;
    infEvento.CNPJ := AReq.CNPJ;
    infEvento.nSeqEvento := AReq.Sequence;
    infEvento.detEvento.xJust := AReq.Justificativa;
    infEvento.detEvento.nProt := AReq.Protocolo;
    infEvento.dhEvento := Now;
    infEvento.tpEvento := teCancelamento;
  end;
  try
    FACBrNFe.EnviarEvento(LEvent.idLote);
  finally
    Result := _FactoryResponse;
  end;
end;

class function TNFeCancelaCommand.New(ANFe: TACBrNFe): INFeCancelaCommand;
begin
  Result := Self.Create(ANFe);
end;

function TNFeCancelaCommand._FactoryResponse: TNFeResponseCancela;
var
  LResponse: TNFeResponseCancela;
begin
  LResponse := TNFeResponseCancela.Create;
  LResponse.Ambiente := TpAmbToStr(FACBrNFe.ResponseCancela.tpAmb);
  LResponse.VerAplic := FACBrNFe.ResponseCancela.verAplic;
  LResponse.Versao := FACBrNFe.ResponseCancela.verAplic;
  LResponse.CodStatus := FACBrNFe.ResponseCancela.cStat;
  LResponse.Motivo := FACBrNFe.ResponseCancela.xMotivo;
  LResponse.UF := FACBrNFe.ResponseCancela.cOrgao;
  LResponse.ChaveAcesso := FACBrNFe.ResponseCancela.chNFe;
  LResponse.DataRecbto := FACBrNFe.ResponseCancela.dhRegEvento;
  LResponse.Protocolo := FACBrNFe.ResponseCancela.nProt;
  LResponse.TipoEvento := TpEventoToStr(FACBrNFe.ResponseCancela.tpEvento);
  LResponse.Justificativa := FACBrNFe.ResponseCancela.xEvento;
  LResponse.Sequencia := FACBrNFe.ResponseCancela.nSeqEvento;
  LResponse.CNPJDest := FACBrNFe.ResponseCancela.CNPJDest;
  LResponse.EmailDest := FACBrNFe.ResponseCancela.emailDest;
  Result := LResponse;
end;

end.



