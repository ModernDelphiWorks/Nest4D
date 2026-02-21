unit nfe.acbr.command.cce;

interface

uses
  SysUtils,
  ACBrNFe,
  ACBrNFe.Classes,
  ACBrNFe.EnvEvento,
  ACBrNFe.RetEnvEvento,
  pcnConversaoNFe,
  pcnConversao,
//  pcnEnvEventoNFe,
//  pcnEventoNFe,
//  pcnRetEnvEventoNFe,
  nfebr.lib.enum,
  nfebr.lib.utils,
  nfebr.lib.include,
  nfe.acbr.interfaces;

type
  TNFeCCeCommand = class(TInterfacedObject, INFeCartaCorrecaoCommand)
  private
    FACBrNFe: TACBrNFe;
    function _FactoryResponse: TNFeResponseCCe;
  public
    class function New(ANFe: TACBrNFe): INFeCartaCorrecaoCommand;
    constructor Create(ANFe: TACBrNFe);
    destructor Destroy; override;
    function Execute(const AReq: TNFeRequestCCe): TNFeResponseCCe;
  end;

implementation

uses
  nfe.acbr.helper;

{ TNFeCCeCommand }

constructor TNFeCCeCommand.Create(ANFe: TACBrNFe);
begin
  FACBrNFe := ANFe;
end;

destructor TNFeCCeCommand.Destroy;
begin
  FACBrNFe := nil;
  inherited;
end;

function TNFeCCeCommand.Execute(const AReq: TNFeRequestCCe): TNFeResponseCCe;
var
  LEvent: TEventoNFe;
begin
  LEvent := FACBrNFe.EventoNFe;
  LEvent.Evento.Clear;
  LEvent.idLote := TUtils.GerarNumeroSequencial;
  with LEvent.Evento.New do
  begin
    infEvento.chNFe := AReq.Chave;
    infEvento.cOrgao := AReq.FOrgao;
    infEvento.CNPJ := AReq.FCNPJ;
    infEvento.nSeqEvento := AReq.Sequence;
    infEvento.detEvento.xCorrecao := AReq.Correcao;
    infEvento.dhEvento := Now;
    infEvento.tpEvento := teCCe;
  end;
  try
    FACBrNFe.EnviarEvento(LEvent.idLote);
  finally
    Result := _FactoryResponse;
  end;
end;

class function TNFeCCeCommand.New(ANFe: TACBrNFe): INFeCartaCorrecaoCommand;
begin
  Result := Self.Create(ANFe);
end;

function TNFeCCeCommand._FactoryResponse: TNFeResponseCCe;
var
  LResponse: TNFeResponseCCe;
  LResponseItem: TNFeItemResponseCCe;
  LRetEventoItem: TRetInfEventoCollectionItem;
  LFor: integer;
begin
  LResponse := TNFeResponseCCe.Create;
  LResponse.VerAplic := FACBrNFe.ResponseCCe.verAplic;
  LResponse.Ambiente := TpAmbToStr(FACBrNFe.ResponseCCe.tpAmb);
  LResponse.CodStatus := FACBrNFe.ResponseCCe.cStat;
  LResponse.Motivo := FACBrNFe.ResponseCCe.xMotivo;
  LResponse.UF := FACBrNFe.ResponseCCe.cOrgao;
  LResponse.IDLote := FACBrNFe.ResponseCCe.idLote;
  //
  for LFor := 0 to FACBrNFe.ResponseCCe.retEvento.Count - 1 do
  begin
    LRetEventoItem := FACBrNFe.ResponseCCe.retEvento.Items[LFor];
    if LRetEventoItem = nil then
      continue;
    LResponseItem := TNFeItemResponseCCe.Create;
    LResponseItem.Id := LRetEventoItem.RetInfEvento.Id;
    LResponseItem.Ambiente := TpAmbToStr(LRetEventoItem.RetInfEvento.tpAmb);
    LResponseItem.VerAplic := LRetEventoItem.RetInfEvento.verAplic;
    LResponseItem.CodStatus := LRetEventoItem.RetInfEvento.cStat;
    LResponseItem.UF := LRetEventoItem.RetInfEvento.cOrgao;
    LResponseItem.Motivo := LRetEventoItem.RetInfEvento.xMotivo;
    LResponseItem.ChaveAcesso := LRetEventoItem.RetInfEvento.chNFe;
    LResponseItem.TipoEvento := TpEventoToStr(LRetEventoItem.RetInfEvento.tpEvento);
    LResponseItem.Justificativa := LRetEventoItem.RetInfEvento.xEvento;
    LResponseItem.Sequencia := LRetEventoItem.RetInfEvento.nSeqEvento;
    LResponseItem.CNPJDest := LRetEventoItem.RetInfEvento.CNPJDest;
    LResponseItem.EmailDest := LRetEventoItem.RetInfEvento.emailDest;
    LResponseItem.dhRegEvento := LRetEventoItem.RetInfEvento.dhRegEvento;
    LResponseItem.Protocolo := LRetEventoItem.RetInfEvento.nProt;
    LResponse.EventItems.Add(LResponseItem);
  end;
  Result := LResponse;
end;

end.



