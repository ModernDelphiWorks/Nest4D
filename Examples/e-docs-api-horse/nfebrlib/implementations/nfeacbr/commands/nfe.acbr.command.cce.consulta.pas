unit nfe.acbr.command.cce.consulta;

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
  TNFeCCeConsultaCommand = class(TInterfacedObject, INFeCCeConsultaCommand)
  private
    FACBrNFe: TACBrNFe;
    function _FactoryResponse: TNFeResponseCCeConsulta;
  public
    class function New(ANFe: TACBrNFe): INFeCCeConsultaCommand;
    constructor Create(ANFe: TACBrNFe);
    destructor Destroy; override;
    function Execute(const AReq: TNFeRequestConsulta): TNFeResponseCCeConsulta;
  end;

implementation

uses
  nfe.acbr.helper;

{ TNFeConsultaCommand }

constructor TNFeCCeConsultaCommand.Create(ANFe: TACBrNFe);
begin
  FACBrNFe := ANFe;
end;

destructor TNFeCCeConsultaCommand.Destroy;
begin
  FACBrNFe := nil;
  inherited;
end;

function TNFeCCeConsultaCommand.Execute(const AReq: TNFeRequestConsulta): TNFeResponseCCeConsulta;
begin
  FACBrNFe.WebServices.Consulta.Clear;
  try
    FACBrNFe.Consultar(AReq.Chave);
  finally
    Result := _FactoryResponse;
  end;
end;

class function TNFeCCeConsultaCommand.New(ANFe: TACBrNFe): INFeCCeConsultaCommand;
begin
  Result := Self.Create(ANFe);
end;

function TNFeCCeConsultaCommand._FactoryResponse: TNFeResponseCCeConsulta;
var
  LResponse: TNFeResponseCCeConsulta;
  LEvent: TNFeResponseEventsConsulta;
  LFor: integer;
begin
  LResponse := TNFeResponseCCeConsulta.Create;
  LResponse.Versao := FACBrNFe.ResponseConsulta.versao;
  LResponse.Ambiente := TpAmbToStr(FACBrNFe.ResponseConsulta.TpAmb);
  LResponse.VerAplic := FACBrNFe.ResponseConsulta.verAplic;
  LResponse.CodStatus := FACBrNFe.ResponseConsulta.cStat;
  LResponse.Motivo := FACBrNFe.ResponseConsulta.XMotivo;
  LResponse.Mensagem := FACBrNFe.ResponseConsulta.protNFe.xMsg;
  LResponse.UF := FACBrNFe.ResponseConsulta.cUF;
  LResponse.DataRecbto := FACBrNFe.ResponseConsulta.DhRecbto;
  LResponse.ChaveAcesso := FACBrNFe.ResponseConsulta.NFeChave;
  LResponse.Protocolo := FACBrNFe.ResponseConsulta.Protocolo;
  LResponse.CodMsg := FACBrNFe.ResponseConsulta.protNFe.cMsg;
  LResponse.Msg := FACBrNFe.ResponseConsulta.protNFe.xMsg;
  LResponse.DigValor := FACBrNFe.ResponseConsulta.protNFe.digVal;
  for LFor := 0 to FACBrNFe.ResponseCCeConsulta.Count - 1 do
  begin
    LEvent := TNFeResponseEventsConsulta.Create;
    LEvent.Id := FACBrNFe.ResponseCCeConsulta.Items[LFor].RetEventoNFe.InfEvento.id;
    LEvent.Ambiente := TpAmbToStr(FACBrNFe.ResponseCCeConsulta.Items[LFor].RetEventoNFe.InfEvento.tpAmb);
    LEvent.UF := FACBrNFe.ResponseCCeConsulta.Items[LFor].RetEventoNFe.InfEvento.cOrgao;
    LEvent.CNPJ := FACBrNFe.ResponseCCeConsulta.Items[LFor].RetEventoNFe.InfEvento.CNPJ;
    LEvent.ChaveAceso := FACBrNFe.ResponseCCeConsulta.Items[LFor].RetEventoNFe.InfEvento.chNFe;
    LEvent.DataRegEvento := FACBrNFe.ResponseCCeConsulta.Items[LFor].RetEventoNFe.InfEvento.dhEvento;
    LEvent.TipoEvento := TpEventoToStr(FACBrNFe.ResponseCCeConsulta.Items[LFor].RetEventoNFe.InfEvento.tpEvento);
    LEvent.Sequencia := FACBrNFe.ResponseCCeConsulta.Items[LFor].RetEventoNFe.InfEvento.nSeqEvento;
    LEvent.VersaoEvent := FACBrNFe.ResponseCCeConsulta.Items[LFor].RetEventoNFe.InfEvento.versaoEvento;
    //
    LEvent.DetEvento.Versao := FACBrNFe.ResponseCCeConsulta.Items[LFor].RetEventoNFe.InfEvento.detEvento.versao;
    LEvent.DetEvento.descEvento := FACBrNFe.ResponseCCeConsulta.Items[LFor].RetEventoNFe.InfEvento.detEvento.descEvento;
    LEvent.DetEvento.xCorrecao := FACBrNFe.ResponseCCeConsulta.Items[LFor].RetEventoNFe.InfEvento.detEvento.xCorrecao;
    LEvent.DetEvento.xCondUso := FACBrNFe.ResponseCCeConsulta.Items[LFor].RetEventoNFe.InfEvento.detEvento.xCondUso;
    LEvent.DetEvento.nProt := FACBrNFe.ResponseCCeConsulta.Items[LFor].RetEventoNFe.InfEvento.detEvento.nProt;
    LEvent.DetEvento.xJust := FACBrNFe.ResponseCCeConsulta.Items[LFor].RetEventoNFe.InfEvento.detEvento.xJust;
    LEvent.DetEvento.cOrgaoAutor := IntToStr(FACBrNFe.ResponseCCeConsulta.Items[LFor].RetEventoNFe.InfEvento.detEvento.cOrgaoAutor);
    LEvent.DetEvento.tpAutor := TipoAutorToStr(FACBrNFe.ResponseCCeConsulta.Items[LFor].RetEventoNFe.InfEvento.detEvento.tpAutor);
    LEvent.DetEvento.dhEmi := FACBrNFe.ResponseCCeConsulta.Items[LFor].RetEventoNFe.InfEvento.detEvento.dhEmi;
    LEvent.DetEvento.tpNF := tpNFToStr(FACBrNFe.ResponseCCeConsulta.Items[LFor].RetEventoNFe.InfEvento.detEvento.tpNF);
    LEvent.DetEvento.IE := FACBrNFe.ResponseCCeConsulta.Items[LFor].RetEventoNFe.InfEvento.detEvento.IE;
    LEvent.DetEvento.DESTCNPJCPF := FACBrNFe.ResponseCCeConsulta.Items[LFor].RetEventoNFe.InfEvento.detEvento.dest.CNPJCPF;
    LEvent.DetEvento.DESTidEstrangeiro := FACBrNFe.ResponseCCeConsulta.Items[LFor].RetEventoNFe.InfEvento.detEvento.dest.idEstrangeiro;
    LEvent.DetEvento.DESTIE := FACBrNFe.ResponseCCeConsulta.Items[LFor].RetEventoNFe.InfEvento.detEvento.dest.IE;
    LEvent.DetEvento.DESTUF := FACBrNFe.ResponseCCeConsulta.Items[LFor].RetEventoNFe.InfEvento.detEvento.dest.UF;
    LEvent.DetEvento.vNF := FACBrNFe.ResponseCCeConsulta.Items[LFor].RetEventoNFe.InfEvento.detEvento.vNF;
    LEvent.DetEvento.vST := FACBrNFe.ResponseCCeConsulta.Items[LFor].RetEventoNFe.InfEvento.detEvento.vST;
    LEvent.DetEvento.idPedidoCancelado := FACBrNFe.ResponseCCeConsulta.Items[LFor].RetEventoNFe.InfEvento.detEvento.idPedidoCancelado;
    //
    LResponse.ConsultaCCeEvents.Add(LEvent);
  end;
  Result := LResponse;
end;

end.
