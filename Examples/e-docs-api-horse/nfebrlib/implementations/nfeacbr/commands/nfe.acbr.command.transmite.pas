unit nfe.acbr.command.transmite;

interface

uses
  SysUtils,
  Classes,
  ACBrNFe,
  ACBrNFeNotasFiscais,
  ACBrNFe.Classes,
  pcnConversaoNFe,
//  pcnEnvEventoNFe,
//  pcnEventoNFe,
  nfebr.lib.enum,
  nfebr.lib.utils,
  nfebr.lib.include,
  nfe.acbr.helper,
  nfe.acbr.factory,
  nfe.acbr.interfaces;

type
  TNFeTransmiteCommand = class(TInterfacedObject, INFeTransmiteCommand)
  private
    FACBrNFe: TACBrNFe;
    function _FactoryResponse: TNFeResponseTransmite;
  public
    class function New(ANFe: TACBrNFe): INFeTransmiteCommand;
    constructor Create(ANFe: TACBrNFe);
    destructor Destroy; override;
    function Execute(const AReq: TNFeRequestTransmite;
      const AIsSincrona: boolean): TNFeResponseTransmite;
  end;

implementation

uses
  pcnConversao;

{ TNFeTransmiteCommand }

constructor TNFeTransmiteCommand.Create(ANFe: TACBrNFe);
begin
  FACBrNFe := ANFe;
end;

destructor TNFeTransmiteCommand.Destroy;
begin
  FACBrNFe := nil;
  inherited;
end;

function TNFeTransmiteCommand.Execute(const AReq: TNFeRequestTransmite;
  const AIsSincrona: boolean): TNFeResponseTransmite;
var
  LNrLote: Int64;
  LAddNF: NotaFiscal;
begin
  Result := nil;
  LNrLote := TUtils.GerarNumeroSequencial;
  try
    FACBrNFe.NotasFiscais.Clear;
    LAddNF := FACBrNFe.NotasFiscais.Add;
    TNFeFactory.New(FACBrNFe).Execute(0, AReq.NotaFiscal);
    FACBrNFe.NotasFiscais.Assinar;
    FACBrNFe.NotasFiscais.Validar;
    FACBrNFe.Enviar(LNrLote, false, AIsSincrona);
  finally
    if FACBrNFe.NotasFiscais.Count > 0 then
      Result := _FactoryResponse;
    FACBrNFe.NotasFiscais.Clear;
  end;
end;

class function TNFeTransmiteCommand.New(ANFe: TACBrNFe): INFeTransmiteCommand;
begin
  Result := Self.Create(ANFe);
end;

function TNFeTransmiteCommand._FactoryResponse: TNFeResponseTransmite;
var
  LResponse: TNFeResponseTransmite;
begin
  LResponse := TNFeResponseTransmite.Create;
  LResponse.Versao := FACBrNFe.WebServices.Enviar.Versao;
  LResponse.Ambiente := TpAmbToStr(FACBrNFe.WebServices.Enviar.TpAmb);
  LResponse.VerAplic := FACBrNFe.WebServices.Enviar.verAplic;
  LResponse.CodStatus := FACBrNFe.WebServices.Enviar.cStat;
  LResponse.Motivo := FACBrNFe.WebServices.Enviar.xMotivo;
  LResponse.UF := FACBrNFe.WebServices.Enviar.cUF;
  LResponse.Mensagem := FACBrNFe.WebServices.Enviar.Msg;
  LResponse.Recibo := FACBrNFe.WebServices.Enviar.Recibo;
  LResponse.DataRecbto := FACBrNFe.WebServices.Enviar.dhRecbto;
  LResponse.Protocolo := FACBrNFe.WebServices.Enviar.Protocolo;
  LResponse.TMResposta := FACBrNFe.WebServices.Enviar.TMed;
  // Informações da NFe
  if FACBrNFe.NotasFiscais.Count > 0 then
  begin
    LResponse.Numero := FACBrNFe.NotasFiscais.Items[0].NFe.Ide.nNF;
    LResponse.DataEmissao := FACBrNFe.NotasFiscais.Items[0].NFe.Ide.dEmi;
    LResponse.Modelo := FACBrNFe.NotasFiscais.Items[0].NFe.Ide.modelo;
    LResponse.Serie := FACBrNFe.NotasFiscais.Items[0].NFe.Ide.serie;
    LResponse.ValorTotal := FACBrNFe.NotasFiscais.Items[0].NFe.Total.ICMSTot.vNF;
  end;
  if FACBrNFe.ResponseTransmiteItem.Count > 0 then
  begin
    LResponse.ItemRetorno.Id := FACBrNFe.ResponseTransmiteItem.Items[0].Id;
    LResponse.ItemRetorno.Ambiente := TpAmbToStr(FACBrNFe.ResponseTransmiteItem.Items[0].tpAmb);
    LResponse.ItemRetorno.VerAplic := FACBrNFe.ResponseTransmiteItem.Items[0].verAplic;
    LResponse.ItemRetorno.ChaveAcesso := FACBrNFe.ResponseTransmiteItem.Items[0].chDFe;
    LResponse.ItemRetorno.DataRecbto := FACBrNFe.ResponseTransmiteItem.Items[0].dhRecbto;
    LResponse.ItemRetorno.Protocolo := FACBrNFe.ResponseTransmiteItem.Items[0].nProt;
    LResponse.ItemRetorno.DigestValue := FACBrNFe.ResponseTransmiteItem.Items[0].digVal;
    LResponse.ItemRetorno.CodStatus := FACBrNFe.ResponseTransmiteItem.Items[0].cStat;
    LResponse.ItemRetorno.Motivo := FACBrNFe.ResponseTransmiteItem.Items[0].xMotivo;
  end;
  Result := LResponse;
end;

end.
