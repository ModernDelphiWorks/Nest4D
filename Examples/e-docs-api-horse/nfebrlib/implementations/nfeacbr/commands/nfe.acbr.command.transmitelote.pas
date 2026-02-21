unit nfe.acbr.command.transmitelote;

interface

uses
  SysUtils,
  Classes,
  Generics.Collections,
  ACBrNFe,
  ACBrNFeNotasFiscais,
  ACBrNFe.Classes,
  pcnConversaoNFe,
  pcnConversao,
//  pcnEnvEventoNFe,
//  pcnEventoNFe,
  nfebr.lib.enum,
  nfebr.lib.utils,
  nfebr.lib.include,
  nfe.acbr.factory,
  nfe.acbr.interfaces;

type
  TNFeTransmiteLoteCommand = class(TInterfacedObject, INFeTransmiteLoteCommand)
  private
    FACBrNFe: TACBrNFe;
    function _FactoryResponse: TNFeResponseTransmiteLote;
    function _NotaFiscalFind(const AChave: String): NotaFiscal;
  public
    constructor Create(ANFe: TACBrNFe);
    destructor Destroy; override;
    class function New(ANFe: TACBrNFe): INFeTransmiteLoteCommand;
    function Execute(const AReq: TObjectList<TNFeRequestTransmite>;
      const AIsSincrona: boolean): TNFeResponseTransmiteLote;
  end;

implementation

uses
  nfe.acbr.helper;

{ TNFeTransmiteLoteCommand }

constructor TNFeTransmiteLoteCommand.Create(ANFe: TACBrNFe);
begin
  FACBrNFe := ANFe;
end;

destructor TNFeTransmiteLoteCommand.Destroy;
begin
  FACBrNFe := nil;
  inherited;
end;

function TNFeTransmiteLoteCommand.Execute(const AReq: TObjectList<TNFeRequestTransmite>;
  const AIsSincrona: boolean): TNFeResponseTransmiteLote;
var
  LNrLote: Int64;
  LAddNF: NotaFiscal;
  LFor: integer;
begin
  Result := nil;
  LNrLote := TUtils.GerarNumeroSequencial;
  try
    FACBrNFe.NotasFiscais.Clear;
    for LFor := 0 to AReq.Count -1 do
    begin
      LAddNF := FACBrNFe.NotasFiscais.Add;
      TNFeFactory.New(FACBrNFe).Execute(LFor, AReq[LFor].NotaFiscal);
    end;
    FACBrNFe.NotasFiscais.Assinar;
    FACBrNFe.NotasFiscais.Validar;
    FACBrNFe.Enviar(LNrLote, false, AIsSincrona);
  finally
    if FACBrNFe.NotasFiscais.Count > 0 then
      Result := _FactoryResponse;
    FACBrNFe.NotasFiscais.Clear;
  end;
end;

class function TNFeTransmiteLoteCommand.New(ANFe: TACBrNFe): INFeTransmiteLoteCommand;
begin
  Result := Self.Create(ANFe);
end;

function TNFeTransmiteLoteCommand._FactoryResponse: TNFeResponseTransmiteLote;
var
  LResponse: TNFeResponseTransmiteLote;
  LNotaResponse: TNFeResponseNota;
  LNFeFind: NotaFiscal;
  LFor: integer;
begin
  LResponse := TNFeResponseTransmiteLote.Create;
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
  //
  for LFor := 0 to FACBrNFe.ResponseTransmiteItem.Count - 1 do
  begin
    LNFeFind := _NotaFiscalFind('NFe' + FACBrNFe.ResponseTransmiteItem.Items[LFor].chDFe);
    if LNFeFind = nil then
      continue;
    LNotaResponse := TNFeResponseNota.Create;
    LNotaResponse.Numero := LNFeFind.NFe.Ide.nNF;
    LNotaResponse.DataEmissao := LNFeFind.NFe.Ide.dEmi;
    LNotaResponse.Modelo := LNFeFind.NFe.Ide.modelo;
    LNotaResponse.Serie := LNFeFind.NFe.Ide.serie;
    LNotaResponse.ValorTotal := LNFeFind.NFe.Total.ICMSTot.vNF;
    //
    LNotaResponse.ItemRetorno.Id := FACBrNFe.ResponseTransmiteItem.Items[LFor].Id;
    LNotaResponse.ItemRetorno.Ambiente := TpAmbToStr(FACBrNFe.ResponseTransmiteItem.Items[LFor].tpAmb);
    LNotaResponse.ItemRetorno.VerAplic := FACBrNFe.ResponseTransmiteItem.Items[LFor].verAplic;
    LNotaResponse.ItemRetorno.ChaveAcesso := FACBrNFe.ResponseTransmiteItem.Items[LFor].chDFe;
    LNotaResponse.ItemRetorno.DataRecbto := FACBrNFe.ResponseTransmiteItem.Items[LFor].dhRecbto;
    LNotaResponse.ItemRetorno.Protocolo := FACBrNFe.ResponseTransmiteItem.Items[LFor].nProt;
    LNotaResponse.ItemRetorno.DigestValue := FACBrNFe.ResponseTransmiteItem.Items[LFor].digVal;
    LNotaResponse.ItemRetorno.CodStatus := FACBrNFe.ResponseTransmiteItem.Items[LFor].cStat;
    LNotaResponse.ItemRetorno.Motivo := FACBrNFe.ResponseTransmiteItem.Items[LFor].xMotivo;
    //
    LResponse.ResponseNotas.Add(LNotaResponse);
  end;
  Result := LResponse;
end;

function TNFeTransmiteLoteCommand._NotaFiscalFind(
  const AChave: String): NotaFiscal;
var
 LFor: integer;
 LResult: NotaFiscal;
begin
  LResult := nil;
  for LFor := 0 to FACBrNFe.NotasFiscais.Count - 1 do
  begin
    LResult := FACBrNFe.NotasFiscais.Items[LFor];
    if AChave = LResult.NFe.infNFe.ID then
      break;
  end;
  Result := LResult;
end;

end.
