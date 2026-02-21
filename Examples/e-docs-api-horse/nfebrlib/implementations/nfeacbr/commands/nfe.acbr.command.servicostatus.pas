unit nfe.acbr.command.servicostatus;

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
  nfebr.lib.include,
  nfe.acbr.interfaces;

type
  TNFeServicoStatusCommand = class(TInterfacedObject, INFeServicoStatusCommand)
  private
    FACBrNFe: TACBrNFe;
    function _FactoryResponse: TNFeResponseServicoStatus;
  public
    class function New(ANFe: TACBrNFe): INFeServicoStatusCommand;
    constructor Create(ANFe: TACBrNFe);
    destructor Destroy; override;
    function Execute: TNFeResponseServicoStatus;
  end;

implementation

uses
  nfe.acbr.helper;

{ TNFeServicoStatusCommand }

constructor TNFeServicoStatusCommand.Create(ANFe: TACBrNFe);
begin
  FACBrNFe := ANFe;
end;

destructor TNFeServicoStatusCommand.Destroy;
begin
  FACBrNFe := nil;
  inherited;
end;

function TNFeServicoStatusCommand.Execute: TNFeResponseServicoStatus;
begin
  try
    FACBrNFe.WebServices.StatusServico.Executar;
  finally
    Result := _FactoryResponse;
  end;
end;

class function TNFeServicoStatusCommand.New(ANFe: TACBrNFe): INFeServicoStatusCommand;
begin
  Result := Self.Create(ANFe);
end;

function TNFeServicoStatusCommand._FactoryResponse: TNFeResponseServicoStatus;
var
  LResponse: TNFeResponseServicoStatus;
begin
  LResponse := TNFeResponseServicoStatus.Create;
  LResponse.Versao := FACBrNFe.WebServices.StatusServico.versao;
  LResponse.Ambiente := TpAmbToStr(FACBrNFe.WebServices.StatusServico.tpAmb);
  LResponse.VerAplic := FACBrNFe.WebServices.StatusServico.verAplic;
  LResponse.CodStatus := FACBrNFe.WebServices.StatusServico.cStat;
  LResponse.Motivo := FACBrNFe.WebServices.StatusServico.xMotivo;
  LResponse.UF := FACBrNFe.WebServices.StatusServico.cUF;
  LResponse.Mensagem := FACBrNFe.WebServices.StatusServico.Msg;
  LResponse.DataRecbto := FACBrNFe.WebServices.StatusServico.dhRecbto;
  LResponse.DataRetorno := FACBrNFe.WebServices.StatusServico.dhRetorno;
  LResponse.TMResposta := FACBrNFe.WebServices.StatusServico.TMed;
  LResponse.Obs := FACBrNFe.WebServices.StatusServico.xObs;
  // Remove quebra de linha
  LResponse.Mensagem := StringReplace(LResponse.Mensagem, #$D#$A, '', [rfReplaceAll]);
  Result := LResponse;
end;

end.
