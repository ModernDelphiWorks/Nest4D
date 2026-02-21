unit nfe.acbr.command.inutiliza;

interface

uses
  SysUtils,
  ACBrNFe,
  ACBrNFe.Classes,
  pcnConversaoNFe,
  pcnConversao,
  nfebr.lib.enum,
  nfe.acbr.interfaces,
  nfe.acbr.helper,
  nfebr.lib.include,
  nfebr.lib.utils;

type
  TNFeInutilizaCommand = class(TInterfacedObject, INFeInutilizaCommand)
  private
    FACBrNFe: TACBrNFe;
    function _FactoryResponse: TNFeResponseInutiliza;
  public
    class function New(ANFe: TACBrNFe): INFeInutilizaCommand;
    constructor Create(ANFe: TACBrNFe);
    destructor Destroy; override;
    function Execute(const AReq: TNFeRequestInutiliza): TNFeResponseInutiliza;
  end;

implementation

{ TNFeInutilizaCommand }

constructor TNFeInutilizaCommand.Create(ANFe: TACBrNFe);
begin
  FACBrNFe := ANFe;
end;

destructor TNFeInutilizaCommand.Destroy;
begin
  FACBrNFe := nil;
  inherited;
end;

function TNFeInutilizaCommand.Execute(const AReq: TNFeRequestInutiliza): TNFeResponseInutiliza;
begin
  try
    FACBrNFe.Inutilizar(FACBrNFe.SSL.CertCNPJ,
                        AReq.Justificativa,
                        AReq.Ano,
                        AReq.Serie,
                        AReq.NumeroInicial,
                        AReq.NumeroFinal);
  finally
    Result := _FactoryResponse;
  end;
end;

class function TNFeInutilizaCommand.New(ANFe: TACBrNFe): INFeInutilizaCommand;
begin
  Result := Self.Create(ANFe);
end;

function TNFeInutilizaCommand._FactoryResponse: TNFeResponseInutiliza;
var
  LResponse: TNFeResponseInutiliza;
begin
  LResponse := TNFeResponseInutiliza.Create;
  LResponse.Versao := FACBrNFe.ResponseInutiliza.versao;
  LResponse.Ambiente := TpAmbToStr(FACBrNFe.ResponseInutiliza.TpAmb);
  LResponse.VerAplic := FACBrNFe.ResponseInutiliza.verAplic;
  LResponse.CodStatus := FACBrNFe.ResponseInutiliza.cStat;
  LResponse.Motivo := FACBrNFe.ResponseInutiliza.xMotivo;
  LResponse.UF := FACBrNFe.ResponseInutiliza.cUF;
  //
  LResponse.Ano := FACBrNFe.ResponseInutiliza.Ano;
  LResponse.Cnpj := FACBrNFe.ResponseInutiliza.CNPJ;
  LResponse.Modelo := FACBrNFe.ResponseInutiliza.Modelo;
  LResponse.Serie := FACBrNFe.ResponseInutiliza.Serie;
  LResponse.NumInicial := FACBrNFe.ResponseInutiliza.NumeroInicial;
  LResponse.NumFinal := FACBrNFe.ResponseInutiliza.NumeroFinal;
  LResponse.DataRecbto := FACBrNFe.ResponseInutiliza.dhRecbto;
  LResponse.Protocolo := FACBrNFe.ResponseInutiliza.Protocolo;
  // No Manual não informa que esses dados abaixo vem no retorno da SEFAZ
  // A verificar no futuro
  LResponse.Mensagem := FACBrNFe.ResponseInutiliza.Msg;
  LResponse.Justificativa := FACBrNFe.ResponseInutiliza.Justificativa;
  Result := LResponse;
end;

end.



