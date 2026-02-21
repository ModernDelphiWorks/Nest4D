unit nfe.acbr.helper;

interface

uses
  Classes,
  ACBrNFe,
  ACBrNFeWebServices,
  pcnRetConsReciDFe,
  ACBrNFe.EventoClass,
  ACBrNFe.RetEnvEvento,
  ACBrNFe.RetConsSit;
//  pcnRetConsSitNFe,
//  pcnEventoNFe,
//  pcnRetEnvEventoNFe;

type
  TACBrNFeHelper = class helper for TACBrNFe
  public
    function ResponseTransmite: TNfeRetRecepcao;
    function ResponseTransmiteItem: TProtDFeCollection;
    function ResponseCancela: TRetInfEvento;
    function ResponseCCe: TRetEventoNFe;
    function ResponseCCeConsulta: TRetEventoNFeCollection;
    function ResponseInutiliza: TNFeInutilizacao;
    function ResponseServicoStatus: TNFeStatusServico;
    function ResponseConsulta: TNFeConsulta;
    function ResponseCancelaConsulta: TRetCancNFe;
    function ResponseRecibo: TNFeRecibo;
    function ServicoStatus: TNFeStatusServico;
  end;

implementation

function TACBrNFeHelper.ResponseCancela: TRetInfEvento;
begin
  Result := Self.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento;
end;

function TACBrNFeHelper.ResponseCancelaConsulta: TRetCancNFe;
begin
  Result := Self.WebServices.Consulta.retCancNFe;
end;

function TACBrNFeHelper.ResponseCCe: TRetEventoNFe;
begin
  Result := Self.WebServices.EnvEvento.EventoRetorno;
end;

function TACBrNFeHelper.ResponseCCeConsulta: TRetEventoNFeCollection;
begin
  Result := Self.WebServices.Consulta.procEventoNFe;
end;

function TACBrNFeHelper.ResponseConsulta: TNFeConsulta;
begin
  Result := Self.WebServices.Consulta;
end;

function TACBrNFeHelper.ResponseTransmite: TNfeRetRecepcao;
begin
  Result := nil;
  // Assincrono
  if Assigned(Self.WebServices.Retorno) and (Self.WebServices.Enviar.Recibo <> '') then
    Result := Self.WebServices.Retorno;
end;

function TACBrNFeHelper.ResponseTransmiteItem: TProtDFeCollection;
begin
  Result := nil;
  // Assincrono
  if Self.NotasFiscais.Count > 0 then
    Result := Self.WebServices.Retorno.NFeRetorno.ProtDFe;
end;

function TACBrNFeHelper.ResponseInutiliza: TNFeInutilizacao;
begin
  Result := Self.WebServices.Inutilizacao;
end;

function TACBrNFeHelper.ResponseRecibo: TNFeRecibo;
begin
  Result := Self.WebServices.Recibo;
end;

function TACBrNFeHelper.ResponseServicoStatus: TNFeStatusServico;
begin
  Result := Self.WebServices.StatusServico;
end;

function TACBrNFeHelper.ServicoStatus: TNFeStatusServico;
begin
  Result := Self.WebServices.StatusServico;
end;

end.
