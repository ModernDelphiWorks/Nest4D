unit nfebr.lib.include;

interface

uses
  nfebr.request.transmite,
  nfebr.request.inutiliza,
  nfebr.request.consulta,
  nfebr.request.cce.consulta,
  nfebr.request.cancela.consulta,
  nfebr.request.inutiliza.consulta,
  nfebr.request.cce,
  nfebr.request.cancela,
  nfebr.response.transmite,
  nfebr.response.cce,
  nfebr.response.cce.consulta,
  nfebr.response.cancela.consulta,
  nfebr.response.servicostatus,
  nfebr.response.cancela,
  nfebr.response.transmite.lote,
  nfebr.response.inutiliza,
  nfebr.response.consulta,
  nfebr.response.base,
  nfebr.config;

type
  TNFeRequestTransmite = nfebr.request.transmite.TNFeRequestTransmite;
  TNFeRequestInutiliza = nfebr.request.inutiliza.TNFeRequestInutiliza;
  TNFeRequestConsulta = nfebr.request.consulta.TNFeRequestConsulta;
  TNFeRequestCCeConsulta = nfebr.request.cce.consulta.TNFeRequestCCeConsulta;
  TNFeRequestCancelaConsulta = nfebr.request.cancela.consulta.TNFeRequestCancelaConsulta;
  TNFeRequestInutilizaConsulta = nfebr.request.inutiliza.consulta.TNFeRequestInutilizaConsulta;
  TNFeRequestCCe = nfebr.request.cce.TNFeRequestCCe;
  TNFeRequestCancela = nfebr.request.cancela.TNFeRequestCancela;
  TNFeResponseTransmite = nfebr.response.transmite.TNFeResponseTransmite;
  TNFeResponseCCe = nfebr.response.cce.TNFeResponseCCe;
  TNFeItemResponseCCe = nfebr.response.cce.TNFeItemResponseCCe;
  TNFeResponseCCeConsulta = nfebr.response.cce.consulta.TNFeResponseCCeConsulta;
  TNFeResponseEventsConsulta = nfebr.response.cce.consulta.TNFeResponseEventsConsulta;
  TNFeResponseCancelaConsulta = nfebr.response.cancela.consulta.TNFeResponseCancelaConsulta;
  TNFeResponseServicoStatus = nfebr.response.servicostatus.TNFeResponseServicoStatus;
  TNFeResponseCancela = nfebr.response.cancela.TNFeResponseCancela;
  TNFeResponseTransmiteLote = nfebr.response.transmite.lote.TNFeResponseTransmiteLote;
  TNFeResponseNota = nfebr.response.transmite.lote.TNFeResponseNota;
  TNFeResponseConsulta = nfebr.response.consulta.TNFeResponseConsulta;
  TNFeResponseInutiliza = nfebr.response.inutiliza.TNFeResponseInutiliza;
  TNFeResponseBase = nfebr.response.base.TNFeResponseBase;
  TNFeConfig = nfebr.config.TNFeConfig;

implementation

end.
