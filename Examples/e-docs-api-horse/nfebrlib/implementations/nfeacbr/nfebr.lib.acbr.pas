unit nfebr.lib.acbr;

{$include ..\..\nfebrlib.inc}

interface

uses
  Classes,
  Variants,
  SysUtils,
  StrUtils,
  DateUtils,
  Generics.Collections,
  Evolution4D.Std,
  // ACBr Terceiros
  // synachar,
  // blcksock,
  // ACBr
  ACBrNFeDANFEClass,
  ACBrDFeDANFeReport,
  {$IFDEF FASTREPORT}
  ACBrNFeDANFEFR,
  ACBrNFeDANFEFRDM,
  {$ENDIF}
  {$IFDEF FORTESREPORT}
  ACBrNFeDANFeRLClass,
  {$ENDIF}
  ACBrBase,
  ACBrDFe,
  ACBrNFe,
  ACBrNFeNotasFiscais,
  ACBrDFeSSL,
  ACBrNFeWebServices,
  ACBrNFeConfiguracoes,
  ACBrDFeConfiguracoes,
//  pcnEnvEventoNFe,
  pcnRetConsReciDFe,
  pcnConversao,
  pcnConversaoNFe,
//  pcnEventoNFe,
  // Unit com todas as classes necess�rias na nota fiscal
  ACBrNFe.Classes,
  // NFeBr Library
  nfebr.lib.enum,
  nfebr.lib.utils,
  nfebr.lib.include,
  nfebr.lib.nfe,
  nfebr.model,
  // ACBr Library
  nfe.acbr.factory,
  nfe.acbr.interfaces,
  nfe.acbr.config.components,
  nfe.acbr.command.cancela,
  nfe.acbr.command.cce,
  nfe.acbr.command.consulta,
  nfe.acbr.command.inutiliza,
  nfe.acbr.command.servicostatus,
  nfe.acbr.command.transmite,
  nfe.acbr.command.transmitelote;

type
  TNFeLibACBr = class(TInterfacedObject, INFeLib)
  private
    FACBrNFe: TACBrNFe;
    {$IFDEF FASTREPORT}
    FDanfe: TACBrNFeDANFEFR;
    {$ENDIF}
    {$IFDEF FORTESREPORT}
    FDanfe: TACBrNFeDANFeRL;
    {$ENDIF}
    FNFeConfig: TNFeConfig;
    FListenerCallback: TProc<TObject, String>;
    FLoggerCallback: TProc<String>;
    // Vari�veis de controle
    FIsSincrona: boolean;
  private
    function _SetNFeConfiguracoes: INFeConfiguracao;
    // Eventos do ACBr
    procedure ACBrNFeStatusChange(Sender: TObject);
    procedure ACBrNFeTransmitError(const HttpError, InternalError: integer;
      const URL, DadosEnviados, SoapAction: String; var Retentar, Tratado: boolean);
    procedure ACBrNFeGerarLog(const ALogLine: String; var Tratado: boolean);
  public
    constructor Create;
    destructor Destroy; override;
    procedure SetConfigracao(const AConfig: TNFeConfig);
    procedure SetListener(const AListenerCallback: TProc<TObject, String>);
    procedure SetLogger(const ALoggerCallback: TProc<String>);
    procedure GerarPDF;
    // Comandos
    function Cancelar(const AReq: TNFeRequestCancela): TNFeResponseCancela;
    function CartaCorrecao(const AReq: TNFeRequestCCe): TNFeResponseCCe;
    function Consultar(const AReq: TNFeRequestConsulta): TNFeResponseConsulta;
    function Inutilizar(const AReq: TNFeRequestInutiliza): TNFeResponseInutiliza;
    function Transmitir(const AReq: TNFeRequestTransmite): TNFeResponseTransmite;
    function TransmitirLote(const AReq: TObjectList<TNFeRequestTransmite>): TNFeResponseTransmiteLote;
    function ServicoStatus: TNFeResponseServicoStatus;
  end;

implementation

uses
  Evolution4D.Threading;

{ TNFeLibACBr }

constructor TNFeLibACBr.Create;
begin
  FACBrNFe := TACBrNFe.Create(nil);
  FACBrNFe.OnStatusChange := ACBrNFeStatusChange;
  FACBrNFe.OnTransmitError := ACBrNFeTransmitError;
  FACBrNFe.OnGerarLog := ACBrNFeGerarLog;
  {$IFDEF FASTREPORT}
  FDanfe := TACBrNFeDANFEFR.Create(nil);
  {$ENDIF}
  {$IFDEF FORTESREPORT}
  FDanfe := TACBrNFeDANFeRL.Create(nil);
  {$ENDIF}
  FIsSincrona := true;
end;

destructor TNFeLibACBr.Destroy;
begin
  FDanfe.Free;
  FACBrNFe.Free;
  inherited;
end;

function TNFeLibACBr._SetNFeConfiguracoes: INFeConfiguracao;
begin
  Result := TNFeConfiguracao.New(FNFeConfig, FACBrNFe, FDanfe);
end;

function TNFeLibACBr.Cancelar(const AReq: TNFeRequestCancela): TNFeResponseCancela;
begin
  _SetNFeConfiguracoes.Execute;
  Result := TNFeCancelaCommand.New(FACBrNFe).Execute(AReq);
end;

function TNFeLibACBr.Inutilizar(const AReq: TNFeRequestInutiliza): TNFeResponseInutiliza;
begin
  _SetNFeConfiguracoes.Execute;
  Result := TNFeInutilizaCommand.New(FACBrNFe).Execute(AReq);
end;

function TNFeLibACBr.CartaCorrecao(const AReq: TNFeRequestCCe): TNFeResponseCCe;
begin
  _SetNFeConfiguracoes.Execute;
  Result := TNFeCCeCommand.New(FACBrNFe).Execute(AReq);
end;

function TNFeLibACBr.Consultar(const AReq: TNFeRequestConsulta): TNFeResponseConsulta;
begin
  _SetNFeConfiguracoes.Execute;
  Result := TNFeConsultaCommand.New(FACBrNFe).Execute(AReq);
end;

procedure TNFeLibACBr.GerarPDF;
begin
  FDanfe.MostraPreview := False;
  FDanfe.MostraSetup := False;
  FACBrNFe.NotasFiscais.ImprimirPDF;
end;

function TNFeLibACBr.Transmitir(const AReq: TNFeRequestTransmite): TNFeResponseTransmite;
begin
  _SetNFeConfiguracoes.Execute;
  FIsSincrona := _SetNFeConfiguracoes.Execute.EnvioSincrona;
  Result := TNFeTransmiteCommand.New(FACBrNFe).Execute(AReq, FIsSincrona);
end;

function TNFeLibACBr.TransmitirLote(
  const AReq: TObjectList<TNFeRequestTransmite>): TNFeResponseTransmiteLote;
begin
  _SetNFeConfiguracoes.Execute;
  FIsSincrona := _SetNFeConfiguracoes.Execute.EnvioSincrona;
  Result := TNFeTransmiteLoteCommand.New(FACBrNFe).Execute(AReq, FIsSincrona);
end;

function TNFeLibACBr.ServicoStatus: TNFeResponseServicoStatus;
var
  LFuture: TFuture;
begin
  LFuture := Async(function: TValue
                   begin
                     Result := TValue.From<INFeConfiguracao>(_SetNFeConfiguracoes.Execute);
                   end).Await();
  if LFuture.IsOk then
    Result := TNFeServicoStatusCommand.New(FACBrNFe).Execute
  else
    raise Exception.Create(LFuture.Err);
end;

procedure TNFeLibACBr.SetConfigracao(const AConfig: TNFeConfig);
begin
  FNFeConfig := AConfig;
end;

procedure TNFeLibACBr.SetListener(const AListenerCallback: TProc<TObject, String>);
begin
  FListenerCallback := AListenerCallback;
end;

procedure TNFeLibACBr.SetLogger(const ALoggerCallback: TProc<String>);
begin
  FLoggerCallback := ALoggerCallback;
end;

procedure TNFeLibACBr.ACBrNFeGerarLog(const ALogLine: String;
  var Tratado: boolean);
begin
  Tratado := False;
  if Assigned(FLoggerCallback) then
    FLoggerCallback(ALogLine);
end;

procedure TNFeLibACBr.ACBrNFeStatusChange(Sender: TObject);
var
  LAmbiente: String;
  LMensagem: String;
begin
  LMensagem := '';
  case FACBrNFe.Configuracoes.WebServices.Ambiente of
    taProducao:    LAmbiente := 'Produ��o' + sLineBreak;
    taHomologacao: LAmbiente := 'Homologa��o' + sLineBreak;
  end;

  case FACBrNFe.Status of
    stIdle:             LMensagem := 'Processo encerrado...';
    stNFeStatusServico: LMensagem := 'Verificando status do servico...';
    stNFeRecepcao:      LMensagem := 'Enviando dados do documento eletr�nico...';
    stNfeRetRecepcao:   LMensagem := 'Recebendo dados do documento eletr�nico...';
    stNfeConsulta:      LMensagem := 'Consultando o documento eletr�nico...';
    stNfeCancelamento:  LMensagem := 'Enviando cancelamento do documento eletr�nico...';
    stNfeInutilizacao:  LMensagem := 'Enviando pedido de inutiliza��o...';
    stNFeRecibo:        LMensagem := 'Consultando recibo de lote...';
    stNFeCadastro:      LMensagem := 'Consultando cadastro...';
    stNFeEmail:         LMensagem := 'Enviando documento eletr�nico por e-mail...';
    stNFeCCe:           LMensagem := 'Enviando carta de corre��o eletr�nica...';
    stNFeEvento:        LMensagem := 'Enviando evento...';
  end;
  if LMensagem <> '' then
    if Assigned(FListenerCallback) then
      FListenerCallback(Sender, Format('(Ambiente: %s) - Messagem[ %s ]', [LAmbiente, LMensagem]));
end;

procedure TNFeLibACBr.ACBrNFeTransmitError(const HttpError,
  InternalError: integer; const URL, DadosEnviados, SoapAction: String;
  var Retentar, Tratado: boolean);
begin
  Retentar := False;
  Tratado := False;
  // Retentar pode ocasionar mais problema, na 1a vez transmitiu e
  // deu um error no retorno, retentando, ir� da duplicidade,
  // por isso prefiro n�o usar esse recurso de retentar.

//  if FRetentarEnviarDFe > 0 then
//  begin
//    Dec(FRetentarEnviarDFe);
//    Retentar := True;
//  end;
end;

end.


