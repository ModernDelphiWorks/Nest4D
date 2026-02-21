unit nfe.acbr.config.components;

{$include ..\..\..\nfebrlib.inc}

interface

uses
  SysUtils,
  ACBrDFeSSL,
  ACBrNFe,
  ACBrNFeDANFEClass,
  {$IFDEF FASTREPORT}
  ACBrNFeDANFEFR,
  ACBrNFeDANFEFRDM,
  {$ENDIF}
  {$IFDEF FORTESREPORT}
  ACBrNFeDANFeRLClass,
  {$ENDIF}
  ACBrNFeConfiguracoes,
  ACBrDFeConfiguracoes,
  pcnConversaoNFe,
  pcnConversao,
  blcksock,
  nfebr.config,
  nfebr.lib.enum,
  nfe.acbr.interfaces;

type
  TNFeConfiguracao = class(TInterfacedObject, INFeConfiguracao)
  private
    FNFe: TACBrNFe;
    {$IFDEF FASTREPORT}
    FDanfe: TACBrNFeDANFEFR;
    {$ENDIF}
    {$IFDEF FORTESREPORT}
    FDanfe: TACBrNFeDANFeRL;
    {$ENDIF}
    FNFeConfig: TNFeConfig;
    // Forma de envio Sincrona = True ou Assincrona = False
    FEnvioSincrona: Boolean;
    procedure _GetDFeConfig;
    procedure _SetDanfeNFe;
    function _GetVersaoDF(const AIndex: Integer): TpcnVersaoDF;
    function _GetTipoDANFE(const AIndex: Integer): TpcnTipoImpressao;
  public
    class function New(const AConfig: TNFeConfig;
                       const ANFe: TACBrNFe;
                       const ADanfe: TACBrNFeDANFEClass): INFeConfiguracao;
    constructor Create(const AConfig: TNFeConfig;
                       const ANFe: TACBrNFe;
                       const ADanfe: TACBrNFeDANFEClass);
    destructor Destroy; override;
    function Execute: INFeConfiguracao;
    function EnvioSincrona: Boolean;
  end;

implementation

{ TDFeConfiguracoes }

constructor TNFeConfiguracao.Create(const AConfig: TNFeConfig;
  const ANFe: TACBrNFe;
  const ADanfe: TACBrNFeDANFEClass);
begin
  FNFeConfig := AConfig;
  FNFe := ANFe;
  {$IFDEF FASTREPORT}
  FDanfe := TACBrNFeDANFEFR(ADanfe);
  {$ENDIF}
  {$IFDEF FORTESREPORT}
  FDanfe := TACBrNFeDANFeRL(ADanfe);
  {$ENDIF}
end;

destructor TNFeConfiguracao.Destroy;
begin

  inherited;
end;

function TNFeConfiguracao.EnvioSincrona: Boolean;
begin
  Result := FEnvioSincrona;
end;

function TNFeConfiguracao.Execute: INFeConfiguracao;
begin
  _GetDFeConfig;
  Result := Self;
end;

procedure TNFeConfiguracao._GetDFeConfig;
begin
  // Configurações Gerais
  FNFe.Configuracoes.Geral.Salvar := True;
  FNFe.Configuracoes.Geral.ModeloDF := moNFe;
  FNFe.Configuracoes.Geral.VersaoDF := _GetVersaoDF(FNFeConfig.DFeGeral.DFeVersao);
  FNFe.Configuracoes.Geral.VersaoQRCode := veqr200;
  FNFe.Configuracoes.Geral.IdCSC := FNFeConfig.DFeGeral.CertificadoIDToken;
  FNFe.Configuracoes.Geral.CSC := FNFeConfig.DFeGeral.CertificadoToken;
  // SSL - fonte de consulta
  // https://www.projetoacbr.com.br/forum/topic/71608-usando-capicom-msxml-lt-all-e-outras-configura%C3%A7%C3%B5es-semelhantes-nos-dfes-saiba-porque-voc%C3%AA-est%C3%A1-atrasado/#comment-464619
  FNFe.Configuracoes.Geral.SSLLib := TSSLLib(FNFeConfig.DFeGeral.DFeSSLLIB);
  case FNFe.Configuracoes.Geral.SSLLib of
    libOpenSSL: // 1
      begin
        FNFe.Configuracoes.Geral.SSLCryptLib   := cryOpenSSL;
        FNFe.Configuracoes.Geral.SSLHttpLib    := httpOpenSSL;
        FNFe.Configuracoes.Geral.SSLXmlSignLib := xsLibXml2;
      end;
    libWinCrypt: // 4
      begin
        FNFe.Configuracoes.Geral.SSLCryptLib   := cryWinCrypt;
        FNFe.Configuracoes.Geral.SSLHttpLib    := httpWinHttp;
        FNFe.Configuracoes.Geral.SSLXmlSignLib := xsMsXml;
      end;
  end;
  // DANFE - Definir gerador de relatório
  FNFe.Configuracoes.Geral.FormaEmissao := TpcnTipoEmissao(FNFeConfig.NFe.FormaEmissao);
  //
  _SetDanfeNFe;
  // SSL
  FNFe.SSL.SSLType := LT_TLSv1_2;
  // Arquivos
  FNFe.Configuracoes.Arquivos.SalvarApenasNFeProcessadas := True;
  FNFe.Configuracoes.Arquivos.EmissaoPathNFe := True;
  FNFe.Configuracoes.Arquivos.AdicionarLiteral := True;
  FNFe.Configuracoes.Arquivos.IniServicos := FNFeConfig.DFePath.AmbienteURL;
  FNFe.Configuracoes.Arquivos.Salvar := FNFeConfig.DFePath.SalvarEnvioResposta;
  FNFe.Configuracoes.Arquivos.SepararPorMes := FNFeConfig.DFePath.SalvarPorMes;
  FNFe.Configuracoes.Arquivos.PathSalvar := '.\' + FNFeConfig.DFePath.FileXMLLogs;
  FNFe.Configuracoes.Arquivos.PathNFe := '.\' + FNFeConfig.DFePath.FileXMLSalvar;
  FNFe.Configuracoes.Arquivos.PathInu := '.\' + FNFeConfig.DFePath.FileXMLInutilizacao;
  FNFe.Configuracoes.Arquivos.PathEvento := '.\' + FNFeConfig.DFePath.FileXMLEvento;
  FNFe.Configuracoes.Arquivos.PathSchemas := '.\' + FNFeConfig.DFePath.FileSchemas;
  // WebServices - fonte de consulta
  // https://www.projetoacbr.com.br/forum/topic/40555-configura%C3%A7%C3%A3o-adequada-para-as-propriedades-de-timeout-etc/?do=findComment&comment=267521
  FNFe.Configuracoes.WebServices.AjustaAguardaConsultaRet := True;
  FNFe.Configuracoes.WebServices.TimeOut := 18000;
  FNFe.Configuracoes.WebServices.AguardarConsultaRet := 5000;
  FNFe.Configuracoes.WebServices.IntervaloTentativas := 3000;
  FNFe.Configuracoes.WebServices.Tentativas := 10;
  FNFe.Configuracoes.WebServices.Ambiente := TpcnTipoAmbiente(FNFeConfig.DFeWS.AmbienteFormaEnvio);
  FNFe.Configuracoes.WebServices.UF := FNFeConfig.DFeWS.AmbienteUF;
  FNFe.Configuracoes.WebServices.Visualizar := FNFeConfig.DFeWS.MostrarRetornoMsg;
  FNFe.Configuracoes.WebServices.ProxyHost := FNFeConfig.DFeWS.ProxyServidor;
  FNFe.Configuracoes.WebServices.ProxyPort := FNFeConfig.DFeWS.ProxyPorta;
  FNFe.Configuracoes.WebServices.ProxyUser := FNFeConfig.DFeWS.ProxyUsuario;
  FNFe.Configuracoes.WebServices.ProxyPass := FNFeConfig.DFeWS.ProxySenha;
  // Certificados
  FNFe.Configuracoes.Certificados.VerificarValidade := True;
  FNFe.Configuracoes.Certificados.ArquivoPFX := '.\certificados\' + FNFeConfig.DFeGeral.CertificadoPFX;
  FNFe.Configuracoes.Certificados.NumeroSerie := FNFeConfig.DFeGeral.CertificadoSerie;
  FNFe.Configuracoes.Certificados.Senha := AnsiString(FNFeConfig.DFeGeral.CertificadoSenha);
  // Forma de envio
  FEnvioSincrona := FNFeConfig.DFeWS.AmbienteFormaEnvio = 1;
end;

function TNFeConfiguracao._GetTipoDANFE(const AIndex: Integer): TpcnTipoImpressao;
begin
  case AIndex of
    0: Result := tiRetrato;
    1: Result := tiPaisagem;
  else
    Result := TpcnTipoImpressao.tiSemGeracao;
  end;
end;

function TNFeConfiguracao._GetVersaoDF(const AIndex: Integer): TpcnVersaoDF;
begin
  // 0-ve200, 1-ve300, 2-ve310, 3-ve400
  case AIndex of
    0: Result := ve400
  else
    Result := ve400;
  end;
end;

class function TNFeConfiguracao.New(const AConfig: TNFeConfig;
  const ANFe: TACBrNFe;
  const ADanfe: TACBrNFeDANFEClass): INFeConfiguracao;
begin
  Result := Self.Create(AConfig, ANFe, ADanfe);
end;

procedure TNFeConfiguracao._SetDanfeNFe;
begin
  FNFe.DANFE := FDanfe;
  FNFe.DANFE.TipoDANFE := _GetTipoDANFE(FNFeConfig.NFe.DanfeFormato);
  {$IFDEF FASTREPORT}
  case FNFe.DANFE.TipoDANFE of
    tiSemGeracao   : ;
    tiRetrato      : FDanfe.FastFile := ADFeConfig.NFe.FileDanfeRetrato;
    tiPaisagem     : FDanfe.FastFile := ADFeConfig.NFe.FileDanfePaisagem;
    tiSimplificado : FDanfe.FastFile := LDFeSet.FieldByName('DFE_REPORTRETRATOPATH').AsString;
    tiNFCe         : FOwner.DanfeFR.FastFile := LDFeSet.FieldByName('DFE_REPORTRETRATOPATH').AsString;
    tiMsgEletronica: ;
  end;
  FDanfe.FastFileEvento := ADFeConfig.NFe.FileDanfeEvento;
  FDanfe.FastFileInutilizacao := ADFeConfig.NFe.FileDanfeInutilizacao;
  {$ENDIF}
  FDanfe.PathPDF := '.\' + FNFeConfig.DFePath.FilePDF;
  FDanfe.Logo := FNFeConfig.NFe.FileLogomarca;
  FDanfe.Site := 'https://www.isaquepinheiro.com.br';
  FDanfe.MostraStatus := True;
  FDanfe.MostraSetup := FNFeConfig.NFe.MostrarDialogoPrint;
  FDanfe.NumCopias := FNFeConfig.NFe.DanfeCopias;
end;

end.
