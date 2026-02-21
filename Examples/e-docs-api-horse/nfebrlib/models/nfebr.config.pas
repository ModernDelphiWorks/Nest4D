unit nfebr.config;

interface

uses
  Classes,
  IniFiles,
  nfebr.constantes;

type
  TNFeGeral = class
  private
    FDFeVersao: integer;
    FDFeSSLLIB: integer;
    FCertificadoPFX: String;
    FCertificadoSerie: String;
    FCertificadoVencto: TDateTime;
    FCertificadoSenha: String;
    FCertificadoIDToken: String;
    FCertificadoToken: String;
  public
    property DFeVersao: integer read FDFeVersao write FDFeVersao;
    property DFeSSLLIB: integer read FDFeSSLLIB write FDFeSSLLIB;
    property CertificadoPFX: String read FCertificadoPFX write FCertificadoPFX;
    property CertificadoSerie: String read FCertificadoSerie write FCertificadoSerie;
    property CertificadoVencto: TDateTime read FCertificadoVencto write FCertificadoVencto;
    property CertificadoSenha: String read FCertificadoSenha write FCertificadoSenha;
    property CertificadoIDToken: String read FCertificadoIDToken write FCertificadoIDToken;
    property CertificadoToken: String read FCertificadoToken write FCertificadoToken;
  end;

  TNFeWS = class
  private
    FAmbienteEmissao: integer;
    FAmbienteUF: String;
    FAmbienteFormaEnvio: integer;
    FMostrarRetornoMsg: Boolean;
    FProxyServidor: String;
    FProxyPorta: String;
    FProxyUsuario: String;
    FProxySenha: String;
  public
    property AmbienteEmissao: integer read FAmbienteEmissao write FAmbienteEmissao;
    property AmbienteUF: String read FAmbienteUF write FAmbienteUF;
    property AmbienteFormaEnvio: integer read FAmbienteFormaEnvio write FAmbienteFormaEnvio;
    property MostrarRetornoMsg: Boolean read FMostrarRetornoMsg write FMostrarRetornoMsg;
    property ProxyServidor: String read FProxyServidor write FProxyServidor;
    property ProxyPorta: String read FProxyPorta write FProxyPorta;
    property ProxyUsuario: String read FProxyUsuario write FProxyUsuario;
    property ProxySenha: String read FProxySenha write FProxySenha;
  end;

  TNFeEmail = class
  private
    FSMTP: String;
    FPorta: String;
    FUsuario: String;
    FSenha: String;
    FAssunto: String;
    FEmail1: String;
    FEmail2: String;
    FSSL: Boolean;
    FTLS: Boolean;
    FMensagem: String;
  public
    property SMTP: String read FSMTP write FSMTP;
    property Porta: String read FPorta write FPorta;
    property Usuario: String read FUsuario write FUsuario;
    property Senha: String read FSenha write FSenha;
    property Assunto: String read FAssunto write FAssunto;
    property Email1: String read FEmail1 write FEmail1;
    property Email2: String read FEmail2 write FEmail2;
    property SSL: Boolean read FSSL write FSSL;
    property TLS: Boolean read FTLS write FTLS;
    property Mensagem: String read FMensagem write FMensagem;
  end;

  TNFePath = class
  private
    FAmbienteURL: String;
    FSalvarEnvioResposta: Boolean;
    FSalvarPorMes: Boolean;
    FFileSchemas: String;
    FFileXMLSalvar: String;
    FFileXMLEvento: String;
    FFileXMLInutilizacao: String;
    FFileXMLLogs: String;
    FFilePDF: String;
  public
    property AmbienteURL: String read FAmbienteURL write FAmbienteURL;
    property SalvarEnvioResposta: Boolean read FSalvarEnvioResposta write FSalvarEnvioResposta;
    property SalvarPorMes: Boolean read FSalvarPorMes write FSalvarPorMes;
    property FileSchemas: String read FFileSchemas write FFileSchemas;
    property FileXMLSalvar: String read FFileXMLSalvar write FFileXMLSalvar;
    property FileXMLEvento: String read FFileXMLEvento write FFileXMLEvento;
    property FileXMLInutilizacao: String read FFileXMLInutilizacao write FFileXMLInutilizacao;
    property FileXMLLogs: String read FFileXMLLogs write FFileXMLLogs;
    property FilePDF: String read FFilePDF write FFilePDF;
  end;

  TNFe = class
  private
    FDanfeFormato: integer;
    FDanfeCopias: integer;
    FFileLogomarca: String;
    FMostrarDialogoPrint: Boolean;
    FFileDanfeRetrato: String;
    FFileDanfePaisagem: String;
    FFileDanfeEvento: String;
    FFileDanfeInutilizacao: String;
    FFormaEmissao: integer;
    FSerie: String;
  public
    property DanfeFormato: integer read FDanfeFormato write FDanfeFormato;
    property DanfeCopias: integer read FDanfeCopias write FDanfeCopias;
    property FileLogomarca: String read FFileLogomarca write FFileLogomarca;
    property MostrarDialogoPrint: Boolean read FMostrarDialogoPrint write FMostrarDialogoPrint;
    property FileDanfeRetrato: String read FFileDanfeRetrato write FFileDanfeRetrato;
    property FileDanfePaisagem: String read FFileDanfePaisagem write FFileDanfePaisagem;
    property FileDanfeEvento: String read FFileDanfeEvento write FFileDanfeEvento;
    property FileDanfeInutilizacao: String read FFileDanfeInutilizacao write FFileDanfeInutilizacao;
    property FormaEmissao: integer read FFormaEmissao write FFormaEmissao;
    property Serie: String read FSerie write FSerie;
  end;

  TNFeConfig = class
  private
    FDFeGeral: TNFeGeral;
    FDFeWS: TNFeWS;
    FDFePath: TNFePath;
    FNFe: TNFe;
    procedure _SetDFeGeral(const Value: TNFeGeral);
    procedure _SetDFePath(const Value: TNFePath);
    procedure _SetDFeWS(const Value: TNFeWS);
    procedure _SetNFe(const Value: TNFe);
    function _GetDFeGeral: TNFeGeral;
    function _GetDFePath: TNFePath;
    function _GetDFeWS: TNFeWS;
    function _GetNFe: TNFe;
  public
    constructor Create;
    destructor Destroy; override;
    class function New: TNFeConfig;
    property DFeGeral: TNFeGeral read _GetDFeGeral write _SetDFeGeral;
    property DFeWS: TNFeWS read _GetDFeWS write _SetDFeWS;
    property DFePath: TNFePath read _GetDFePath write _SetDFePath;
    property NFe: TNFe read _GetNFe write _SetNFe;
    procedure SaveConfig(const AFilePath: String);
    procedure LoadConfig(const AFilePath: String);
  end;

implementation

{ TNFeConfig }

constructor TNFeConfig.Create;
begin
  FDFeGeral := TNFeGeral.Create;
  FDFeWS := TNFeWS.Create;
  FDFePath := TNFePath.Create;
  FNFe := TNFe.Create;
  // Será um Singleton, e instânciado no AppModule uma única vez.
  Self.LoadConfig('./data/config.ini');
end;

destructor TNFeConfig.Destroy;
begin
  FDFeGeral.Free;
  FDFeWS.Free;
  FDFePath.Free;
  FNFe.Free;
  inherited;
end;

class function TNFeConfig.New: TNFeConfig;
begin
  Result := Self.Create;
end;

function TNFeConfig._GetDFeGeral: TNFeGeral;
begin
  Result := FDFeGeral;
end;

function TNFeConfig._GetDFePath: TNFePath;
begin
  Result := FDFePath;
end;

function TNFeConfig._GetDFeWS: TNFeWS;
begin
  Result := FDFeWS;
end;

function TNFeConfig._GetNFe: TNFe;
begin
  Result := FNFe;
end;

procedure TNFeConfig.LoadConfig(const AFilePath: String);
var
  LFileIni: TIniFile;
begin
  LFileIni := TIniFile.Create(AFilePath);
  try
    // Session GERAL
    FDFeGeral.DFeVersao := LFileIni.ReadInteger(cSESSIONGERAL, 'DFEVERSAO', 0);
    FDFeGeral.DFeSSLLIB := LFileIni.ReadInteger(cSESSIONGERAL, 'DFESSLLIB', 0);
    FDFeGeral.CertificadoPFX := LFileIni.ReadString(cSESSIONGERAL, 'CERTIFICADOPFX', '');
    FDFeGeral.CertificadoSerie := LFileIni.ReadString(cSESSIONGERAL, 'CERTIFICADOSERIE', '');
    FDFeGeral.CertificadoVencto := LFileIni.ReadDateTime(cSESSIONGERAL, 'CERTIFICADOVENCTO', 0);
    FDFeGeral.CertificadoSenha := LFileIni.ReadString(cSESSIONGERAL, 'CERTIFICADOSENHA', '');
    FDFeGeral.CertificadoIDToken := LFileIni.ReadString(cSESSIONGERAL, 'CERTIFICADOIDTOKEN', '');
    FDFeGeral.CertificadoToken := LFileIni.ReadString(cSESSIONGERAL, 'CERTIFICADOTOKEN', '');
    // Session WS
    FDFeWS.AmbienteEmissao := LFileIni.ReadInteger(cSESSIONWS, 'AMBIENTEEMISSAO', 0);
    FDFeWS.AmbienteUF := LFileIni.ReadString(cSESSIONWS, 'AMBIENTEUF', '');
    FDFeWS.AmbienteFormaEnvio := LFileIni.ReadInteger(cSESSIONWS, 'AMBIENTEFORMAENVIO', 0);
    FDFeWS.MostrarRetornoMsg := LFileIni.ReadBool(cSESSIONWS, 'MOSTRARRETORNOMSG', False);
    FDFeWS.ProxyServidor := LFileIni.ReadString(cSESSIONWS, 'PROXYSERVIDOR', '');
    FDFeWS.ProxyPorta := LFileIni.ReadString(cSESSIONWS, 'PROXYPORTA', '');
    FDFeWS.ProxyUsuario := LFileIni.ReadString(cSESSIONWS, 'PROXYUSUARIO', '');
    FDFeWS.ProxySenha := LFileIni.ReadString(cSESSIONWS, 'PROXYSENHA', '');
    // Session PATH
    FDFePath.AmbienteURL := LFileIni.ReadString(cSESSIONPATH, 'AMBIENTEURL', '');
    FDFePath.SalvarEnvioResposta := LFileIni.ReadBool(cSESSIONPATH, 'SALVARENVIORESPOSTA', True);
    FDFePath.SalvarPorMes := LFileIni.ReadBool(cSESSIONPATH, 'SALVARPORMES', True);
    FDFePath.FileSchemas := LFileIni.ReadString(cSESSIONPATH, 'FILESCHEMAS', '');
    FDFePath.FileXMLSalvar := LFileIni.ReadString(cSESSIONPATH, 'FILEXMLSALVAR', '');
    FDFePath.FileXMLEvento := LFileIni.ReadString(cSESSIONPATH, 'FILEXMLEVENTO', '');
    FDFePath.FileXMLInutilizacao := LFileIni.ReadString(cSESSIONPATH, 'FILEXMLINUTILIZACAO', '');
    FDFePath.FileXMLLogs := LFileIni.ReadString(cSESSIONPATH, 'FILEXMLLOGS', '');
    FDFePath.FilePDF := LFileIni.ReadString(cSESSIONPATH, 'FILEPDF', '');
    // Session NF-e
    FNFe.DanfeFormato := LFileIni.ReadInteger(cSESSIONNFE, 'DANFEFORMATO', 0);
    FNFe.DanfeCopias := LFileIni.ReadInteger(cSESSIONNFE, 'DANFECOPIAS', 1);
    FNFe.FileLogomarca := LFileIni.ReadString(cSESSIONNFE, 'FILELOGOMARCA', '');
    FNFe.MostrarDialogoPrint := LFileIni.ReadBool(cSESSIONNFE, 'MOSTRARDIALOGOPRINT', False);
    FNFe.FileDanfeRetrato := LFileIni.ReadString(cSESSIONNFE, 'FILEDANFERETRATO', '');
    FNFe.FileDanfePaisagem := LFileIni.ReadString(cSESSIONNFE, 'FILEDANFEPAISAGEM', '');
    FNFe.FileDanfeEvento := LFileIni.ReadString(cSESSIONNFE, 'FILEDANFEEVENTO', '');
    FNFe.FileDanfeInutilizacao := LFileIni.ReadString(cSESSIONNFE, 'FILEDANFEINUTILIZACAO', '');
    FNFe.FormaEmissao := LFileIni.ReadInteger(cSESSIONNFE, 'FORMAEMISSAO', 0);
    FNFe.Serie := LFileIni.ReadString(cSESSIONNFE, 'SERIE', '001');
  finally
    LFileIni.Free;
  end;
end;

procedure TNFeConfig.SaveConfig(const AFilePath: String);
var
  LFileIni: TIniFile;
begin
  LFileIni := TIniFile.Create(AFilePath);
  try
    // Session GERAL
    LFileIni.WriteInteger(cSESSIONGERAL, 'DFEVERSAO', FDFeGeral.DFeVersao);
    LFileIni.WriteInteger(cSESSIONGERAL, 'DFESSLLIB', FDFeGeral.DFeSSLLIB);
    LFileIni.WriteString(cSESSIONGERAL, 'CERTIFICADOPFX', FDFeGeral.CertificadoPFX);
    LFileIni.WriteString(cSESSIONGERAL, 'CERTIFICADOSERIE', FDFeGeral.CertificadoSerie);
    LFileIni.WriteDateTime(cSESSIONGERAL, 'CERTIFICADOVENCTO', FDFeGeral.CertificadoVencto);
    LFileIni.WriteString(cSESSIONGERAL, 'CERTIFICADOSENHA', String(FDFeGeral.CertificadoSenha));
    LFileIni.WriteString(cSESSIONGERAL, 'CERTIFICADOIDTOKEN', FDFeGeral.CertificadoIDToken);
    LFileIni.WriteString(cSESSIONGERAL, 'CERTIFICADOTOKEN', FDFeGeral.CertificadoToken);
    // Session WS
    LFileIni.WriteInteger(cSESSIONWS, 'AMBIENTEEMISSAO', FDFeWS.AmbienteEmissao);
    LFileIni.WriteString(cSESSIONWS, 'AMBIENTEUF', FDFeWS.AmbienteUF);
    LFileIni.WriteInteger(cSESSIONWS, 'AMBIENTEFORMAENVIO', FDFeWS.AmbienteFormaEnvio);
    LFileIni.WriteBool(cSESSIONWS, 'MOSTRARRETORNOMSG', FDFeWS.MostrarRetornoMsg);
    LFileIni.WriteString(cSESSIONWS, 'PROXYSERVIDOR', FDFeWS.ProxyServidor);
    LFileIni.WriteString(cSESSIONWS, 'PROXYPORTA', FDFeWS.ProxyPorta);
    LFileIni.WriteString(cSESSIONWS, 'PROXYUSUARIO', FDFeWS.ProxyUsuario);
    LFileIni.WriteString(cSESSIONWS, 'PROXYSENHA', FDFeWS.ProxySenha);
    // Session PATH
    LFileIni.WriteString(cSESSIONPATH, 'AMBIENTEURL', FDFePath.AmbienteURL);
    LFileIni.WriteBool(cSESSIONPATH, 'SALVARENVIORESPOSTA', FDFePath.SalvarEnvioResposta);
    LFileIni.WriteBool(cSESSIONPATH, 'SALVARPORMES', FDFePath.SalvarPorMes);
    LFileIni.WriteString(cSESSIONPATH, 'FILESCHEMAS', FDFePath.FileSchemas);
    LFileIni.WriteString(cSESSIONPATH, 'FILEXMLSALVAR', FDFePath.FileXMLSalvar);
    LFileIni.WriteString(cSESSIONPATH, 'FILEXMLEVENTO', FDFePath.FileXMLEvento);
    LFileIni.WriteString(cSESSIONPATH, 'FILEXMLINUTILIZACAO', FDFePath.FileXMLInutilizacao);
    LFileIni.WriteString(cSESSIONPATH, 'FILEXMLLOGS', FDFePath.FileXMLLogs);
    LFileIni.WriteString(cSESSIONPATH, 'FILEPDF', FDFePath.FilePDF);
    // Session NF-e
    LFileIni.WriteInteger(cSESSIONNFE, 'DANFEFORMATO', FNFe.DanfeFormato);
    LFileIni.WriteInteger(cSESSIONNFE, 'DANFECOPIAS', FNFe.DanfeCopias);
    LFileIni.WriteString(cSESSIONNFE, 'FILELOGOMARCA', FNFe.FileLogomarca);
    LFileIni.WriteBool(cSESSIONNFE, 'MOSTRARDIALOGOPRINT', FNFe.MostrarDialogoPrint);
    LFileIni.WriteString(cSESSIONNFE, 'FILEDANFERETRATO', FNFe.FileDanfeRetrato);
    LFileIni.WriteString(cSESSIONNFE, 'FILEDANFEPAISAGEM', FNFe.FileDanfePaisagem);
    LFileIni.WriteString(cSESSIONNFE, 'FILEDANFEEVENTO', FNFe.FileDanfeEvento);
    LFileIni.WriteString(cSESSIONNFE, 'FILEDANFEINUTILIZACAO', FNFe.FileDanfeInutilizacao);
    LFileIni.WriteInteger(cSESSIONNFE, 'FORMAEMISSAO', FNFe.FormaEmissao);
    LFileIni.WriteString(cSESSIONNFE, 'SERIE', FNFe.Serie);
  finally
    LFileIni.Free;
  end;
end;

procedure TNFeConfig._SetDFeGeral(const Value: TNFeGeral);
begin
  FDFeGeral := Value;
end;

procedure TNFeConfig._SetDFePath(const Value: TNFePath);
begin
  FDFePath := Value;
end;

procedure TNFeConfig._SetDFeWS(const Value: TNFeWS);
begin
  FDFeWS := Value;
end;

procedure TNFeConfig._SetNFe(const Value: TNFe);
begin
  FNFe := Value;
end;

end.
