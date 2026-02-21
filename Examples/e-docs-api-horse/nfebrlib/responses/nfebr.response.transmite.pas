unit nfebr.response.transmite;

interface

uses
  Generics.Collections,
  nfebr.response.base;

type
  TNFeItemResponse = class;

  TNFeResponseTransmite = class(TNFeResponseBase)
  private
    FRecibo: String;
    FProtocolo: String;
    FTMResposta: integer;
    FNumero: integer;
    FDataEmissao: TDateTime;
    FModelo: integer;
    FSerie: integer;
    FValorTotal: Currency;
    FItemRetorno: TNFeItemResponse;
  public
    constructor Create;
    destructor Destroy; override;
    property Recibo: String read FRecibo write FRecibo;
    property Protocolo: String read FProtocolo write FProtocolo;
    property TMResposta: integer read FTMResposta write FTMResposta;
    property Numero: integer read FNumero write FNumero;
    property DataEmissao: TDateTime read FDataEmissao write FDataEmissao;
    property Modelo: integer read FModelo write FModelo;
    property Serie: integer read FSerie write FSerie;
    property ValorTotal: Currency read FValorTotal write FValorTotal;
    //
    property ItemRetorno: TNFeItemResponse read FItemRetorno;
  end;

  TNFeItemResponse = class
  private
    FId: String;
    FAmbiente: String;
    FVerAplic: String;
    FChaveAcesso: String;
    FDataRecbto: TDateTime;
    FnProtocolo: String;
    FDigestValue: String;
    FcStatus: integer;
    FMotivo: String;
  public
    property Id: String read FId write FId;
    property Ambiente: String read FAmbiente write FAmbiente;
    property VerAplic: String read FVerAplic write FVerAplic;
    property ChaveAcesso: String read FChaveAcesso write FChaveAcesso;
    property DataRecbto: TDateTime read FDataRecbto write FDataRecbto;
    property Protocolo: String read FnProtocolo write FnProtocolo;
    property DigestValue: String read FDigestValue write FDigestValue;
    property CodStatus: integer read FcStatus write FcStatus;
    property Motivo: String read FMotivo write FMotivo;
  end;

implementation

{ TRespondeTransmite }

constructor TNFeResponseTransmite.Create;
begin
  FItemRetorno := TNFeItemResponse.Create;
end;

destructor TNFeResponseTransmite.Destroy;
begin
  FItemRetorno.Free;
  inherited;
end;

end.
