unit nfebr.response.transmite.lote;

interface

uses
  Generics.Collections,
  nfebr.response.transmite,
  nfebr.response.base;

type
  TNFeResponseNota = class;

  TNFeResponseTransmiteLote = class(TNFeResponseBase)
  private
    FRecibo: String;
    FProtocolo: String;
    FTMResposta: integer;
    FResponseNotas: TObjectList<TNFeResponseNota>;
  public
    constructor Create;
    destructor Destroy; override;
    property Recibo: String read FRecibo write FRecibo;
    property Protocolo: String read FProtocolo write FProtocolo;
    property TMResposta: integer read FTMResposta write FTMResposta;
    property ResponseNotas: TObjectList<TNFeResponseNota> read FResponseNotas;
  end;

  TNFeResponseNota = class
  private
    FNumero: integer;
    FDataEmissao: TDateTime;
    FModelo: integer;
    FSerie: integer;
    FValorTotal: Currency;
    FItemRetorno: TNFeItemResponse;
  public
    constructor Create;
    destructor Destroy; override;
    property Numero: integer read FNumero write FNumero;
    property DataEmissao: TDateTime read FDataEmissao write FDataEmissao;
    property Modelo: integer read FModelo write FModelo;
    property Serie: integer read FSerie write FSerie;
    property ValorTotal: Currency read FValorTotal write FValorTotal;
    //
    property ItemRetorno: TNFeItemResponse read FItemRetorno;
  end;

implementation

{ TResponseDocumentos }

constructor TNFeResponseNota.Create;
begin
  FItemRetorno := TNFeItemResponse.Create;
end;

destructor TNFeResponseNota.Destroy;
begin
  FItemRetorno.Free;
  inherited;
end;

{ TresponseTransmiteLote }

constructor TNFeResponseTransmiteLote.Create;
begin
  FResponseNotas := TObjectList<TNFeResponseNota>.Create;
end;

destructor TNFeResponseTransmiteLote.Destroy;
begin
  FResponseNotas.Free;
  inherited;
end;

end.
