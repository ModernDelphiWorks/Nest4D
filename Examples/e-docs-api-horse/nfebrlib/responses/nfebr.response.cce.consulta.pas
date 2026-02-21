unit nfebr.response.cce.consulta;

interface

uses
  Generics.Collections,
  nfebr.response.base;

type
  TNFeResponseEventsConsulta = class;
  TNFeResponseDetEventoConsulta = class;

  TNFeResponseCCeConsulta = class(TNFeResponseBase)
  private
    FChaveAcesso: String;
    FProtocolo: String;
    FDigValor: String;
    FMsg: String;
    FCodMsg: integer;
    FConsultaCCeEvents: TObjectList<TNFeResponseEventsConsulta>;
  public
    constructor Create;
    destructor Destroy; override;
    property ChaveAcesso: String read FChaveAcesso write FChaveAcesso;
    property Protocolo: String read FProtocolo write FProtocolo;
    property DigValor: String read FDigValor write FDigValor;
    property CodMsg: integer read FCodMsg write FCodMsg;
    property Msg: String read FMsg write FMsg;
    property ConsultaCCeEvents: TObjectList<TNFeResponseEventsConsulta> read FConsultaCCeEvents write FConsultaCCeEvents;
  end;

  TNFeResponseEventsConsulta = class
  private
    FId: String;
    FAmbiente: String;
    FUF: integer;
    FCNPJ: String;
    FChaveAceso: String;
    FDataRegEvento: TDateTime;
    FTipoEvento: String;
    FSequencia: integer;
    FVersaoEvent: String;
    FDetEvento: TNFeResponseDetEventoConsulta;
    // FRetEventos: TObjectList;
  public
    constructor Create;
    destructor Destroy; override;
    property Id: String read FId write FId;
    property Ambiente: String read FAmbiente write FAmbiente;
    property UF: integer read FUF write FUF;
    property CNPJ: String read FCNPJ write FCNPJ;
    property ChaveAceso: String read FChaveAceso write FChaveAceso;
    property DataRegEvento: TDateTime read FDataRegEvento write FDataRegEvento;
    property TipoEvento: String read FTipoEvento write FTipoEvento;
    property Sequencia: integer read FSequencia write FSequencia;
    property VersaoEvent: String read FVersaoEvent write FVersaoEvent;
    property DetEvento: TNFeResponseDetEventoConsulta read FDetEvento write FDetEvento;
  end;

  { TNFeResponseDetEventoConsulta }

  TNFeResponseDetEventoConsulta = class
  private
    Fversao: String;
    FverAplic: String;
    FdescEvento: String;
    FxCorrecao: String;
    FxCondUso: String;
    FnProt: String;
    FxJust: String;
    FcOrgaoAutor: String;
    FtpAutor: String;
    FdhEmi: TDateTime;
    FtpNF: String;
    FIE: String;
    FDESTCNPJCPF: String;
    FDESTidEstrangeiro: String;
    FDESTIE: String;
    FDESTUF: String;
    FvNF: Double;
    FvICMS: Double;
    FvST: Double;
    FidPedidoCancelado: String;
    // FItems: TObjectList;
  public
    constructor Create;
    destructor Destroy; override;
    property Versao: String read Fversao write Fversao;
    property VerAplic: String read FverAplic write FverAplic;
    property descEvento: String read FdescEvento write FdescEvento;
    property xCorrecao: String read FxCorrecao write FxCorrecao;
    property xCondUso: String read FxCondUso write FxCondUso;
    property nProt: String read FnProt write FnProt;
    property xJust: String read FxJust write FxJust;
    property cOrgaoAutor: String read FcOrgaoAutor write FcOrgaoAutor;
    property tpAutor: String read FtpAutor write FtpAutor;
    property dhEmi: TDateTime read FdhEmi write FdhEmi;
    property tpNF: String read FtpNF write FtpNF;
    property IE: String read FIE write FIE;
    property DESTCNPJCPF: String read FDESTCNPJCPF write FDESTCNPJCPF;
    property DESTidEstrangeiro: String read FDESTidEstrangeiro write FDESTidEstrangeiro;
    property DESTIE: String read FDESTIE write FDESTIE;
    property DESTUF: String read FDESTUF write FDESTUF;
    property vNF: Double read FvNF write FvNF;
    property vICMS: Double read FvICMS write FvICMS;
    property vST: Double read FvST write FvST;
    property idPedidoCancelado: String read FidPedidoCancelado write FidPedidoCancelado;
    // property Items: TObjectList read FItems;
  end;

implementation

{ TNFeResponseCCeConsulta }

constructor TNFeResponseCCeConsulta.Create;
begin
  FConsultaCCeEvents := TObjectList<TNFeResponseEventsConsulta>.Create;
end;

destructor TNFeResponseCCeConsulta.Destroy;
begin
  FConsultaCCeEvents.Free;
  inherited;
end;

{ TNFeResponseDetEventoConsulta }

constructor TNFeResponseDetEventoConsulta.Create;
begin

end;

destructor TNFeResponseDetEventoConsulta.Destroy;
begin

  inherited;
end;

{ TNFeResponseEventsConsulta }

constructor TNFeResponseEventsConsulta.Create;
begin
  FDetEvento := TNFeResponseDetEventoConsulta.Create;
end;

destructor TNFeResponseEventsConsulta.Destroy;
begin
  FDetEvento.Free;
  inherited;
end;

end.
