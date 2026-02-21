unit nfebr.response.cce;

interface

uses
  Generics.Collections,
  nfebr.response.base;

type
  TNFeItemResponseCCe = class;

  TNFeResponseCCe = class(TNFeResponseBase)
  private
    FIDLote: Int64;
    FEventItems: TObjectList<TNFeItemResponseCCe>;
  public
    constructor Create;
    destructor Destroy; override;
    property IDLote: Int64 read FIDLote write FIDLote;
    property EventItems: TObjectList<TNFeItemResponseCCe> read FEventItems write FEventItems;
  end;

  TNFeItemResponseCCe = class
  private
    FId: String;
    FAmbiente: String;
    FVerAplic: String;
    FCodStatus: integer;
    FMotivo: String;
    FUF: integer;
    FChaveAceso: String;
    FCNPJDest: String;
    FdhRegEvento: TDateTime;
    FDigValue: String;
    FEmailDest: String;
    FProtocolo: String;
    FSequencia: Integer;
    FTipoEvento: String;
    FJustificativa: String;
  public
    property Id: String read FId write FId;
    property Ambiente: String read FAmbiente write FAmbiente;
    property VerAplic: String read FVerAplic write FVerAplic;
    property CodStatus: integer read FCodStatus write FCodStatus;
    property Motivo: String read FMotivo write FMotivo;
    property UF: integer read FUF write FUF;
    property ChaveAcesso: String read FChaveAceso write FChaveAceso;
    property Protocolo: String read FProtocolo write FProtocolo;
    property DigValue: String read FDigValue write FDigValue;
    property dhRegEvento: TDateTime read FdhRegEvento write FdhRegEvento;
    property TipoEvento: String read FTipoEvento write FTipoEvento;
    property Justificativa: String read FJustificativa write FJustificativa;
    property Sequencia: Integer read FSequencia write FSequencia;
    property CNPJDest: String read FCNPJDest write FCNPJDest;
    property EmailDest: String read FemailDest write FemailDest;
  end;

implementation

{ TNFeResponseCCe }

constructor TNFeResponseCCe.Create;
begin
  FEventItems := TObjectList<TNFeItemResponseCCe>.Create;
end;

destructor TNFeResponseCCe.Destroy;
begin
  FEventItems.Free;
  inherited;
end;

end.

