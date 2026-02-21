unit nfebr.tag.comb.model;

interface

uses
  Generics.Collections,
  nfebr.tag.cide.model,
  nfebr.tag.origcomb.model,
  nfebr.tag.encerrante.model,
  nfebr.tag.icmscomb.model,
  nfebr.tag.icmsinter.model,
  nfebr.tag.icmscons.model;

type
  TCombModel = class
  private
    FCIDE: TCIDEModel;
    FCODIF: String;
    FCProdANP: integer;
    FDescANP: String;
    FEncerrante: TEncerranteModel;
    FOrigComb: TObjectList<TOrigCombModel>;
    FPBio: Double;
    FPGLP: Double;
    FPGNi: Double;
    FPGNn: Double;
    FQTemp: Double;
    FPMixGN: Double;
    FUFCons: String;
    FVPart: Currency;
    FICMSComb: TICMSCombModel;
    FICMSInter: TICMSInterModel;
    FICMSCons: TICMSConsModel;
  public
    constructor Create;
    destructor Destroy; override;
    property CODIF: String read FCODIF write FCODIF;
    property CProdANP: integer read FCProdANP write FCProdANP;
    property DescANP: String read FDescANP write FDescANP;
    property PBio: Double read FPBio write FPBio;
    property PGLP: Double read FPGLP write FPGLP;
    property PGNi: Double read FPGNi write FPGNi;
    property PGNn: Double read FPGNn write FPGNn;
    property PMixGN: Double read FPMixGN write FPMixGN;
    property QTemp: Double read FQTemp write FQTemp;
    property UFCons: String read FUFCons write FUFCons;
    property VPart: Currency read FVPart write FVPart;
    property Encerrante: TEncerranteModel read FEncerrante;
    property CIDE: TCIDEModel read FCIDE;
    property OrigComb: TObjectList<TOrigCombModel> read FOrigComb;
    property ICMSComb: TICMSCombModel read FICMSComb;
    property ICMSInter: TICMSInterModel read FICMSInter;
    property ICMSCons: TICMSConsModel read FICMSCons;
  end;

implementation

{ TCombModel }

constructor TCombModel.Create;
begin
  FCIDE := TCIDEModel.Create;
  FICMSComb := TICMSCombModel.Create;
  FICMSInter := TICMSInterModel.Create;
  FICMSCons := TICMSConsModel.Create;
  FEncerrante := TEncerranteModel.Create;
  FOrigComb := TObjectList<TOrigCombModel>.Create;
end;

destructor TCombModel.Destroy;
begin
  FCIDE.Free;
  FEncerrante.Free;
  FOrigComb.Free;
  FICMSComb.Free;
  FICMSInter.Free;
  FICMSCons.Free;
  inherited;
end;

end.

