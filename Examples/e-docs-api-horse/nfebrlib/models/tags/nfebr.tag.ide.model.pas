unit nfebr.tag.ide.model;

interface

uses
  Generics.Collections,
  nfebr.tag.nfref.model;

type
  TIdeModel = class
  private
    FCDV: integer;
    FCMunFG: String;
    FCNF: String;
    FCUF: integer;
    FDhCont: TDateTime;
    FDhEmi: TDateTime;
    FDhSaiEnt: TDateTime;
    FFinNFe: integer;
    FIdDest: integer;
    FIndFinal: integer;
    FIndIntermed: integer;
    FIndPres: integer;
    FMod: integer;
    FNFref: TObjectList<TNFrefModel>;
    FNNF: integer;
    FNatOp: String;
    FProcEmi: integer;
    FSerie: integer;
    FTpAmb: integer;
    FTpEmis: integer;
    FTpImp: integer;
    FTpNF: integer;
    FVerProc: String;
    FXJust: String;
  public
    constructor Create;
    destructor Destroy; override;
    property CDV: integer read FCDV write FCDV;
    property CMunFG: String read FCMunFG write FCMunFG;
    property CNF: String read FCNF write FCNF;
    property CUF: integer read FCUF write FCUF;
    property DhCont: TDateTime read FDhCont write FDhCont;
    property DhEmi: TDateTime read FDhEmi write FDhEmi;
    property DhSaiEnt: TDateTime read FDhSaiEnt write FDhSaiEnt;
    property FinNFe: integer read FFinNFe write FFinNFe;
    property IdDest: integer read FIdDest write FIdDest;
    property IndFinal: integer read FIndFinal write FIndFinal;
    property IndIntermed: integer read FIndIntermed write FIndIntermed;
    property IndPres: integer read FIndPres write FIndPres;
    property Modelo: integer read FMod write FMod;
    property NFref: TObjectList<TNFrefModel> read FNFref;
    property NNF: integer read FNNF write FNNF;
    property NatOp: String read FNatOp write FNatOp;
    property ProcEmi: integer read FProcEmi write FProcEmi;
    property Serie: integer read FSerie write FSerie;
    property TpAmb: integer read FTpAmb write FTpAmb;
    property TpEmis: integer read FTpEmis write FTpEmis;
    property TpImp: integer read FTpImp write FTpImp;
    property TpNF: integer read FTpNF write FTpNF;
    property VerProc: String read FVerProc write FVerProc;
    property XJust: String read FXJust write FXJust;
  end;

implementation

{ TIdeModel }

constructor TIdeModel.Create;
begin
  FNFref := TObjectList<TNFrefModel>.Create;
end;

destructor TIdeModel.Destroy;
begin
  FNFref.Free;
  inherited;
end;

end.

