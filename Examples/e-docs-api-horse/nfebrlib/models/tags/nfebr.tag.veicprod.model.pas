unit nfebr.tag.veicprod.model;

interface

type
  TVeicProdModel = class
  private
    FAnoFab: integer;
    FAnoMod: integer;
    FCCor: String;
    FCCorDENATRAN: String;
    FCMT: String;
    FCMod: String;
    FChassi: String;
    FCilin: String;
    FCondVeic: integer;
    FDist: String;
    FEspVeic: integer;
    FLota: integer;
    FNMotor: String;
    FNSerie: String;
    FPesoB: String;
    FPesoL: String;
    FPot: String;
    FTpComb: String;
    FTpOp: integer;
    FTpPint: String;
    FTpRest: integer;
    FTpVeic: integer;
    FVIN: String;
    FXCor: String;
  public
    property AnoFab: integer read FAnoFab write FAnoFab;
    property AnoMod: integer read FAnoMod write FAnoMod;
    property CCor: String read FCCor write FCCor;
    property CCorDENATRAN: String read FCCorDENATRAN write FCCorDENATRAN;
    property CMT: String read FCMT write FCMT;
    property CMod: String read FCMod write FCMod;
    property Chassi: String read FChassi write FChassi;
    property Cilin: String read FCilin write FCilin;
    property CondVeic: integer read FCondVeic write FCondVeic;
    property Dist: String read FDist write FDist;
    property EspVeic: integer read FEspVeic write FEspVeic;
    property Lota: integer read FLota write FLota;
    property NMotor: String read FNMotor write FNMotor;
    property NSerie: String read FNSerie write FNSerie;
    property PesoB: String read FPesoB write FPesoB;
    property PesoL: String read FPesoL write FPesoL;
    property Pot: String read FPot write FPot;
    property TpComb: String read FTpComb write FTpComb;
    property TpOp: integer read FTpOp write FTpOp;
    property TpPint: String read FTpPint write FTpPint;
    property TpRest: integer read FTpRest write FTpRest;
    property TpVeic: integer read FTpVeic write FTpVeic;
    property VIN: String read FVIN write FVIN;
    property XCor: String read FXCor write FXCor;
  end;

implementation

end.

