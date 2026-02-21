unit nfebr.tag.prod.model;

interface

uses
  nfebr.tag.arma.model,
  nfebr.tag.comb.model,
  nfebr.tag.di.model,
  nfebr.tag.detexport.model,
  nfebr.tag.infprodemb.model,
  nfebr.tag.infprodnff.model,
  nfebr.tag.med.model,
  nfebr.tag.rastro.model,
  nfebr.tag.veicprod.model,
  Generics.Collections;

type
  TProdModel = class
  private
    FCBarra: String;
    FCBarraTrib: String;
    FCBenef: String;
    FCEAN: String;
    FCEANTrib: String;
    FCEST: String;
    FCFOP: String;
    FCNPJFab: String;
    FCProd: String;
    FEXTIPI: String;
    FIndEscala: String;
    FIndTot: integer;
    FNCM: String;
    FNFCI: String;
    FNItemPed: integer;
    FNRECOPI: String;
    FQCom: Double;
    FQTrib: Double;
    FUCom: String;
    FUTrib: String;
    FVDesc: Currency;
    FVFrete: Currency;
    FVOutro: Currency;
    FVProd: Currency;
    FVSeg: Currency;
    FVUnCom: Currency;
    FVUnTrib: Currency;
    FXPed: String;
    FXProd: String;
    FNVE: TArray<String>;
    FInfProdEmb: TInfProdEmbModel;
    FInfProdNFF: TInfProdNFFModel;
    FComb: TCombModel;
    FMed: TMedModel;
    FDI: TObjectList<TDIModel>;
    FDetExport: TObjectList<TDetExportModel>;
    FVeicProd: TVeicProdModel;
    FRastro: TObjectList<TRastroModel>;
    FArma: TObjectList<TArmaModel>;
  public
    constructor Create;
    destructor Destroy; override;
    property CBarra: String read FCBarra write FCBarra;
    property CBarraTrib: String read FCBarraTrib write FCBarraTrib;
    property CBenef: String read FCBenef write FCBenef;
    property CEAN: String read FCEAN write FCEAN;
    property CEANTrib: String read FCEANTrib write FCEANTrib;
    property CEST: String read FCEST write FCEST;
    property CFOP: String read FCFOP write FCFOP;
    property CNPJFab: String read FCNPJFab write FCNPJFab;
    property CProd: String read FCProd write FCProd;
    property EXTIPI: String read FEXTIPI write FEXTIPI;
    property IndEscala: String read FIndEscala write FIndEscala;
    property IndTot: integer read FIndTot write FIndTot;
    property NCM: String read FNCM write FNCM;
    property NFCI: String read FNFCI write FNFCI;
    property NItemPed: integer read FNItemPed write FNItemPed;
    property NRECOPI: String read FNRECOPI write FNRECOPI;
    property QCom: Double read FQCom write FQCom;
    property QTrib: Double read FQTrib write FQTrib;
    property UCom: String read FUCom write FUCom;
    property UTrib: String read FUTrib write FUTrib;
    property VDesc: Currency read FVDesc write FVDesc;
    property VFrete: Currency read FVFrete write FVFrete;
    property VOutro: Currency read FVOutro write FVOutro;
    property VProd: Currency read FVProd write FVProd;
    property VSeg: Currency read FVSeg write FVSeg;
    property VUnCom: Currency read FVUnCom write FVUnCom;
    property VUnTrib: Currency read FVUnTrib write FVUnTrib;
    property XPed: String read FXPed write FXPed;
    property XProd: String read FXProd write FXProd;
    property Comb: TCombModel read FComb;
    property InfProdEmb: TInfProdEmbModel read FInfProdEmb;
    property InfProdNFF: TInfProdNFFModel read FInfProdNFF;
    property Med: TMedModel read FMed;
    property NVE: TArray<String> read FNVE;
    property Arma: TObjectList<TArmaModel> read FArma;
    property DI: TObjectList<TDIModel> read FDI;
    property VeicProd: TVeicProdModel read FVeicProd;
    property Rastro: TObjectList<TRastroModel> read FRastro;
    property DetExport: TObjectList<TDetExportModel> read FDetExport;
  end;

implementation

constructor TProdModel.Create;
begin
  FArma := TObjectList<TArmaModel>.Create;
  FComb := TCombModel.Create;
  FDI := TObjectList<TDIModel>.Create;
  FDetExport := TObjectList<TDetExportModel>.Create;
  FInfProdEmb := TInfProdEmbModel.Create;
  FInfProdNFF := TInfProdNFFModel.Create;
  FMed := TMedModel.Create;
  FRastro := TObjectList<TRastroModel>.Create;
  FVeicProd := TVeicProdModel.Create;
end;

destructor TProdModel.Destroy;
begin
  FArma.Free;
  FComb.Free;
  FDI.Free;
  FDetExport.Free;
  FInfProdEmb.Free;
  FInfProdNFF.Free;
  FMed.Free;
  FRastro.Free;
  FVeicProd.Free;
  inherited;
end;

end.

