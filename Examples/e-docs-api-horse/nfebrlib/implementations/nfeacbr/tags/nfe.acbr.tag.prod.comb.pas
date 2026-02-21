unit nfe.acbr.tag.prod.comb;

interface

uses
  SysUtils,
  ACBrNFe,
  ACBrNFe.Classes,
  pcnConversaoNFe,
  pcnConversao,
  nfebr.lib.enum,
  nfebr.model,
  nfe.acbr.interfaces,
  nfebr.tag.det.model;

type
  TNFeTagProdComb = class(TInterfacedObject, INFeImposto)
  private
    FACBrNFe: TACBrNFe;
  public
    class function New(AACBrNFe: TACBrNFe): INFeImposto;
    constructor Create(AACBrNFe: TACBrNFe);
    destructor Destroy; override;
    procedure Execute(const ADetItem: TDetCollectionItem;
      const ANFeModel: TNFeModel; const ADetModel: TDetModel);
  end;

implementation

{ TDFeTagProdComb }

constructor TNFeTagProdComb.Create(AACBrNFe: TACBrNFe);
begin
  FACBrNFe := AACBrNFe;
end;

destructor TNFeTagProdComb.Destroy;
begin

  inherited;
end;

procedure TNFeTagProdComb.Execute(const ADetItem: TDetCollectionItem;
  const ANFeModel: TNFeModel; const ADetModel: TDetModel);
begin
  ADetItem.Prod.comb.cProdANP := ADetModel.Prod.Comb.CProdANP;
  ADetItem.Prod.comb.UFcons := ADetModel.Prod.Comb.UFCons;
  ADetItem.Prod.comb.CODIF := ADetModel.Prod.Comb.CODIF;
  ADetItem.Prod.comb.qTemp := ADetModel.Prod.Comb.QTemp;
  ADetItem.Prod.comb.pBio := ADetModel.Prod.Comb.PBio;
  ADetItem.Prod.comb.pGLP := ADetModel.Prod.Comb.PGLP;
  ADetItem.Prod.comb.pGNi := ADetModel.Prod.Comb.PGNi;
  ADetItem.Prod.comb.pGNn := ADetModel.Prod.Comb.PGNn;
  ADetItem.Prod.comb.pMixGN := ADetModel.Prod.Comb.PMixGN;
  // TAG de grupo da CIDE - <CIDE> - Ocorrência 0-1
  ADetItem.Prod.comb.CIDE.qBCprod := ADetModel.Prod.Comb.CIDE.QBCProd;
  ADetItem.Prod.comb.CIDE.vAliqProd := ADetModel.Prod.Comb.CIDE.VAliqProd;
  ADetItem.Prod.comb.CIDE.vCIDE := ADetModel.Prod.Comb.CIDE.VCIDE;
  // TAG de grupo do ICMS - <ICMS> - Ocorrência 1-1
  ADetItem.Prod.comb.ICMS.vBCICMS := ADetModel.Prod.Comb.ICMSComb.vBCICMS;
  ADetItem.Prod.comb.ICMS.vICMS := ADetModel.Prod.Comb.ICMSComb.vICMS;
  ADetItem.Prod.comb.ICMS.vBCICMSST := ADetModel.Prod.Comb.ICMSComb.vBCICMSST;
  ADetItem.Prod.comb.ICMS.vICMSST := ADetModel.Prod.Comb.ICMSComb.vICMSST;
  // TAG de grupo do ICMSST de operação interestadual - <ICMSInter> - Ocorrência 0-1
  ADetItem.Prod.comb.ICMSInter.vBCICMSSTDest := ADetModel.Prod.Comb.ICMSInter.vBCICMSSTDest;
  ADetItem.Prod.comb.ICMSInter.vICMSSTDest := ADetModel.Prod.Comb.ICMSInter.vICMSSTDest;
  // TAG de ICMS para consumo em UF diversa da UF de localização do destinatário do produto - <ICMSCons> - Ocorrência 0-1
  ADetItem.Prod.comb.ICMSCons.vBCICMSSTCons := ADetModel.Prod.Comb.ICMSCons.vBCICMSSTCons;
  ADetItem.Prod.comb.ICMSCons.vICMSSTCons := ADetModel.Prod.Comb.ICMSCons.vICMSSTCons;
  ADetItem.Prod.comb.ICMSCons.UFcons := ADetModel.Prod.Comb.ICMSCons.UFcons;
end;

class function TNFeTagProdComb.New(AACBrNFe: TACBrNFe): INFeImposto;
begin
  Result := Self.Create(AACBrNFe);
end;

end.
