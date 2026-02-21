unit nfe.acbr.tag.prod.cofins;

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
  TNFeTagProdCOFINS = class(TInterfacedObject, INFeImposto)
  private
    FDFe: TACBrNFe;
    procedure SetImpostoCOFINS(const ADetItem: TDetCollectionItem;
      const ADetModel: TDetModel);
    procedure SetImpostoCOFINSST(const ADetItem: TDetCollectionItem;
      const ADetModel: TDetModel);
  public
    class function New(ADFe: TACBrNFe): INFeImposto;
    constructor Create(ADFe: TACBrNFe);
    destructor Destroy; override;
    procedure Execute(const ADetItem: TDetCollectionItem;
      const ANFeModel: TNFeModel; const ADetModel: TDetModel);
  end;

implementation

{ TDFeTagProdCOFINS }

constructor TNFeTagProdCOFINS.Create(ADFe: TACBrNFe);
begin
  FDFe := ADFe;
end;

destructor TNFeTagProdCOFINS.Destroy;
begin

  inherited;
end;

procedure TNFeTagProdCOFINS.Execute(const ADetItem: TDetCollectionItem;
  const ANFeModel: TNFeModel; const ADetModel: TDetModel);
begin
  // COFINS
  SetImpostoCOFINS(ADetItem, ADetModel);
  // COFINSST
  SetImpostoCOFINSST(ADetItem, ADetModel);
end;

class function TNFeTagProdCOFINS.New(ADFe: TACBrNFe): INFeImposto;
begin
  Result := Self.Create(ADFe);
end;

procedure TNFeTagProdCOFINS.SetImpostoCOFINS(const ADetItem: TDetCollectionItem;
  const ADetModel: TDetModel);
var
  LIsOK: Boolean;
begin
  // COFINS
  ADetItem.Imposto.COFINS.CST       := StrToCSTCOFINS(LIsOK, ADetModel.Imposto.COFINS.CST);
  ADetItem.Imposto.COFINS.vBC       := ADetModel.Imposto.COFINS.COFINSAliq.VBC;
  ADetItem.Imposto.COFINS.pCOFINS   := ADetModel.Imposto.COFINS.COFINSAliq.PCOFINS;
  ADetItem.Imposto.COFINS.vCOFINS   := ADetModel.Imposto.COFINS.COFINSAliq.VCOFINS;
  ADetItem.Imposto.COFINS.qBCProd   := ADetModel.Imposto.COFINS.COFINSQtde.QBCProd;
  ADetItem.Imposto.COFINS.vAliqProd := ADetModel.Imposto.COFINS.COFINSQtde.VAliqProd;
end;

procedure TNFeTagProdCOFINS.SetImpostoCOFINSST(const ADetItem: TDetCollectionItem;
  const ADetModel: TDetModel);
var
  LIsOK: Boolean;
begin
  // COFINS ST
  ADetItem.Imposto.COFINS.CST       := StrToCSTCOFINS(LIsOK, ADetModel.Imposto.COFINS.CST);
  ADetItem.Imposto.COFINS.vBC       := ADetModel.Imposto.COFINS.COFINSAliq.VBC;
  ADetItem.Imposto.COFINS.pCOFINS   := ADetModel.Imposto.COFINS.COFINSAliq.PCOFINS;
  ADetItem.Imposto.COFINS.vCOFINS   := ADetModel.Imposto.COFINS.COFINSAliq.VCOFINS;
  ADetItem.Imposto.COFINS.qBCProd   := ADetModel.Imposto.COFINS.COFINSQtde.QBCProd;
  ADetItem.Imposto.COFINS.vAliqProd := ADetModel.Imposto.COFINS.COFINSQtde.VAliqProd;
end;

end.
