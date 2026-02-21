unit nfe.acbr.tag.prod.pis;

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
  TNFeTagProdPIS = class(TInterfacedObject, INFeImposto)
  private
    FDFe: TACBrNFe;
    procedure SetImpostoPIS(const ADetItem: TDetCollectionItem;
      const ADetModel: TDetModel);
    procedure SetImpostoPISST(const ADetItem: TDetCollectionItem;
      const ADetModel: TDetModel);
  public
    class function New(ADFe: TACBrNFe): INFeImposto;
    constructor Create(ADFe: TACBrNFe);
    destructor Destroy; override;
    procedure Execute(const ADetItem: TDetCollectionItem;
      const ANFeModel: TNFeModel; const ADetModel: TDetModel);
  end;

implementation

{ TDFeTagProdPIS }

constructor TNFeTagProdPIS.Create(ADFe: TACBrNFe);
begin
  FDFe := ADFe;
end;

destructor TNFeTagProdPIS.Destroy;
begin

  inherited;
end;

procedure TNFeTagProdPIS.Execute(const ADetItem: TDetCollectionItem;
  const ANFeModel: TNFeModel; const ADetModel: TDetModel);
begin
  // PIS
  SetImpostoPIS(ADetItem, ADetModel);
  // PISST
  SetImpostoPISST(ADetItem, ADetModel);
end;

class function TNFeTagProdPIS.New(ADFe: TACBrNFe): INFeImposto;
begin
  Result := Self.Create(ADFe);
end;

procedure TNFeTagProdPIS.SetImpostoPIS(const ADetItem: TDetCollectionItem;
  const ADetModel: TDetModel);
var
  LisOK: boolean;
begin
  // PIS
  ADetItem.Imposto.PIS.CST := StrToCSTPIS(LisOk, ADetModel.Imposto.PIS.CST);
  ADetItem.Imposto.PIS.vBC := ADetModel.Imposto.PIS.PISAliq.VBC;
  ADetItem.Imposto.PIS.pPIS := ADetModel.Imposto.PIS.PISAliq.PPIS;
  ADetItem.Imposto.PIS.vPIS := ADetModel.Imposto.PIS.PISAliq.VPIS;
  ADetItem.Imposto.PIS.qBCProd := ADetModel.Imposto.PIS.PISQtde.QBCProd;
  ADetItem.Imposto.PIS.vAliqProd := ADetModel.Imposto.PIS.PISQtde.VAliqProd;
end;

procedure TNFeTagProdPIS.SetImpostoPISST(const ADetItem: TDetCollectionItem;
  const ADetModel: TDetModel);
var
  LIsOK: boolean;
begin
  // PIS ST
  ADetItem.Imposto.PIS.CST := StrToCSTPIS(LisOk, ADetModel.Imposto.PIS.CST);
  ADetItem.Imposto.PIS.vBC := ADetModel.Imposto.PIS.PISAliq.VBC;
  ADetItem.Imposto.PIS.pPIS := ADetModel.Imposto.PIS.PISAliq.PPIS;
  ADetItem.Imposto.PIS.vPIS := ADetModel.Imposto.PIS.PISAliq.VPIS;
  ADetItem.Imposto.PIS.qBCProd := ADetModel.Imposto.PIS.PISQtde.QBCProd;
  ADetItem.Imposto.PIS.vAliqProd := ADetModel.Imposto.PIS.PISQtde.VAliqProd;
end;

end.
