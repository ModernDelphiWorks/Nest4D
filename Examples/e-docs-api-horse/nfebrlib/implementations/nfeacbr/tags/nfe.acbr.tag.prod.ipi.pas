unit nfe.acbr.tag.prod.ipi;

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
  TNFeTagProdIPI = class(TInterfacedObject, INFeImposto)
  private
    FDFe: TACBrNFe;
  public
    class function New(ADFe: TACBrNFe): INFeImposto;
    constructor Create(ADFe: TACBrNFe);
    destructor Destroy; override;
    procedure Execute(const ADetItem: TDetCollectionItem;
      const ANFeModel: TNFeModel; const ADetModel: TDetModel);
  end;

implementation

{ TDFeTagProdIPI }

constructor TNFeTagProdIPI.Create(ADFe: TACBrNFe);
begin
  FDFe := ADFe;
end;

destructor TNFeTagProdIPI.Destroy;
begin

  inherited;
end;

procedure TNFeTagProdIPI.Execute(const ADetItem: TDetCollectionItem;
  const ANFeModel: TNFeModel; const ADetModel: TDetModel);
var
  LisOK: Boolean;
begin
  ADetItem.Imposto.IPI.clEnq := ADetModel.Imposto.IPI.CLEnq;
  ADetItem.Imposto.IPI.CNPJProd := ADetModel.Imposto.IPI.CNPJProd;
  ADetItem.Imposto.IPI.cSelo := ADetModel.Imposto.IPI.CSelo;
  ADetItem.Imposto.IPI.qSelo := ADetModel.Imposto.IPI.QSelo;
  ADetItem.Imposto.IPI.cEnq := ADetModel.Imposto.IPI.CEnq;
  ADetItem.Imposto.IPI.CST := StrToCSTIPI(LisOK, ADetModel.Imposto.IPI.CST);
  ADetItem.Imposto.IPI.vBC := ADetModel.Imposto.IPI.VBC;
  ADetItem.Imposto.IPI.pIPI := ADetModel.Imposto.IPI.PIPI;
  ADetItem.Imposto.IPI.qUnid := ADetModel.Imposto.IPI.QUnId;
  ADetItem.Imposto.IPI.vUnid := ADetModel.Imposto.IPI.VUnId;
  ADetItem.Imposto.IPI.vIPI := ADetModel.Imposto.IPI.VIPI;
end;

class function TNFeTagProdIPI.New(ADFe: TACBrNFe): INFeImposto;
begin
  Result := Self.Create(ADFe);
end;

end.
