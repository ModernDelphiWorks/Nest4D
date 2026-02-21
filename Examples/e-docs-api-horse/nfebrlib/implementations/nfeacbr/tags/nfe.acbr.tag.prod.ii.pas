unit nfe.acbr.tag.prod.ii;

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
  TNFeTagProdII = class(TInterfacedObject, INFeImposto)
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

{ TDFeTagProdII }

constructor TNFeTagProdII.Create(ADFe: TACBrNFe);
begin
  FDFe := ADFe;
end;

destructor TNFeTagProdII.Destroy;
begin

  inherited;
end;

procedure TNFeTagProdII.Execute(const ADetItem: TDetCollectionItem;
  const ANFeModel: TNFeModel; const ADetModel: TDetModel);
begin
  // II
  ADetItem.Imposto.II.vBc      := ADetModel.Imposto.II.VBC;
  ADetItem.Imposto.II.vDespAdu := ADetModel.Imposto.II.VDespAdu;
  ADetItem.Imposto.II.vII      := ADetModel.Imposto.II.VII;
  ADetItem.Imposto.II.vIOF     := ADetModel.Imposto.II.VIOF;
end;

class function TNFeTagProdII.New(ADFe: TACBrNFe): INFeImposto;
begin
  Result := Self.Create(ADFe);
end;

end.

