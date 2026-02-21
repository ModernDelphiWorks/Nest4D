unit nfe.acbr.tag.prod.issqn;

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
  TNFeTagProdISSQN = class(TInterfacedObject, INFeImposto)
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

{ TDFeTagProdISSQN }

constructor TNFeTagProdISSQN.Create(ADFe: TACBrNFe);
begin
  FDFe := ADFe;
end;

destructor TNFeTagProdISSQN.Destroy;
begin

  inherited;
end;

procedure TNFeTagProdISSQN.Execute(const ADetItem: TDetCollectionItem;
  const ANFeModel: TNFeModel; const ADetModel: TDetModel);
var
  LIsOK: Boolean;
begin
  // ISSQN
  ADetItem.Imposto.ISSQN.vBC := ADetModel.Imposto.ISSQN.VBC;
  ADetItem.Imposto.ISSQN.vAliq := ADetModel.Imposto.ISSQN.VAliq;
  ADetItem.Imposto.ISSQN.vISSQN := ADetModel.Imposto.ISSQN.VISSQN;
  ADetItem.Imposto.ISSQN.cMunFG := ADetModel.Imposto.ISSQN.CMun;
  ADetItem.Imposto.ISSQN.cListServ := ADetModel.Imposto.ISSQN.CListServ;
  ADetItem.Imposto.ISSQN.cSitTrib := StrToISSQNcSitTrib(LIsOK, ADetModel.Imposto.ISSQN.CServico);
end;

class function TNFeTagProdISSQN.New(ADFe: TACBrNFe): INFeImposto;
begin
  Result := Self.Create(ADFe);
end;

end.
