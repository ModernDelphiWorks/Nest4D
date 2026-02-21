unit nfe.acbr.tag.prod;

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
  TNFeTagProd = class(TInterfacedObject, INFeExecute)
  private
    FACBrNFe: TACBrNFe;
    procedure SetImpostoICMS_CRTRegimeNormal(
      const ADetItem: TDetCollectionItem; const ANFeModel: TNFeModel;
        const ADetModel: TDetModel);
    procedure SetImpostoICMS_CRTSimpleNacional(
      const ADetItem: TDetCollectionItem; const ANFeModel: TNFeModel;
        const ADetModel: TDetModel);
  public
    class function Factory(AACBrNFe: TACBrNFe): INFeExecute;
    constructor Create(AACBrNFe: TACBrNFe);
    destructor Destroy; override;
    procedure Execute(const AIndex: integer; const ANFeModel: TNFeModel);
  end;

implementation

uses
  nfe.acbr.tag.prod.icms.simplesnacional,
  nfe.acbr.tag.prod.icms.regimenormal,
  nfe.acbr.tag.prod.ipi,
  nfe.acbr.tag.prod.ii,
  nfe.acbr.tag.prod.pis,
  nfe.acbr.tag.prod.cofins,
  nfe.acbr.tag.prod.issqn,
  nfe.acbr.tag.prod.comb;

{ TDFeTagProd }

constructor TNFeTagProd.Create(AACBrNFe: TACBrNFe);
begin
  FACBrNFe := AACBrNFe;
end;

destructor TNFeTagProd.Destroy;
begin
  FACBrNFe := nil;
  inherited;
end;

class function TNFeTagProd.Factory(AACBrNFe: TACBrNFe): INFeExecute;
begin
  Result := Self.Create(AACBrNFe);
end;

procedure TNFeTagProd.Execute(const AIndex: integer; const ANFeModel: TNFeModel);
var
  LDetItem: TDetCollectionItem;
  LNFe: TNFe;
  LFor: integer;
begin
  LNFe := FACBrNFe.NotasFiscais.Items[AIndex].NFe;
  for LFor := 0 to ANFeModel.InfNFe.Det.Count - 1 do
  begin
    LDetItem := LNFe.Det.New;
    //
    LDetItem.infAdProd := '';
    // Complemento de IPI de uma NFe de devolução emitida errada
    if LNFe.Ide.finNFe = fnComplementar then
    begin
      LDetItem.pDevol := 0;
      LDetItem.vIPIDevol := 0;
    end;
    LDetItem.Prod.IndTot := TpcnIndicadorTotal(ANFeModel.InfNFe.Det.Items[LFor].Prod.IndTot);
    LDetItem.Prod.nItem := ANFeModel.InfNFe.Det.Items[LFor].NItem;
    LDetItem.Prod.cProd := ANFeModel.InfNFe.Det.Items[LFor].Prod.CProd;
    LDetItem.Prod.xProd := ANFeModel.InfNFe.Det.Items[LFor].Prod.XProd;
    LDetItem.Prod.NCM := ANFeModel.InfNFe.Det.Items[LFor].Prod.NCM;
    LDetItem.Prod.CEST := ANFeModel.InfNFe.Det.Items[LFor].Prod.CEST;
    LDetItem.Prod.EXTIPI := ANFeModel.InfNFe.Det.Items[LFor].Prod.EXTIPI;
    LDetItem.Prod.CFOP := ANFeModel.InfNFe.Det.Items[LFor].Prod.CFOP;
    // Unidade Comercial
    // EXEMPLO // LDetItem.Prod.uCom    := 'CX';
               // LDetItem.Prod.qCom    :=    2;   Vendidas 2 caixas ( com 10 unidades cada )
               // LDetItem.Prod.vUnCom  :=   50;   R$ 50,00 cada caixa
               // LDetItem.Prod.vProd   :=  100;   R$ 100,00 Valor dos produtos
    LDetItem.Prod.uCom := ANFeModel.InfNFe.Det.Items[LFor].Prod.UCom;
    LDetItem.Prod.qCom := ANFeModel.InfNFe.Det.Items[LFor].Prod.QCom;
    LDetItem.Prod.vUnCom := ANFeModel.InfNFe.Det.Items[LFor].Prod.VUnCom;
    LDetItem.Prod.vProd := ANFeModel.InfNFe.Det.Items[LFor].Prod.VProd;
    LDetItem.Prod.cEAN := ANFeModel.InfNFe.Det.Items[LFor].Prod.CEAN;
    // Unidade Tributável
    // EXEMPLO // LDetItem.Prod.uTrib   := 'UN';
               // LDetItem.Prod.qTrib   :=   20;   2 caixas X 10 unidades por caixa = 20 unidades
               // LDetItem.Prod.vUnTrib :=    5;   R$ 100,00 / 20 unidades = R$ 5,00 cada unidade
    LDetItem.Prod.uTrib := ANFeModel.InfNFe.Det.Items[LFor].Prod.UTrib;
    LDetItem.Prod.qTrib := ANFeModel.InfNFe.Det.Items[LFor].Prod.QTrib;
    LDetItem.Prod.vUnTrib := ANFeModel.InfNFe.Det.Items[LFor].Prod.VUnTrib;
    LDetItem.Prod.cEANTrib := ANFeModel.InfNFe.Det.Items[LFor].Prod.CEANTrib;

    LDetItem.Prod.vFrete := ANFeModel.InfNFe.Det.Items[LFor].Prod.VFrete;
    LDetItem.Prod.vOutro := ANFeModel.InfNFe.Det.Items[LFor].Prod.VOutro;
    LDetItem.Prod.vSeg := ANFeModel.InfNFe.Det.Items[LFor].Prod.VSeg;
    LDetItem.Prod.vDesc := ANFeModel.InfNFe.Det.Items[LFor].Prod.VDesc;
    // Definição dos Impostos
    case LNFe.Emit.CRT of
      crtSimplesNacional:
      begin
        SetImpostoICMS_CRTSimpleNacional(LDetItem, ANFeModel,
                                                   ANFeModel.InfNFe.Det.Items[LFor]);
      end;
      crtRegimeNormal,
      crtSimplesExcessoReceita:
      begin
        SetImpostoICMS_CRTRegimeNormal(LDetItem, ANFeModel,
                                                 ANFeModel.InfNFe.Det.Items[LFor]);
      end;
    end;
  end;
end;

procedure TNFeTagProd.SetImpostoICMS_CRTRegimeNormal(
  const ADetItem: TDetCollectionItem; const ANFeModel: TNFeModel;
  const ADetModel: TDetModel);
begin
  // ICMS/ICMSST
  TNFeTagProdICMSRegimeNormal.New(FACBrNFe).Execute(ADetItem, ANFeModel, ADetModel);
  // IPI
  TNFeTagProdIPI.New(FACBrNFe).Execute(ADetItem, ANFeModel, ADetModel);
  // PIS/PISST
  TNFeTagProdPIS.New(FACBrNFe).Execute(ADetItem, ANFeModel, ADetModel);
  // COFINS/COFINSST
  TNFeTagProdCOFINS.New(FACBrNFe).Execute(ADetItem, ANFeModel, ADetModel);
  // II
  TNFeTagProdII.New(FACBrNFe).Execute(ADetItem, ANFeModel, ADetModel);
  // ISSQN
  TNFeTagProdISSQN.New(FACBrNFe).Execute(ADetItem, ANFeModel, ADetModel);
end;

procedure TNFeTagProd.SetImpostoICMS_CRTSimpleNacional(
  const ADetItem: TDetCollectionItem; const ANFeModel: TNFeModel;
  const ADetModel: TDetModel);
begin
  TNFeTagProdICMSSimplesNacional.New(FACBrNFe).Execute(ADetItem, ANFeModel, ADetModel);
end;

end.
