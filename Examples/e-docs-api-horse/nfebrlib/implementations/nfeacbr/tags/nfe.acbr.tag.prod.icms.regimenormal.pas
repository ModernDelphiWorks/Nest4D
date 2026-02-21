unit nfe.acbr.tag.prod.icms.regimenormal;

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
  TNFeTagProdICMSRegimeNormal = class(TInterfacedObject, INFeImposto)
  private
    FNFeModel: TNFeModel;
    procedure _SetImpostoICMSST_Efetivo(const ADetItem: TDetCollectionItem;
      const ADetModel: TDetModel);
    procedure SetImpostoICMS_CST00(const ADetItem: TDetCollectionItem;
      const ADetModel: TDetModel);
    procedure SetImpostoICMS_CST10(const ADetItem: TDetCollectionItem;
      const ADetModel: TDetModel);
    procedure SetImpostoICMS_CST20(const ADetItem: TDetCollectionItem;
      const ADetModel: TDetModel);
    procedure SetImpostoICMS_CST30(const ADetItem: TDetCollectionItem;
      const ADetModel: TDetModel);
    procedure SetImpostoICMS_CST40(const ADetItem: TDetCollectionItem;
      const ADetModel: TDetModel);
    procedure SetImpostoICMS_CST41(const ADetItem: TDetCollectionItem;
      const ADetModel: TDetModel);
    procedure SetImpostoICMS_CST45(const ADetItem: TDetCollectionItem;
      const ADetModel: TDetModel);
    procedure SetImpostoICMS_CST50(const ADetItem: TDetCollectionItem;
      const ADetModel: TDetModel);
    procedure SetImpostoICMS_CST51(const ADetItem: TDetCollectionItem;
      const ADetModel: TDetModel);
    procedure SetImpostoICMS_CST60(const ADetItem: TDetCollectionItem;
      const ADetModel: TDetModel);
    procedure SetImpostoICMS_CST70(const ADetItem: TDetCollectionItem;
      const ADetModel: TDetModel);
    procedure SetImpostoICMS_CST90(const ADetItem: TDetCollectionItem;
      const ADetModel: TDetModel);
    procedure SetImpostoICMSST_Repasse(const ADetItem: TDetCollectionItem;
      const ADetModel: TDetModel);
    procedure SetImpostoICMS_Partilha(const ADetItem: TDetCollectionItem;
      const ADetModel: TDetModel);
    procedure SetImpostoICMS_Difal(const ADetItem: TDetCollectionItem;
      const ADetModel: TDetModel);
  public
    constructor Create(const AACBrNFe: TACBrNFe);
    destructor Destroy; override;
    class function New(const AACBrNFe: TACBrNFe): INFeImposto;
    procedure Execute(const ADetItem: TDetCollectionItem;
      const ANFeModel: TNFeModel; const ADetModel: TDetModel);
  end;

implementation

{ TDFeTagProdICMSRegimeNormal }

constructor TNFeTagProdICMSRegimeNormal.Create(const AACBrNFe: TACBrNFe);
begin

end;

destructor TNFeTagProdICMSRegimeNormal.Destroy;
begin
  FNFeModel := nil;
  inherited;
end;

procedure TNFeTagProdICMSRegimeNormal.Execute(const ADetItem: TDetCollectionItem;
  const ANFeModel: TNFeModel; const ADetModel: TDetModel);
var
  LIsOK: Boolean;
begin
  FNFeModel := ANFeModel;
  //
  ADetItem.Imposto.ICMS.Orig := TpcnOrigemMercadoria
    (ADetModel.Imposto.ICMS.Orig);
  ADetItem.Imposto.ICMS.CST := StrToCSTIcms(LIsOK, ADetModel.Imposto.ICMS.CST);
  case ADetItem.Imposto.ICMS.CST of
    cst00: SetImpostoICMS_CST00(ADetItem, ADetModel);
    cst10: SetImpostoICMS_CST10(ADetItem, ADetModel);
    cst20: SetImpostoICMS_CST20(ADetItem, ADetModel);
    cst30: SetImpostoICMS_CST30(ADetItem, ADetModel);
    cst40: SetImpostoICMS_CST40(ADetItem, ADetModel);
    cst41: SetImpostoICMS_CST41(ADetItem, ADetModel);
    cst45: SetImpostoICMS_CST45(ADetItem, ADetModel);
    cst50: SetImpostoICMS_CST50(ADetItem, ADetModel);
    cst51: SetImpostoICMS_CST51(ADetItem, ADetModel);
    cst60: SetImpostoICMS_CST60(ADetItem, ADetModel);
    cst70: SetImpostoICMS_CST70(ADetItem, ADetModel);
    // cst80: SetImpostoICMS_CST80(ADetItem, ADetModel);
    // cst81: SetImpostoICMS_CST81(ADetItem, ADetModel);
    cst90: SetImpostoICMS_CST90(ADetItem, ADetModel);
    // cstPart10: ;
    // cstPart90: ;
    // cstRep41: ;
    // cstVazio: ;
    // cstICMSOutraUF: ;
    // cstICMSSN: ;
    // cstRep60: ;
  end;
  SetImpostoICMS_Partilha(ADetItem, ADetModel);
  SetImpostoICMS_Difal(ADetItem, ADetModel);
  ADetItem.Imposto.vTotTrib := ADetModel.Imposto.vTotTrib;
end;

class function TNFeTagProdICMSRegimeNormal.New(const AACBrNFe: TACBrNFe): INFeImposto;
begin
  Result := Self.Create(AACBrNFe);
end;

procedure TNFeTagProdICMSRegimeNormal._SetImpostoICMSST_Efetivo(
  const ADetItem: TDetCollectionItem; const ADetModel: TDetModel);
begin
  ADetItem.Imposto.ICMS.pRedBCEfet := 0;
  ADetItem.Imposto.ICMS.vBCEfet := 0;
  ADetItem.Imposto.ICMS.pICMSEfet := 0;
  ADetItem.Imposto.ICMS.vICMSEfet := 0;
end;

procedure TNFeTagProdICMSRegimeNormal.SetImpostoICMSST_Repasse(
  const ADetItem: TDetCollectionItem; const ADetModel: TDetModel);
begin
  ADetItem.Imposto.ICMS.vBCSTRet := ADetModel.Imposto.ICMS.ICMS60.VBCSTRet;
  ADetItem.Imposto.ICMS.vICMSSTRet := ADetModel.Imposto.ICMS.ICMS60.VICMSSTRet;
  ADetItem.Imposto.ICMS.vBCSTDest := 0;
  ADetItem.Imposto.ICMS.vICMSSTDest := 0;
end;

procedure TNFeTagProdICMSRegimeNormal.SetImpostoICMS_CST00(
  const ADetItem: TDetCollectionItem; const ADetModel: TDetModel);
begin
  ADetItem.Imposto.ICMS.modBC := TpcnDeterminacaoBaseIcms(ADetModel.Imposto.ICMS.ICMS00.modBC);
  ADetItem.Imposto.ICMS.vBC := ADetModel.Imposto.ICMS.ICMS00.vBC;
  ADetItem.Imposto.ICMS.pICMS := ADetModel.Imposto.ICMS.ICMS00.pICMS;
  ADetItem.Imposto.ICMS.vICMS := ADetModel.Imposto.ICMS.ICMS00.vICMS;
end;

procedure TNFeTagProdICMSRegimeNormal.SetImpostoICMS_CST10(
  const ADetItem: TDetCollectionItem; const ADetModel: TDetModel);
begin
  ADetItem.Imposto.ICMS.modBC := TpcnDeterminacaoBaseIcms(ADetModel.Imposto.ICMS.ICMS10.modBC);
  ADetItem.Imposto.ICMS.vBC := ADetModel.Imposto.ICMS.ICMS10.vBC;
  ADetItem.Imposto.ICMS.pICMS := ADetModel.Imposto.ICMS.ICMS10.pICMS;
  ADetItem.Imposto.ICMS.vICMS := ADetModel.Imposto.ICMS.ICMS10.vICMS;
  // ICMSST
  ADetItem.Imposto.ICMS.modBCST := TpcnDeterminacaoBaseIcmsST(ADetModel.Imposto.ICMS.ICMS10.modBCST);
  ADetItem.Imposto.ICMS.pMVAST := ADetModel.Imposto.ICMS.ICMS10.pMVAST;
  ADetItem.Imposto.ICMS.pRedBCST := ADetModel.Imposto.ICMS.ICMS10.pRedBCST;
  ADetItem.Imposto.ICMS.vBCST := ADetModel.Imposto.ICMS.ICMS10.vBCST;
  ADetItem.Imposto.ICMS.pICMSST := ADetModel.Imposto.ICMS.ICMS10.pICMSST;
  ADetItem.Imposto.ICMS.vICMSST := ADetModel.Imposto.ICMS.ICMS10.vICMSST;
end;

procedure TNFeTagProdICMSRegimeNormal.SetImpostoICMS_CST20(
  const ADetItem: TDetCollectionItem; const ADetModel: TDetModel);
begin
  ADetItem.Imposto.ICMS.modBC := TpcnDeterminacaoBaseIcms(ADetModel.Imposto.ICMS.ICMS20.ModBC);
  ADetItem.Imposto.ICMS.pRedBC := ADetModel.Imposto.ICMS.ICMS20.PRedBC;
  ADetItem.Imposto.ICMS.vBC := ADetModel.Imposto.ICMS.ICMS20.VBC;
  ADetItem.Imposto.ICMS.pICMS := ADetModel.Imposto.ICMS.ICMS20.PICMS;
  ADetItem.Imposto.ICMS.vICMS := ADetModel.Imposto.ICMS.ICMS20.VICMS;
end;

procedure TNFeTagProdICMSRegimeNormal.SetImpostoICMS_CST30(
  const ADetItem: TDetCollectionItem; const ADetModel: TDetModel);
begin
  // ICMSST
  ADetItem.Imposto.ICMS.modBCST := TpcnDeterminacaoBaseIcmsST(ADetModel.Imposto.ICMS.ICMS30.ModBCST);
  ADetItem.Imposto.ICMS.pMVAST := ADetModel.Imposto.ICMS.ICMS30.PMVAST;
  ADetItem.Imposto.ICMS.pRedBCST := ADetModel.Imposto.ICMS.ICMS30.PRedBCST;
  ADetItem.Imposto.ICMS.vBCST := ADetModel.Imposto.ICMS.ICMS30.VBCST;
  ADetItem.Imposto.ICMS.pICMSST := ADetModel.Imposto.ICMS.ICMS30.PICMSST;
  ADetItem.Imposto.ICMS.vICMSST := ADetModel.Imposto.ICMS.ICMS30.VICMSST;
end;

procedure TNFeTagProdICMSRegimeNormal.SetImpostoICMS_CST40(
  const ADetItem: TDetCollectionItem; const ADetModel: TDetModel);
begin
  ADetItem.Imposto.ICMS.motDesICMS := TpcnMotivoDesoneracaoICMS(ADetModel.Imposto.ICMS.ICMS40.MotDesICMS);
  ADetItem.Imposto.ICMS.vICMSDeson := ADetModel.Imposto.ICMS.ICMS40.VICMSDeson;
end;

procedure TNFeTagProdICMSRegimeNormal.SetImpostoICMS_CST41(
  const ADetItem: TDetCollectionItem; const ADetModel: TDetModel);
begin
  ADetItem.Imposto.ICMS.motDesICMS := TpcnMotivoDesoneracaoICMS(ADetModel.Imposto.ICMS.ICMS40.MotDesICMS);
  ADetItem.Imposto.ICMS.vICMSDeson := ADetModel.Imposto.ICMS.ICMS40.VICMSDeson;
  // ICMSST Repasse
  SetImpostoICMSST_Repasse(ADetItem, ADetModel);
end;

procedure TNFeTagProdICMSRegimeNormal.SetImpostoICMS_CST45(
  const ADetItem: TDetCollectionItem; const ADetModel: TDetModel);
begin

end;

procedure TNFeTagProdICMSRegimeNormal.SetImpostoICMS_CST50(
  const ADetItem: TDetCollectionItem; const ADetModel: TDetModel);
begin
  ADetItem.Imposto.ICMS.motDesICMS := TpcnMotivoDesoneracaoICMS(ADetModel.Imposto.ICMS.ICMS40.MotDesICMS);
  ADetItem.Imposto.ICMS.vICMSDeson := ADetModel.Imposto.ICMS.ICMS40.VICMSDeson;
end;

procedure TNFeTagProdICMSRegimeNormal.SetImpostoICMS_CST51(
  const ADetItem: TDetCollectionItem; const ADetModel: TDetModel);
begin
  ADetItem.Imposto.ICMS.modBC := TpcnDeterminacaoBaseIcms(ADetModel.Imposto.ICMS.ICMS51.ModBC);
  ADetItem.Imposto.ICMS.pRedBC := ADetModel.Imposto.ICMS.ICMS51.PRedBC;
  ADetItem.Imposto.ICMS.vBC := ADetModel.Imposto.ICMS.ICMS51.VBC;
  ADetItem.Imposto.ICMS.pICMS := ADetModel.Imposto.ICMS.ICMS51.PICMS;
  ADetItem.Imposto.ICMS.vICMS := ADetModel.Imposto.ICMS.ICMS51.VICMS;
end;

procedure TNFeTagProdICMSRegimeNormal.SetImpostoICMS_CST60(
  const ADetItem: TDetCollectionItem; const ADetModel: TDetModel);
begin
  SetImpostoICMSST_Repasse(ADetItem, ADetModel);
end;

procedure TNFeTagProdICMSRegimeNormal.SetImpostoICMS_CST70(
  const ADetItem: TDetCollectionItem; const ADetModel: TDetModel);
begin
  // ICMS Reduzido
  ADetItem.Imposto.ICMS.modBC := TpcnDeterminacaoBaseIcms(ADetModel.Imposto.ICMS.ICMS70.ModBC);
  ADetItem.Imposto.ICMS.pRedBC := ADetModel.Imposto.ICMS.ICMS70.PRedBC;
  ADetItem.Imposto.ICMS.vBC := ADetModel.Imposto.ICMS.ICMS70.VBC;
  ADetItem.Imposto.ICMS.pICMS := ADetModel.Imposto.ICMS.ICMS70.PICMS;
  ADetItem.Imposto.ICMS.vICMS := ADetModel.Imposto.ICMS.ICMS70.VICMS;
  // ICMSST
  ADetItem.Imposto.ICMS.modBCST := TpcnDeterminacaoBaseIcmsST(ADetModel.Imposto.ICMS.ICMS70.ModBCST);
  ADetItem.Imposto.ICMS.pMVAST := ADetModel.Imposto.ICMS.ICMS70.PMVAST;
  ADetItem.Imposto.ICMS.pRedBCST := ADetModel.Imposto.ICMS.ICMS70.PRedBCST;
  ADetItem.Imposto.ICMS.vBCST := ADetModel.Imposto.ICMS.ICMS70.VBCST;
  ADetItem.Imposto.ICMS.pICMSST := ADetModel.Imposto.ICMS.ICMS70.PICMSST;
  ADetItem.Imposto.ICMS.vICMSST := ADetModel.Imposto.ICMS.ICMS70.VICMSST;
end;

procedure TNFeTagProdICMSRegimeNormal.SetImpostoICMS_CST90(
  const ADetItem: TDetCollectionItem; const ADetModel: TDetModel);
begin
  ADetItem.Imposto.ICMS.modBC := TpcnDeterminacaoBaseIcms(ADetModel.Imposto.ICMS.ICMS90.ModBC);
  ADetItem.Imposto.ICMS.pRedBC := ADetModel.Imposto.ICMS.ICMS90.PRedBC;
  ADetItem.Imposto.ICMS.vBC := ADetModel.Imposto.ICMS.ICMS90.VBC;
  ADetItem.Imposto.ICMS.pICMS := ADetModel.Imposto.ICMS.ICMS90.PICMS;
  ADetItem.Imposto.ICMS.vICMS := ADetModel.Imposto.ICMS.ICMS90.VICMS;
  // ICMSST
  ADetItem.Imposto.ICMS.modBCST := TpcnDeterminacaoBaseIcmsST(ADetModel.Imposto.ICMS.ICMS90.ModBCST);
  ADetItem.Imposto.ICMS.pMVAST := ADetModel.Imposto.ICMS.ICMS90.PMVAST;
  ADetItem.Imposto.ICMS.pRedBCST := ADetModel.Imposto.ICMS.ICMS90.PRedBCST;
  ADetItem.Imposto.ICMS.vBCST := ADetModel.Imposto.ICMS.ICMS90.VBCST;
  ADetItem.Imposto.ICMS.pICMSST := ADetModel.Imposto.ICMS.ICMS90.PICMSST;
  ADetItem.Imposto.ICMS.vICMSST := ADetModel.Imposto.ICMS.ICMS90.VICMSST;
end;

procedure TNFeTagProdICMSRegimeNormal.SetImpostoICMS_Difal(
  const ADetItem: TDetCollectionItem; const ADetModel: TDetModel);
begin
  ADetItem.Imposto.ICMSUFDest.vBCUFDest := ADetModel.Imposto.ICMSUFDest.VBCUFDest;
  ADetItem.Imposto.ICMSUFDest.pICMSInter := ADetModel.Imposto.ICMSUFDest.PICMSInter;
  ADetItem.Imposto.ICMSUFDest.pICMSUFDest := ADetModel.Imposto.ICMSUFDest.PICMSUFDest;
  ADetItem.Imposto.ICMSUFDest.pICMSInterPart := ADetModel.Imposto.ICMSUFDest.PICMSInterPart;
  ADetItem.Imposto.ICMSUFDest.vICMSUFDest := ADetModel.Imposto.ICMSUFDest.VICMSUFDest;
  ADetItem.Imposto.ICMSUFDest.vICMSUFRemet := ADetModel.Imposto.ICMSUFDest.VICMSUFRemet;
  // FCP
  ADetItem.Imposto.ICMSUFDest.vBCFCPUFDest := ADetModel.Imposto.ICMSUFDest.VBCFCPUFDest;
  ADetItem.Imposto.ICMSUFDest.pFCPUFDest := ADetModel.Imposto.ICMSUFDest.PFCPUFDest;
  ADetItem.Imposto.ICMSUFDest.vFCPUFDest := ADetModel.Imposto.ICMSUFDest.VFCPUFDest;
end;

procedure TNFeTagProdICMSRegimeNormal.SetImpostoICMS_Partilha(
  const ADetItem: TDetCollectionItem; const ADetModel: TDetModel);
begin
  if (TpcnDestinoOperacao(FNFeModel.InfNFe.Ide.IdDest) <> doInterestadual) then
    exit;
  if (TpcnindIEDest(FNFeModel.InfNFe.Dest.indIEDest) <> inNaoContribuinte) and
     (TpcnConsumidorFinal(FNFeModel.InfNFe.Ide.indFinal) <> cfConsumidorFinal) then
    exit;
  ADetItem.Imposto.ICMS.modBC := TpcnDeterminacaoBaseIcms(ADetModel.Imposto.ICMS.ICMSPart.ModBC);
  ADetItem.Imposto.ICMS.pRedBC := ADetModel.Imposto.ICMS.ICMSPart.PRedBC;
  ADetItem.Imposto.ICMS.vBC := ADetModel.Imposto.ICMS.ICMSPart.VBC;
  ADetItem.Imposto.ICMS.pICMS := ADetModel.Imposto.ICMS.ICMSPart.PICMS;
  ADetItem.Imposto.ICMS.vICMS := ADetModel.Imposto.ICMS.ICMSPart.VICMS;
  ADetItem.Imposto.ICMS.modBCST := TpcnDeterminacaoBaseIcmsST(ADetModel.Imposto.ICMS.ICMSPart.ModBCST);
  ADetItem.Imposto.ICMS.pMVAST := ADetModel.Imposto.ICMS.ICMSPart.PMVAST;
  ADetItem.Imposto.ICMS.pRedBCST := ADetModel.Imposto.ICMS.ICMSPart.PRedBCST;
  ADetItem.Imposto.ICMS.vBCST := ADetModel.Imposto.ICMS.ICMSPart.VBCST;
  ADetItem.Imposto.ICMS.pICMSST := ADetModel.Imposto.ICMS.ICMSPart.PICMSST;
  ADetItem.Imposto.ICMS.vICMSST := ADetModel.Imposto.ICMS.ICMSPart.VICMSST;
  if (ADetItem.Imposto.ICMS.CST = cst10 ) or (ADetItem.Imposto.ICMS.CST = cst90) then
  begin
    ADetItem.Imposto.ICMS.UFST := ADetModel.Imposto.ICMS.ICMSPart.UFST;
    ADetItem.Imposto.ICMS.pBCOp := ADetModel.Imposto.ICMS.ICMSPart.PBCOp;
  end;
end;

end.
