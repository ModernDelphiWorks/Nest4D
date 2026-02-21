unit nfe.acbr.tag.prod.icms.simplesnacional;

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
  TNFeTagProdICMSSimplesNacional = class(TInterfacedObject, INFeImposto)
  private
    FNFeModel: TNFeModel;
    procedure SetImpostoICMS_CSOSN101(const ADetItem: TDetCollectionItem;
      const ADetModel: TDetModel);
    procedure SetImpostoICMS_CSOSN102(const ADetItem: TDetCollectionItem;
      const ADetModel: TDetModel);
    procedure SetImpostoICMS_CSOSN103(const ADetItem: TDetCollectionItem;
      const ADetModel: TDetModel);
    procedure SetImpostoICMS_CSOSN201(const ADetItem: TDetCollectionItem;
      const ADetModel: TDetModel);
    procedure SetImpostoICMS_CSOSN202(const ADetItem: TDetCollectionItem;
      const ADetModel: TDetModel);
//    procedure SetImpostoICMS_CSOSN203(const ADetItem: TDetCollectionItem;
//      const ADetModel: TDetModel);
    procedure SetImpostoICMS_CSOSN300(const ADetItem: TDetCollectionItem;
      const ADetModel: TDetModel);
    procedure SetImpostoICMS_CSOSN400(const ADetItem: TDetCollectionItem;
      const ADetModel: TDetModel);
    procedure SetImpostoICMS_CSOSN500(const ADetItem: TDetCollectionItem;
      const ADetModel: TDetModel);
    procedure SetImpostoICMS_CSOSN900(const ADetItem: TDetCollectionItem;
      const ADetModel: TDetModel);
    procedure SetImpostoICMSST_Repasse(const ADetItem: TDetCollectionItem;
      const ADetModel: TDetModel);
    procedure SetImpostoICMS_Partilha(const ADetItem: TDetCollectionItem;
      const ADetModel: TDetModel);
  public
    class function New(const AACBrNFe: TACBrNFe): INFeImposto;
    constructor Create(const AACBrNFe: TACBrNFe);
    destructor Destroy; override;
    procedure Execute(const ADetItem: TDetCollectionItem;
      const ANFeModel: TNFeModel; const ADetModel: TDetModel);
  end;

implementation

{ TDFeTagProdICMSSimplesNacional }

constructor TNFeTagProdICMSSimplesNacional.Create(const AACBrNFe: TACBrNFe);
begin

end;

destructor TNFeTagProdICMSSimplesNacional.Destroy;
begin
  FNFeModel := nil;
  inherited;
end;

procedure TNFeTagProdICMSSimplesNacional.Execute(const ADetItem: TDetCollectionItem;
  const ANFeModel: TNFeModel; const ADetModel: TDetModel);
var
  LIsOK: Boolean;
begin
  FNFeModel := ANFeModel;
  //
  ADetItem.Imposto.ICMS.Orig := TpcnOrigemMercadoria(ADetModel.Imposto.ICMS.Orig);
  ADetItem.Imposto.ICMS.CSOSN := StrToCSOSNIcms(LIsOK, ADetModel.Imposto.ICMS.CSOSN);
  case ADetItem.Imposto.ICMS.CSOSN of
    csosn101: SetImpostoICMS_CSOSN101(ADetItem, ADetModel);
    csosn102: SetImpostoICMS_CSOSN102(ADetItem, ADetModel);
    csosn103: SetImpostoICMS_CSOSN103(ADetItem, ADetModel);
    csosn201: SetImpostoICMS_CSOSN201(ADetItem, ADetModel);
    csosn202: SetImpostoICMS_CSOSN202(ADetItem, ADetModel);
//    csosn203: SetImpostoICMS_CSOSN203(ADetItem, ADetModel);
    csosn300: SetImpostoICMS_CSOSN300(ADetItem, ADetModel);
    csosn400: SetImpostoICMS_CSOSN400(ADetItem, ADetModel);
    csosn500: SetImpostoICMS_CSOSN500(ADetItem, ADetModel);
    csosn900: SetImpostoICMS_CSOSN900(ADetItem, ADetModel);
  end;
  SetImpostoICMS_Partilha(ADetItem, ADetModel);
  ADetItem.Imposto.vTotTrib := ADetModel.Imposto.VTotTrib;
end;

class function TNFeTagProdICMSSimplesNacional.New(const AACBrNFe: TACBrNFe): INFeImposto;
begin
  Result := Self.Create(AACBrNFe);
end;

procedure TNFeTagProdICMSSimplesNacional.SetImpostoICMSST_Repasse(
  const ADetItem: TDetCollectionItem; const ADetModel: TDetModel);
begin

end;

procedure TNFeTagProdICMSSimplesNacional.SetImpostoICMS_CSOSN101(
  const ADetItem: TDetCollectionItem;
  const ADetModel: TDetModel);
begin
  ADetItem.Imposto.ICMS.pCredSN     := ADetModel.Imposto.ICMS.ICMSSN101.PCredSN;
  ADetItem.Imposto.ICMS.vCredICMSSN := ADetModel.Imposto.ICMS.ICMSSN101.VCredICMSSN;
end;

procedure TNFeTagProdICMSSimplesNacional.SetImpostoICMS_CSOSN102(
  const ADetItem: TDetCollectionItem;
  const ADetModel: TDetModel);
begin
  ADetItem.Imposto.ICMS.pCredSN     := 0;
  ADetItem.Imposto.ICMS.vCredICMSSN := 0;
end;

procedure TNFeTagProdICMSSimplesNacional.SetImpostoICMS_CSOSN103(
  const ADetItem: TDetCollectionItem;
  const ADetModel: TDetModel);
begin
  ADetItem.Imposto.ICMS.pCredSN     := 0;
  ADetItem.Imposto.ICMS.vCredICMSSN := 0;
end;

procedure TNFeTagProdICMSSimplesNacional.SetImpostoICMS_CSOSN201(
  const ADetItem: TDetCollectionItem;
  const ADetModel: TDetModel);
begin
  // ICMSST
  ADetItem.Imposto.ICMS.modBCST  := TpcnDeterminacaoBaseIcmsST(ADetModel.Imposto.ICMS.ICMSSN201.ModBCST);
  ADetItem.Imposto.ICMS.pMVAST   := ADetModel.Imposto.ICMS.ICMSSN201.PMVAST;
  ADetItem.Imposto.ICMS.pRedBCST := ADetModel.Imposto.ICMS.ICMSSN201.PRedBCST;
  ADetItem.Imposto.ICMS.vBCST    := ADetModel.Imposto.ICMS.ICMSSN201.VBCST;
  ADetItem.Imposto.ICMS.pICMSST  := ADetModel.Imposto.ICMS.ICMSSN201.PICMSST;
  ADetItem.Imposto.ICMS.vICMSST  := ADetModel.Imposto.ICMS.ICMSSN201.VICMSST;
  // Crédito SN
  ADetItem.Imposto.ICMS.pCredSN     := ADetModel.Imposto.ICMS.ICMSSN201.PCredSN;
  ADetItem.Imposto.ICMS.vCredICMSSN := ADetModel.Imposto.ICMS.ICMSSN201.VCredICMSSN;
end;

procedure TNFeTagProdICMSSimplesNacional.SetImpostoICMS_CSOSN202(
  const ADetItem: TDetCollectionItem;
  const ADetModel: TDetModel);
begin
  // ICMSST
  ADetItem.Imposto.ICMS.modBCST  := TpcnDeterminacaoBaseIcmsST(ADetModel.Imposto.ICMS.ICMSSN202.ModBCST);
  ADetItem.Imposto.ICMS.pMVAST   := ADetModel.Imposto.ICMS.ICMSSN202.PMVAST;
  ADetItem.Imposto.ICMS.pRedBCST := ADetModel.Imposto.ICMS.ICMSSN202.PRedBCST;
  ADetItem.Imposto.ICMS.vBCST    := ADetModel.Imposto.ICMS.ICMSSN202.VBCST;
  ADetItem.Imposto.ICMS.pICMSST  := ADetModel.Imposto.ICMS.ICMSSN202.PICMSST;
  ADetItem.Imposto.ICMS.vICMSST  := ADetModel.Imposto.ICMS.ICMSSN202.VICMSST;
end;

//procedure TNFeTagProdICMSSimplesNacional.SetImpostoICMS_CSOSN203(
//  const ADetItem: TDetCollectionItem;
//  const ADetModel: TDetModel);
//begin
//  // ICMSST
//  ADetItem.Imposto.ICMS.modBCST  := ADetModel.Imposto.ICMS.ICMSSN203.
//  ADetItem.Imposto.ICMS.pMVAST   := ADFeDataSet.FieldByName('DFE_ICMSSTMVA').AsFloat;
//  ADetItem.Imposto.ICMS.pRedBCST := ADFeDataSet.FieldByName('DFE_ICMSSTBASEREDUCAO').AsFloat;
//  ADetItem.Imposto.ICMS.vBCST    := ADFeDataSet.FieldByName('DFE_ICMSSTBASE').AsFloat;
//  ADetItem.Imposto.ICMS.pICMSST  := ADFeDataSet.FieldByName('DFE_ICMSSTALIQUOTA').AsFloat;
//  ADetItem.Imposto.ICMS.vICMSST  := ADFeDataSet.FieldByName('DFE_ICMSST').AsFloat;
//  // FCP Retido
//  ADetItem.Imposto.ICMS.vBCFCPST := ADFeDataSet.FieldByName('DFE_FCPSTBASE').AsFloat;
//  ADetItem.Imposto.ICMS.pFCPST   := ADFeDataSet.FieldByName('DFE_FCPSTALIQUOTA').AsFloat;
//  ADetItem.Imposto.ICMS.vFCPST   := ADFeDataSet.FieldByName('DFE_FCPST').AsFloat;
//end;

procedure TNFeTagProdICMSSimplesNacional.SetImpostoICMS_CSOSN300(
  const ADetItem: TDetCollectionItem;
  const ADetModel: TDetModel);
begin
  ADetItem.Imposto.ICMS.pCredSN     := 0;
  ADetItem.Imposto.ICMS.vCredICMSSN := 0;
end;

procedure TNFeTagProdICMSSimplesNacional.SetImpostoICMS_CSOSN400(
  const ADetItem: TDetCollectionItem;
  const ADetModel: TDetModel);
begin
  ADetItem.Imposto.ICMS.pCredSN     := 0;
  ADetItem.Imposto.ICMS.vCredICMSSN := 0;
end;

procedure TNFeTagProdICMSSimplesNacional.SetImpostoICMS_CSOSN500(
  const ADetItem: TDetCollectionItem;
  const ADetModel: TDetModel);
begin
  ADetItem.Imposto.ICMS.vBCSTRet := ADetModel.Imposto.ICMS.ICMSSN500.VBCSTRet;
  ADetItem.Imposto.ICMS.vICMSSTRet := ADetModel.Imposto.ICMS.ICMSSN500.VICMSSTRet;
end;

procedure TNFeTagProdICMSSimplesNacional.SetImpostoICMS_CSOSN900(
  const ADetItem: TDetCollectionItem;
  const ADetModel: TDetModel);
begin
  ADetItem.Imposto.ICMS.modBC := TpcnDeterminacaoBaseIcms(ADetModel.Imposto.ICMS.ICMSSN900.ModBC);
  ADetItem.Imposto.ICMS.pRedBC := ADetModel.Imposto.ICMS.ICMSSN900.PRedBC;
  ADetItem.Imposto.ICMS.vBC := ADetModel.Imposto.ICMS.ICMSSN900.VBC;
  ADetItem.Imposto.ICMS.pICMS := ADetModel.Imposto.ICMS.ICMSSN900.PICMS;
  ADetItem.Imposto.ICMS.vICMS := ADetModel.Imposto.ICMS.ICMSSN900.VICMS;
  // ICMSST
  ADetItem.Imposto.ICMS.modBCST := TpcnDeterminacaoBaseIcmsST(ADetModel.Imposto.ICMS.ICMSSN900.ModBCST);
  ADetItem.Imposto.ICMS.pMVAST := ADetModel.Imposto.ICMS.ICMSSN900.PMVAST;
  ADetItem.Imposto.ICMS.pRedBCST := ADetModel.Imposto.ICMS.ICMSSN900.PRedBCST;
  ADetItem.Imposto.ICMS.vBCST := ADetModel.Imposto.ICMS.ICMSSN900.VBCST;
  ADetItem.Imposto.ICMS.pICMSST := ADetModel.Imposto.ICMS.ICMSSN900.PICMSST;
  ADetItem.Imposto.ICMS.vICMSST := ADetModel.Imposto.ICMS.ICMSSN900.VICMSST;
  // SN
  ADetItem.Imposto.ICMS.pCredSN := ADetModel.Imposto.ICMS.ICMSSN900.PCredSN;
  ADetItem.Imposto.ICMS.vCredICMSSN := ADetModel.Imposto.ICMS.ICMSSN900.VCredICMSSN;;
end;

procedure TNFeTagProdICMSSimplesNacional.SetImpostoICMS_Partilha(
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
  if (ADetItem.Imposto.ICMS.CSOSN = csosn500) or (ADetItem.Imposto.ICMS.CSOSN = csosn900) then
  begin
    ADetItem.Imposto.ICMS.UFST := ADetModel.Imposto.ICMS.ICMSPart.UFST;
    ADetItem.Imposto.ICMS.pBCOp := ADetModel.Imposto.ICMS.ICMSPart.PBCOp;
  end;
end;

end.
