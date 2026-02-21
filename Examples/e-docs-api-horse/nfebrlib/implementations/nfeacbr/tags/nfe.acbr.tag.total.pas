unit nfe.acbr.tag.total;

interface

uses
  SysUtils,
  ACBrNFe,
  ACBrNFe.Classes,
  pcnConversaoNFe,
  pcnConversao,
  nfebr.lib.enum,
  nfebr.model,
  nfe.acbr.interfaces;

type
  TNFeTagTotal = class(TInterfacedObject, INFeExecute)
  private
    FNFe: TACBrNFe;
  public
    class function Factory(ADFe: TACBrNFe): INFeExecute;
    constructor Create(ADFe: TACBrNFe);
    destructor Destroy; override;
    procedure Execute(const AIndex: integer; const ANFeModel: TNFeModel);
  end;

implementation

{ TDFeTagTotal }

constructor TNFeTagTotal.Create(ADFe: TACBrNFe);
begin
  FNFe := ADFe;
end;

destructor TNFeTagTotal.Destroy;
begin
  FNFe := nil;
  inherited;
end;

procedure TNFeTagTotal.Execute(const AIndex: integer; const ANFeModel: TNFeModel);
var
  LICMSTot: TICMSTot;
begin
  LICMSTot := FNFe.NotasFiscais.Items[AIndex].NFe.Total.ICMSTot;
  // TOTALIZAÇÃO
  LICMSTot.vNF := ANFeModel.InfNFe.Total.ICMSTot.VNF;
  LICMSTot.vProd := ANFeModel.InfNFe.Total.ICMSTot.VProd;
  LICMSTot.vFrete := ANFeModel.InfNFe.Total.ICMSTot.VFrete;
  LICMSTot.vSeg := ANFeModel.InfNFe.Total.ICMSTot.VSeg;
  LICMSTot.vOutro := ANFeModel.InfNFe.Total.ICMSTot.VOutro;
  LICMSTot.vDesc := ANFeModel.InfNFe.Total.ICMSTot.VDesc;
  // ICMS
  LICMSTot.vBC := ANFeModel.InfNFe.Total.ICMSTot.VBC;
  LICMSTot.vICMS := ANFeModel.InfNFe.Total.ICMSTot.VICMS;
  LICMSTot.vICMSDeson := ANFeModel.InfNFe.Total.ICMSTot.VICMSDeson;
  // ICMSST
  LICMSTot.vBCST := ANFeModel.InfNFe.Total.ICMSTot.VBCST;
  LICMSTot.vST := ANFeModel.InfNFe.Total.ICMSTot.VST;
  // IPI
  LICMSTot.vIPI := ANFeModel.InfNFe.Total.ICMSTot.VIPI;
  LICMSTot.vIPIDevol := ANFeModel.InfNFe.Total.ICMSTot.VIPIDevol;
  // PIS
  LICMSTot.vPIS := ANFeModel.InfNFe.Total.ICMSTot.VPIS;
  // COFINS
  LICMSTot.vCOFINS := ANFeModel.InfNFe.Total.ICMSTot.VCOFINS;
  // DE OLHO NO IMPOSTO
  LICMSTot.vTotTrib := ANFeModel.InfNFe.Total.ICMSTot.VTotTrib;
  // IMPORTAÇÃO
  LICMSTot.vII := ANFeModel.InfNFe.Total.ICMSTot.VII;
  // PARTILHA ICMS INTERESTADUAL
  LICMSTot.vICMSUFDest := ANFeModel.InfNFe.Total.ICMSTot.VICMSUFDest;
  LICMSTot.vICMSUFRemet := ANFeModel.InfNFe.Total.ICMSTot.VICMSUFRemet;
  // FUNDO DE COMBATE A POBREZA
  LICMSTot.vFCP := ANFeModel.InfNFe.Total.ICMSTot.VFCP;
  LICMSTot.vFCPUFDest := ANFeModel.InfNFe.Total.ICMSTot.VFCPUFDest;
  // FCPST
  LICMSTot.vFCPST := ANFeModel.InfNFe.Total.ICMSTot.VFCPST;
  LICMSTot.vFCPSTRet := ANFeModel.InfNFe.Total.ICMSTot.VFCPSTRet;
end;

class function TNFeTagTotal.Factory(ADFe: TACBrNFe): INFeExecute;
begin
  Result := Self.Create(ADFe);
end;

end.
