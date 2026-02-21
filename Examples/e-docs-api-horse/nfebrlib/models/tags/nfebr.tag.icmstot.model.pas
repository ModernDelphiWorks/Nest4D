unit nfebr.tag.icmstot.model;

interface

type
  TICMSTotModel = class
  private
    FVBC: Currency;
    FVBCST: Currency;
    FVCOFINS: Currency;
    FVDesc: Currency;
    FVFCP: Currency;
    FVFCPST: Currency;
    FVFCPSTRet: Currency;
    FVFCPUFDest: Currency;
    FVFrete: Currency;
    FVICMS: Currency;
    FVICMSDeson: Currency;
    FVICMSMono: Currency;
    FVICMSMonoRet: Currency;
    FVICMSMonoReten: Currency;
    FVICMSUFDest: Currency;
    FVICMSUFRemet: Currency;
    FVII: Currency;
    FVIPI: Currency;
    FVIPIDevol: Currency;
    FVNF: Currency;
    FVOutro: Currency;
    FVPIS: Currency;
    FVProd: Currency;
    FVST: Currency;
    FVSeg: Currency;
    FVTotTrib: Currency;
  public
    property VBC: Currency read FVBC write FVBC;
    property VBCST: Currency read FVBCST write FVBCST;
    property VCOFINS: Currency read FVCOFINS write FVCOFINS;
    property VDesc: Currency read FVDesc write FVDesc;
    property VFCP: Currency read FVFCP write FVFCP;
    property VFCPST: Currency read FVFCPST write FVFCPST;
    property VFCPSTRet: Currency read FVFCPSTRet write FVFCPSTRet;
    property VFCPUFDest: Currency read FVFCPUFDest write FVFCPUFDest;
    property VFrete: Currency read FVFrete write FVFrete;
    property VICMS: Currency read FVICMS write FVICMS;
    property VICMSDeson: Currency read FVICMSDeson write FVICMSDeson;
    property VICMSMono: Currency read FVICMSMono write FVICMSMono;
    property VICMSMonoRet: Currency read FVICMSMonoRet write FVICMSMonoRet;
    property VICMSMonoReten: Currency read FVICMSMonoReten write FVICMSMonoReten;
    property VICMSUFDest: Currency read FVICMSUFDest write FVICMSUFDest;
    property VICMSUFRemet: Currency read FVICMSUFRemet write FVICMSUFRemet;
    property VII: Currency read FVII write FVII;
    property VIPI: Currency read FVIPI write FVIPI;
    property VIPIDevol: Currency read FVIPIDevol write FVIPIDevol;
    property VNF: Currency read FVNF write FVNF;
    property VOutro: Currency read FVOutro write FVOutro;
    property VPIS: Currency read FVPIS write FVPIS;
    property VProd: Currency read FVProd write FVProd;
    property VST: Currency read FVST write FVST;
    property VSeg: Currency read FVSeg write FVSeg;
    property VTotTrib: Currency read FVTotTrib write FVTotTrib;
  end;

implementation

end.

