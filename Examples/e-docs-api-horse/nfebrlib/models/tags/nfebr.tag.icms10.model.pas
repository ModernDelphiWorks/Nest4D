unit nfebr.tag.icms10.model;

interface

type
  TICMS10Model = class
  private
    FModBC: integer;
    FModBCST: integer;
    FMotDesICMSST: integer;
    FPFCP: Double;
    FPFCPST: Double;
    FPICMS: Double;
    FPICMSST: Double;
    FPMVAST: Double;
    FPRedBCST: Double;
    FVBC: Currency;
    FVBCFCP: Currency;
    FVBCFCPST: Currency;
    FVBCST: Currency;
    FVFCP: Currency;
    FVFCPST: Currency;
    FVICMS: Currency;
    FVICMSST: Currency;
    FVICMSSTDeson: Currency;
  public
    property ModBC: integer read FModBC write FModBC;
    property ModBCST: integer read FModBCST write FModBCST;
    property MotDesICMSST: integer read FMotDesICMSST write FMotDesICMSST;
    property PFCP: Double read FPFCP write FPFCP;
    property PFCPST: Double read FPFCPST write FPFCPST;
    property PICMS: Double read FPICMS write FPICMS;
    property PICMSST: Double read FPICMSST write FPICMSST;
    property PMVAST: Double read FPMVAST write FPMVAST;
    property PRedBCST: Double read FPRedBCST write FPRedBCST;
    property VBC: Currency read FVBC write FVBC;
    property VBCFCP: Currency read FVBCFCP write FVBCFCP;
    property VBCFCPST: Currency read FVBCFCPST write FVBCFCPST;
    property VBCST: Currency read FVBCST write FVBCST;
    property VFCP: Currency read FVFCP write FVFCP;
    property VFCPST: Currency read FVFCPST write FVFCPST;
    property VICMS: Currency read FVICMS write FVICMS;
    property VICMSST: Currency read FVICMSST write FVICMSST;
    property VICMSSTDeson: Currency read FVICMSSTDeson write FVICMSSTDeson;
  end;

implementation

end.

