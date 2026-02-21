unit nfebr.tag.icms90.model;

interface

type
  TICMS90Model = class
  private
    FModBC: integer;
    FModBCST: integer;
    FMotDesICMS: integer;
    FMotDesICMSST: integer;
    FPFCP: Double;
    FPFCPST: Double;
    FPICMS: Double;
    FPICMSST: Double;
    FPMVAST: Double;
    FPRedBC: Double;
    FPRedBCST: Double;
    FVBC: Currency;
    FVBCFCP: Currency;
    FVBCFCPST: Currency;
    FVBCST: Currency;
    FVFCP: Currency;
    FVFCPST: Currency;
    FVICMS: Currency;
    FVICMSDeson: Currency;
    FVICMSST: Currency;
    FVICMSSTDeson: Currency;
  public
    property ModBC: integer read FModBC write FModBC;
    property ModBCST: integer read FModBCST write FModBCST;
    property MotDesICMS: integer read FMotDesICMS write FMotDesICMS;
    property MotDesICMSST: integer read FMotDesICMSST write FMotDesICMSST;
    property PFCP: Double read FPFCP write FPFCP;
    property PFCPST: Double read FPFCPST write FPFCPST;
    property PICMS: Double read FPICMS write FPICMS;
    property PICMSST: Double read FPICMSST write FPICMSST;
    property PMVAST: Double read FPMVAST write FPMVAST;
    property PRedBC: Double read FPRedBC write FPRedBC;
    property PRedBCST: Double read FPRedBCST write FPRedBCST;
    property VBC: Currency read FVBC write FVBC;
    property VBCFCP: Currency read FVBCFCP write FVBCFCP;
    property VBCFCPST: Currency read FVBCFCPST write FVBCFCPST;
    property VBCST: Currency read FVBCST write FVBCST;
    property VFCP: Currency read FVFCP write FVFCP;
    property VFCPST: Currency read FVFCPST write FVFCPST;
    property VICMS: Currency read FVICMS write FVICMS;
    property VICMSDeson: Currency read FVICMSDeson write FVICMSDeson;
    property VICMSST: Currency read FVICMSST write FVICMSST;
    property VICMSSTDeson: Currency read FVICMSSTDeson write FVICMSSTDeson;
  end;

implementation

end.

