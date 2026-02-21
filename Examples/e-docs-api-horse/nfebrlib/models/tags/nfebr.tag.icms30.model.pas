unit nfebr.tag.icms30.model;

interface

type
  TICMS30Model = class
  private
    FModBCST: integer;
    FMotDesICMS: integer;
    FPFCPST: Double;
    FPICMSST: Double;
    FPMVAST: Double;
    FPRedBCST: Double;
    FVBCFCPST: Currency;
    FVBCST: Currency;
    FVFCPST: Currency;
    FVICMSDeson: Currency;
    FVICMSST: Currency;
  public
    property ModBCST: integer read FModBCST write FModBCST;
    property MotDesICMS: integer read FMotDesICMS write FMotDesICMS;
    property PFCPST: Double read FPFCPST write FPFCPST;
    property PICMSST: Double read FPICMSST write FPICMSST;
    property PMVAST: Double read FPMVAST write FPMVAST;
    property PRedBCST: Double read FPRedBCST write FPRedBCST;
    property VBCFCPST: Currency read FVBCFCPST write FVBCFCPST;
    property VBCST: Currency read FVBCST write FVBCST;
    property VFCPST: Currency read FVFCPST write FVFCPST;
    property VICMSDeson: Currency read FVICMSDeson write FVICMSDeson;
    property VICMSST: Currency read FVICMSST write FVICMSST;
  end;

implementation

end.

