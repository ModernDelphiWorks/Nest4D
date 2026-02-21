unit nfebr.tag.icmssn900.model;

interface

type
  TICMSSN900Model = class
  private
    FModBC: integer;
    FModBCST: integer;
    FPCredSN: Double;
    FPFCPST: Double;
    FPICMS: Double;
    FPICMSST: Double;
    FPMVAST: Double;
    FPRedBC: Double;
    FPRedBCST: Double;
    FVBC: Currency;
    FVBCFCPST: Currency;
    FVBCST: Currency;
    FVCredICMSSN: Currency;
    FVFCPST: Currency;
    FVICMS: Currency;
    FVICMSST: Currency;
  public
    property ModBC: integer read FModBC write FModBC;
    property ModBCST: integer read FModBCST write FModBCST;
    property PCredSN: Double read FPCredSN write FPCredSN;
    property PFCPST: Double read FPFCPST write FPFCPST;
    property PICMS: Double read FPICMS write FPICMS;
    property PICMSST: Double read FPICMSST write FPICMSST;
    property PMVAST: Double read FPMVAST write FPMVAST;
    property PRedBC: Double read FPRedBC write FPRedBC;
    property PRedBCST: Double read FPRedBCST write FPRedBCST;
    property VBC: Currency read FVBC write FVBC;
    property VBCFCPST: Currency read FVBCFCPST write FVBCFCPST;
    property VBCST: Currency read FVBCST write FVBCST;
    property VCredICMSSN: Currency read FVCredICMSSN write FVCredICMSSN;
    property VFCPST: Currency read FVFCPST write FVFCPST;
    property VICMS: Currency read FVICMS write FVICMS;
    property VICMSST: Currency read FVICMSST write FVICMSST;
  end;

implementation

end.

