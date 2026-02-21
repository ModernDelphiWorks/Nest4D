unit nfebr.tag.icmssn201.model;

interface

type
  TICMSSN201Model = class
  private
    FModBCST: integer;
    FPCredSN: Double;
    FPFCPST: Double;
    FPICMSST: Double;
    FPMVAST: Double;
    FPRedBCST: Double;
    FVBCFCPST: Currency;
    FVBCST: Currency;
    FVCredICMSSN: Currency;
    FVFCPST: Currency;
    FVICMSST: Currency;
  public
    property ModBCST: integer read FModBCST write FModBCST;
    property PCredSN: Double read FPCredSN write FPCredSN;
    property PFCPST: Double read FPFCPST write FPFCPST;
    property PICMSST: Double read FPICMSST write FPICMSST;
    property PMVAST: Double read FPMVAST write FPMVAST;
    property PRedBCST: Double read FPRedBCST write FPRedBCST;
    property VBCFCPST: Currency read FVBCFCPST write FVBCFCPST;
    property VBCST: Currency read FVBCST write FVBCST;
    property VCredICMSSN: Currency read FVCredICMSSN write FVCredICMSSN;
    property VFCPST: Currency read FVFCPST write FVFCPST;
    property VICMSST: Currency read FVICMSST write FVICMSST;
  end;


implementation

end.

