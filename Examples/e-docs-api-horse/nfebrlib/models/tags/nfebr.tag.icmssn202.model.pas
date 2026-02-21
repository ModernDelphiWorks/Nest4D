unit nfebr.tag.icmssn202.model;

interface

type
  TICMSSN202Model = class
  private
    FModBCST: integer;
    FPFCPST: Double;
    FPICMSST: Double;
    FPMVAST: Double;
    FPRedBCST: Double;
    FVBCFCPST: Currency;
    FVBCST: Currency;
    FVFCPST: Currency;
    FVICMSST: Currency;
  public
    property ModBCST: integer read FModBCST write FModBCST;
    property PFCPST: Double read FPFCPST write FPFCPST;
    property PICMSST: Double read FPICMSST write FPICMSST;
    property PMVAST: Double read FPMVAST write FPMVAST;
    property PRedBCST: Double read FPRedBCST write FPRedBCST;
    property VBCFCPST: Currency read FVBCFCPST write FVBCFCPST;
    property VBCST: Currency read FVBCST write FVBCST;
    property VFCPST: Currency read FVFCPST write FVFCPST;
    property VICMSST: Currency read FVICMSST write FVICMSST;
  end;

implementation

end.

