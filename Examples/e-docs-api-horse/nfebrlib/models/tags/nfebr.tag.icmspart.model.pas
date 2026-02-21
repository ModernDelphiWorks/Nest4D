unit nfebr.tag.icmspart.model;

interface

type
  TICMSPartModel = class
  private
    FModBC: integer;
    FModBCST: integer;
    FPBCOp: Double;
    FPFCPST: Double;
    FPICMS: Double;
    FPICMSST: Double;
    FPMVAST: Double;
    FPRedBC: Double;
    FPRedBCST: Double;
    FUFST: String;
    FVBC: Currency;
    FVBCFCPST: Currency;
    FVBCST: Currency;
    FVFCPST: Currency;
    FVICMS: Currency;
    FVICMSST: Currency;
  public
    property ModBC: integer read FModBC write FModBC;
    property ModBCST: integer read FModBCST write FModBCST;
    property PBCOp: Double read FPBCOp write FPBCOp;
    property PFCPST: Double read FPFCPST write FPFCPST;
    property PICMS: Double read FPICMS write FPICMS;
    property PICMSST: Double read FPICMSST write FPICMSST;
    property PMVAST: Double read FPMVAST write FPMVAST;
    property PRedBC: Double read FPRedBC write FPRedBC;
    property PRedBCST: Double read FPRedBCST write FPRedBCST;
    property VBC: Currency read FVBC write FVBC;
    property UFST: String read FUFST write FUFST;
    property VBCFCPST: Currency read FVBCFCPST write FVBCFCPST;
    property VBCST: Currency read FVBCST write FVBCST;
    property VFCPST: Currency read FVFCPST write FVFCPST;
    property VICMS: Currency read FVICMS write FVICMS;
    property VICMSST: Currency read FVICMSST write FVICMSST;
  end;

implementation

end.

