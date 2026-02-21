unit nfebr.tag.icms00.model;

interface

type
  TICMS00Model = class
  private
    FModBC: integer;
    FPFCP: Double;
    FPICMS: Double;
    FVBC: Currency;
    FVFCP: Currency;
    FVICMS: Currency;
  public
    property ModBC: integer read FModBC write FModBC;
    property PFCP: Double read FPFCP write FPFCP;
    property PICMS: Double read FPICMS write FPICMS;
    property VBC: Currency read FVBC write FVBC;
    property VFCP: Currency read FVFCP write FVFCP;
    property VICMS: Currency read FVICMS write FVICMS;
  end;

implementation

end.

